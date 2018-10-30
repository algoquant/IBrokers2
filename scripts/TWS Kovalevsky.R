# Copyright (C) 2018 Stanislav Kovalevsky

Tws = R6::R6Class( 'Tws', lock_objects = F )

Tws$set( 'public', 'connect', function( port = 4001 ) {
  
  if( is.null( self$tws ) || !IBrokers::isConnected( self$tws ) ) {
    
    self$tws = IBrokers::twsConnect( clientId = 0, port = port )
    
  }
  
  self$last_update_orders     = Sys.time()
  self$last_update_executions = Sys.time()
  
} )
Tws$set( 'public', 'update_positions', function( ) {
  
  sink("/dev/null")
  
  ac = IBrokers::reqAccountUpdates( self$tws );
  
  self$ac        = ac
  self$positions = IBrokers::twsPortfolioValue( ac )
  
  sink()
  
} )
Tws$set( 'public', 'get_tws_info', function( outgoing, incoming, end, verbose = F ) {
  
  con = self$tws[[1]]
  # send request
  writeBin( c( outgoing, ver = '1' ), con )
  
  eWrapper = IBrokers::eWrapper()
  socketSelect( list( con ), F, NULL )
  processed_messages = list()
  k = 1
  # read messages until end message found
  while( T ) {
    
    current_message = readBin( con, character(), 1 )
    
    if( length( current_message ) != 0 ) { 
      
      sink("/dev/null"); processed_message = IBrokers::processMsg( current_message, con, eWrapper ); sink();
      
      if ( current_message == incoming ) { processed_messages[[ k ]] = processed_message; k = k + 1; } else { if( current_message == end ) break }
      
    }
    
  }
  
  return( processed_messages )
  
} )

Tws$set( 'public', 'update_executions', function() {
  
  if( Sys.time() - self$last_update_executions < as.difftime( 0.1, units = 'secs' ) ) return()
  
  # NOTE
  # use last execution message if multiple executions present for single order:
  #    orderId clientId                  execId               time acctNumber exchange side shares   price     permId liquidation cumQty avgPrice
  # 1:   88670        1 0001f4e8.5a1768f8.01.01 20171124  10:44:41   DU758675 IDEALPRO sell  30000 1.18380 1955945889           0  30000 1.183800
  # 2:   88670        1 0001f4e8.5a1768f9.01.01 20171124  10:44:41   DU758675 IDEALPRO sell  75582 1.18379 1955945889           0 105582 1.183793
  # ! second execution contains cumulative quantity and average price
  
  self$executions = self$get_tws_info(
    
    outgoing = IBrokers::.twsOutgoingMSG$'REQ_EXECUTIONS',
    incoming = IBrokers::.twsIncomingMSG$'EXECUTION_DATA',
    end      = IBrokers::.twsIncomingMSG$'EXECUTION_DATA_END'
    
  )
  
  if( length( self$executions ) > 0 ) {
    
    self$executions = lapply( self$executions, function( x ) {
      
      x = x$execution
      class( x ) = NULL
      as.data.table( t( x ) )
      
    } )
    
    self$executions = rbindlist( self$executions )
    self$executions = setDT( lapply( self$executions, unlist ) )
    self$executions = self$executions[, ':='(
      
      side     = ifelse( side == 'BOT', 'buy', 'sell' ),
      cumQty   = as.numeric( cumQty ),
      avgPrice = as.numeric( avgPrice )
      
    ) ][]
    
  } else { 
    
    self$executions = NULL
    
  }
  
  self$last_update_executions = Sys.time()
  
} )

Tws$set( 'public', 'update_orders', function() {
  
  if( Sys.time() - self$last_update_orders < as.difftime( 0.1, units = 'secs' ) ) return()
  
  self$orders = self$get_tws_info( 
    
    outgoing = IBrokers::.twsOutgoingMSG$'REQ_OPEN_ORDERS', 
    incoming = IBrokers::.twsIncomingMSG$'OPEN_ORDER', 
    end      = IBrokers::.twsIncomingMSG$'OPEN_ORDER_END'
    
  )
  
  if( length( self$orders ) > 0 ) {
    
    self$orders = data.table( do.call( rbind, self$orders ) )[, .(
      
      orderId     = V3,
      clientId    = V8,
      execId      = NA,
      time        = V7,
      acctNumber  = V20,
      exchange    = V10,
      side        = V13,
      shares      = V14,
      price       = V16,
      permId      = V25,
      liquidation = NA,
      cumQty      = NA,
      avgPrice    = NA, 
      status      = V77
      
    ) ][, -'status' ][]
    
  } else { 
    
    self$orders = NULL
    
  }
  
  self$last_update_orders = Sys.time()
  
} )
Tws$set( 'public', 'send_order', function( symbol, type, price, side, size ) {
  
  security = switch(
    symbol,

    
    CL = IBrokers::twsFUT( symbol, 'NYMEX', expiry = '20180122', multiplier = '1000', conId = '117170262' ),
    
    stop( paste( symbol, 'security not supported' ) )
  )
  side = ifelse( side == 'buy', 'BUY', 'SELL' )
  size = as.character( size )
  price = as.character( price )
  
  self$order_id = as.numeric( IBrokers::reqIds( self$tws ) )
  
  order = switch(
    type,
    
    MKT = IBrokers::twsOrder( self$order_id, orderType = 'MKT',                   action = side, totalQuantity = size, transmit = T ),
    LMT = IBrokers::twsOrder( self$order_id, orderType = 'LMT', lmtPrice = price, action = side, totalQuantity = size, transmit = T ),
    STP = IBrokers::twsOrder( self$order_id, orderType = 'STP', auxPrice = price, action = side, totalQuantity = size, transmit = T, outsideRTH = '1' ),
    
    stop( paste( type, 'type not supported' ) )
  )
  
  IBrokers::placeOrder( self$tws, security, order )
  return( self$order_id )
  
} )
Tws$set( 'public', 'get_order', function( id_order ) {
  
  self$update_executions()
  execution = if( !is.null( self$executions ) ) self$executions[ orderId == id_order ][ .N ] # .N? see update_executions note
  
  Sys.sleep( 0.1 )
  
  self$update_orders()
  order = if( !is.null( self$orders ) ) self$orders[ orderId == id_order ]
  
  rbind( execution, order )
  
} )
Tws$set( 'public', 'cancel_order', function( id_order ) {
  
  IBrokers::cancelOrder( self$tws, id_order )
  
} )
Tws$set( 'public', 'disconnect', function() {
  
  IBrokers::twsDisconnect( self$tws )
  
} )
