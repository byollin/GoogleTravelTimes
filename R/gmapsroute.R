suppressWarnings(suppressMessages({
    library(xml2)
    library(RCurl)
}))

gmapsroute = function(origin, destination, waypoints = '', mode = 'driving', key = '',
                      alternatives = FALSE, avoid = '', departure = 'now', dep_date = '',
                      dep_time = '', traffic_model = 'best_guess', arrival = '', arr_date = '',
                      arr_time = '', transit_mode = '', transit_routing_preference = '') {
    
    # VALIDATE INPUT PARAMETERS ########################################################################################
    
    if (!(mode %in% c('driving',  'walking',  'bicycling',  'transit'))) {
        stop(
            'Mode of transportation not recognized. Mode should be one of: ',
            '\'bicycling\', \'transit\', \'driving\', \'walking\'.'
        )
    }
    
    if (!(avoid %in% c('', 'tolls',  'highways',  'ferries',  'indoor'))) {
        stop(
            'Avoid parameters not recognized. Avoid should be one of: ',
            '\'tolls\', \'highways\', \'ferries\', \'indoor\'.'
        )
    }
    
    if (!(traffic_model %in% c('best_guess',  'pessimistic', 'optimistic'))) {
        stop(
            'Traffic model not recognized. Traffic model should be one of: ',
            '\'best_guess\', \'pessimistic\', \'optimistic\'.'
        )
    }
    
    ####################################################################################################################
    
    # VALIDATE DEPARTURE AND ARRIVAL TIMES #############################################################################
    
    seconds         = 'now'
    seconds_arrival = ''
    
    UTCtime  = strptime('1970-01-01 00:00:00', '%Y-%m-%d %H:%M:%OS', tz = 'GMT')
    min_secs = round(as.numeric(difftime(as.POSIXlt(Sys.time(), 'GMT'), UTCtime, units = 'secs')))
    
    # convert departure time to seconds after January 1, 1970, 00:00:00 UCT
    if(dep_date != '' && dep_time != '') {
        depart = strptime(paste(dep_date, dep_time), '%Y-%m-%d %H:%M:%OS', tz='GMT')
        seconds = round(as.numeric(difftime(depart, UTCtime, units = 'secs')))
    }
    
    if(departure != 'now') {
        seconds = departure
    }
    
    if(departure != 'now' && departure < min_secs){
        stop('The departure time has to be some time in the future!')
    }
    
    if(dep_date != '' && dep_time == '') {
        stop('You should also specify a departure time in the format HH:MM:SS UTC')
    }
    
    if(dep_date == '' && dep_time != '') {
        stop('You should also specify a departure date in the format YYYY-MM-DD UTC')
    }
    
    if(dep_date != '' && dep_time != '' && seconds < min_secs) {
        stop('The departure time has to be some time in the future!')
    }
    
    # convert arrival time to seconds after January 1, 1970, 00:00:00 UCT
    if(arr_date != '' && arr_time != '') {
        arriv = strptime(paste(arr_date, arr_time), '%Y-%m-%d %H:%M:%OS', tz='GMT')
        seconds_arrival = round(as.numeric(difftime(arriv, UTCtime, units = 'secs')))
    }
    
    if(arrival != '') {
        seconds_arrival = arrival
    }
    
    if(arrival != '' && arrival < min_secs) {
        stop('The arrival time has to be some time in the future!')
    }
    
    if(arr_date != '' && arr_time == '') {
        stop('You should also specify an arrival time in the format HH:MM:SS UTC')
    }
    
    if(arr_date == '' && arr_time != '') {
        stop('You should also specify an arrival date in the format YYYY-MM-DD UTC')
    }
    
    if(arr_date != '' && arr_time != '' && seconds_arrival < min_secs) {
        stop('The arrival time has to be some time in the future!')
    }
    
    if((dep_date != '' || dep_time != '' || departure != 'now') && (arr_date != '' || arr_time != '' || arrival != '')) {
        stop('Cannot input departure and arrival times. Only one can be used at a time. ')
    }
    
    ####################################################################################################################
    
    data = data.frame('or' = origin, 'de' = destination)
    n    = dim(data)
    n    = n[1]
    
    data$Time     = NA
    data$Distance = NA
    data$Status   = 'OK'
    data$Route    = NA
    avoidmsg      = ''
    
    if(avoid != '') {
        avoidmsg = paste0('&avoid=', avoid)
    }
    
    # GENERATE REQUESTS ################################################################################################
        
    # construct url
    url = paste0('maps.googleapis.com/maps/api/directions/xml?origin=', origin,
                 '&destination=', destination,
                 '&waypoints=', waypoints,
                 '&mode=', mode,
                 '&units=metric',
                 '&departure_time=', seconds,
                 '&traffic_model=', traffic_model,
                 avoidmsg)
    
    # if key is provided...
    if (!is.null(key)) {
        # use https and google maps key
        key = gsub(' ', '', key)
        url = paste0('https://', url, '&key=', key)
    } else {
        # use http otherwise
        url = paste0('http://', url)
    }
    
    ################################################################################################################
    
    # PARSE RESPONSE ###############################################################################################
    
    # call web service and store XML
    webpageXML   = read_xml(getURL(url, .mapUnicode = F))
    error_nodes  = webpageXML %>% xml_find_all("//error_message") %>% xml_text()
    status_nodes = webpageXML %>% xml_find_all("//status") %>% xml_text()
    
    if (!length(error_nodes) == 0) {
        stop(paste(c('Google Directions API returned an error: ', error_nodes), sep = ''))
    }
    
    if (status_nodes == 'REQUEST_DENIED') {
        set.api.key(NULL)
        data$Status = 'REQUEST_DENIED'
    }
    
    if (status_nodes == 'ZERO_RESULTS') {
        data$Status = 'ROUTE_NOT_FOUND'
    }
    
    if (status_nodes == 'NOT_FOUND') {
        data$Status = 'PLACE_NOT_FOUND'
    }
    
    # check quota
    if (status_nodes == 'OVER_QUERY_LIMIT') {
        data$Status = 'OVER_QUERY_LIMIT'
    }
    
    if(status_nodes == 'OK') {
        data$Status    = 'OK'
        duration_nodes = webpageXML %>% xml_find_all("//route/leg/duration_in_traffic/value") %>% xml_integer()
        data$Time      = duration_nodes
        distance_nodes = webpageXML %>% xml_find_all("//route/leg/distance/value") %>% xml_integer()
        data$Distance  = distance_nodes
        route_node     = webpageXML %>% xml_find_all("//route/overview_polyline/points") %>% xml_text()
        data$Route     = route_node
    }
    
    # RETURN ###########################################################################################################
    
    output = list(Time     = data$Time,
                  Distance = data$Distance,
                  Status   = data$Status,
                  Route    = data$Route)
    
    ####################################################################################################################
    
    return(output)
    
}
