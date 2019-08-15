#-----------------------------------------------------------------------------------------------------------------------
#   Name:    gmapdirections.R
#   Purpose: Query Google Directions API
#
#   Notes: Adapted from package gmapdistance. Available on GitHub and CRAN.
#
#   Calls:
#
#
#   Called By:
#
#-----------------------------------------------------------------------------------------------------------------------

suppressWarnings(suppressMessages({
    library(xml2)
    library(RCurl)
}))

gmapsdirection = function(origin, destination, waypoints = '', mode = 'driving', key = '', departure = 'now',
                          traffic_model = 'best_guess') {
    
    # VALIDATE INPUT PARAMETERS ########################################################################################
    
    if (!(mode %in% c('driving',  'walking',  'bicycling',  'transit'))) {
        stop('Mode of transportation not recognized. Mode should be one of: ',
             '\'bicycling\', \'transit\', \'driving\', \'walking\'.')
    }
    
    if (!(traffic_model %in% c('best_guess',  'pessimistic', 'optimistic'))) {
        stop('Traffic model not recognized. Traffic model should be one of: ',
             '\'best_guess\', \'pessimistic\', \'optimistic\'.')
    }
    
    ####################################################################################################################
    
    # VALIDATE DEPARTURE AND ARRIVAL TIMES #############################################################################
    
    utc_time = strptime('1970-01-01 00:00:00', '%Y-%m-%d %H:%M:%OS', tz = 'GMT')
    min_secs = round(as.numeric(difftime(as.POSIXlt(Sys.time(), 'GMT'), utc_time, units = 'secs')))
    
    if(departure != 'now' && departure < min_secs){
        stop('The departure time has to be some time in the future!')
    }
    
    ####################################################################################################################
    
    data = data.frame(Time = NA, Distance = NA, Status = '')
    
    # GENERATE REQUESTS ################################################################################################
    
    # Set up URL
    url = paste0('maps.googleapis.com/maps/api/directions/xml?origin=', origin,
                 '&destination=', destination,
                 '&waypoints=', waypoints,
                 '&mode=', mode,
                 '&units=metric',
                 '&departure_time=', departure,
                 '&traffic_model=', traffic_model)
    
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
    
    # call web service
    webpageXML   = read_xml(getURL(url, .mapUnicode = F))
    error_nodes  = webpageXML %>% xml_find_all("//error_message") %>% xml_text()
    status_nodes = webpageXML %>% xml_find_all("//status") %>% xml_text()
    
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

    if (status_nodes == 'OVER_QUERY_LIMIT') {
        data$Status = 'OVER_QUERY_LIMIT'
    }
    
    if(status_nodes == 'OK') {
        if (!length(error_nodes) == 0) {
            # stop(paste(c('Google Directions API returned an error: ', error_nodes), sep = ''))
            data$Status = paste0('API_ERROR: ', error_nodes)
        } else {
            data$Status = 'OK'   
        }
        data$Status    = 'OK'
        duration_nodes = webpageXML %>% xml_find_all("//route/leg/duration_in_traffic/value") %>% xml_integer()
        data$Time      = duration_nodes
        distance_nodes = webpageXML %>% xml_find_all("//route/leg/distance/value") %>% xml_integer()
        data$Distance  = distance_nodes
    }
    
    
    # RETURN ###########################################################################################################
    
    output = list(Time     = data$Time,
                  Distance = data$Distance,
                  Status   = data$Status)
    
    ####################################################################################################################
    
    return(output)
    
}
