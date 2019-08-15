#' test_route
#' 
#' Test the route
#' 
#' @param session Shiny session
#' @param origin origin coordinates
#' @param destination destination coordinates
#' @param waypoints vector of waypoint coordinates
#' @param key Google Directions API key
#' 
#' @return TRUE or FALSE

test_route = function(session, origin, destination, waypoints, key = test_key) {
    route_is_valid = FALSE
    success        = FALSE
    if(key != '') {
        if(validate_coords(origin)) {
            origin = origin %>% str_replace(' ', '')
            if(validate_coords(destination)) {
                destination = destination %>% str_replace(' ', '')
                if(!is.null(waypoints)) {
                    waypoints = sapply(waypoints,
                                       function(x) if(validate_coords(x)) { x %>% str_replace(' ', '') } else { '' })
                    if('' %in% waypoints) {
                        send_error_message(session, text = tags$span(tags$span('Invalid waypoint coordinates. Provide \
                                                                               coordinates in the following format: '),
                                                                     tags$code('latitude, longitude'), tags$span('.')))
                    } else {
                        route_is_valid = TRUE
                    }
                } else {
                    route_is_valid = TRUE
                }
            } else {
                destination = ''
                send_error_message(session, text = tags$span(tags$span('Invalid destination coordinates. Provide \
                                                                       coordinates in the following format: '),
                                                             tags$code('latitude, longitude'), tags$span('.')))
            }
        } else {
            origin = ''
            send_error_message(session, text = tags$span(tags$span('Invalid origin coordinates. Provide coordinates in \
                                                                   the following format: '),
                                                         tags$code('latitude, longitude'), tags$span('.')))
        }
    } else {
        send_error_message(session, text = tags$span(tags$span('Please provide an API key to process your requests.')))
    }
    if(route_is_valid) {
        request = try(gmapsroute(origin = origin, destination = destination, waypoints = via_paste(waypoints),
                                 key = key), silent = TRUE)
        if(!'try-error' %in% class(request)) {
            if(request$Status == 'OK') {
                decoded_polyline = decode_pl(request$Route)
                travel_time = round(request$Time / 60, digits = 0)
                popup_content = paste0('<b>Travel Time: </b>', travel_time, ' minutes')
                leafletProxy('map') %>% addPolylines(lng = decoded_polyline$lon, lat = decoded_polyline$lat,
                                                     layerId = 'route', popup = popup_content)
                success = TRUE
            } else {
                send_error_message(session, text = tags$span('Request failed. Google Directions API returned the \
                                                             following status code: ', request$Status))
            }
        } else {
            leafletProxy('map') %>% removeShape('route')
            send_error_message(session, text = tags$span(tags$span('Request failed with the following exception: '),
                                                         tags$pre(request[1])))
        }
    }
    return(success)
}
