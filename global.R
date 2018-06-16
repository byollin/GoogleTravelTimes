library(htmlwidgets)
library(shinyjs)
library(shinyWidgets)
library(shinyBS)
library(shinycssloaders)
library(leaflet)
library(googleway)
library(stringr)

source('R/gmapsroute.R')
source('R/get_travel_times.R')

test_key <- readLines('_conf/api_key.txt')
tzs      <- OlsonNames() %>% str_replace_all('_', ' ')

add_waypoint_div <- function(id) {
    insertUI(selector = '#add_waypoint', where = 'beforeBegin', ui = {
        div(id = paste0('waypoint_div_', id),
            div(style = 'display: inline-block; vertical-align:bottom; min-width: 100%; overflow: hidden;',
            column(10, style = 'padding-left: 0',
                textInput(paste0('waypoint_', id), label = NULL, placeholder = 'Waypoint', width = '100%')
            ),
            column(1, style = 'padding-left: 0; z-index: 300;  margin-left: -18px;',
                br(),
                actionLink(paste0('remove_waypoint_', id), label = '',
                           icon = icon('minus-circle', class = 'fa-lg alignleft'))
            ),
            column(1, style = 'margin-top: -22px; margin-left: -20px',
                br(),
                prettyToggle(paste0('add_waypoint_marker_', id), label_on = NULL, label_off = NULL,
                             outline = TRUE, plain = TRUE, icon_on = icon('map-marker'), 
                             icon_off = icon('map-marker'), status_on = 'danger',
                             status_off = 'info', bigger = TRUE, inline = TRUE, value = TRUE)
            )
        ),
        br())
    })
}

toggle_markers <- function(session, selected_id, ids) {
    ids <- ids[ids != selected_id]
    lapply(ids, function(x) updatePrettyToggle(session, x, value = FALSE))
}

validate_coords <- function(coords) {
    str_detect(coords, '^[-+]?([1-8]?\\d(\\.\\d+)?|90(\\.0+)?)\\s*,\\s*[-+]?(180(\\.0+)?|((1[0-7]\\d)|([1-9]?\\d))(\\.\\d+)?)$')
}

via_paste <- function(x) {
    x   <- x[!is.na(x)]
    ret <- paste0(x, collapse = '|via:')
    paste0('via:', ret)
}

test_route <- function(session, origin, destination, waypoints, key = test_key) {
    route_is_valid <- FALSE
    success        <- FALSE
    if(key != '') {
        if(validate_coords(origin)) {
            origin <- origin %>% str_replace(' ', '')
            if(validate_coords(destination)) {
                destination <- destination %>% str_replace(' ', '')
                if(!is.null(waypoints)) {
                    waypoints <- sapply(waypoints, function(x) if(validate_coords(x)) { x %>% str_replace(' ', '') } else { '' })
                    if('' %in% waypoints) {
                        sendSweetAlert(session, title = '',
                                       text = tags$span(tags$span('Invalid waypoint coordinates. Provide coordinates in the following format: '),
                                                        tags$code('latitude, longitude'), tags$span('.')),
                                       type = 'error', btn_labels = 'OK', html = TRUE)
                    } else {
                        route_is_valid <- TRUE
                    }
                } else {
                    route_is_valid <- TRUE
                }
            } else {
                destination <- ''
                sendSweetAlert(session, title = '', text = tags$span(tags$span('Invalid destination coordinates. Provide coordinates in the following format: '),
                                                                     tags$code('latitude, longitude'), tags$span('.')), type = 'error', btn_labels = 'OK', html = TRUE)
            }
        } else {
            origin <- ''
            sendSweetAlert(session, title = '', text = tags$span(tags$span('Invalid origin coordinates. Provide coordinates in the following format: '),
                                                                 tags$code('latitude, longitude'), tags$span('.')), type = 'error', btn_labels = 'OK', html = TRUE)
        }
    } else {
        sendSweetAlert(session, title = '', text = tags$span(tags$span('Please provide an API key to process your requests.')),
                       type = 'error', btn_labels = 'OK', html = TRUE)
    }
    if(route_is_valid) {
        request <- try(gmapsroute(origin = origin, destination = destination, waypoints = via_paste(waypoints), key = key),
                       silent = TRUE)
        if(!'try-error' %in% class(request)) {
            if(request$Status == 'OK') {
                decoded_polyline <- decode_pl(request$Route)
                travel_time      <- request$Time / 60 %>% round(digits = 2)
                # TODO: customize popup content
                leafletProxy('map') %>% addPolylines(lng = decoded_polyline$lon,
                                                     lat = decoded_polyline$lat, layerId = 'route', popup = paste0('Travel Time: ', travel_time, ' minutes'))
                success <- TRUE
            } else {
                sendSweetAlert(session, title = '', text = tags$span('Request failed. Google Directions API returned the following status code: ', request$Status),
                               type = 'error', btn_labels = 'OK', html = TRUE)
            }
        } else {
            leafletProxy('map') %>% removeShape('route')
            sendSweetAlert(session, title = '', text = tags$span(tags$span('Request failed with the following exception: '),
                                                                 tags$pre(request[1])),
                           type = 'error', btn_labels = 'OK', html = TRUE)
        }
    }
    return(success)
}

validate_inputs <- function(session, start_date, end_date, time_period_1, time_period_2, freq, days_of_week,
                            traffic_model, tz) {
    
    valid_inputs <- FALSE
    if(start_date <= end_date) {
        date_range <- seq(ymd(start_date), ymd(end_date), by = '1 day')
        if(!length(tz) == 0) {
            if(!is.null(traffic_model)) {
                if(TRUE %in% (days_of_week %in% weekdays(date_range))) {
                    valid_inputs <- TRUE
                } else {
                    sendSweetAlert(session, title = '', text = tags$span(tags$span('Date range must contain selected days of the week.')),
                                   type = 'error', btn_labels = 'OK', html = TRUE)
                }
            } else {
                sendSweetAlert(session, title = '', text = tags$span(tags$span('At least one traffic model must be selected.')),
                               type = 'error', btn_labels = 'OK', html = TRUE)
            }
        } else {
            sendSweetAlert(session, title = '', text = tags$span(tags$span('A time zone must be selected.')),
                           type = 'error', btn_labels = 'OK', html = TRUE)
        }
        
    } else {
        sendSweetAlert(session, title = '', text = tags$span(tags$span('End date must be later than start date.')),
                       type = 'error', btn_labels = 'OK', html = TRUE)
        
    }
    return(valid_inputs)
    
}

make_cluster <- function() {
    cores   <- detectCores()
    cluster <- makeCluster(cores[1] - 1, outfile = '')
    registerDoParallel(cluster)
    pids <- foreach(i = 1:3, .combine=c) %dopar% {
        Sys.getpid()
    }
    return(pids)
}