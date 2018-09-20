suppressWarnings(suppressMessages({
    library(shiny)
    library(htmlwidgets)
    library(shinyjs)
    library(shinyWidgets)
    library(shinyBS)
    library(shinycssloaders)
    library(leaflet)
    library(googleway)
    library(stringr)
}))

source('R/gmapsroute.R')
source('R/get_travel_times.R')

test_key         <- readLines('_conf/api_key.txt')
tzs              <- OlsonNames() %>% str_replace_all('_', ' ')
price_multiplier <- 5

contact = data.frame('Name' = c('<b>Ryan Avery</b>', '<b>Nicholas Richter</b>', '<b>Bethany Yollin</b>', '<b>Yihong Zou</b>'),
                     'Title' = c('Lead Transportation Planner', 'Systems Analyst', 'Data Scientist', 'Transportation Planner'),
                     'Email' = c(paste(tags$a(href="mailto:ryan.avery@wsp.com", target="_blank", icon('envelope'))),
                                 paste(tags$a(href="mailto:nicholas.richter@wsp.com", target="_blank", icon('envelope'))),
                                 paste(tags$a(href="mailto:bethany.yollin@wsp.com", target="_blank", icon('envelope'))),
                                 paste(tags$a(href="mailto:yihong.zou@wsp.com", target="_blank", icon('envelope')))
                     )
)

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

download_handler <- function(file_prefix, last_updated_timestamp, data) {
    
    downloadHandler(filename = paste0(file_prefix, '_', create_timestamp(last_updated_timestamp), '.csv'),
                    content = function(file) {
                        write.csv(data, file, row.names = FALSE)
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
                travel_time      <- round(request$Time / 60, digits = 0)
                popup_content    <- paste0('<b>Travel Time: </b>', travel_time, ' minutes')
                leafletProxy('map') %>% addPolylines(lng = decoded_polyline$lon,
                                                     lat = decoded_polyline$lat, layerId = 'route',
                                                     popup = popup_content)
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
    if(start_date > Sys.time() | end_date > Sys.time()) {
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
        
    } else {
        sendSweetAlert(session, title = '', text = tags$span(tags$span('Date range must be sometime in the future.')),
                       type = 'error', btn_labels = 'OK', html = TRUE)
    }
    return(valid_inputs)
}

confirm_requests <- function(start_date, end_date, time_period_1, time_period_2, interval, days_of_week,
                             traffic_model, tz, coords, key, session) {
    
    tz       <- str_replace_all(tz, ' ', '_')
    date_seq <- seq(ymd(start_date, tz = tz), ymd(end_date, tz = tz), by = '1 day')
    date_seq <- date_seq[weekdays(date_seq) %in% days_of_week]
    time_seq <- lapply(date_seq, function(x) { seq(ymd_hms(paste0(x, ' ', time_period_1[1], ':00:00'), tz = tz),
                                                   ymd_hms(paste0(x, ' ', time_period_1[2], ':00:00'), tz = tz),
                                                   by = interval) })
    time_seq        <- do.call('c', time_seq)
    if(time_period_2 != '') {
        time_seq_2 <- lapply(date_seq, function(x) { seq(ymd_hms(paste0(x, ' ', time_period_2[1], ':00:00'), tz = tz),
                                                         ymd_hms(paste0(x, ' ', time_period_2[2], ':00:00'), tz = tz),
                                                         by = interval) })
        time_seq_2 <- do.call('c', time_seq_2)
        time_seq   <- c(time_seq, time_seq_2) %>% sort()
    }
    coords          <- str_replace_all(coords, ' ', '')
    od_pairs        <- data.frame('o' = coords[1:(length(coords) - 1)], 'd' = coords[2:length(coords)],
                                  segment = seq(1:(length(coords) - 1)), stringsAsFactors = FALSE)
    cross_df        <- crossing(od_pairs, time_seq, traffic_model)
    names(cross_df) <- c('o', 'd', 'segment', 'departure_time', 'traffic_model')
    
    price <- format(price_multiplier * ceiling(nrow(cross_df) / 1000), digits = 2, nsmall = 2,
                    decimal.mark = '.', big.mark = ',', scientific = FALSE)
    
    confirm_text <- paste0('You are about to submit a request for ', format(nrow(cross_df),
                                                                            big.mark = ',',
                                                                            scientific = FALSE),
                           ' travel times. The maximum charge against your API key will be $',
                           price, '. \nAre you sure you wish to proceed?')
    confirmSweetAlert(session, 'confirm', text = confirm_text, type = 'warning', danger_mode = TRUE, html = TRUE)
    
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