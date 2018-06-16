origin_icon      <- makeAwesomeIcon(icon = 'circle', markerColor = 'green', library = 'fa', iconColor = '#ffffff')
destination_icon <- makeAwesomeIcon(icon = 'circle', markerColor = 'red', library = 'fa', iconColor = '#ffffff')
waypoint_icon    <- makeAwesomeIcon(icon = 'circle', markerColor = 'blue', library = 'fa', iconColor = '#ffffff')

shinyServer(function(input, output, session) {
    
    waypoint_values <- reactiveValues()
    time_period_div <- reactiveValues(visible = FALSE)
    
    # keep track of which markers are currently active
    marker_status                      <- reactiveValues()
    marker_status[['add_origin']]      <- FALSE
    marker_status[['add_destination']] <- FALSE
    
    # display lat/lng on map load
    js_code <- 'function(el, x) {
                    this.addEventListener("mousemove", function(e) {
                        document.getElementById("info").innerHTML = e.latlng.lat.toFixed(6) + ", " + e.latlng.lng.toFixed(6);
                    })
                }'
    
    output$map <- renderLeaflet({
        
        leaflet::leaflet(options = leafletOptions(minZoom = 4, maxZoom = 18,
                                                  zoomControl = FALSE)) %>%
            addTiles(urlTemplate = 'https://maps.wikimedia.org/osm-intl/{z}/{x}/{y}.png') %>%
            setView(lng = -122.239144, lat = 47.57552, zoom = 12) %>%
            addMiniMap(position = 'bottomleft',
                       tiles = 'https://maps.wikimedia.org/osm-intl/{z}/{x}/{y}.png',
                       toggleDisplay = TRUE) %>% onRender(js_code)
        
    })
    
    observeEvent(input$add_range, {
        shinyjs::show('time_range_div')
        time_period_div$visible <- TRUE
    })
    
    observeEvent(input$remove_range, {
        shinyjs::hide('time_range_div')
        time_period_div$visible <- FALSE
    })
    
    observeEvent(input$add_waypoint, {
        
        waypoint_layer_id <- paste0('waypoint_', input$add_waypoint)
        remove_button     <- paste0('remove_waypoint_', input$add_waypoint)
        waypoint_div      <- paste0('waypoint_div_', input$add_waypoint)
        waypoint_marker   <- paste0('add_waypoint_marker_', input$add_waypoint)
    
        add_waypoint_div(input$add_waypoint)
        
        waypoint_values[[waypoint_div]]  <- TRUE
        marker_status[[waypoint_marker]] <- TRUE
        
        observeEvent(input[[remove_button]], ignoreInit = TRUE, once = TRUE, {
            
            removeUI(selector = paste0('#', waypoint_div))
            waypoint_values[[waypoint_div]]  <- NULL
            marker_status[[waypoint_marker]] <- NULL
            leafletProxy('map') %>% removeMarker(waypoint_layer_id)
            leafletProxy('map') %>% removeShape('route')
            
        })
        
        observeEvent(input[[waypoint_marker]], ignoreInit = TRUE, {
            
            if(input[[waypoint_marker]] == TRUE) {
                toggle_markers(session, waypoint_marker, names(marker_status))
                marker_status[[waypoint_marker]] <- TRUE
            } else {
                marker_status[[waypoint_marker]] <- FALSE
            }
            
        })
        
        observeEvent(input[[waypoint_layer_id]], ignoreInit = TRUE, {
            
            if(validate_coords(input[[waypoint_layer_id]])) {
                
                lat <- str_replace(input[[waypoint_layer_id]], ' ', '') %>% str_split(',')
                lat <- lat[[1]][1] %>% as.numeric()
                lng <- str_replace(input[[waypoint_layer_id]], ' ', '') %>% str_split(',')
                lng <- lng[[1]][2] %>% as.numeric()
                
                leafletProxy('map') %>% addAwesomeMarkers(lng = lng, lat = lat, layerId = waypoint_layer_id,
                                                          icon = waypoint_icon)
                leafletProxy('map') %>% removeShape('route')
                
            } else {
                
                leafletProxy('map') %>% removeMarker(waypoint_layer_id)
                leafletProxy('map') %>% removeShape('route')
                
            }
            
        })
                 
    })
    
    observeEvent(input$add_origin, ignoreInit = TRUE, {
        
        if(input$add_origin == TRUE) {
            toggle_markers(session, 'add_origin', names(marker_status))
            marker_status[['add_origin']] <- TRUE
        } else {
            marker_status[['add_origin']] <- FALSE
        }
        
    })
    
    observeEvent(input$add_destination, ignoreInit = TRUE, {
        
        if(input$add_destination == TRUE) {
            toggle_markers(session, 'add_destination', names(marker_status))
            marker_status[['add_destination']] <- TRUE
        } else {
            marker_status[['add_destination']] <- FALSE
        }
        
    })
    
    observeEvent(input$map_click, {
        
        marker_status_list <- reactiveValuesToList(marker_status)
        selected_marker    <- marker_status_list[sapply(marker_status_list, isTRUE, simplify = T)] %>% names()

        if(length(selected_marker) > 0) {

            if(selected_marker == 'add_origin') {
                
                lat <- round(input$map_click$lat, 6)
                lng <- round(input$map_click$lng, 6)
                
                updateTextInput(session, 'origin', value = paste0(lat, ', ', lng))

            } else if(selected_marker == 'add_destination') {
                
                lat <- round(input$map_click$lat, 6)
                lng <- round(input$map_click$lng, 6)
                
                updateTextInput(session, 'destination', value = paste0(lat, ', ', lng))

            } else {
                
                lat <- round(input$map_click$lat, 6)
                lng <- round(input$map_click$lng, 6)
                
                updateTextInput(session, paste0('waypoint_',
                                                str_extract(selected_marker, '\\d{1,}')),
                                value = paste0(lat, ', ', lng))

            }

        }
        
    })
    
    observeEvent(input$test, {
        
        shinyjs::show('spinner')
        
        marker_status_list <- reactiveValuesToList(marker_status)
        active_waypoints   <- marker_status_list[sapply(marker_status_list,
                                                        function(x) isTRUE(x) | !is.null(x),
                                                        simplify = T)] %>% names()
        marker_nums        <- str_extract(active_waypoints, pattern = '\\d{1,}')
        marker_nums        <- marker_nums[!is.na(marker_nums)] %>% as.numeric()
        if(length(marker_nums) > 0) {
            waypoints <- sapply(paste0('waypoint_', marker_nums), function(x) isolate(input[[x]]) )
        } else {
            waypoints <- NULL
        }

        success <- test_route(session, isolate(input$origin), isolate(input$destination), waypoints)
        
        # hide spinner
        delay(ms = 50, {
            
            shinyjs::hide('spinner')
            
        })
        
    })
    
    observeEvent(input$origin, ignoreInit = TRUE, {
        
        if(validate_coords(input$origin)) {
            
            lat <- str_replace(input$origin, ' ', '') %>% str_split(',')
            lat <- lat[[1]][1] %>% as.numeric()
            lng <- str_replace(input$origin, ' ', '') %>% str_split(',')
            lng <- lng[[1]][2] %>% as.numeric()
            
            leafletProxy('map') %>% addAwesomeMarkers(lng = lng, lat = lat, layerId = 'origin',
                                                      icon = origin_icon)
            leafletProxy('map') %>% removeShape('route')
            
        } else {
            
            leafletProxy('map') %>% removeMarker('origin')
            leafletProxy('map') %>% removeShape('route')
            
        }
        
    })
    
    observeEvent(input$destination, ignoreInit = TRUE, {
        
        if(validate_coords(input$destination)) {
            
            lat <- str_replace(input$destination, ' ', '') %>% str_split(',')
            lat <- lat[[1]][1] %>% as.numeric()
            lng <- str_replace(input$destination, ' ', '') %>% str_split(',')
            lng <- lng[[1]][2] %>% as.numeric()
            
            leafletProxy('map') %>% addAwesomeMarkers(lng = lng, lat = lat, layerId = 'destination',
                                                      icon = destination_icon)
            leafletProxy('map') %>% removeShape('route')
            
        } else {
            
            leafletProxy('map') %>% removeMarker('destination')
            leafletProxy('map') %>% removeShape('route')
            
        }
        
    })
    
    observeEvent(input$api_key, ignoreInit = FALSE, {
        if(nchar(input$api_key) > 0) {
            shinyjs::show('api_message')
        } else {
            shinyjs::hide('api_message')
        }
    })
    
    observeEvent(input$submit, ignoreInit = TRUE, {
        
        shinyjs::show('spinner')
        
        start_date    <- isolate(input$date_range[1])
        end_date      <- isolate(input$date_range[2])
        time_period_1 <- isolate(input$time_range_1)
        time_period_2 <- isolate(input$time_range_2)
        freq          <- isolate(input$frequency)
        freq          <- switch(freq, '5 minutes' = '5 mins', '10 minutes' = '10 mins',
                                '15 minutes' = '15 mins', '30 minutes' = '30 mins',
                                '45 minutes' = '45 mins', '1 hour' = '1 hour')
        days_of_week  <- isolate(input$days_of_week)
        if(!is.null(days_of_week)) {
            days_of_week  <- sapply(days_of_week, function(x) switch(x, 'Su' = 'Sunday',
                                                                     'M'  = 'Monday', 'T'  = 'Tuesday',
                                                                     'W'  = 'Wednesday',
                                                                     'Th' = 'Thursday', 'F'  = 'Friday',
                                                                     'S'  = 'Saturday'))
        }
        traffic_model <- isolate(input$traffic_models)
        if(!is.null(traffic_model)) {
            traffic_model <- switch(traffic_model, 'Optimistic' = 'optimistic',
                                    'Best Guess' = 'best_guess', 'Pessimistic' = 'pessimistic')
        }
        time_zone <- str_replace_all(isolate(input$time_zone), ' ', '_')
        if(time_period_div$visible == FALSE) {
            time_period_2 <- ''
        }
            
        marker_status_list <- reactiveValuesToList(marker_status)
        active_waypoints   <- marker_status_list[sapply(marker_status_list,
                                                        function(x) isTRUE(x) | !is.null(x),
                                                        simplify = T)] %>% names()
        marker_nums        <- str_extract(active_waypoints, pattern = '\\d{1,}')
        marker_nums        <- marker_nums[!is.na(marker_nums)] %>% as.numeric()
        if(length(marker_nums) > 0) {
            waypoints <- sapply(paste0('waypoint_', marker_nums), function(x) isolate(input[[x]]) )
        } else {
            waypoints <- NULL
        }
        
        valid_inputs <- validate_inputs(session, start_date, end_date, time_period_1,
                                        time_period_2, freq, days_of_week, traffic_model, time_zone)
        
        if(valid_inputs) {
        
            success <- test_route(session, isolate(input$origin), isolate(input$destination), waypoints,
                                  key = isolate(input$api_key))
            
            if(success) {
                
                # cluster_pids <- make_cluster()
                
                origin      <- isolate(input$origin) %>% str_replace(' ', '')
                destination <- isolate(input$destination) %>% str_replace(' ', '')
                waypoints   <- sapply(waypoints, function(x) if(validate_coords(x)) { x %>% str_replace(' ', '') } else { '' })
                
                coords <- c(origin, waypoints, destination)
                
                # TODO: implement cancel task
                tt <<- travel_times(start_date, end_date, time_period_1, time_period_2, freq,
                                    days_of_week, traffic_model, time_zone, coords,
                                    isolate(input$api_key), session)
                
            }
            
        }
        
        # hide spinner
        delay(ms = 50, {
            
            shinyjs::hide('spinner')
            
        })
        
    })
    
    shinyjs::runjs('document.getElementById("map").style.cursor = "crosshair"')
    
})