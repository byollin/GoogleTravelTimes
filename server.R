shinyServer(function(input, output, session) {
    
    js$getIP()
    
    client_info     = reactiveValues('local_ip' = NULL, 'count' = NULL)
    waypoint_values = reactiveValues()
    time_period_div = reactiveValues(visible = FALSE)
    
    # keep track of which markers are currently active
    marker_status                      = reactiveValues()
    marker_status[['add_origin']]      = FALSE
    marker_status[['add_destination']] = FALSE
    
    progress = reactiveValues(status = NULL)
    results  = reactiveValues(data = data.frame())
    
    # display lat/lng on map load
    js_code = 'function(el, x) {
                    this.addEventListener("mousemove", function(e) {
                        document.getElementById("coords").innerHTML = e.latlng.lat.toFixed(6) + ", " + e.latlng.lng.toFixed(6);
                    })
                }'
    
    observeEvent(input$getIP, once = TRUE, {
        client_info$local_ip = input$getIP
    })
    
    output$map = renderLeaflet({
        leaflet::leaflet(options = leafletOptions(minZoom = 4, maxZoom = 18,
                                                  zoomControl = FALSE)) %>%
            addTiles(urlTemplate = 'https://maps.wikimedia.org/osm-intl/{z}/{x}/{y}.png',
                     attribution = '<a href="https://maps.wikimedia.org/" title="Wikimedia Maps Beta">Wikimedia Maps</a> | Map data provided by <a href="https://www.openstreetmap.org/copyright" title="OpenStreetMap Contributors">OpenStreetMap © Contributors</a>') %>%
            setView(lng = -122.239144, lat = 47.57552, zoom = 12) %>% onRender(js_code)
    })
    
    observeEvent(input$add_range, {
        shinyjs::show('time_range_div')
        time_period_div$visible = TRUE
    })
    
    observeEvent(input$remove_range, {
        shinyjs::hide('time_range_div')
        time_period_div$visible = FALSE
    })
    
    observeEvent(input$add_waypoint, {
        
        waypoint_layer_id = paste0('waypoint_', input$add_waypoint)
        remove_button     = paste0('remove_waypoint_', input$add_waypoint)
        waypoint_div      = paste0('waypoint_div_', input$add_waypoint)
        waypoint_marker   = paste0('add_waypoint_marker_', input$add_waypoint)
    
        add_waypoint_div(input$add_waypoint)
        
        waypoint_values[[waypoint_div]]  = TRUE
        marker_status[[waypoint_marker]] = TRUE
        
        observeEvent(input[[remove_button]], ignoreInit = TRUE, once = TRUE, {
            removeUI(selector = paste0('#', waypoint_div))
            waypoint_values[[waypoint_div]]  = NULL
            marker_status[[waypoint_marker]] = NULL
            leafletProxy('map') %>% removeMarker(waypoint_layer_id)
            leafletProxy('map') %>% removeShape('route')
        })
        
        observeEvent(input[[waypoint_marker]], ignoreInit = TRUE, {
            if(input[[waypoint_marker]] == TRUE) {
                toggle_markers(session, waypoint_marker, names(marker_status))
                marker_status[[waypoint_marker]] = TRUE
            } else {
                marker_status[[waypoint_marker]] = FALSE
            }
        })
        
        observeEvent(input[[waypoint_layer_id]], ignoreInit = TRUE, {
            if(validate_coords(input[[waypoint_layer_id]])) {
                lat = str_replace(input[[waypoint_layer_id]], ' ', '') %>% str_split(',')
                lat = lat[[1]][1] %>% as.numeric()
                lng = str_replace(input[[waypoint_layer_id]], ' ', '') %>% str_split(',')
                lng = lng[[1]][2] %>% as.numeric()
                leafletProxy('map') %>% addAwesomeMarkers(lng = lng, lat = lat, layerId = waypoint_layer_id,
                                                          icon = waypoint_icon, popup = paste0(lat,', ', lng))
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
            marker_status[['add_origin']] = TRUE
        } else {
            marker_status[['add_origin']] = FALSE
        }
        
    })
    
    observeEvent(input$add_destination, ignoreInit = TRUE, {
        
        if(input$add_destination == TRUE) {
            toggle_markers(session, 'add_destination', names(marker_status))
            marker_status[['add_destination']] = TRUE
        } else {
            marker_status[['add_destination']] = FALSE
        }
        
    })
    
    observeEvent(input$map_click, {
        
        marker_status_list = reactiveValuesToList(marker_status)
        selected_marker    = marker_status_list[sapply(marker_status_list, isTRUE, simplify = T)] %>% names()
        if(length(selected_marker) > 0) {
            if(selected_marker == 'add_origin') {
                lat = round(input$map_click$lat, 6)
                lng = round(input$map_click$lng, 6)
                updateTextInput(session, 'origin', value = paste0(lat, ', ', lng))
            } else if(selected_marker == 'add_destination') {
                lat = round(input$map_click$lat, 6)
                lng = round(input$map_click$lng, 6)
                updateTextInput(session, 'destination', value = paste0(lat, ', ', lng))
            } else {
                lat = round(input$map_click$lat, 6)
                lng = round(input$map_click$lng, 6)
                updateTextInput(session, paste0('waypoint_', str_extract(selected_marker, '\\d{1,}')),
                                value = paste0(lat, ', ', lng))
            }
        }
        
    })
    
    observeEvent(input$test, {
        
        shinyjs::show('spinner')
        
        marker_status_list = reactiveValuesToList(marker_status)
        active_waypoints   = marker_status_list[sapply(marker_status_list,
                                                        function(x) isTRUE(x) | !is.null(x),
                                                        simplify = T)] %>% names()
        marker_nums        = str_extract(active_waypoints, pattern = '\\d{1,}')
        marker_nums        = marker_nums[!is.na(marker_nums)] %>% as.numeric()
        if(length(marker_nums) > 0) {
            waypoints = sapply(paste0('waypoint_', marker_nums), function(x) isolate(input[[x]]) )
        } else {
            waypoints = NULL
        }

        success = test_route(session, isolate(input$origin), isolate(input$destination), waypoints)
        
        # hide spinner
        delay(ms = 50, {
            
            shinyjs::hide('spinner')
            
        })
        
    })
    
    observeEvent(input$origin, ignoreInit = TRUE, {
        if(validate_coords(input$origin)) {
            lat = str_replace(input$origin, ' ', '') %>% str_split(',')
            lat = lat[[1]][1] %>% as.numeric()
            lng = str_replace(input$origin, ' ', '') %>% str_split(',')
            lng = lng[[1]][2] %>% as.numeric()
            leafletProxy('map') %>% addAwesomeMarkers(lng = lng, lat = lat, layerId = 'origin',
                                                      icon = origin_icon, popup = paste0(lat,', ', lng))
            leafletProxy('map') %>% removeShape('route')
        } else {
            leafletProxy('map') %>% removeMarker('origin')
            leafletProxy('map') %>% removeShape('route')
        }
    })
    
    observeEvent(input$destination, ignoreInit = TRUE, {
        if(validate_coords(input$destination)) {
            lat = str_replace(input$destination, ' ', '') %>% str_split(',')
            lat = lat[[1]][1] %>% as.numeric()
            lng = str_replace(input$destination, ' ', '') %>% str_split(',')
            lng = lng[[1]][2] %>% as.numeric()
            leafletProxy('map') %>% addAwesomeMarkers(lng = lng, lat = lat, layerId = 'destination',
                                                      icon = destination_icon, popup = paste0(lat,', ', lng))
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
    
    observeEvent(input$email, ignoreInit = FALSE, {
        if(nchar(input$email) > 0) {
            shinyjs::show('email_message')
        } else {
            shinyjs::hide('email_message')
        }
    })
    
    observeEvent(input$submit, ignoreInit = TRUE, {
        
        shinyjs::show('spinner')
        
        # TODO: eliminate redundancies here
        start_date    = isolate(input$date_range[1])
        end_date      = isolate(input$date_range[2])
        time_period_1 = isolate(input$time_range_1)
        time_period_2 = isolate(input$time_range_2)
        freq          = isolate(input$frequency)
        freq          = switch(freq, '5 minutes' = '5 mins', '10 minutes' = '10 mins',
                               '15 minutes' = '15 mins', '30 minutes' = '30 mins',
                               '45 minutes' = '45 mins', '1 hour' = '1 hour')
        days_of_week  = isolate(input$days_of_week)
        if(!is.null(days_of_week)) {
            days_of_week  = sapply(days_of_week, function(x) switch(x, 'Su' = 'Sunday',
                                                                    'M'  = 'Monday',
                                                                    'T'  = 'Tuesday',
                                                                    'W'  = 'Wednesday',
                                                                    'Th' = 'Thursday',
                                                                    'F'  = 'Friday',
                                                                    'S'  = 'Saturday'))
        }
        traffic_model = isolate(input$traffic_models)
        if(!is.null(traffic_model)) {
            traffic_model = sapply(traffic_model, function(x) switch(x, 'Optimistic' = 'optimistic',
                                                                     'Best Guess' = 'best_guess',
                                                                     'Pessimistic' = 'pessimistic'))
        }
        time_zone = str_replace_all(isolate(input$time_zone), ' ', '_')
        if(time_period_div$visible == FALSE) {
            time_period_2 = ''
        }
            
        marker_status_list = reactiveValuesToList(marker_status)
        active_waypoints   = marker_status_list[sapply(marker_status_list,
                                                        function(x) isTRUE(x) | !is.null(x),
                                                        simplify = T)] %>% names()
        marker_nums        = str_extract(active_waypoints, pattern = '\\d{1,}')
        marker_nums        = marker_nums[!is.na(marker_nums)] %>% as.numeric()
        if(length(marker_nums) > 0) {
            waypoints = sapply(paste0('waypoint_', marker_nums), function(x) isolate(input[[x]]) )
        } else {
            waypoints = NULL
        }
        
        origin      = isolate(input$origin) %>% str_replace(' ', '')
        destination = isolate(input$destination) %>% str_replace(' ', '')
        waypoints   = sapply(waypoints, function(x) if(validate_coords(x)) { x %>% str_replace(' ', '') } else { '' })
        
        coords = c(origin, waypoints, destination)
        
        progress$status = Progress$new(session)
        progress$status$set(message = 'Validating inputs...')
        
        valid_inputs = validate_inputs(session, start_date, end_date, time_period_1,
                                       time_period_2, freq, days_of_week, traffic_model, time_zone,
                                       input$email, input$project, input$desc)
        
        if(valid_inputs) {
            
            progress$status$set(message = 'Testing route...')
        
            success = test_route(session, isolate(input$origin), isolate(input$destination), waypoints,
                                  key = isolate(input$api_key))
            
            if(success) {
                
                # cluster_pids = make_cluster()
                
                confirm_requests(start_date, end_date, time_period_1, time_period_2, freq,
                                 days_of_week, traffic_model, time_zone, coords,
                                 isolate(input$api_key), session, client_info)
                
                progress$status$set(message = 'Please confirm your request...')
                
            } else {
                
                progress$status$set(message = 'Routing failed!')
                Sys.sleep(2)
                progress$status$close()
                
            }
            
        } else {
            
            progress$status$set(message = 'Invalid input parameters!')
            Sys.sleep(2)
            progress$status$close()
            
        }
        
        # hide spinner
        delay(ms = 50, {
            
            shinyjs::hide('spinner')
            
        })
        
    })
    
    observeEvent(input$confirm, ignoreInit = TRUE, {
        
        shinyjs::show('spinner')
        
        if(input$confirm == TRUE) {
            
            # log submitted job
            try(silent = TRUE, {
                db_conn = dbConnect(dbDriver('PostgreSQL'), host = '10.68.193.183', user = 'dig',
                                    password = db_pw, dbname = 'api_data')
                insert = paste0("SELECT log_google_distance(apikey := '", input$api_key, "',
                                ip_address := '", client_info$local_ip, "',
                                email := '", input$email, "',
                                project_no := '", input$project, "',
                                description := '", input$desc, "',
                                rec_count := ", client_info$count, ",
                                status := '", 'SUBMITTED', "')")
                suppressWarnings(dbExecute(db_conn, insert))
                dbDisconnect(db_conn)
            })
            
            start_date    = isolate(input$date_range[1])
            end_date      = isolate(input$date_range[2])
            time_period_1 = isolate(input$time_range_1)
            time_period_2 = isolate(input$time_range_2)
            freq          = isolate(input$frequency)
            freq          = switch(freq, '5 minutes' = '5 mins', '10 minutes' = '10 mins',
                                   '15 minutes' = '15 mins', '30 minutes' = '30 mins',
                                   '45 minutes' = '45 mins', '1 hour' = '1 hour')
            days_of_week  = isolate(input$days_of_week)
            if(!is.null(days_of_week)) {
                days_of_week  = sapply(days_of_week, function(x) switch(x, 'Su' = 'Sunday',
                                                                        'M'  = 'Monday',
                                                                        'T'  = 'Tuesday',
                                                                        'W'  = 'Wednesday',
                                                                        'Th' = 'Thursday',
                                                                        'F'  = 'Friday',
                                                                        'S'  = 'Saturday'))
            }
            traffic_model = isolate(input$traffic_models)
            if(!is.null(traffic_model)) {
                traffic_model = sapply(traffic_model, function(x) switch(x, 'Optimistic' = 'optimistic',
                                                                          'Best Guess' = 'best_guess',
                                                                          'Pessimistic' = 'pessimistic'))
            }
            time_zone = str_replace_all(isolate(input$time_zone), ' ', '_')
            if(time_period_div$visible == FALSE) {
                time_period_2 = ''
            }
            
            marker_status_list = reactiveValuesToList(marker_status)
            active_waypoints   = marker_status_list[sapply(marker_status_list,
                                                            function(x) isTRUE(x) | !is.null(x),
                                                            simplify = T)] %>% names()
            marker_nums        = str_extract(active_waypoints, pattern = '\\d{1,}')
            marker_nums        = marker_nums[!is.na(marker_nums)] %>% as.numeric()
            if(length(marker_nums) > 0) {
                waypoints = sapply(paste0('waypoint_', marker_nums), function(x) isolate(input[[x]]) )
            } else {
                waypoints = NULL
            }
            
            shinyjs::hide('download_div')
            shinyjs::hide('view_results')
            
            progress$status$set(message = 'Requesting...')
            
            origin      = isolate(input$origin) %>% str_replace(' ', '')
            destination = isolate(input$destination) %>% str_replace(' ', '')
            waypoints   = sapply(waypoints, function(x) if(validate_coords(x)) { x %>% str_replace(' ', '') } else { '' })
            
            coords = c(origin, waypoints, destination)
            
            # TODO: IMPLEMENT CANCEL TASK
            tt = travel_times(start_date, end_date, time_period_1, time_period_2, freq, days_of_week, traffic_model,
                              time_zone, coords, isolate(input$api_key), session)
            # log completed job
            try(silent = TRUE, {
                db_conn = dbConnect(dbDriver('PostgreSQL'), host = '10.68.193.183', user = 'dig',
                                    password = db_pw, dbname = 'api_data')
                insert = paste0("SELECT log_google_distance(apikey := '", input$api_key, "',
                                ip_address := '", client_info$local_ip, "',
                                email := '", input$email, "',
                                project_no := '", input$project, "',
                                description := '", input$desc, "',
                                rec_count := ", client_info$count, ",
                                status := '", 'COMPLETED', "')")
                suppressWarnings(dbExecute(db_conn, insert))
                dbDisconnect(db_conn)
            })
            
            results$data = tt
            output$results = renderDataTable(server = FALSE, {
                DT::datatable(results$data, class = 'compact', rownames = F, width = '100%', style = 'bootstrap',
                              extensions = 'Buttons',
                              options = list(dom = 'Bfrtip', pageLength = 20, scrollX = T, ordering = T, scrollY = 450,
                                             fixedHeader = T, deferRender = T,
                                             selection = list(mode = 'single', target = 'row'),
                                             buttons = list(list(extend = 'collection', buttons = c('csv', 'excel'),
                                                       text = 'Download'))))
            })
            shinyjs::show('view_results')
            progress$status$set(message = 'Complete!')
            Sys.sleep(2)
        } else {
            progress$status$set(message = 'Canceling...')
            Sys.sleep(2)
        }
        progress$status$close()
        
        # hide spinner
        delay(ms = 50, { shinyjs::hide('spinner') })
        
    })
    
    observeEvent(input$contact, {
        sendSweetAlert(session = session, title = NULL, text = tags$span(style = 'text-align: left;',
                       tags$h3('Contact Us', style = 'color: #d73926;'),
                       tags$div(id = 'contact_table',
                           renderDataTable(contact, escape = F, rownames = FALSE, selection = 'none',
                                           style = 'bootstrap',
                                           options = list(paging = FALSE, searching = FALSE, dom = 't',
                                                          # remove table header
                                                          initComplete = JS('function(settings, json) {',
                                                                                '$(this.api().table().header()).css({"display": "none"});',
                                                                            '}')))
                        )), html = TRUE, btn_labels = c('Close')
        )
    })
    
    observeEvent(input$view_results, {
        shinyjs::show('results_panel', anim = TRUE, animType = 'slide')
    })
    
    observeEvent(input$close_results, {
        shinyjs::hide('results_panel', anim = TRUE, animType = 'slide')
    })
    
    # add crosshair cursor to map
    shinyjs::runjs('document.getElementById("map").style.cursor = "crosshair"')
    
})