suppressWarnings(suppressMessages({
    library(shiny)
    library(htmlwidgets)
    library(shinyjs)
    library(shinydashboard)
    library(shinyWidgets)
    library(shinyBS)
    library(shinycssloaders)
    library(DT)
    library(leaflet)
    library(googleway)
    library(tidyr)
    library(dplyr)
    library(lubridate)
    library(stringr)
    library(RPostgreSQL)
}))

source('R/gmapsroute.R')
source('R/get_travel_times.R')

# source server modules
source('modules/server/add_waypoint_div.R')
source('modules/server/render_contact_table.R')
source('modules/server/send_error_message.R')
source('modules/server/test_route.R')
source('modules/server/toggle_markers.R')
source('modules/server/validate_coords.R')
source('modules/server/validate_inputs.R')
source('modules/server/via_paste.R')

# configuration
price_multiplier = 10
test_key = readLines('_conf/api_key.txt')
db_pw = readLines('_conf/db_password.txt')
tzs              = OlsonNames() %>% str_replace_all('_', ' ')
origin_icon      = makeAwesomeIcon(icon = 'circle', markerColor = 'green', library = 'fa', iconColor = '#ffffff')
destination_icon = makeAwesomeIcon(icon = 'circle', markerColor = 'red', library = 'fa', iconColor = '#ffffff')
waypoint_icon    = makeAwesomeIcon(icon = 'circle', markerColor = 'blue', library = 'fa', iconColor = '#ffffff')
system_date      = system("date '+%Y-%m-%d'", intern = T) %>% ymd()

# https://github.com/diafygi/webrtc-ips
# only works on Chrome and Firefox
detect_ip = "shinyjs.getIP = function() {
                window.RTCPeerConnection = window.RTCPeerConnection || window.mozRTCPeerConnection || window.webkitRTCPeerConnection;
                var pc = new RTCPeerConnection({iceServers:[]}), noop = function() {};      
                pc.createDataChannel('');
                pc.createOffer(pc.setLocalDescription.bind(pc), noop);
                pc.onicecandidate = function(ice) {
                    if (ice && ice.candidate && ice.candidate.candidate) {
                        var client_ip = /([0-9]{1,3}(\\.[0-9]{1,3}){3}|[a-f0-9]{1,4}(:[a-f0-9]{1,4}){7})/.exec(ice.candidate.candidate)[1];
                        Shiny.onInputChange(\"getIP\", client_ip);
                        pc.onicecandidate = noop;
                    }
                };
            };"

confirm_requests = function(start_date, end_date, time_period_1, time_period_2, interval, days_of_week, traffic_model,
                            tz, coords, key, session, client_info) {
    
    tz       = str_replace_all(tz, ' ', '_')
    date_seq = seq(ymd(start_date, tz = tz), ymd(end_date, tz = tz), by = '1 day')
    date_seq = date_seq[weekdays(date_seq) %in% days_of_week]
    time_seq = lapply(date_seq, function(x) { seq(ymd_hms(paste0(x, ' ', time_period_1[1], ':00:00'), tz = tz),
                                                  ymd_hms(paste0(x, ' ', time_period_1[2], ':00:00'), tz = tz),
                                                  by = interval) })
    time_seq = do.call('c', time_seq)
    if(time_period_2 != '') {
        time_seq_2 = lapply(date_seq, function(x) { seq(ymd_hms(paste0(x, ' ', time_period_2[1], ':00:00'), tz = tz),
                                                        ymd_hms(paste0(x, ' ', time_period_2[2], ':00:00'), tz = tz),
                                                        by = interval) })
        time_seq_2 = do.call('c', time_seq_2)
        time_seq   = c(time_seq, time_seq_2) %>% sort()
    }
    coords          = str_replace_all(coords, ' ', '')
    od_pairs        = data.frame('o' = coords[1:(length(coords) - 1)], 'd' = coords[2:length(coords)],
                                 segment = seq(1:(length(coords) - 1)), stringsAsFactors = FALSE)
    cross_df        = crossing(od_pairs, time_seq, traffic_model)
    names(cross_df) = c('o', 'd', 'segment', 'departure_time', 'traffic_model')
    
    price = format(price_multiplier * ceiling(nrow(cross_df) / 1000), digits = 2, nsmall = 2, decimal.mark = '.',
                   big.mark = ',', scientific = FALSE)
    client_info$count = nrow(cross_df)
    
    confirm_text = paste0('You are about to submit a request for ', format(nrow(cross_df), big.mark = ',',
                                                                           scientific = FALSE),
                          ' travel times. The maximum charge against your API key will be $', price,
                          '. \nAre you sure you wish to proceed?')
    confirmSweetAlert(session, 'confirm', text = confirm_text, type = 'warning', danger_mode = TRUE, html = TRUE)
    
}

progress_message = function(progress, msg, sleep, close) {
    progress$status$set(message = msg)
    Sys.sleep(sleep)
    if(close) {
        progress$status$close()   
    }
}

make_cluster = function() {
    cores   = detectCores()
    cluster = makeCluster(cores[1] - 1, outfile = '')
    registerDoParallel(cluster)
    pids = foreach(i = 1:3, .combine=c) %dopar% {
        Sys.getpid()
    }
    return(pids)
}