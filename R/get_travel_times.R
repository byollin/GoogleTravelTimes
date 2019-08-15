suppressWarnings(suppressMessages({
    library(foreach)
    library(doParallel)
}))

source('R/gmapdirections.R')

travel_times = function(start_date, end_date, time_period_1, time_period_2, interval, days_of_week, traffic_model, tz,
                        coords, key) {
    
    get_departure_offset = function(i, cum_tt, res) {
        if (i == 1) {
            cum_tt + 0
        } else if ('try-error' %in% class(res)) {
            cum_tt + 0
        } else if (res$Status != 'OK') {
            cum_tt + 0
        } else {
            cum_tt + res$Time
        }
    }
    
    tz       = str_replace_all(tz, ' ', '_')
    date_seq = seq(ymd(start_date, tz = tz), ymd(end_date, tz = tz), by = '1 day')
    date_seq = date_seq[weekdays(date_seq) %in% days_of_week]
    time_seq = lapply(date_seq, function(x) { seq(ymd_hms(paste0(x, ' ', time_period_1[1], ':00:00'), tz = tz),
                                                  ymd_hms(paste0(x, ' ', time_period_1[2], ':00:00'), tz = tz),
                                                  by = interval) })
    time_seq = do.call('c', time_seq)
    if(length(time_period_2) > 1) {
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

    indx = select(cross_df, traffic_model, departure_time) %>%
                group_by(traffic_model, departure_time) %>%
                arrange(traffic_model, departure_time) %>% distinct()
    
    trip_results = data.frame(o = numeric(), d = numeric(), segment = numeric(),
                               departure_time = character(), traffic_model = character(),
                               computed_departure = character(), status = character(),
                               tt = numeric(), distance = numeric())
    
    cores   = detectCores()
    # outfile = '/dev/null' prevents package messages and warnings from appearing in the log files
    cluster = makeCluster(cores[1] - 1, outfile = '/dev/null')
    registerDoParallel(cluster)
    on.exit(stopCluster(cluster))
    
    num_error   = 0
    par_results = foreach(i = 1:nrow(indx), .combine = 'rbind', .packages = c('dplyr', 'xml2', 'RCurl', 'foreach', 'shiny'),
                          .errorhandling = 'stop', .export = c('gmapsdirection')) %dopar% {
        
        curr_model   = indx[[i, 1]]
        curr_time    = indx[[i, 2]]
        trip         = filter(cross_df, traffic_model == curr_model, departure_time == curr_time)
        cum_tt       = 0
        
        if(num_error > 100) {stop('Too many requests failed to return a valid response.')}
        
        foreach(j = 1:nrow(trip), .combine = 'rbind', .export = c('num_error', 'gmapsdirection')) %do% {
            row           = trip[j, ]
            o             = row$o
            d             = row$d
            segment       = row$segment
            cum_tt        = get_departure_offset(j, cum_tt, res = res)
            departure     = row$departure_time %>% as.numeric() + cum_tt
            traffic_model = row$traffic_model
            res           = try(gmapsdirection(o, d, departure = departure, key = key, traffic_model = traffic_model),
                                silent = T)
            if('try-error' %in% class(res)) {
                status    = 'R_ERROR'
                tt        = -9999
                distance  = -9999
                num_error = num_error + 1
            } else if (res$Status != 'OK') {
                status    = res$Status
                tt        = -9999
                distance  = -9999
                num_error = num_error + 1
            } else {
                status   = res$Status
                tt       = res$Time
                distance = res$Distance
            }
            c(row, computed_departure = departure, status = status, tt = tt, distance = distance)
        }
    }
    
    tt = as.data.frame(par_results) %>% apply(2, unlist, use.names = FALSE) %>%
         as.data.frame(row.names = FALSE, stringsAsFactors = FALSE) %>% as_tibble()
    tt = mutate(tt, departure_time = as.numeric(departure_time) %>% as.POSIXct(origin = '1970-01-01', tz = tz),
                computed_departure = as.numeric(computed_departure) %>% as.POSIXct(origin = '1970-01-01', tz = tz))
    
    return(tt)
    
}
