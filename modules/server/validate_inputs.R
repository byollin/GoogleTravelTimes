#' validate_inputs
#' 
#' Validate request inputs.
#' 
#' @param session Shiny session
#' @param start_date start date
#' @param end_date end date
#' @param time_period_1 range of time
#' @param time_period_2 range of time
#' @param freq frequency
#' @param days_of_week days of week
#' @param traffic_model traffic model
#' @param tz time zone
#' @param email email
#' @param project project number
#' @param desc description of request
#' 
#' @return TRUE or FALSE

validate_inputs = function(session, start_date, end_date, time_period_1, time_period_2, freq, days_of_week,
                           traffic_model, tz, email, project, desc) {
    valid_inputs = FALSE
    if(start_date > Sys.time() | end_date > Sys.time()) {
        if(start_date <= end_date) {
            date_range = seq(ymd(start_date), ymd(end_date), by = '1 day')
            if(!length(tz) == 0) {
                if(!is.null(traffic_model)) {
                    if(TRUE %in% (days_of_week %in% weekdays(date_range))) {
                        if(email != '') {
                            if(project != '') {
                                if(desc != '') {
                                    valid_inputs = TRUE
                                } else {
                                    send_error_message(session, text = tags$span(tags$span('Provide a description.')))
                                }
                            } else {
                                send_error_message(session, text = tags$span(tags$span('Provide a project number.')))
                            }
                        } else {
                            send_error_message(session, text = tags$span(tags$span('Provide an email address.')))
                        }
                    } else {
                        send_error_message(session, text = tags$span(tags$span('Date range must contain selected days \
                                                                               of the week.')))
                    }
                } else {
                    send_error_message(session, text = tags$span(tags$span('At least one traffic model must be \
                                                                           selected.')))
                }
            } else {
                send_error_message(session, text = tags$span(tags$span('A time zone must be selected.')))
            }
        } else {
            send_error_message(session, text = tags$span(tags$span('End date must be later than start date.')))
        }
    } else {
        send_error_message(session, text = tags$span(tags$span('Date range must be sometime in the future.')))
    }
    return(valid_inputs)
}
