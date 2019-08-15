#' send_error_message
#' 
#' Test the route
#' 
#' @param session Shiny session
#' @param text text to display in error message
#' 
#' @return HTML

send_error_message = function(session, text) {
    sendSweetAlert(session, title = '', type = 'error', btn_labels = 'OK', html = TRUE, text = text)
}
