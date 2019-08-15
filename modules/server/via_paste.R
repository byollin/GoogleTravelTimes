#' via_paste
#' 
#' Create waypoint coordinate string.
#' 
#' @param x vector of waypoint coordinates
#' 
#' @return string

via_paste = function(x) {
    x = x[!is.na(x)]
    via = paste0(x, collapse = '|via:')
    paste0('via:', via)
}
