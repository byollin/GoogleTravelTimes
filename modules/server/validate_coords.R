#' validate_coords
#' 
#' Validate latitude/longitude coordinates.
#' 
#' @param coords latitude/longitude coordinates
#' 
#' @return TRUE or FALSE

validate_coords = function(coords) {
    str_detect(coords, paste0('^[-+]?([1-8]?\\d(\\.\\d+)?|90(\\.0+)?)\\s*,\\s*[-+]?(180(\\.0+)?|((1[0-7]\\d)|([1-9]?',
               '\\d))(\\.\\d+)?)$'))
}
