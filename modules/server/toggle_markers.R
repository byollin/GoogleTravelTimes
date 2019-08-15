#' toggle_markers
#' 
#' Update style of markers.
#' 
#' @param session shiny session
#' @param selected_id selected marker id
#' @param ids all marker ids
#' 
#' @return None

toggle_markers = function(session, selected_id, ids) {
    ids = ids[ids != selected_id]
    lapply(ids, function(x) updatePrettyToggle(session, x, value = FALSE))
}
