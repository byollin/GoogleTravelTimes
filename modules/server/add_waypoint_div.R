#' add_waypoint_div
#' 
#' Add a waypoint div containing a text input, remove waypoint button and add marker button.
#' 
#' @param id shiny input id
#' 
#' @return HTML

add_waypoint_div = function(id) {
    insertUI(selector = '#add_waypoint', where = 'beforeBegin', ui = {
        div(id = paste0('waypoint_div_', id),
            div(style = 'display: inline-block; vertical-align: bottom; min-width: 100%; overflow: hidden;',
                column(10, style = 'padding-left: 0;',
                    textInput(paste0('waypoint_', id), label = NULL, placeholder = 'Waypoint', width = '100%')
                ),
                column(1, style = 'padding-left: 0; z-index: 300; margin-left: -18px;',
                    br(),
                    actionLink(paste0('remove_waypoint_', id), label = '', icon = icon('minus-sign', lib = 'glyphicon'))
                ),
                column(1, style = 'margin-top: -22px; margin-left: -20px',
                    br(),
                    prettyToggle(paste0('add_waypoint_marker_', id), label_on = NULL, label_off = NULL, outline = TRUE,
                                 plain = TRUE, icon_on = icon('map-marker', lib = 'glyphicon'),
                                 icon_off = icon('map-marker', lib = 'glyphicon'), status_on = 'danger',
                                 status_off = 'info', bigger = TRUE, inline = TRUE, value = TRUE)
                )
            ),
            br()
        )
    })
}
