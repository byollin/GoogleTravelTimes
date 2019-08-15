inline_block_css = 'display: inline-block; vertical-align: bottom; min-width: 100%;'

dashboardPage(skin = 'red', title = 'Google Travel Times',
    # DASHBOARD HEADER #################################################################################################
    dashboardHeader(title = span(tagList(icon('google', class = 'fa-lg alignright'), 'oogle Travel Times')),
                    titleWidth = 350,
                    tags$li(class = 'dropdown',
                            actionLink('contact', label = '', icon = icon('envelope-o', class = 'fa-lg'))),
                    tags$li(class = 'dropdown',
                            tags$a(href = "https://www.wsp.com/en-GL", target = "_blank",
                                   tags$img(height = "20px", alt = "WSP USA ©", src = "wsp_logo.png")))
    ),
    # DASHBOARD SIDEBAR ################################################################################################                
    dashboardSidebar(width = 350,
        # >> STEP 1 ####################################################################################################
        sidebarMenu(menuItem(text = 'Step 1: Travel Time Parameters', icon = icon('list'), startExpanded = TRUE,
            # date range selection
            dateRangeInput('date_range', 'Date range: ', start = system_date + days(7), end = system_date + days(14),
                           min = system_date, width = '100%'),
            # time of day sliders
            div(style = inline_block_css,
                column(11, style = 'padding-left: 0',
                    sliderInput('time_range_1', 'Time range: ', min = 0, max = 24, post = ':00', step = 1,
                                value = c(6, 18))
                ),
                column(1, style = 'padding-left: 0; margin-top: 38px; margin-left: -13px;',
                    br(),
                    actionLink('add_range', label = '', icon = icon('plus-sign', lib = 'glyphicon'))
                )
            ),
            br(),
            # time of day sliders
            div(style = inline_block_css, id = 'time_range_div',
                column(11, style = 'padding-left: 0',
                    sliderInput('time_range_2', label = NULL, min = 0, max = 24, post = ':00', step = 1,
                                value = c(6, 18))
                ),
                column(1, style = 'padding-left: 0; margin-top: 13px; margin-left: -13px;',
                    br(),
                    actionLink('remove_range', label = '', icon = icon('minus-sign', lib = 'glyphicon'))
                )
            ) %>% hidden(),
            # time zone selection
            # TODO: SIMPLIFY LIST OF TIME ZONES
            selectizeInput('time_zone', label = 'Time zone: ', choices = tzs, selected = 'America/Los Angeles',
                           width = '100%', multiple = TRUE,
                           options = list(maxItems = 1, placeholder = 'Choose a time zone')),
            # time frequency selection
            selectInput('frequency', label = 'Frequency: ', choices = c('5 minutes', '10 minutes', '15 minutes',
                                                                        '30 minutes', '45 minutes', '1 hour'),
                        selected = '30 minutes', width = '100%'),
            # traffic model selection
            checkboxGroupButtons('traffic_models', 'Traffic models: ', choices = c('Optimistic', 'Best Guess',
                                                                                   'Pessimistic'),
                                 selected = c('Best Guess'), status = 'danger',
                                 checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                  no = icon("remove", lib = "glyphicon")), size = 'sm',
                                 justified = TRUE),
            # day of week selection
            checkboxGroupButtons('days_of_week', 'Days of week: ', choices = c('Su', 'M', 'T', 'W', 'Th', 'F', 'S'),
                                 selected = c('T', 'W', 'Th'), status = 'danger',
                                 checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                  no = icon("remove", lib = "glyphicon")), size = 'sm',
                                 justified = TRUE),
            br()
        ),
        # >> STEP 2 ####################################################################################################
        menuItem(text = 'Step 2: Define a Route', icon = icon('location-arrow'), fluidRow(column(12,
            # origin
            div(style = inline_block_css,
                column(11, style = 'padding-left: 0',
                    textInput('origin', label = 'Origin: ', placeholder = 'Origin (e.g., 47.5887, -122.2327)',
                              width = '100%')
                ),
                 column(1, style = 'margin-top: 2px; margin-left: -40px',
                    br(),
                    prettyToggle(inputId = 'add_origin', label_on = NULL, label_off = NULL, outline = TRUE,
                                 plain = TRUE, icon_on = icon('map-marker', lib = 'glyphicon'),
                                 icon_off = icon('map-marker', lib = 'glyphicon'), status_on = 'danger',
                                 status_off = 'info', bigger = TRUE)
            )),
            br(),
            # add waypoint
            actionLink('add_waypoint', label = 'Add waypoint', icon = icon('plus-sign', lib = 'glyphicon'),
                       style = 'margin-left: 15px'),
            # destination
            div(style = inline_block_css,
                column(11, style = 'padding-left: 0',
                    textInput('destination', label = 'Destination: ',
                              placeholder = 'Destination (e.g., 47.6055, -122.3339)', width = '100%')
                ),
                column(1, style = 'margin-top: 2px; margin-left: -40px',
                    br(),
                    prettyToggle(inputId = 'add_destination', label_on = NULL, label_off = NULL, outline = TRUE,
                                 plain = TRUE, icon_on = icon('map-marker', lib = 'glyphicon'),
                                 icon_off = icon('map-marker', lib = 'glyphicon'), status_on = 'danger',
                                 status_off = 'info', bigger = TRUE)
                )
            ),
            br(),
            column(12,
                actionBttn('test', 'Test Route', style = 'simple', color = 'danger', block = TRUE),
                p('-', style = 'font-size: smaller; color: #2b3b41;')
            ),
            br()
        ))),
        # >> STEP 3 ####################################################################################################
        menuItem(text = 'Step 3: Verify and Submit Request', icon = icon('check'),
            textInput('email', label = 'Email:', placeholder = 'first.last@wsp.com', width = '100%'),
            column(12,
                p('Results are NOT sent to your email address.', style = 'margin-top: -12px; font-size: smaller;')
            ),
            textInput('project', label = 'Project number:', width = '100%', placeholder = '160363P'),
            textAreaInput('desc', label = 'Request description:', width = '100%', resize = 'none', 
                          placeholder = "e.g., Travel times from Lynnwood Transit Center to Totem Lake."),
            div(passwordInput('api_key', label = 'API key: ', width = '100%'), style = 'margin-top:8px;'),
            column(12, style = 'margin-top: -12px; font-size: smaller; padding-bottom: 14px;',
                tags$a(href = 'google_api.html', target = '_blank', "Don't have an API key? See how to create one.")
            ),
            column(12,
                actionBttn('submit', 'Submit Requests', style = 'simple', color = 'danger', block = TRUE),
                br(),
                actionBttn('view_results', 'View Results', style = 'simple', color = 'danger',
                           block = TRUE) %>% hidden(),
                p('-', style = 'font-size: smaller; color: #2b3b41;')
            )
        ),
        div(id = 'release',
            p('WSP USA Ⓒ 2019')
        ))
    ),
    # DASHBOARD BODY ###################################################################################################
    dashboardBody(
        useShinyjs(),
        extendShinyjs(text = detect_ip, functions = 'getIP'),
        # add scroll to sidebar on overflow
        tags$head(tags$script(type = 'text/javascript',
            '$(document).ready(function() {
                 $(".main-sidebar").css("height", "100%");
                 $(".main-sidebar .sidebar").css({"position": "relative", "max-height": "100%", "overflow": "auto"})
             })')),
        tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'custom.css')),
        div(class = 'outer',
            absolutePanel(id = 'coord_panel', top = 18, right = 8, width = 'auto', pre(id = 'coords')),
            leafletOutput('map', width = '100%', height = '100%'),
            hidden(div(id = 'spinner',
                div() %>% withSpinner(type = 8, proxy.height = '400px', color = '#d73926')
            ))
        ),
        absolutePanel(id = 'results_panel', top = 100, left = 400, width = '75%',
            wellPanel(id = 'results_well', draggable = FALSE, top = 'auto', bottom = 'auto', left = 'auto',
                      right = 'auto', height = '800',
                h3('Results', style = 'color: #d73926;'),
                dataTableOutput('results') %>% withSpinner(type = 8, proxy.height = '450px', color = '#d73926'),
                br(),
                column(10),
                column(2,
                    actionBttn('close_results', 'Close', style = 'simple', color = 'danger', block = TRUE)
                ),
                p('-', style = 'font-size: smaller; color: white;')
            )
        ) %>% hidden(),
        tags$img(height = '10px', alt = 'Google ©', src = 'powered_by_google_on_white.png',
                 style = 'position: absolute; right: 8px; bottom: 20px')
    )
)