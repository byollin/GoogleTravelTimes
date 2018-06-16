library(shinydashboard)
library(lubridate)

dashboardPage(skin = 'red', title = 'Google Travel Times',
              
    dashboardHeader(title = span(tagList(icon('hourglass-2', class = 'fa-lg alignleft'),
                                         '  Google Travel Times')), titleWidth = 350,
                    
        tags$li(class = "dropdown", actionLink('contact', label = '',
                                               icon = icon('envelope-o', class = 'fa-lg alignleft'))),
        
        tags$li(class = "dropdown", actionLink('about', label = '',
                                               icon = icon('question-circle-o', class = 'fa-lg alignleft'))),
                    
        tags$li(class = "dropdown", tags$a(href="https://www.wsp.com/en-GL", target="_blank",
                                           tags$img(height = "20px", alt="wsp_logo", src="wsp_logo.png")))                
                    
    ),
                    
    dashboardSidebar(width = 350,
        
        sidebarMenu(menuItem(text = 'Step 1: Travel Time Parameters', icon = icon('list'),
                             startExpanded = TRUE,
        
        dateRangeInput('date_range', 'Date range: ', start = Sys.Date() + days(7),
                       end = Sys.Date() + days(14), min = Sys.Date(), width = '100%'),
        
        div(style = 'display: inline-block; vertical-align:bottom; min-width: 100%',
            
            column(11, style = 'padding-left: 0',
                   
                sliderInput('time_range_1', 'Time range: ', min = 0, max = 24, post = ':00',
                            step = 1, value = c(6, 18))
                   
            ),
            
            column(1, style = 'padding-left: 0; margin-top: 36px; margin-left: -12px;',
                   
                br(),
                
                actionLink('add_range', label = '', icon = icon('plus-circle',
                                                                class = 'fa-lg alignleft'))
                   
            )
            
        ),
        
        br(),
        
        div(style = 'display: inline-block; vertical-align:bottom; min-width: 100%',
            id = 'time_range_div',
            
            column(11, style = 'padding-left: 0',
                   
                   sliderInput('time_range_2', label = NULL, min = 0, max = 24, post = ':00',
                               step = 1, value = c(6, 18))
                   
            ),
            
            column(1, style = 'padding-left: 0; margin-top: 12px; margin-left: -12px;',
                   
                   br(),
                   
                   actionLink('remove_range', label = '', icon = icon('minus-circle',
                                                                   class = 'fa-lg alignleft'))
                   
            )
            
        ) %>% hidden(),
        
        selectizeInput('time_zone', label = 'Time zone: ', choices = tzs,
                        selected = 'America/Los Angeles', width = '100%',
                        multiple = TRUE, options = list(maxItems = 1, placeholder = 'Choose a time zone')),
        
        selectInput('frequency', label = 'Frequency: ', choices = c('5 minutes', '10 minutes',
                                                                    '15 minutes', '30 minutes',
                                                                    '45 minutes', '1 hour'),
                    selected = '30 minutes', width = '100%'),
        
        checkboxGroupButtons('traffic_models', 'Traffic models: ', choices = c('Optimistic',
                                                                             'Best Guess',
                                                                             'Pessimistic'),
                             selected = c('Best Guess'), status = 'danger',
                             checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                              no = icon("remove", lib = "glyphicon")), size = 'sm',
                             justified = TRUE),
        
        checkboxGroupButtons('days_of_week', 'Days of week: ',
                             choices = c('Su', 'M', 'T', 'W', 'Th', 'F', 'S'),
                             selected = c('T', 'W', 'Th'), status = 'danger',
                             checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                              no = icon("remove", lib = "glyphicon")), size = 'sm',
                             justified = TRUE),
        
        br()),
        
        menuItem(text = 'Step 2: Define a Route', icon = icon('location-arrow'),
                 
            fluidRow(column(12,
            
            div(style = 'display: inline-block; vertical-align:bottom; min-width: 100%',
             
             column(11, style = 'padding-left: 0',
                    
                    textInput('origin', label = 'Origin: ',
                              placeholder = 'Origin (e.g., 47.5887, -122.2327)', width = '100%')
                    
             ),
             
             column(1, style = 'margin-top: 2px; margin-left: -40px',
                    
                    br(),
                    
                    prettyToggle(inputId = 'add_origin', label_on = NULL, label_off = NULL,
                                 outline = TRUE, plain = TRUE, icon_on = icon('map-marker'), 
                                 icon_off = icon('map-marker'), status_on = 'danger',
                                 status_off = 'info', bigger = TRUE)
             )
             
            ),
        
        br(),
        
        actionLink('add_waypoint', label = 'Add waypoint', icon = icon('plus-circle',
                                                                       class = 'alignleft'),
                   style = 'margin-left: 15px'),
        
        div(style = 'display: inline-block; vertical-align:bottom; min-width: 100%',
            
            column(11, style = 'padding-left: 0',
                   
                   textInput('destination', label = 'Destination: ',
                             placeholder = 'Destination (e.g., 47.6055, -122.3339)', width = '100%')
                   
            ),
            
            column(1, style = 'margin-top: 2px; margin-left: -40px',
                   
                   br(),
                   
                   prettyToggle(inputId = 'add_destination', label_on = NULL, label_off = NULL,
                                outline = TRUE, plain = TRUE, icon_on = icon('map-marker'), 
                                icon_off = icon('map-marker'), status_on = 'danger',
                                status_off = 'info', bigger = TRUE)
            )
            
        ),
        
        br(),
        
        # fileInput('upload_route', 'Upload coordinates: ', accept = '.csv', width = '100%'),
        
        column(12,
               actionBttn('test', 'Test Route', style = 'simple', color = 'danger',
                              block = TRUE),
        
        div(id = 'test_message', style = 'margin-top: 2px;', p('Note: you will not be billed for this operation.'))),
        br()
        
        ))),
        
        menuItem(text = 'Step 3: Verify and Submit Request', icon = icon('check'),
                 
            br(),
            
            column(12,
                 
            div(style = 'display: inline-block; vertical-align:top; width: 154px',
            
                actionBttn('cost_estimate', 'Estimate Cost', icon = icon('usd'), style = 'simple',
                           color = 'danger', block = TRUE)
                
            ),
            
            div(style = 'display: inline-block; vertical-align:top; width: 154px',
                actionBttn('review_requests', 'Review Requests', icon = icon('table'), style = 'simple',
                           color = 'danger', block = TRUE)
            )),
            
            br(),
            
            div(passwordInput('api_key', label = 'API key: ', width = '100%'), style = 'margin-top:8px;'),
            
            column(12,div(id = 'api_message', style = 'margin-top: -6px;', p('Note: your requests are made securely over HTTPS.')) %>% hidden()),
            
            column(12,
            actionBttn('submit', 'Submit Requests', style = 'simple', color = 'danger', block = TRUE),
            
            br())
        
        ))
        
    ),
    
    dashboardBody(
        
        useShinyjs(),
        
        # add scroll to sidebar on overflow
        tags$head(tags$script(type = 'text/javascript',
            '$(document).ready(function() {
                $(".main-sidebar").css("height", "100%");
                $(".main-sidebar .sidebar").css({"position": "relative", "max-height": "100%", "overflow": "auto"})
            })')),
        
        # custom CSS
        tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'custom.css')),
        
        div(class = 'outer',
            
            absolutePanel(id = 'info_panel', top = 18, right = 8, width = 'auto', pre(id = 'info')),
            
            leafletOutput('map', width='100%', height='100%'),
            
            hidden(div(id = 'spinner',
                       
                div() %>% withSpinner(type = 8, proxy.height = '400px', color = '#d73926')
                       
            ))
        )
    )
)