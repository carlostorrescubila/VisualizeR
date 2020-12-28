# ##### UI ########################################################################
# r_base_histogram_ui <- function(id){
#   
#   ns <- NS(id)
#   
#   fluidPage(
#     
#     ##### > Title #####
#     h2(
#       strong("Histogram with R base"),
#       class = "text-center"
#     ), 
#     
#     ##### > Dropdown button #####
#     dropdown(
#       
#       ##### >>Dropdown options #####
#       style = "bordered", 
#       status = "primary",
#       icon = icon("gear"), 
#       width = "300px",
#       circle = TRUE, 
#       tooltip = tooltipOptions(title = "Click to see inputs!"), 
#       label = "Inputs", 
#       animate = animateOptions(enter = "fadeInDown", exit = "fadeOutUp", duration = 0.5),
#       
#       div(
#         style='max-height: 55vh; overflow-y: auto;',
#         
#         ##### >> Title #####
#         h4(strong("List of Inputs"), class = "text-center"),
#         
#         ##### >> Selet data #####
#         pickerInput(
#           inputId = ns('r_base_histogram_select_data'),
#           label = 'Select data:',
#           choices = NULL, 
#           options = list(size = 4)
#         ),
#         
#         ##### >> Selet variable #####
#         pickerInput(
#           inputId = ns("r_base_histogram_select_variable"),
#           label = 'Select variable:',
#           choices = NULL, 
#           options = list(size = 4)
#         ),
#         
#         ##### >> Selet groups #####
#         pickerInput(
#           inputId = ns("r_base_histogram_select_groups"),
#           label = 'Select groups:',
#           choices = NULL, 
#           options = list(size = 4)
#         ),
#         
#         ##### >> Breaks #####
#         sliderTextInput(
#           inputId = ns("r_base_histogram_breaks"),
#           label = "Number of bins:", 
#           choices = seq(from = 1, to = 50, by = 1), 
#           # selected = 8, 
#           width = "99%"
#         ),
#         
#         ##### >> Switch to frecuency #####
#         h5(strong("Variable selected is frecuency:")), 
#         switchInput(
#           inputId = ns("r_base_histogram_frequency"),
#           label = img(icon("percent")),
#           value = TRUE,
#           onLabel = "TRUE",
#           offLabel = "FALSE",
#           onStatus = "success",
#           offStatus = "danger"
#         ),
#         
#         ##### >> Color #####
#         uiOutput(ns('r_base_histogram_color_ui')),
#         
#         # ##### >> Nrow | Ncol #####
#         # disabled(div(
#         #   id = ns("r_base_histogram_nrow_ncol"),
#         #   h5(strong("Number of")),
#         #   shiny::fluidRow(
#         #     column(width = 6,
#         #            sliderTextInput(
#         #              inputId = ns("r_base_histogram_nrow"),
#         #              label = "rows:", 
#         #              choices = seq(from = 1, to = 5, by = 1), 
#         #              width = "99%"
#         #            )
#         #            ), 
#         #     column(width = 6,
#         #            sliderTextInput(
#         #              inputId = ns("r_base_histogram_ncol"),
#         #              label = "columns:", 
#         #              choices = seq(from = 1, to = 5, by = 1), 
#         #              width = "99%"
#         #            )
#         #            )
#         #     )
#         #   )),
#         
#         ##### >>> Main #####
#         textInputAddon(
#           inputId = ns("r_base_histogram_title"), 
#           label = "Title:", 
#           placeholder = "Your title", 
#           addon = icon("font")
#           ),
#         
#         ##### >>> Pre-Main #####
#         disabled(
#           textInputAddon(
#             inputId = ns("r_base_histogram_pre_title"), 
#             label = "Pre-title:", 
#             placeholder = "Your pre-title", 
#             addon = icon("bold")
#             )
#         ),
#         
#         ##### >> x-label #####
#         textInputAddon(
#           inputId = ns("r_base_histogram_x_label"),
#           label = "X label:", 
#           placeholder = "Your x-label",
#           addon = icon("grip-lines")
#         ),
#         
#         ##### >> y-label #####
#         textInputAddon(
#           inputId = ns("r_base_histogram_y_label"),
#           label = "Y label:", 
#           placeholder = "Your y-label",
#           addon = icon("grip-lines-vertical")
#         )
#           
#       )
#       
#     ), 
#   br(),
#   
#   ##### > Bar chart #####
#   plotOutput(ns("r_base_histogram_plot")), 
#   br(),
#   
#   ##### > Show code #####
#   
#   ##### >> Action buttons show #####
#   actionBttn(
#     inputId = ns("r_base_histogram_action_showcode"),
#     label = "Show code",
#     icon = icon("code"),
#     style = "stretch",
#     color = "primary"
#   ),
#   
#   ##### >> Code #####
#   shinyjs::useShinyjs(),
#   shinyjs::hidden(
#     div(
#       id = ns("hidden_code"),
#       .noWS = "inside",
#       codeOutput(ns("r_base_histogram_code")), 
#       actionButton(
#         inputId = ns("clipbtn"),
#         label = "Copy code to clipboard",
#         # style = "color: white; background-color: #2C3E50;", 
#         icon = icon("copy")
#         ),
#       bsTooltip(id = ns("clipbtn"), title = "Copy to clipboard", placement = "right")
#       )
#     )
# 
#   )
# 
# }
# 
# ##### Server ######################################################################
# 
# r_base_histogram_server <- function(input, output, session){
# 
#   ns <- session$ns
# 
#   ##### > Update data choices #####
#   observe({
#     updatePickerInput(
#       session,
#       inputId = "r_base_histogram_select_data",
#       label = NULL,
#       choices = names(Uploaded_Data)
#     )
#   })
# 
#   ##### > Update variable choice #####
#   observe({
#     req(input$r_base_histogram_select_data)
#     updatePickerInput(
#       session,
#       inputId = "r_base_histogram_select_variable",
#       label = NULL,
#       choices = Uploaded_Data[[input$r_base_histogram_select_data]] %>%
#         select_if(function(col){
#           is.numeric(col)
#         }
#         ) %>%
#         colnames
#     )
#   })
# 
#   ##### > Update groups choice #####
#   observe({
#     req(input$r_base_histogram_select_data)
#     updatePickerInput(
#       session,
#       inputId = "r_base_histogram_select_groups",
#       label = NULL,
#       choices = Uploaded_Data[[input$r_base_histogram_select_data]] %>%
#         select_if(function(col){
#           is.character(col) | is.factor(col)
#           }
#         ) %>%
#         colnames %>%
#         c("NULL", .)#,
#       # choicesOpt = list("max-options" = 2, `multiple-separator` = " | ",size = 5)
#     )
#   })
# 
#   ##### > Update breaks #####
#   N_Bins_Dafault <- reactive({
#     paste0("Uploaded_Data[['", input$r_base_histogram_select_data, "']]", 
#            "$", input$r_base_histogram_select_variable) %>%
#       parse(text = .) %>%
#       eval %>%
#       nclass.Sturges()
#   })
#   observe({
#     req(input$r_base_histogram_select_variable)
#     updateSliderTextInput(
#       session,
#       inputId = "r_base_histogram_breaks", 
#       selected = N_Bins_Dafault()
#     )
#   })
#   
#   ##### > Update colors #####
#   output$r_base_histogram_color_ui <- renderUI({
#     if(is.null(input$r_base_histogram_select_data)){
#       pickerInput(
#         inputId = ns('r_base_histogram_color'),
#         label = 'Colors',
#         choices = colors(),
#         options = list(`live-search` = TRUE, size = 5)
#       )
#     } else {
#       pickerInput(
#         inputId = ns('r_base_histogram_color'),
#         label = 'Colors',
#         choices = colors(),
#         selected = if_else(input$r_base_histogram_select_groups == "NULL", 
#                            "white", "gray90"), 
#         options = list(`live-search` = TRUE, size = 5)
#       )
#     }
#   })
#   
#   ##### > Enable-disable inputs #####
#   observeEvent(input$r_base_histogram_select_groups, {
#     # toggleState(
#     #   id = "r_base_histogram_nrow_ncol",
#     #   condition = input$r_base_histogram_select_groups != "NULL"
#     # )
#     toggleState(
#       id = "r_base_histogram_title",
#       condition = input$r_base_histogram_select_groups == "NULL"
#     )
#     toggleState(
#       id = "r_base_histogram_pre_title",
#       condition = input$r_base_histogram_select_groups != "NULL"
#     )
#   })
# 
#   ##### > Update X label #####
#   observe({
#     req(input$r_base_histogram_select_data)
#     updateTextInput(
#       session,
#       inputId = "r_base_histogram_x_label",
#       label = NULL,
#       value = ifelse(input$r_base_histogram_select_groups == "NULL", 
#                      paste0(input$r_base_histogram_select_data, "$", 
#                             input$r_base_histogram_select_variable), 
#                      input$r_base_histogram_select_variable)
#     )
#     })
# 
#   ##### > Update Y label #####
#   observe({
#     req(input$r_base_histogram_select_groups)
#     if(input$r_base_histogram_select_groups == "NULL"){
#       updateTextInput(
#         session,
#         inputId = "r_base_histogram_y_label",
#         label = NULL,
#         value = ifelse(isTRUE(input$r_base_histogram_frequency), 
#                        "Frecuency", "Density")
#       )
#     } else {
#       updateTextInput(
#         session,
#         inputId = "r_base_histogram_y_label",
#         label = NULL,
#         value = "Frecuency"
#       )
#     }
#     
#   })
# 
#   ##### > Formula used in plot #####
#   My_Formula <- reactive({
#     req(input$r_base_histogram_select_groups)
#     if(input$r_base_histogram_select_groups == "NULL"){
#       ## True condition ##
#       paste0("Uploaded_Data[['", input$r_base_histogram_select_data, "']]", "$",
#              input$r_base_histogram_select_variable) %>%
#         parse(text = .) %>%
#         eval 
#     } else {
#       ## False condition ##
#       paste0(
#         "Uploaded_Data[['", input$r_base_histogram_select_data, "']]",  
#         "$", input$r_base_histogram_select_variable, 
#         "~", 
#         "Uploaded_Data[['", input$r_base_histogram_select_data, "']]",  
#         "$", input$r_base_histogram_select_groups
#         ) %>% 
#         as.formula()
#     }
#   })
#   
#   ##### > Min #####
#   Min_Breaks <- reactive({
#     req(input$r_base_histogram_select_groups)
#     ifelse(
#       test = input$r_base_histogram_select_groups == "NULL", 
#       yes = min(My_Formula()), 
#       no = paste0("Uploaded_Data[['", input$r_base_histogram_select_data, "']]", "$",
#                      input$r_base_histogram_select_variable) %>%
#         parse(text = .) %>%
#         eval   %>% 
#         min()
#     )
#   })
#   ##### > Max #####
#   Max_Breaks <- reactive({
#     req(input$r_base_histogram_select_groups)
#     ifelse(
#       test = input$r_base_histogram_select_groups == "NULL", 
#       yes = max(My_Formula()), 
#       no = paste0("Uploaded_Data[['", input$r_base_histogram_select_data, "']]", "$",
#                      input$r_base_histogram_select_variable) %>%
#         parse(text = .) %>%
#         eval %>% 
#         max()
#     )
#   })
# 
#   ##### > Histogram #####
#   output$r_base_histogram_plot <- renderPlot({
#     req(input$r_base_histogram_select_data)
#     if(input$r_base_histogram_select_groups == "NULL"){
#       hist(
#         x = My_Formula(),
#         breaks = seq(Min_Breaks(), Max_Breaks(), 
#                      length.out = input$r_base_histogram_breaks + 1),
#         freq = input$r_base_histogram_frequency,
#         col = input$r_base_histogram_color,
#         main = input$r_base_histogram_title,
#         xlab = input$r_base_histogram_x_label,
#         ylab = input$r_base_histogram_y_label
#       )
#     } else {
#       FSA:::hist.formula(
#         formula = My_Formula(),
#         breaks = seq(Min_Breaks(), Max_Breaks(),
#                      length.out = input$r_base_histogram_breaks + 1),
#         freq = input$r_base_histogram_frequency,
#         col = input$r_base_histogram_color,
#         pre.main = input$r_base_histogram_pre_title,
#         xlab = input$r_base_histogram_x_label,
#         ylab = input$r_base_histogram_y_label
#         )
#     }
#   })
#   
#   ##### > Code #####
#   Text_Data <- reactive({
#     {if(input$r_base_histogram_select_groups == "NULL")
#       paste("hist(",
#             paste0("  x = ", input$r_base_histogram_select_data, "$",
#                    input$r_base_histogram_select_variable),
#             sep = "\n")
#       else
#         paste("library(FSA)",
#               "## This package let you use a formula in hist function masking the function hist.formula", "",
#               "hist(",
#               paste0("  formula = ",
#                      input$r_base_histogram_select_variable, "~",
#                      input$r_base_histogram_select_groups, ","),
#               paste0("  data = ", input$r_base_histogram_select_data),
#               sep = "\n")
#       } %>%
#       ## Breaks ## 
#       {if(input$r_base_histogram_breaks != N_Bins_Dafault())
#       paste(
#         paste0(., ","),
#         paste0("  breaks = ", 
#                "seq(", Min_Breaks(), ", ", Max_Breaks(), ", ", 
#                "length.out = ", input$r_base_histogram_breaks + 1, ")"), 
#         sep = "\n"
#       ) else .} %>% 
#       ## Frecuency ##
#       {if(isFALSE(input$r_base_histogram_frequency))
#         paste(
#           paste0(., ","),
#           paste0("  freq = ", input$r_base_histogram_frequency),
#           sep = "\n"
#         ) else .} %>%
#       ## Color ##
#       {if(input$r_base_histogram_select_groups == "NULL"){
#         if(input$r_base_histogram_color != "white"){
#           paste(
#             paste0(., ","),
#             paste0('  col = "', input$r_base_histogram_color, '"'),
#             sep = "\n"
#           )
#           } else . 
#         } else {
#           if(input$r_base_histogram_color != "gray90"){
#             paste(
#               paste0(., ","),
#               paste0('  col = "', input$r_base_histogram_color, '"'),
#               sep = "\n"
#             )
#             } else .
#           }
#         } %>%
#       ## Title ##
#       {if(input$r_base_histogram_title != "" &
#           input$r_base_histogram_select_groups == "NULL")
#         paste(
#           paste0(., ","),
#           paste0('  main = "', input$r_base_histogram_title, '"'),
#           sep = "\n"
#         ) else .} %>%
#       ## Pre-Title ##
#       {if(input$r_base_histogram_pre_title != "" &
#           input$r_base_histogram_select_groups != "NULL")
#         paste(
#           paste0(., ","),
#           paste0('  pre.main = "', input$r_base_histogram_pre_title, '"'),
#           sep = "\n"
#         ) else .} %>%
#       ## X label ##
#       {if(input$r_base_histogram_x_label != "")
#         paste(
#           paste0(., ","),
#           paste0('  xlab = "', input$r_base_histogram_x_label, '"'),
#           sep = "\n"
#         ) else .} %>%
#       ## Y label ##
#       {if(input$r_base_histogram_y_label != "")
#         paste(
#           paste0(., ","),
#           paste0('  ylab = "', input$r_base_histogram_y_label, '"'),
#           sep = "\n"
#         ) else .} %>%
#       paste(., ")", sep = "\n")
# })
# 
#   ##### > Render code #####
#   output$r_base_histogram_code <- renderCode({
#     highlight(Text_Data())
#   })
# 
#   ##### > Toggle Code #####
#   observeEvent(input$r_base_histogram_action_showcode, {
#     req(input$r_base_histogram_select_data)
#     shinyjs::toggle(id = "hidden_code", anim = TRUE)
#   })
# 
#   ##### > Copy to clipboard #####
#   observeEvent(
#     input$clipbtn,
#     write_clip(Text_Data())
#     )
# 
# }