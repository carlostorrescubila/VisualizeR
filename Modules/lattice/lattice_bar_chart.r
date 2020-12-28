##### UI ########################################################################
lattice_bar_chart_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    
    ##### > Title #####
    h2(
      strong("Bar chart with lattice"),
      class = "text-center"
    ), 
    
    ##### > Dropdown button #####
    dropdown(
      
      ##### >>Dropdown options #####
      style = "bordered", 
      status = "primary",
      icon = icon("gear"), 
      width = "300px",
      circle = TRUE, 
      tooltip = tooltipOptions(title = "Click to see inputs!"), 
      label = "Inputs", 
      animate = animateOptions(enter = "fadeInDown", exit = "fadeOutUp", duration = 0.5),
      
      div(
        style='max-height: 55vh; overflow-y: auto;',
        
        ##### >> Title #####
        h4(strong("List of Inputs"), class = "text-center"),
        
        ##### >> Selet data #####
        pickerInput(
          inputId = ns('lattice_bar_chart_select_data'),
          label = 'Select data:',
          choices = NULL, 
          options = list(size = 4)
        ),
        
        ##### >> Selet variable #####
        pickerInput(
          inputId = ns("lattice_bar_chart_select_variable"),
          label = 'Select variable:',
          choices = NULL, 
          options = list(size = 4)
        ),
        
        ##### >> Bars horizontally #####
        h5(strong("Bars drawn horizontally:")), 
        switchInput(
          inputId = ns("lattice_bar_chart_horizontally"),
          label = img(src = "https://www.flaticon.com/svg/static/icons/svg/64/64728.svg", 
                      style="width: 50%; height:auto;"),
          value = TRUE,
          onLabel = "TRUE",
          offLabel = "FALSE",
          onStatus = "success",
          offStatus = "danger"
        ),
        
        ##### >> Color #####
        uiOutput(ns('lattice_bar_chart_color_ui')),
        
        ##### >> Main #####
        textInputAddon(
          inputId = ns("lattice_bar_chart_title"), 
          label = "Title:", 
          placeholder = "Your title", 
          addon = icon("font")
        ),
        
        ##### >> x-label #####
        textInputAddon(
          inputId = ns("lattice_bar_chart_x_label"),
          label = "X label:", 
          placeholder = "Your x-label",
          addon = icon("grip-lines")
        ),
        
        ##### >> y-label #####
        textInputAddon(
          inputId = ns("lattice_bar_chart_y_label"),
          label = "Y label:", 
          placeholder = "Your y-label",
          addon = icon("grip-lines-vertical")
        )
        
      )
      
    ), 
    br(),
    
    ##### > Bar chart #####
    plotOutput(ns("lattice_bar_chart_plot")), 
    br(),
    
    ##### > Show code #####
    
    ##### >> Action buttons show #####
    actionBttn(
      inputId = ns("lattice_bar_chart_action_showcode"),
      label = "Show code",
      icon = icon("code"),
      style = "stretch",
      color = "primary"
    ),
    
    ##### >> Code #####
    shinyjs::useShinyjs(),
    shinyjs::hidden(
      div(
        id = ns("hidden_code"),
        .noWS = "inside",
        codeOutput(ns("lattice_bar_chart_code")), 
        actionButton(
          inputId = ns("clipbtn"),
          label = "Copy code to clipboard",
          # style = "color: white; background-color: #2C3E50;", 
          icon = icon("copy")
        ),
        bsTooltip(id = ns("clipbtn"), title = "Copy to clipboard", placement = "right")
      )
    )
    
  )
  
}

##### Server ######################################################################

lattice_bar_chart_server <- function(input, output, session){
  
  ns <- session$ns
  
  ##### > Update data choices #####
  observe({
    updatePickerInput(
      session, 
      inputId = "lattice_bar_chart_select_data", 
      label = NULL, 
      choices = names(Uploaded_Data)
    )
  })
  
  ##### > Update variables choices #####
  observe({
    req(input$lattice_bar_chart_select_data)
    updatePickerInput(
      session, 
      inputId = "lattice_bar_chart_select_variable", 
      label = NULL, 
      choices = Uploaded_Data[[input$lattice_bar_chart_select_data]] %>% 
        select_if(function(col){
          is.character(col) | is.factor(col)
        } 
        ) %>% 
        colnames
    )
  })
  
  ##### > Update colors #####
  Max_Colors <- reactive({
    req(input$lattice_bar_chart_select_data)
    paste0(
      "Uploaded_Data[['", input$lattice_bar_chart_select_data, "']]",
      "$", input$lattice_bar_chart_select_variable) %>%
      parse(text = .) %>%
      eval %>%
      summary %>% 
      length()
  })
  output$lattice_bar_chart_color_ui <- renderUI({
    if(is.null(input$lattice_bar_chart_select_data)){
      pickerInput(
        inputId = ns('lattice_bar_chart_color'),
        label = 'Colors',
        choices = colors(),
        multiple = TRUE,
        options = list(`multiple-separator` = " | ", 
                       size = 5, 
                       `live-search` = TRUE
        )
      )
    } else {
      # req(input$lattice_bar_chart_select_data)
      pickerInput(
        inputId = ns('lattice_bar_chart_color'),
        label = 'Colors',
        choices = colors(),
        multiple = TRUE,
        options = list(`multiple-separator` = " | ", 
                       size = 5, 
                       `live-search` = TRUE,
                       "max-options" = Max_Colors())
      )
    }
    
  })
  
  ##### > Update X label #####
  observe({
    req(input$lattice_bar_chart_select_variable)
    if(isFALSE(input$lattice_bar_chart_horizontally)){
      updateTextInput(
        session, 
        inputId = "lattice_bar_chart_x_label", 
        label = NULL, 
        value = input$lattice_bar_chart_select_variable
      )
    } else {
      updateTextInput(
        session, 
        inputId = "lattice_bar_chart_x_label", 
        label = NULL, 
        value = "count"
      )
    }
  })
  
  ##### > Update Y label #####
  observe({
    req(input$lattice_bar_chart_select_variable)
    if(isTRUE(input$lattice_bar_chart_horizontally)){
      updateTextInput(
        session, 
        inputId = "lattice_bar_chart_y_label", 
        label = NULL, 
        value = input$lattice_bar_chart_select_variable
      )
    } else {
      updateTextInput(
        session, 
        inputId = "lattice_bar_chart_y_label", 
        label = NULL, 
        value = "count"
      )
    }
  })
  
  ##### > Data to use in plot #####
  Plot_Data <- reactive({
    paste0(
      "summary(", 
      "Uploaded_Data[['", input$lattice_bar_chart_select_data, "']]",  
      "$", 
      input$lattice_bar_chart_select_variable,
      ")"
    ) %>% 
      parse(text = .) %>% 
      eval
  })
  
  ##### > Bar chart #####
  output$lattice_bar_chart_plot <- renderPlot({
    req(input$lattice_bar_chart_select_variable)
    barchart(
      x = Plot_Data(),
      horizontal = input$lattice_bar_chart_horizontally,
      col = input$lattice_bar_chart_color, 
      main = input$lattice_bar_chart_title,
      xlab = input$lattice_bar_chart_x_label,
      ylab = input$lattice_bar_chart_y_label
    )
  })
  
  ##### > Code #####
  Text_Data <- reactive({
    paste(
      "barchart(",
      paste0("  ",
             input$lattice_bar_chart_select_data, "$", 
             input$lattice_bar_chart_select_variable
             ),
      sep = "\n"
    ) %>% 
      
      ## Horizontal ##
      {if(isFALSE(input$lattice_bar_chart_horizontally)) 
        paste(
          paste0(., ","),
          paste0("  horizontal = ", input$lattice_bar_chart_horizontally), 
          sep = "\n"
        ) else .} %>% 
      
      ## Color ##
      {if(!is.null(input$lattice_bar_chart_color)) 
        paste(
          paste0(., ","), 
          paste0('  col = ', vector_format(input$lattice_bar_chart_color)), 
          sep = "\n"
        ) else .} %>%
      
      ## Title ##
      {if(input$lattice_bar_chart_title != "")
        paste(
          paste0(., ","),  
          paste0('  main = "', input$lattice_bar_chart_title, '"'),
          sep = "\n"
        ) else .} %>%
      
      ## X label ##
      {if(input$lattice_bar_chart_x_label != "")
        paste(
          paste0(., ","), 
          paste0('  xlab = "', input$lattice_bar_chart_x_label, '"'),
          sep = "\n"
        ) else .} %>%
      
      ## Y label ##
      {if(input$lattice_bar_chart_y_label != "")
        paste(
          paste0(., ","), 
          paste0('  ylab = "', input$lattice_bar_chart_y_label, '"'),
          sep = "\n"
        ) else .} %>%
      paste(., ")", sep = "\n")
  })
  
  ##### > Render code #####
  output$lattice_bar_chart_code <- renderCode({
    highlight(Text_Data())
  })
  
  ##### > Toggle Code #####
  observeEvent(input$lattice_bar_chart_action_showcode, {
    req(input$lattice_bar_chart_select_data)
    shinyjs::toggle(id = "hidden_code", anim = TRUE)
  })
  
  ##### > Copy to clipboard #####
  observeEvent(
    input$clipbtn, 
    write_clip(Text_Data())
  )
  
}