##### UI ########################################################################
r_base_scatter_plot_ui <- function(id) {
  ns <- NS(id)

  fluidPage(

    ##### > Title #####
    h2(
      strong("Scatter plot with R base"),
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
        style = "max-height: 55vh; overflow-y: auto;",

        ##### >> Title #####
        h4(strong("List of Inputs"), class = "text-center"),

        ##### >> Selet data #####
        pickerInput(
          inputId = ns("r_base_scatter_plot_select_data"),
          label = "Select data:",
          choices = NULL,
          options = list(size = 4)
        ),

        ##### >> Selet variable X #####
        pickerInput(
          inputId = ns("r_base_scatter_plot_select_variable_x"),
          label = "Select variable on X:",
          choices = NULL,
          options = list(size = 4)
        ),

        ##### >> Selet variable Y #####
        pickerInput(
          inputId = ns("r_base_scatter_plot_select_variable_y"),
          label = "Select variable on Y:",
          choices = NULL,
          options = list(size = 4)
        ),
        
        ##### >> Size #####
        sliderTextInput(
          inputId = ns("r_base_scatter_plot_size"),
          label = "Size of the points:", 
          choices = seq(from = 0, to = 4, by = 0.1), 
          selected = 1, 
          width = "99%"
        ),
        
        ##### >> Color #####
        pickerInput(
          inputId = ns("r_base_scatter_plot_color"),
          label = "Color of the points:",
          choices = colors(), 
          selected = "black",
          width = "99.5%",
          options = list(size = 5, `live-search` = TRUE)
        ),
        
        ##### >> Shape #####
        sliderTextInput(
          inputId = ns("r_base_scatter_plot_shape"),
          label = "Shape of the points:", 
          choices = 0:25, 
          selected = "1",
          width = "99%"
        ),
        
        ##### >> Disabled buttons #####
        div(
          id = ns("r_base_scatter_plot_fill_button"),
          ### Line width ###
          sliderTextInput(
            inputId = ns("r_base_scatter_plot_lwd"),
            label = "Points border width:", 
            choices = seq(from = 0, to = 4, by = 0.1), 
            selected = 1, 
            width = "99%"
          ),
          ### Background ###
          pickerInput(
            inputId = ns("r_base_scatter_plot_fill"),
            label = "Points fill:",
            choices = colors(), 
            selected = "red",
            width = "99%",
            options = list(size = 5, `live-search` = TRUE)
          )
          ),
        
        ##### >> Main #####
        textInputAddon(
          inputId = ns("r_base_scatter_plot_title"),
          label = "Title:",
          placeholder = "Your title",
          addon = icon("font")
        ),

        ##### >> x-label #####
        textInputAddon(
          inputId = ns("r_base_scatter_plot_x_label"),
          label = "X label:",
          placeholder = "Your x-label",
          addon = icon("grip-lines")
        ),

        ##### >> y-label #####
        textInputAddon(
          inputId = ns("r_base_scatter_plot_y_label"),
          label = "Y label:",
          placeholder = "Your y-label",
          addon = icon("grip-lines-vertical")
        )
      )
    ),
    br(),

    ##### > Scatter plot #####
    plotOutput(ns("r_base_scatter_plot_plot")),
    br(),

    ##### > Show code #####

    ##### >> Action buttons show #####
    actionBttn(
      inputId = ns("r_base_scatter_plot_action_showcode"),
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
        codeOutput(ns("r_base_scatter_plot_code")),
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

r_base_scatter_plot_server <- function(input, output, session) {

  ##### > Update data choices #####
  observe({
    updatePickerInput(
      session,
      inputId = "r_base_scatter_plot_select_data",
      label = NULL,
      choices = names(Uploaded_Data)
    )
  })

  ##### > Update variables choices on X #####
  observe({
    req(input$r_base_scatter_plot_select_data)
    updatePickerInput(
      session,
      inputId = "r_base_scatter_plot_select_variable_x",
      label = NULL,
      choices = Uploaded_Data[[input$r_base_scatter_plot_select_data]] %>%
        select_if(function(col) {
          is.numeric(col)
        }) %>%
        colnames()
    )
  })

  ##### > Update variables choices on Y #####
  observe({
    req(input$r_base_scatter_plot_select_data)
    updatePickerInput(
      session,
      inputId = "r_base_scatter_plot_select_variable_y",
      label = NULL,
      choices = Uploaded_Data[[input$r_base_scatter_plot_select_data]] %>%
        select_if(function(col) {
          is.numeric(col)
        }) %>%
        colnames()
    )
  })

  ##### > Update X label #####
  observe({
    req(input$r_base_scatter_plot_select_variable_x)
    updateTextInput(
      session,
      inputId = "r_base_scatter_plot_x_label",
      label = NULL,
      value = input$r_base_scatter_plot_select_variable_x
    )
  })

  ##### > Update Y label #####
  observe({
    req(input$r_base_scatter_plot_select_variable_y)
    updateTextInput(
      session,
      inputId = "r_base_scatter_plot_y_label",
      label = NULL,
      value = input$r_base_scatter_plot_select_variable_y
    )
  })
  
  ##### > Show-hide color fill #####
  observeEvent(input$r_base_scatter_plot_shape, {
    toggleState(
      id = "r_base_scatter_plot_fill_button",
      condition = input$r_base_scatter_plot_shape %in% 21:25
      )
    toggleState(
      id = "r_base_scatter_plot_fill_lwd",
      condition = input$r_base_scatter_plot_shape %in% 21:25
    )
  })

  ##### > Data to use in plot #####
  X_Data <- reactive({
    paste0(
      "Uploaded_Data[['", input$r_base_scatter_plot_select_data, "']]",
      "$",
      input$r_base_scatter_plot_select_variable_x
    ) %>%
      parse(text = .) %>%
      eval()
  })
  Y_Data <- reactive({
    paste0(
      "Uploaded_Data[['", input$r_base_scatter_plot_select_data, "']]",
      "$",
      input$r_base_scatter_plot_select_variable_y
    ) %>%
      parse(text = .) %>%
      eval()
  })

  ##### > Scatter plot #####
  output$r_base_scatter_plot_plot <- renderPlot({
    req(input$r_base_scatter_plot_select_data)
    plot(
      x = X_Data(),
      y = Y_Data(),
      cex = input$r_base_scatter_plot_size,
      col = input$r_base_scatter_plot_color,
      pch = input$r_base_scatter_plot_shape,
      lwd = input$r_base_scatter_plot_lwd,
      bg = input$r_base_scatter_plot_fill, 
      main = input$r_base_scatter_plot_title,
      xlab = input$r_base_scatter_plot_x_label,
      ylab = input$r_base_scatter_plot_y_label
    )
  })

  ##### > Code #####
  Text_Data <- reactive({
    
    paste(
      "plot(",
      paste0(
        "  x = ",
        input$r_base_scatter_plot_select_data, "$",
        input$r_base_scatter_plot_select_variable_x, ","
      ),
      paste0(
        "  y = ",
        input$r_base_scatter_plot_select_data, "$",
        input$r_base_scatter_plot_select_variable_y
      ),
      sep = "\n"
    ) %>% 
      
      ## Color ##
      {if(input$r_base_scatter_plot_color != "black")
        paste(
          paste0(., ","),
          paste0('  col = "', input$r_base_scatter_plot_color, '"'),
          sep = "\n"
        )
        else .} %>%
      
      ## Size ##
      {if(input$r_base_scatter_plot_size != 1) 
        paste(
          paste0(., ","), 
          paste0('  cex = ', input$r_base_scatter_plot_size),
          sep = "\n"
        ) 
        else .} %>% 
      
      ## Shape of points ##
      {if(input$r_base_scatter_plot_shape != 1) 
        paste(
          paste0(., ","), 
          paste0('  pch = ', input$r_base_scatter_plot_shape),
          sep = "\n"
        ) 
        else .} %>% 
      
      ## Filled points ##
      {if(input$r_base_scatter_plot_shape %in% 21:25) 
        paste(
          paste0(., ","), 
          paste0('  lwd = "', input$r_base_scatter_plot_lwd, '",'),
          paste0('  bg = "', input$r_base_scatter_plot_fill, '"'),
          sep = "\n"
        ) 
        else .} %>% 
      
      ## Title ##
      {if(input$r_base_scatter_plot_title != "")
        paste(
          paste0(., ","),  
          paste0('  main = "', input$r_base_scatter_plot_title, '"'),
          sep = "\n"
        ) else .} %>%
      
      ## X label ##
      {if(input$r_base_scatter_plot_x_label != "")
        paste(
          paste0(., ","), 
          paste0('  xlab = "', input$r_base_scatter_plot_x_label, '"'),
          sep = "\n"
        ) else .} %>%
      
      ## Y label ##
      {if(input$r_base_scatter_plot_y_label != "")
        paste(
          paste0(., ","), 
          paste0('  ylab = "', input$r_base_scatter_plot_y_label, '"'),
          sep = "\n"
        ) else .} %>%
      paste(., ")", sep = "\n")
    
  })

  ##### > Render code #####
  output$r_base_scatter_plot_code <- renderCode({
    highlight(Text_Data())
  })

  ##### > Toggle Code #####
  observeEvent(input$r_base_scatter_plot_action_showcode, {
    req(input$r_base_scatter_plot_select_data)
    shinyjs::toggle(id = "hidden_code", anim = TRUE)
  })

  ##### > Copy to clipboard #####
  observeEvent(
    input$clipbtn,
    write_clip(Text_Data())
  )
}