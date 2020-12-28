##### UI ########################################################################
lattice_scatter_plot_ui <- function(id) {
  ns <- NS(id)

  fluidPage(

    ##### > Title #####
    h2(
      strong("Scatter plot with lattice"),
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
          inputId = ns("lattice_scatter_plot_select_data"),
          label = "Select data:",
          choices = NULL,
          options = list(size = 4)
        ),

        ##### >> Selet variable X #####
        pickerInput(
          inputId = ns("lattice_scatter_plot_select_variable_x"),
          label = "Select variable on X:",
          choices = NULL,
          options = list(size = 4)
        ),

        ##### >> Selet variable Y #####
        pickerInput(
          inputId = ns("lattice_scatter_plot_select_variable_y"),
          label = "Select variable on Y:",
          choices = NULL,
          options = list(size = 4)
        ),
        
        #### >> Groups #####
        pickerInput(
          inputId = ns("lattice_scatter_plot_select_groups"),
          label = "Groups:",
          choices = NULL,
          options = list(size = 4)
        ),
        
        ##### >> Type #####
        pickerInput(
          inputId = ns("lattice_scatter_plot_type"), 
          label = "Add:", 
          choices = c(
            "Points" = "p", 
            "Reference Grid" = "g", 
            "Linear Regression Line"  = "r", 
            "LOESS Curve Fitting" = "smooth"
            ), 
          multiple = TRUE, 
          selected = "p", 
          options = list(`actions-box` = TRUE),
        ),
        
        ##### >> Size #####
        sliderTextInput(
          inputId = ns("lattice_scatter_plot_size"),
          label = "Size of the points:", 
          choices = seq(from = 0, to = 4, by = 0.1), 
          selected = 1, 
          width = "99%"
        ),
        
        ##### >> Color #####
        pickerInput(
          inputId = ns("lattice_scatter_plot_color"),
          label = "Color of the points:",
          choices = c("default" = "#7AC5CD", colors()), 
          width = "99.5%",
          options = list(size = 5, `live-search` = TRUE)
        ),
        
        ##### >> Shape #####
        sliderTextInput(
          inputId = ns("lattice_scatter_plot_shape"),
          label = "Shape of the points:", 
          choices = 0:25, 
          selected = "1",
          width = "99%"
        ),
        
        ##### >> Disabled buttons #####
        div(
          id = ns("lattice_scatter_plot_fill_button"),
          ### Line width ###
          sliderTextInput(
            inputId = ns("lattice_scatter_plot_lwd"),
            label = "Points border width:", 
            choices = seq(from = 0, to = 4, by = 0.1), 
            selected = 1, 
            width = "99%"
          ),
          ### Background ###
          pickerInput(
            inputId = ns("lattice_scatter_plot_fill"),
            label = "Points fill:",
            choices = colors(), 
            selected = "red",
            width = "99%",
            options = list(size = 5, `live-search` = TRUE)
          )
          ),
        
        ##### >> Main #####
        textInputAddon(
          inputId = ns("lattice_scatter_plot_title"),
          label = "Title:",
          placeholder = "Your title",
          addon = icon("font")
        ),

        ##### >> x-label #####
        textInputAddon(
          inputId = ns("lattice_scatter_plot_x_label"),
          label = "X label:",
          placeholder = "Your x-label",
          addon = icon("grip-lines")
        ),

        ##### >> y-label #####
        textInputAddon(
          inputId = ns("lattice_scatter_plot_y_label"),
          label = "Y label:",
          placeholder = "Your y-label",
          addon = icon("grip-lines-vertical")
        )
      )
    ),
    br(),

    ##### > Scatter plot #####
    plotOutput(ns("lattice_scatter_plot_plot")),
    br(),

    ##### > Show code #####

    ##### >> Action buttons show #####
    actionBttn(
      inputId = ns("lattice_scatter_plot_action_showcode"),
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
        codeOutput(ns("lattice_scatter_plot_code")),
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

lattice_scatter_plot_server <- function(input, output, session) {

  ##### > Update data choices #####
  observe({
    updatePickerInput(
      session,
      inputId = "lattice_scatter_plot_select_data",
      label = NULL,
      choices = names(Uploaded_Data)
    )
  })

  ##### > Update variables choices on X #####
  observe({
    req(input$lattice_scatter_plot_select_data)
    updatePickerInput(
      session,
      inputId = "lattice_scatter_plot_select_variable_x",
      label = NULL,
      choices = Uploaded_Data[[input$lattice_scatter_plot_select_data]] %>%
        select_if(function(col) {
          is.numeric(col)
        }) %>%
        colnames()
    )
  })

  ##### > Update variables choices on Y #####
  observe({
    req(input$lattice_scatter_plot_select_data)
    updatePickerInput(
      session,
      inputId = "lattice_scatter_plot_select_variable_y",
      label = NULL,
      choices = Uploaded_Data[[input$lattice_scatter_plot_select_data]] %>%
        select_if(function(col) {
          is.numeric(col)
        }) %>%
        colnames()
    )
  })
  
  ##### > Update groups #####
  observe({
    req(input$lattice_scatter_plot_select_data)
    updatePickerInput(
      session,
      inputId = "lattice_scatter_plot_select_groups",
      label = NULL,
      choices = Uploaded_Data[[input$lattice_scatter_plot_select_data]] %>%
        select_if(function(col) {
          is.factor(col)
        }) %>%
        colnames() %>% 
        c("NULL", .)
    )
  })

  ##### > Update X label #####
  observe({
    req(input$lattice_scatter_plot_select_variable_x)
    updateTextInput(
      session,
      inputId = "lattice_scatter_plot_x_label",
      label = NULL,
      value = input$lattice_scatter_plot_select_variable_x
    )
  })

  ##### > Update Y label #####
  observe({
    req(input$lattice_scatter_plot_select_variable_y)
    updateTextInput(
      session,
      inputId = "lattice_scatter_plot_y_label",
      label = NULL,
      value = input$lattice_scatter_plot_select_variable_y
    )
  })
  
  ##### > Show-hide color fill #####
  observeEvent(input$lattice_scatter_plot_shape, {
    toggleState(
      id = "lattice_scatter_plot_fill_button",
      condition = input$lattice_scatter_plot_shape %in% 21:25
      )
    toggleState(
      id = "lattice_scatter_plot_fill_lwd",
      condition = input$lattice_scatter_plot_shape %in% 21:25
    )
  })

  ##### > Data to use in plot #####
  My_Formula <- reactive({
    paste0(
      input$lattice_scatter_plot_select_variable_y,
      "~",
      input$lattice_scatter_plot_select_variable_x
      ) %>%
      as.formula()
  })

  ##### > Scatter plot #####
  output$lattice_scatter_plot_plot <- renderPlot({
    req(input$lattice_scatter_plot_select_data)
    if(input$lattice_scatter_plot_select_groups == "NULL"){
      xyplot(
        My_Formula(),
        data = Uploaded_Data[[input$lattice_scatter_plot_select_data]],
        type = input$lattice_scatter_plot_type, 
        cex = input$lattice_scatter_plot_size,
        col = input$lattice_scatter_plot_color,
        pch = input$lattice_scatter_plot_shape,
        lwd = input$lattice_scatter_plot_lwd,
        bg = input$lattice_scatter_plot_fill, 
        main = input$lattice_scatter_plot_title,
        xlab = input$lattice_scatter_plot_x_label,
        ylab = input$lattice_scatter_plot_y_label
      )
    } else {
      xyplot(
        My_Formula(),
        data = Uploaded_Data[[input$lattice_scatter_plot_select_data]], 
        groups = input$lattice_scatter_plot_select_groups,
        type = input$lattice_scatter_plot_type,
        cex = input$lattice_scatter_plot_size,
        # col = input$lattice_scatter_plot_color,
        pch = input$lattice_scatter_plot_shape,
        lwd = input$lattice_scatter_plot_lwd,
        bg = input$lattice_scatter_plot_fill, 
        main = input$lattice_scatter_plot_title,
        xlab = input$lattice_scatter_plot_x_label,
        ylab = input$lattice_scatter_plot_y_label
      )
    }
  })

  ##### > Code #####
  Text_Data <- reactive({
    
    paste(
      "xyplot(",
      paste0(
        "  ",
        input$lattice_scatter_plot_select_variable_y, "~",
        input$lattice_scatter_plot_select_variable_x, ","
      ),
      paste0("  data = ", input$lattice_scatter_plot_select_data),
      sep = "\n"
    ) %>% 
      
      ## Groups ##
      {if(input$lattice_scatter_plot_select_groups != "NULL")
        paste(
          paste0(., ","),
          paste0("  groups = ", input$lattice_scatter_plot_select_groups), 
          sep = "\n"
        )
      else .} %>% 
      
      ## Type ##
      {if(
        length(input$lattice_scatter_plot_type) == 1 & 
        "p" %in% input$lattice_scatter_plot_type
        ){.} else {
          if(length(input$lattice_scatter_plot_type) == 0){
            paste(
              paste0(., ","),
              paste0("  type = ", "NULL"), 
              sep = "\n"
            )
          } else {
            paste(
            paste0(., ","),
            paste0("  type = ", vector_format(input$lattice_scatter_plot_type)), 
            sep = "\n"
            )
          }
        }
      } %>% 
      
      ## Color ##
      {if(input$lattice_scatter_plot_color != "black")
        paste(
          paste0(., ","),
          paste0('  col = "', input$lattice_scatter_plot_color, '"'),
          sep = "\n"
        )
        else .} %>%
      
      ## Size ##
      {if(input$lattice_scatter_plot_size != 1) 
        paste(
          paste0(., ","), 
          paste0('  cex = ', input$lattice_scatter_plot_size),
          sep = "\n"
        ) 
        else .} %>% 
      
      ## Shape of points ##
      {if(input$lattice_scatter_plot_shape != 1) 
        paste(
          paste0(., ","), 
          paste0('  pch = ', input$lattice_scatter_plot_shape),
          sep = "\n"
        ) 
        else .} %>% 
      
      ## Filled points ##
      {if(input$lattice_scatter_plot_shape %in% 21:25) 
        paste(
          paste0(., ","), 
          paste0('  lwd = "', input$lattice_scatter_plot_lwd, '",'),
          paste0('  bg = "', input$lattice_scatter_plot_fill, '"'),
          sep = "\n"
        ) 
        else .} %>% 
      
      ## Title ##
      {if(input$lattice_scatter_plot_title != "")
        paste(
          paste0(., ","),  
          paste0('  main = "', input$lattice_scatter_plot_title, '"'),
          sep = "\n"
        ) else .} %>%
      
      ## X label ##
      {if(input$lattice_scatter_plot_x_label != "")
        paste(
          paste0(., ","), 
          paste0('  xlab = "', input$lattice_scatter_plot_x_label, '"'),
          sep = "\n"
        ) else .} %>%
      
      ## Y label ##
      {if(input$lattice_scatter_plot_y_label != "")
        paste(
          paste0(., ","), 
          paste0('  ylab = "', input$lattice_scatter_plot_y_label, '"'),
          sep = "\n"
        ) else .} %>%
      paste(., ")", sep = "\n")
    
  })

  ##### > Render code #####
  output$lattice_scatter_plot_code <- renderCode({
    highlight(Text_Data())
  })

  ##### > Toggle Code #####
  observeEvent(input$lattice_scatter_plot_action_showcode, {
    req(input$lattice_scatter_plot_select_data)
    shinyjs::toggle(id = "hidden_code", anim = TRUE)
  })

  ##### > Copy to clipboard #####
  observeEvent(
    input$clipbtn,
    write_clip(Text_Data())
  )
}