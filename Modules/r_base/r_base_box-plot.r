##### UI ########################################################################
r_base_box_plot_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    
    ##### > Title #####
    h2(
      strong("Box-plot with R base"),
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
          inputId = ns('r_base_box_plot_select_data'),
          label = 'Select data:',
          choices = NULL, 
          options = list(size = 4)
        ),
        
        ##### >> Selet variable #####
        pickerInput(
          inputId = ns("r_base_box_plot_select_variable"),
          label = 'Select variable:',
          choices = NULL, 
          options = list(size = 4)
        ),
        
        ##### >> Selet groups #####
        pickerInput(
          inputId = ns("r_base_box_plot_select_groups"),
          label = 'Select groups:',
          choices = NULL, 
          options = list(size = 4)
        ),
        
        ##### >> Box vertically #####
        h5(strong("Bars drawn vertically:")), 
        switchInput(
          inputId = ns("r_base_box_plot_vertically"),
          label = img(icon("chart-bar", class = "solid")),
          value = TRUE,
          onLabel = "TRUE",
          offLabel = "FALSE",
          onStatus = "success",
          offStatus = "danger"
        ),
        
        ##### >> Color #####
        selectizeInput(
          inputId = ns('r_base_box_plot_color'),
          label = 'Color of the bars:',
          choices = colors(),
          multiple = TRUE,
          width = "99.5%",
          options = list(size = 5)
        ),
        
        # ###
        # sliderInput(
        #   inputId = ns("r_base_box_plot_n"), 
        #   min = 1, max = 5, value = 5, 
        #   label = "Number of groups"
        #   ),
        
        ##### >> Main #####
        textInputAddon(
          inputId = ns("r_base_box_plot_title"), 
          label = "Title:", 
          placeholder = "Your title", 
          addon = icon("font")
          ),
        
        ##### >> x-label #####
        textInputAddon(
          inputId = ns("r_base_box_plot_x_label"),
          label = "X label:", 
          placeholder = "Your x-label",
          addon = icon("grip-lines")
        ),
        
        ##### >> y-label #####
        textInputAddon(
          inputId = ns("r_base_box_plot_y_label"),
          label = "Y label:", 
          placeholder = "Your y-label",
          addon = icon("grip-lines-vertical")
        )
          
      )
      
    ), 
  br(),
  
  ##### > Bar chart #####
  plotOutput(ns("r_base_box_plot_plot")), 
  br(),
  
  ##### > Show code #####
  
  ##### >> Action buttons show #####
  actionBttn(
    inputId = ns("r_base_box_plot_action_showcode"),
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
      codeOutput(ns("r_base_box_plot_code")), 
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

r_base_box_plot_server <- function(input, output, session){
  
  ##### > Update data choices #####
  observe({
    updatePickerInput(
      session, 
      inputId = "r_base_box_plot_select_data", 
      label = NULL, 
      choices = names(Uploaded_Data)
    )
  })
  
  ##### > Update variable choice #####
  observe({
    req(input$r_base_box_plot_select_data)
    updatePickerInput(
      session, 
      inputId = "r_base_box_plot_select_variable", 
      label = NULL, 
      choices = Uploaded_Data[[input$r_base_box_plot_select_data]] %>% 
        select_if(function(col){
          is.numeric(col) 
        } 
        ) %>% 
        colnames
    )
  })
  
  ##### > Update groups choice #####
  observe({
    req(input$r_base_box_plot_select_data)
    updatePickerInput(
      session,
      inputId = "r_base_box_plot_select_groups",
      label = NULL,
      choices = Uploaded_Data[[input$r_base_box_plot_select_data]] %>%
        select_if(function(col){
          is.character(col) | is.factor(col)
          }
        ) %>%
        colnames
    )
  })
  
  ##### > Update colors #####

  # Max_Colors <- reactive({
  #   paste0(
  #     "Uploaded_Data[['", input$r_base_box_plot_select_data, "']]",
  #     "$", input$r_base_box_plot_select_groups
  #     ) %>%
  #     parse(text = .) %>%
  #     eval %>%
  #     levels %>%
  #     length()
  # })
  # observe({
  #   req(input$r_base_box_plot_select_data)
  #   updateSelectizeInput(
  #     session,
  #     inputId = "r_base_box_plot_color",
  #     options = list(maxItems = 3)
  #     )
  # })

  ##### > Update X label #####
  observe({
    req(input$r_base_box_plot_select_groups)
    if(isTRUE(input$r_base_box_plot_vertically)){
      updateTextInput(
        session,
        inputId = "r_base_box_plot_x_label",
        label = NULL,
        value = input$r_base_box_plot_select_groups
        )
    } else {
      updateTextInput(
        session,
        inputId = "r_base_box_plot_x_label",
        label = NULL,
        value = input$r_base_box_plot_select_variable
        )
      }
    })

  ##### > Update Y label #####
  observe({
    req(input$r_base_box_plot_select_groups)
    if(isFALSE(input$r_base_box_plot_vertically)){
      updateTextInput(
        session,
        inputId = "r_base_box_plot_y_label",
        label = NULL,
        value = input$r_base_box_plot_select_groups
      )
    } else {
      updateTextInput(
        session,
        inputId = "r_base_box_plot_y_label",
        label = NULL,
        value = input$r_base_box_plot_select_variable
      )
    }
  })

  ##### > Formula used in plot #####
  My_Formula <- reactive({
    paste0(
      "Uploaded_Data[['", input$r_base_box_plot_select_data, "']]",  
      "$", 
      input$r_base_box_plot_select_variable, 
      "~",
      "Uploaded_Data[['", input$r_base_box_plot_select_data, "']]",  
      "$", 
      input$r_base_box_plot_select_groups
      ) %>%
      as.formula()
  })

  ##### > Box-plot #####
  output$r_base_box_plot_plot <- renderPlot({
    req(input$r_base_box_plot_select_data)
    boxplot(
      My_Formula(),
      horizontal = !input$r_base_box_plot_vertically,
      col = input$r_base_box_plot_color,
      main = input$r_base_box_plot_title,
      xlab = input$r_base_box_plot_x_label,
      ylab = input$r_base_box_plot_y_label
      )
  })

  ##### > Code #####
  Text_Data <- reactive({
    paste(
      "boxplot(",
      paste0("  ", input$r_base_box_plot_select_variable, "~", 
             input$r_base_box_plot_select_groups, ","),
      paste0("  data = ",        input$r_base_box_plot_select_data,  ","   ),
      paste0("  horizontal = ", !input$r_base_box_plot_vertically,   ","   ),
      sep = "\n"
      ) %>% 
      {if(!is.null(input$r_base_box_plot_color)) 
        paste(
          ., 
          paste0('  col = "', vector_2_string(input$r_base_box_plot_color), '",'), 
          sep = "\n"
          ) else .} %>% 
      paste(
        paste0('  main = "',       input$r_base_box_plot_title,        '",'  ),
        paste0('  xlab = "',       input$r_base_box_plot_x_label,      '",'  ),
        paste0('  ylab = "',       input$r_base_box_plot_y_label,      '"'   ),
        ")",
        sep = "\n"
      )
  })

  ##### > Render code #####
  output$r_base_box_plot_code <- renderCode({
    highlight(Text_Data())
  })

  ##### > Toggle Code #####
  observeEvent(input$r_base_box_plot_action_showcode, {
    req(input$r_base_box_plot_select_data)
    shinyjs::toggle(id = "hidden_code", anim = TRUE)
  })

  ##### > Copy to clipboard #####
  observeEvent(
    input$clipbtn,
    write_clip(Text_Data())
    )
  
}