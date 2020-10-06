##### UI ########################################################################
r_base_bar_chart_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    
    ##### > Title #####
    h2(
      strong("Bar chart with R base"),
      class = "text-center"
    ), 
    
    ##### > Dropdown button #####
    dropdown(
      
      ##### >>Dropdown options #####
      # circle = TRUE, 
      style = "unite", 
      status = "primary",
      icon = icon("gear"), 
      width = "300px",
      tooltip = tooltipOptions(title = "Click to see inputs !"), 
      
      div(
        style='max-height: 55vh; overflow-y: auto;',
        
        ##### >> Title #####
        h4(strong("List of Inputs"), class = "text-center"),
        
        ##### >> Selet data #####
        pickerInput(
          inputId = ns('r_base_bar_chart_select_data'),
          label = 'Select data:',
          choices = NULL, 
          options = list(size = 4)
        ),
        
        ##### >> Selet variable #####
        pickerInput(
          inputId = ns("r_base_bar_chart_select_variable"),
          label = 'Select variable:',
          choices = NULL, 
          options = list(size = 4)
        ),
        
        ##### >> Bars vertically #####
        h5(strong("Bars drawn vertically:")), 
        switchInput(
          inputId = ns("r_base_bar_chart_vertically"),
          label = img(icon("chart-bar", class = "solid")),
          value = TRUE,
          onLabel = "TRUE",
          offLabel = "FALSE",
          onStatus = "success",
          offStatus = "danger"
        ),
        
        ##### >> Color #####
        pickerInput(
          inputId = ns('r_base_bar_chart_color'),
          label = 'Color of the bars:',
          choices = Colors_Choices, 
          options = list(size = 4)
        ),
        
        ##### >> Main #####
        textInputAddon(
          inputId = ns("r_base_bar_chart_title"), 
          label = "Title:", 
          placeholder = "Your title", 
          addon = icon("font")
          ), 
        # textInput(
        #   inputId = ns("r_base_bar_chart_title"),
        #   label = "Title:",
        #   value = "Bar chart with R base"
        # ),
        
        ##### >> x-label #####
        textInputAddon(
          inputId = ns("r_base_bar_chart_x_label"),
          label = "X label:", 
          placeholder = "Your x-label",
          addon = icon("grip-lines")
        ),
        # textInput(
        #   inputId = ns("r_base_bar_chart_x_label"),
        #   label = "X label:",
        #   value = ""
        # ),
        
        ##### >> y-label #####
        textInputAddon(
          inputId = ns("r_base_bar_chart_y_label"),
          label = "Y label:", 
          placeholder = "Your y-label",
          addon = icon("grip-lines-vertical")
        )
        # textInput(
        #   inputId = ns("r_base_bar_chart_y_label"),
        #   label = "Y label:",
        #   value = ""
        # )
          
      )
      
    ), 
  br(),
  
  ##### > Bar chart #####
  plotOutput(ns("r_base_bar_chart_plot")), 
  br(), 
  
  ##### > Show code #####
  
  ##### >> Action buttons show #####
  actionBttn(
    inputId = ns("r_base_bar_chart_action_showcode"),
    label = "Show code",
    icon = icon("code"),
    style = "stretch",
    color = "primary"
  ),
  
  ##### >> Code #####
  shinyjs::useShinyjs(),
  rclipboardSetup(), 
  shinyjs::hidden(
    verbatimTextOutput(ns("r_base_bar_chart_code")), 
    uiOutput(ns("r_base_bar_chart_copy_clipboard"))
    )
  
  )

}

##### Server ######################################################################

r_base_bar_chart_server <- function(input, output, session){
  
  ##### > Update data choices #####
  observe({
    updatePickerInput(
      session, 
      inputId = "r_base_bar_chart_select_data", 
      label = NULL, 
      choices = names(Uploaded_Data)
    )
  })
  
  ##### > Update variables choices #####
  observe({
    req(input$r_base_bar_chart_select_data)
    updatePickerInput(
      session, 
      inputId = "r_base_bar_chart_select_variable", 
      label = NULL, 
      choices = Uploaded_Data[[input$r_base_bar_chart_select_data]] %>% 
        select_if(function(col){
          is.character(col) | is.factor(col)
          } 
        ) %>% 
        colnames
    )
  })
  
  ##### > Update X label #####
  observe({
    req(input$r_base_bar_chart_select_variable)
    if(isTRUE(input$r_base_bar_chart_vertically)){  #!is.null(input$r_base_bar_chart_select_variable)
      updateTextInput(
        session, 
        inputId = "r_base_bar_chart_x_label", 
        label = NULL, 
        value = input$r_base_bar_chart_select_variable
        )
    } else {
      updateTextInput(
        session, 
        inputId = "r_base_bar_chart_x_label", 
        label = NULL, 
        value = "count"
        )
      }
    })
  
  ##### > Update Y label #####
  observe({
    req(input$r_base_bar_chart_select_variable)
    if(isFALSE(input$r_base_bar_chart_vertically)){
      updateTextInput(
        session, 
        inputId = "r_base_bar_chart_y_label", 
        label = NULL, 
        value = input$r_base_bar_chart_select_variable
      )
    } else {
      updateTextInput(
        session, 
        inputId = "r_base_bar_chart_y_label", 
        label = NULL, 
        value = "count"
      )
    }
  })
  
  ##### > Data to use in plot #####
  Plot_Data <- reactive({
    paste0(
      "summary(", 
      "Uploaded_Data[['", input$r_base_bar_chart_select_data, "']]",  
      "$", 
      input$r_base_bar_chart_select_variable,
      ")"
      ) %>% 
      parse(text = .) %>% 
      eval
  })
  
  ##### > Bar chart #####
  output$r_base_bar_chart_plot <- renderPlot({
    req(input$r_base_bar_chart_select_variable)
    barplot(
      height = Plot_Data(),
      horiz = !input$r_base_bar_chart_vertically,
      col = input$r_base_bar_chart_color, 
      main = input$r_base_bar_chart_title,
      xlab = input$r_base_bar_chart_x_label,
      ylab = input$r_base_bar_chart_y_label
      )
  })
  
  ##### > Text to use in show code #####
  Text_Data <- reactive({
    paste(
      "barplot(", 
      paste0("summary(",
      input$r_base_bar_chart_select_data, "$", input$r_base_bar_chart_select_variable,
      ")", ","), 
      paste0("horiz = ", input$r_base_bar_chart_vertically, ","), 
      paste0('col = "', input$r_base_bar_chart_color, '"', ","), 
      paste0('main = "', input$r_base_bar_chart_title, '"', ","),
      paste0('xlab = "', input$r_base_bar_chart_x_label, '"', ","),
      paste0('ylab = "', input$r_base_bar_chart_y_label, '"'), 
      ")", 
      sep = "\n"
    ) 
  })
  output$r_base_bar_chart_code <- renderText({Text_Data()})
  
  ##### > Toggle text #####
  observeEvent(input$r_base_bar_chart_action_showcode, {
    req(input$r_base_bar_chart_select_data)
    
    ##### >> Show/Hide code #####
    shinyjs::toggle(id = "r_base_bar_chart_code", anim = TRUE)
    
    ##### >> Copy to clipboard button #####
    output$r_base_bar_chart_copy_clipboard <- renderUI({
      rclipButton(
        inputId = "r_base_bar_chart_copy_clipboard_button", 
        label = "Copy to clipboard", 
        clipText = Text_Data(), 
        icon("clipboard")
        )
    })
  })

  # Workaround for execution within RStudio version < 1.2
  observeEvent(input$r_base_bar_chart_copy_clipboard_button, {
    clipr::write_clip(Text_Data())
    })
  
}
