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
    dropdownButton(
      
      ##### >> Title #####
      h4(strong("List of Inputs"), class = "text-center"),
      
      ##### >> Selet data #####
      selectInput(
        inputId = ns('r_base_bar_chart_select_data'),
        label = 'Select data:',
        choices = NULL
      ),
      
      ##### >> Selet variable #####
      selectInput(
        inputId = ns("r_base_bar_chart_select_variable"),
        label = 'Select variable:',
        choices = NULL
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
      selectInput(
        inputId = ns('r_base_bar_chart_color'),
        label = 'Color of the bars:',
        choices = Colors_Choices
      ),
      
      ##### >> Main #####
      textInput(
        inputId = ns("r_base_bar_chart_title"),
        label = "Title:",
        value = "Bar chart with R base"
      ),
      
      ##### >> x-label #####
      textInput(
        inputId = ns("r_base_bar_chart_x_label"),
        label = "X label:",
        value = ""
      ),
      
      ##### >> y-label #####
      textInput(
        inputId = ns("r_base_bar_chart_y_label"),
        label = "Y label:",
        value = ""
      ),
      
      ##### >>Dropdown oprions #####
      circle = TRUE, 
      status = "primary",
      icon = icon("gear"), 
      width = "300px",
      tooltip = tooltipOptions(title = "Click to see inputs !")
      
    ),
    
    ##### > Bar chart #####
    plotOutput(ns("r_base_bar_chart_plot"))
    
  )
  
}

##### Server ######################################################################

r_base_bar_chart_server <- function(input, output, session){
  
  ##### > Update data choices #####
  observe({
    updateSelectInput(
      session, 
      inputId = "r_base_bar_chart_select_data", 
      label = NULL, 
      choices = names(Uploaded_Data)
    )
  })
  
  ##### > Update variables choices #####
  observe({
    req(input$r_base_bar_chart_select_data)
    updateSelectInput(
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
    if(isTRUE(input$r_base_bar_chart_vertically)){
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
    if(isFALSE(input$r_base_bar_chart_vertically)){
      req(input$r_base_bar_chart_select_variable)
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
  
  ##### > Text to use in show code #####
  Text_Data <- reactive({
    paste0(
      "summary(", 
      input$r_base_bar_chart_select_data, 
      "$", 
      input$r_base_bar_chart_select_variable,
      ")"
    ) 
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
  
  ##### > Show code #####
  
}
