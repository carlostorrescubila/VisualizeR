##### UI ##################################################################################################

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
      pickerInput(
        inputId = ns('r_base_bar_chart_select_data'),
        label = 'Select data:',
        choices = NULL
        ),
      
      ##### >> Selet variable #####
      pickerInput(
        inputId = ns('r_base_bar_chart_select_variable'),
        label = 'Select variable:',
        choices = NULL
      ),
      
      ##### >> Bars vertically #####
      h5(strong("Bars drawn vertically:")), 
      switchInput(
        inputId = ns("r_base_bar_chart_select_horizontal"), 
        label = img(icon("chart-bar", class = "solid")), 
        value = TRUE, 
        onLabel = "TRUE",
        offLabel = "FALSE", 
        onStatus = "success", 
        offStatus = "danger" 
        ), 
      
      ##### >> Color #####
      pickerInput(
        inputId = ns("r_base_bar_chart_color"),
        label = "Color of the bars:",
        choices = Colors_Choices
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

##### Server ##############################################################################################

r_base_bar_chart_server <- function(input, output, session){
  
  ##### > Update choices in select data input #####
  observe({
    updatePickerInput(
      session, 
      inputId = "r_base_bar_chart_select_data", 
      label = NULL,
      choices = names(Uploaded_Data)
    )
  })
  
}
