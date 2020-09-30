##### UI ##################################################################################################

edit_data_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    
    ##### > Select data #####
    pickerInput(
      inputId = ns("edit_data_select"),
      label = "Select data:",
      choices = NULL
    ),
    
    ### > #####
    dataTableOutput(ns("edit_data_table"))
    
  )
  
}

##### Server ##############################################################################################

edit_data_server <- function(input, output, session){
  
  ##### > Update choices in select data #####
  observe({
    updatePickerInput(
      session, 
      inputId = "edit_data_select", 
      label = NULL,
      choices = names(Uploaded_Data)
    )
  })
  
  ##### > Show selectet data #####
  output$edit_data_table <- DT::renderDataTable(
    expr = {
      Uploaded_Data[[input$edit_data_select]]
    }, 
    filter = 'top',
    options = list(scrollX = 500, deferRender = TRUE, scroller = TRUE, fixedColumns = TRUE),
    editable = TRUE,
    rownames = FALSE) 
  
}
