##### UI ##################################################################################################

upload_excel_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    
    ### Title
    h2(
      strong("Here you can upload your data to visualize in an excel format"),
      class = "text-center"
    )
    
  )
    
  
}

##### Server ##############################################################################################

upload_excel_server <- function(input, output, session){
  
  
  
}
