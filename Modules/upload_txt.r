##### UI ##################################################################################################

upload_txt_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    
    ##### > Title #####
    h2(
      strong("Here you can upload your data to visualize in format txt"),
      class = "text-center"
    )
    
  )
    
  
}

##### Server ##############################################################################################

upload_txt_server <- function(input, output, session){
  
  
  
}
