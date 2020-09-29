##### UI ##################################################################################################

r_base_bar_chart_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    
    ### Title
    h2(
      strong("Bar chart with R base"),
      class = "text-center"
    )
    
  )
  
  
}

##### Server ##############################################################################################

r_base_bar_chart_server <- function(input, output, session){
  
  
  
}
