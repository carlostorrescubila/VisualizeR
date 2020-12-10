##### UI #######################################################################################

home_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    
    h1(
      class = "home_title", 
      style="text-align:center",  
      "Welcome to", div(class = "VisualizeR", "VisualizeR", style = "font-size: 32px;"), "!"
      ),
    
    br(),
    
    ##### > Logo #####
    img(class = "Logo", src = 'VisualizeR.png'), 
    
    ##### > Paragraph #####
    p(
      "This is a Shiny web application that will help you make plots in a fast and simple 
      way with a friendly user interface. You can also get the chunk code needed to run 
      the plot you want. The motivation of this project is to help all those new users 
      of the R language to continue learning about data visualization while offering an 
      open source tool that allows to make plots in some seconds. To improve your experience 
      with visualizer, here you can choose different themes for the app:"
    ),
    
    ##### > Themes #####
    div(class = "Theme_changer", uiChangeThemeDropdown(title = "")),

    br(),
    
    ##### > How use ir ####
    h2("How to use ", div(class = "VisualizeR", "VisualizeR"), "?"),
    p("Just follow the next three steps:"), br(),
    timelineBlock(
      reversed = FALSE,
      timelineItem(
        title = "Step 1: upload your data",
        icon = "file-upload",
        color = "blue",
        "Upload your data in the following formats:", br(),
        timelineItemMedia(src = "https://www.flaticon.com/svg/static/icons/svg/337/337956.svg", width = "64px"),
        timelineItemMedia(src = "https://www.flaticon.com/svg/static/icons/svg/180/180855.svg", width = "64px"), 
        timelineItemMedia(src = "https://www.flaticon.com/svg/static/icons/svg/732/732220.svg", width = "64px"),
        timelineItemMedia(src = "https://www.flaticon.com/svg/static/icons/svg/882/882625.svg", width = "64px")
      ),
      timelineItem(
        title = "Step 2: edit your data",
        icon = "edit",
        color = "blue",
        "Edit your data..."
        ),
      timelineItem(
        title = "Step 3: visualize",
        icon = "paint-brush",
        color = "blue",
        "Visualize your data using the different plots available"
      )
    )
    
  )
  
}

##### Server ################################################################################

home_server <- function(input, output, session){
  

  
}
