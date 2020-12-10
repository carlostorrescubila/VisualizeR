# Ui functions ------------------------------------------------------------
uiChangeThemeDropdown <- function(title = "Change theme:") {
  
  ns <- NS("moduleChangeTheme")
  dropdown <- tagList(
    pickerInput(
      inputId = ns("dbxChangeTheme"),
      label = title,
      choices = changeThemeChoices,
      selected = "grey_light"
    )
  )
  
  return(dropdown)
}

uiChangeThemeOutput <- function() {
  ns <- NS("moduleChangeTheme")
  themeOutput <- tagList(
    uiOutput(ns("uiChangeTheme"))
  )
  
  return(themeOutput)
}


# Server functions --------------------------------------------------------
serverChangeTheme <- function(input, output, session)
{
  observeEvent(
    input$dbxChangeTheme, 
    {
      output$uiChangeTheme <- renderUI({
        shinyDashboardThemes(theme = input$dbxChangeTheme)
      })
    }
  )
}