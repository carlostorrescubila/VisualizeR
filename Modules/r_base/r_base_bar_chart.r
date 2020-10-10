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
      style = "unite", 
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
        
        ##### >> x-label #####
        textInputAddon(
          inputId = ns("r_base_bar_chart_x_label"),
          label = "X label:", 
          placeholder = "Your x-label",
          addon = icon("grip-lines")
        ),
        
        ##### >> y-label #####
        textInputAddon(
          inputId = ns("r_base_bar_chart_y_label"),
          label = "Y label:", 
          placeholder = "Your y-label",
          addon = icon("grip-lines-vertical")
        )
          
      )
      
    ), 
  br(),
  
  ##### > Bar chart #####
  plotOutput(ns("r_base_bar_chart_plot")), 
  br(),
  # '<pre><code class="language-r" id="codeId002">checkboxGroupButtons(
  #   inputId = "Id002",
  #   label = "Choices", 
  #   choices = c("Choice 1", "Choice 2", "Choice 3"),
  #   status = "danger"
  # )</code></pre>',
  
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
  shinyjs::hidden(
    div(
      id = ns("hidden_code"),
      codeOutput(ns("r_base_bar_chart_code")),
      div(
        style = "float: right", 
        actionButton(
          inputId = ns("clipbtn"),
          label = "",
          icon = icon("copy", "fa-2x"),
          style = "color: white; background-color: #2C3E50"
        ),
        bsTooltip(id = ns("clipbtn"), title = "Copy to clipboard", placement = "left")
      )
      )
    ) 
  # HTML(highlight('ggplot2::ggplot()')),
  # pre(code(HTML(highlight('ggplot2::ggplot()'))))
  
  )

}

##### Server ######################################################################

r_base_bar_chart_server <- function(input, output, session){
  output$message <- renderText({
    code("TRUE", class = "language-r")
    })
  
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
  
  ##### > Code #####
  Text_Data <- reactive({
    paste(
      "barplot(",
      paste0("  summary(",
             input$r_base_bar_chart_select_data, "$", input$r_base_bar_chart_select_variable,
             ")", ","),
      paste0("  horiz = ", !input$r_base_bar_chart_vertically,  ","   ),
      paste0('  col = "',  input$r_base_bar_chart_color,        '",'  ),
      paste0('  main = "', input$r_base_bar_chart_title,        '",'  ),
      paste0('  xlab = "', input$r_base_bar_chart_x_label,      '",'  ),
      paste0('  ylab = "', input$r_base_bar_chart_y_label,      '"'   ),
      ")",
      sep = "\n"
    )
  })
  
  ##### > Render code #####
  output$r_base_bar_chart_code <- renderCode({ 
    highlight(Text_Data())
    })

  ##### > Toggle Code #####
  observeEvent(input$r_base_bar_chart_action_showcode, {
    req(input$r_base_bar_chart_select_data)
    shinyjs::toggle(id = "hidden_code", anim = TRUE)
  })
  
  ##### > Copy to clipboard #####
  observeEvent(
    input$clipbtn, 
    write_clip(Text_Data())
    )
  
}
