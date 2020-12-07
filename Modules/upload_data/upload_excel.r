##### UI #############################################################################

upload_excel_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    
    ##### > Title #####
    h2(
      strong("Here you can upload your data to visualize in an Excel format"),
      class = "text-center"
    ), 
    
    ##### > Select file #####
    fileInput(
      inputId = ns("upload_data_excel_file"),
      label = h4(strong("Select file to upload")),
      accept = c('.xlsx', '.xls')
    ),
    
    ##### > Select options #####
    fluidRow(
      
      ##### >> Name of data #####
      column(4, textInput(
        inputId = ns("upload_data_excel_text"),
        label = h4(strong("Name your database:")),
        value = NULL
      )),
      
      ##### >> Select sheet #####
      column(4, pickerInput(
        inputId = ns("upload_data_excel_sheet"),
        label = h4(strong("Select sheet:")),
        choices = NULL
      )), 
      
      ##### >> Range #####
      column(4, textInputAddon(
        inputId = ns("upload_data_excel_range"),
        label = h4(strong("Range to select:")),
        value = NULL, 
        placeholder = "A1:D10",
        addon = icon("filter")
      ))
      
    ),
    
    br(),
    
    fluidRow(
      
      ##### > Skip and max rows #####
      div(
        id = ns("upload_data_excel_range_disable"), 
        
        ##### >> Skip rows #####
        column(4, numericInput(
          inputId = ns("upload_data_excel_skip"),
          label = h4(strong("Number of rows to skip:")),
          value = 0, 
          min = 0
        )), 
      
        ##### >> Max rows #####
        column(4, numericInput(
          inputId = ns("upload_data_excel_max_rows"),
          label = h4(strong("Maximum number of data rows to read:")),
          value = Inf, 
          min = 0
        ))
        
        ), 
      
      ##### >> NA #####
      column(4, textInput(
        inputId = ns("upload_data_excel_na"),
        label = h4(strong("Consider as missing values:")),
        value = NULL
      ))
      
    ),
    
    ##### >> Checkbox for row names #####
    column(4, awesomeCheckbox(
      inputId = ns("upload_data_excel_first_row"),
      label = h4(strong("First row as colnames")),
      value = TRUE,
      status = "success"
    )),
    
    ##### > Show data #####
    dataTableOutput(ns("upload_data_excel_table")),
    
    br(), 
    
    ##### > Upload data button #####
    actionBttn(
      inputId = ns("upload_data_excel_confirm"),
      label = "Upload data",
      style = "bordered",
      color = "primary",
      size = "lg",
      block = TRUE
    )
    
  )
    
}

##### Server ##########################################################################

upload_excel_server <- function(input, output, session){
  
  ##### > Update the data name #####
  observe({
    req(input$upload_data_excel_file)
    updateTextInput(
      session,
      inputId =  'upload_data_excel_text',
      label = NULL,
      value = input$upload_data_excel_file$name %>% 
        str_replace(".xlsx", "") %>% 
        str_replace(".xls", "") %>% 
        paste0(., "_", input$upload_data_excel_sheet)
    )
  })
  
  ##### > Update sheets #####
  observe({
    req(input$upload_data_excel_file)
    updatePickerInput(
      session,
      inputId = "upload_data_excel_sheet",
      choices = excel_sheets(input$upload_data_excel_file$datapath)
      )
  })
  
  
  ##### > Enable other delim #####
  observeEvent(input$upload_data_excel_range, {
    toggleState(
      id = "upload_data_excel_range_disable",
      condition = input$upload_data_excel_range == ""
    )
  })
  
  observe({
    if(is.na(input$upload_data_excel_skip)){
      updateNumericInput(
        session, 
        inputId = "upload_data_excel_skip", 
        value = 0
        )
    }
  })
  
  ##### > Max rows reactive #####
  n_max <- reactive({
    ifelse(
      is.na(input$upload_data_excel_max_rows), 
      Inf, 
      input$upload_data_excel_max_rows
    )
  })
  
  ##### > Show data #####
  output$upload_data_excel_table <- DT::renderDataTable(
    expr = {
      file <- input$upload_data_excel_file
      ## If range is empty require skip rows ##
      if (input$upload_data_excel_range == "") {
        req(input$upload_data_excel_skip)
      }
      ## If there is a file display data ##
      if(!is.null(file)){
        read_excel(
          path = file$datapath,
          sheet = input$upload_data_excel_sheet, 
          range = 
            if(input$upload_data_excel_range != ""){
              input$upload_data_excel_range
            } else { NULL }, 
          col_names = input$upload_data_excel_first_row, 
          na = input$upload_data_excel_na, 
          skip = input$upload_data_excel_skip, 
          n_max = n_max()
        )
      }
    },
    filter = 'top',
    options = list(scrollX = 500, deferRender = TRUE, 
                   scroller = TRUE, fixedColumns = TRUE),
    editable = TRUE,
    rownames = FALSE
  )
  
  ##### > Upload data button actions #####
  observeEvent(input$upload_data_excel_confirm, {
    
    file <- input$upload_data_excel_file
    
    ##### >> Message of empty file #####
    if(is.null(file)) {
      sendSweetAlert(
        session = session,
        title = "Failed upload",
        text = "Select a .xls or .xlsx file",
        type = "error",
        btn_colors = "#3085d6"
      )
    }
    
    ##### >> Message of empty name #####
    if(input$upload_data_excel_text == ""){
      sendSweetAlert(
        session = session,
        title = "Failed upload",
        text = "Write a name for your data",
        type = "error",
        btn_colors = "#3085d6"
      )
    }
    
    ##### >> Upload file #####
    if(!is.null(file) && input$upload_data_excel_text != ""){
      Uploaded_Data[[input$upload_data_excel_text]] <- read_excel(
        path = file$datapath,
        sheet = input$upload_data_excel_sheet, 
        range = 
          if(input$upload_data_excel_range != ""){
            input$upload_data_excel_range
          } else { NULL }, 
        col_names = input$upload_data_excel_first_row, 
        na = input$upload_data_excel_na, 
        skip = input$upload_data_excel_skip, 
        n_max = n_max()
      ) 
    }
    
    ##### >> Show success message #####
    if(input$upload_data_excel_text %in% names(Uploaded_Data)){
      sendSweetAlert(
        session = session,
        title = "Done!",
        text = paste0('Your data "', 
                      input$upload_data_excel_text, 
                      '" have been uploaded'),
        type = "success"
      )
    }
    
  })
  
}
