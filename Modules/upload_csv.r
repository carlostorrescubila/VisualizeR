##### UI ##################################################################################################

upload_csv_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    
    ### Title
    h2(
      strong("Here you can upload your data to visualize in format csv"),
      class = "text-center"
    ),
    
    ### Select file
    fileInput(
      inputId = ns("upload_data_csv_file"),
      label = h4(strong("Select file to upload")),
      accept = '.csv'
    ),
    
    
    renderTable(ns("upload_data_view")),
    
    #### Select options 
    fluidRow(
      
      ### Name of data
      column(4, textInput(
        inputId = ns("upload_data_csv_text"),
        label = h4(strong("Name your data base")),
        value = NULL
        )
      ),
      
      ### Select separator
      column(4, pickerInput(
        inputId = ns("upload_data_csv_delimiter"),
        label = h4(strong("Delimiter:")),
        choices = c(
          "comma" = ",",
          "semicolon" = ";",
          "tab" = "\t",
          "whitespace"
          )
      )),
      
      ### Fill NA
      column(4, pickerInput(
        inputId = ns("upload_data_csv_na"),
        label = h4(strong("Fill missing values as:")),
        choices = c(
          "NA",
          "null",
          "0",
          "empty"
        )
      ))
      
    ),
    
    br(),
    
    ### Checkbox for row names
    awesomeCheckbox(
      inputId = ns("upload_data_csv_first_row"),
      label = h4(strong("First row as colnames"), style = "vertical-align: -20px;"),
      value = TRUE,
      status = "succes"
    ),
    
    ### Show data
    dataTableOutput(ns("upload_data_csv_table")),
    
    br(),
    
    ### Upload data button
    actionBttn(
      inputId = ns("upload_data_csv_confirm"),
      label = "Upload to VisualizeR",
      style = "unite",
      size = "lg",
      block = TRUE
      )
    
  )
  
}

##### Server ##############################################################################################

upload_csv_server <- function(input, output, session){
  
  ### Load data base 
  output$upload_data_csv_table <- DT::renderDataTable(
    expr = {
      file <- input$upload_data_csv_file
      if(!is.null(file)){
        read.csv(
          file = file$datapath,
          header = input$upload_data_csv_first_row,
          sep = input$upload_data_csv_delimiter,
          na = input$upload_data_csv_na
        )
      }
    },
    # spacing = 'xs',
    # align = 'c',
    filter = 'top',
    options = list(scrollX = 500, deferRender = TRUE, scroller = TRUE, fixedColumns = TRUE),
    editable = TRUE,
    rownames = FALSE
  )
  
  ### Update the name of the data in session
  observe({
      updateTextInput(
        session,
        'upload_data_csv_text',
        label = NULL,
        value = input$upload_data_csv_file$name %>% str_replace(".csv", "")
      )
  })
  
  ### Upload data button actions
  observeEvent(input$upload_data_csv_confirm, {
    
    file <- input$upload_data_csv_file
    
    ### Message of empry file
    if(is.null(file)) {
      sendSweetAlert(
        session = session,
        title = "Failed upload",
        text = "Select a .csv file",
        type = "error",
        btn_colors = "#3085d6"
      )
    }
    
    ### Message of empty name
    if(input$upload_data_csv_text == ""){
      sendSweetAlert(
        session = session,
        title = "Failed upload",
        text = "Write a name for your data",
        type = "error",
        btn_colors = "#3085d6"
      )
    }
    
    ### Upload file
    if(!is.null(file) && input$upload_data_csv_text != ""){
      Uploaded_Data[[input$upload_data_csv_text]] <- read.csv(
        file = file$datapath,
        header = input$upload_data_csv_first_row,
        sep = input$upload_data_csv_delimiter,
        na = input$upload_data_csv_na
      )
    }
    
    ### Show success message
    if(input$upload_data_csv_text %in% names(Uploaded_Data)){
      sendSweetAlert(
        session = session,
        title = "Done!",
        text = paste0('Your data "', input$upload_data_csv_text, '" have been uploaded'),
        type = "success"
      )
    }
    
  })
  
}
