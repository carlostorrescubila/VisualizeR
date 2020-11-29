##### UI ###########################################################################

upload_csv_txt_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    
    ##### > Title #####
    h2(
      strong("Here you can upload your data to visualize in format csv or txt"),
      class = "text-center"
    ),
    
    ##### > Select file #####
    fileInput(
      inputId = ns("upload_data_csv_txt_file"),
      label = h4(strong("Select file to upload")),
      accept = c('.csv', '.txt')
    ),
    
    ##### > Select options #####
    fluidRow(
      
      ##### >> Name of data #####
      column(4, textInput(
        inputId = ns("upload_data_csv_txt_text"),
        label = h4(strong("Name your data base")),
        value = NULL
        )
      ),
      
      ##### >> Select separator #####
      column(4, pickerInput(
        inputId = ns("upload_data_csv_txt_delimiter"),
        label = h4(strong("Delimiter:")),
        choices = c(
          "comma" = ",",
          "semicolon" = ";",
          "tab" = "\t", 
          "other" = "other"
          )
      )),
      
      ##### >> Fill NA #####
      column(4, pickerInput(
        inputId = ns("upload_data_csv_txt_na"),
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
    
    fluidRow(
      
      ##### >> Checkbox for row names #####
      column(4, awesomeCheckbox(
        inputId = ns("upload_data_csv_txt_first_row"),
        label = h4(strong("First row as colnames")),
        value = TRUE,
        status = "success"
      )),
      
      ##### >> Other delimiter #####
      disabled(column(4, textInput(
        inputId = ns("upload_data_csv_txt_other_delim"),
        label = h4(strong("Write your special delimiter")),
        value = NULL
      )))
      
    ),
    
    ##### > Show data #####
    dataTableOutput(ns("upload_data_csv_txt_table")),
    
    br(),
    
    ##### > Upload data button #####
    actionBttn(
      inputId = ns("upload_data_csv_txt_confirm"),
      label = "Upload to VisualizeR",
      style = "bordered",
      color = "primary",
      size = "lg",
      block = TRUE
      )
    
  )
  
}

##### Server ######################################################################

upload_csv_txt_server <- function(input, output, session){
  
  ##### > Update delimiter #####
  observe({
    req(input$upload_data_csv_txt_file)
    updatePickerInput(
      session,
      'upload_data_csv_txt_delimiter',
      label = NULL,
      selected = if_else(
        condition = str_ends(string = input$upload_data_csv_txt_file$name, 
                             pattern = ".csv"),
        true = ",", 
        false = "\t"
      )
    )
  })
  
  ##### > Enable other delim #####
  observeEvent(input$upload_data_csv_txt_delimiter, {
    toggleState(
      id = "upload_data_csv_txt_other_delim",
      condition = input$upload_data_csv_txt_delimiter == "other"
    )
  })
  
  ##### > Show data #####
  output$upload_data_csv_txt_table <- DT::renderDataTable(
    expr = {
      file <- input$upload_data_csv_txt_file
      if(!is.null(file)){
        read.csv(
          file = file$datapath,
          header = input$upload_data_csv_txt_first_row,
          sep = if_else(condition = {input$upload_data_csv_txt_delimiter == "other"}, 
                        true = input$upload_data_csv_txt_other_delim, 
                        false = input$upload_data_csv_txt_delimiter), 
          na = input$upload_data_csv_txt_na
        )
      }
    },
    # spacing = 'xs',
    # align = 'c',
    filter = 'top',
    options = list(scrollX = 500, deferRender = TRUE, 
                   scroller = TRUE, fixedColumns = TRUE),
    editable = TRUE,
    rownames = FALSE
  )
  
  ##### > Update the data name #####
  observe({
      updateTextInput(
        session,
        'upload_data_csv_txt_text',
        label = NULL,
        value = input$upload_data_csv_txt_file$name %>% 
          str_replace(".csv", "") %>% 
          str_replace(".txt", "")
      )
  })
  
  ##### > Upload data button actions #####
  observeEvent(input$upload_data_csv_txt_confirm, {
    
    file <- input$upload_data_csv_txt_file
    
    ##### > Message of empty file #####
    if(is.null(file)) {
      sendSweetAlert(
        session = session,
        title = "Failed upload",
        text = "Select a .csv file",
        type = "error",
        btn_colors = "#3085d6"
      )
    }
    
    ##### > Message of empty name #####
    if(input$upload_data_csv_txt_text == ""){
      sendSweetAlert(
        session = session,
        title = "Failed upload",
        text = "Write a name for your data",
        type = "error",
        btn_colors = "#3085d6"
      )
    }
    
    ##### > Upload file #####
    if(!is.null(file) && input$upload_data_csv_txt_text != ""){
      Uploaded_Data[[input$upload_data_csv_txt_text]] <- read.csv(
        file = file$datapath,
        header = input$upload_data_csv_txt_first_row,
        sep = if_else(condition = {input$upload_data_csv_txt_delimiter == "other"}, 
                      true = input$upload_data_csv_txt_other_delim, 
                      false = input$upload_data_csv_txt_delimiter),
        na = input$upload_data_csv_txt_na
        ) 
    }
    
    ##### > Show success message #####
    if(input$upload_data_csv_txt_text %in% names(Uploaded_Data)){
      sendSweetAlert(
        session = session,
        title = "Done!",
        text = paste0('Your data "', 
                      input$upload_data_csv_txt_text, 
                      '" have been uploaded'),
        type = "success"
      )
    }
    
  })
  
}
