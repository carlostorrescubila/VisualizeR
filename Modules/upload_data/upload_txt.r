##### UI ###########################################################################

upload_txt_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    
    br(),
    
    ##### > Title #####
    h2(
      strong("Here you can upload your data to visualize in txt format"),
      class = "text-center"
    ),
    br(),
    
    ##### > Select file #####
    fileInput(
      inputId = ns("upload_data_txt_file"),
      label = h4(strong("Select file to upload")),
      accept = '.txt'
    ),
    
    ##### > Select options #####
    fluidRow(
      
      ##### >> Name of data #####
      column(4, textInput(
        inputId = ns("upload_data_txt_text"),
        label = h4(strong("Name your database:")),
        value = NULL
        )
      ),
      
      ##### >> Select separator #####
      column(4, pickerInput(
        inputId = ns("upload_data_txt_delimiter"),
        label = h4(strong("Delimiter:")),
        choices = c(
          "Tab" = "\t", 
          "Comma" = ",",
          "Semicolon" = ";",
          "Other" = "other"
          )
      )),
      
      ##### >> Fill NA #####
      column(4, pickerInput(
        inputId = ns("upload_data_txt_na"),
        label = h4(strong("Consider as missing values:")),
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
        inputId = ns("upload_data_txt_first_row"),
        label = h4(strong("First row as colnames")),
        value = TRUE,
        status = "success"
      )),
      
      ##### >> Other delimiter #####
      disabled(column(4, textInput(
        inputId = ns("upload_data_txt_other_delim"),
        label = h4(strong("Write your special delimiter")),
        value = NULL
      ))), 
      
      ##### >> Decimal points #####
      column(4, pickerInput(
        inputId = ns("upload_data_txt_dec"),
        label = h4(strong("Consider as decimal points:")),
        choices = NULL
      ))
      
    ),
    
    ##### > Show data #####
    dataTableOutput(ns("upload_data_txt_table")),
    
    br(),
    
    ##### > Upload data button #####
    actionBttn(
      inputId = ns("upload_data_txt_confirm"),
      label = "Upload data",
      style = "bordered",
      color = "primary",
      size = "lg",
      block = TRUE
      )
    
  )
  
}

##### Server ######################################################################

upload_txt_server <- function(input, output, session){
  
  ##### > Enable other delim #####
  observeEvent(input$upload_data_txt_delimiter, {
    toggleState(
      id = "upload_data_txt_other_delim",
      condition = input$upload_data_txt_delimiter == "other"
    )
  })
  
  ##### > Update decimal points #####
  observe({
    if(input$upload_data_txt_delimiter == ","){
      updatePickerInput(
        session,
        inputId = 'upload_data_txt_dec',
        choices = c("Dot" = ".")
      )
    } else {
      updatePickerInput(
        session,
        inputId = 'upload_data_txt_dec',
        choices = c("Dot" = ".", "Comma" = ",")
      )
    }
  })
  
  ##### > Show data #####
  output$upload_data_txt_table <- DT::renderDataTable(
    expr = {
      file <- input$upload_data_txt_file
      if(!is.null(file)){
        read.delim(
          file = file$datapath,
          header = input$upload_data_txt_first_row,
          sep = if_else(condition = {input$upload_data_txt_delimiter == "other"}, 
                        true = input$upload_data_txt_other_delim, 
                        false = input$upload_data_txt_delimiter), 
          dec = input$upload_data_txt_dec,
          na = input$upload_data_txt_na
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
        'upload_data_txt_text',
        label = NULL,
        value = input$upload_data_txt_file$name %>% 
          str_replace(".txt", "") 
      )
  })
  
  ##### > Upload data button actions #####
  observeEvent(input$upload_data_txt_confirm, {
    
    file <- input$upload_data_txt_file
    
    ##### >> Message of empty file #####
    if(is.null(file)) {
      sendSweetAlert(
        session = session,
        title = "Failed upload",
        text = "Select a txt file",
        type = "error",
        btn_colors = "#3085d6"
      )
    }
    
    ##### >> Message of empty name #####
    if(input$upload_data_txt_text == ""){
      sendSweetAlert(
        session = session,
        title = "Failed upload",
        text = "Write a name for your data",
        type = "error",
        btn_colors = "#3085d6"
      )
    }
    
    ##### >> Upload file #####
    if(!is.null(file) && input$upload_data_txt_text != ""){
      Uploaded_Data[[input$upload_data_txt_text]] <- read.delim(
        file = file$datapath,
        header = input$upload_data_txt_first_row,
        sep = if_else(condition = {input$upload_data_txt_delimiter == "other"}, 
                      true = input$upload_data_txt_other_delim, 
                      false = input$upload_data_txt_delimiter),
        dec = input$upload_data_txt_dec, 
        na = input$upload_data_txt_na
        ) 
    }
    
    ##### >> Show success message #####
    if(input$upload_data_txt_text %in% names(Uploaded_Data)){
      sendSweetAlert(
        session = session,
        title = "Done!",
        text = paste0('Your data "', 
                      input$upload_data_txt_text, 
                      '" have been uploaded'),
        type = "success"
      )
    }
    
  })
  
}
