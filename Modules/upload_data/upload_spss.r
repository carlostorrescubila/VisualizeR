upload_spss_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    
    br(),
    
    ##### > Title #####
    h2(
      strong("Here you can upload your data to visualize in SPSS format"),
      class = "text-center"
    ),
    br(),
    
    ##### > Select file #####
    fileInput(
      inputId = ns("upload_data_spss_file"),
      label = h4(strong("Select file to upload")),
      accept = c('.sav')
    ), 
    
    ##### > Select options #####
    fluidRow(
      
      ##### >> Name of data #####
      column(4, textInput(
        inputId = ns("upload_data_spss_text"),
        label = h4(strong("Name your database:")),
        value = NULL
      )),
    
    ##### >> Skip rows #####
    column(4, numericInput(
      inputId = ns("upload_data_spss_skip"),
      label = h4(strong("Number of rows to skip:")),
      value = 0, 
      min = 0
    )), 
    
    ##### >> Max rows #####
    column(4, numericInput(
      inputId = ns("upload_data_spss_max_rows"),
      label = h4(strong("Maximum number of data rows to read:")),
      value = Inf, 
      min = 0
    ))
    
  ), 
    
    ##### > Show data #####
    dataTableOutput(ns("upload_data_spss_table")),
    
    br(), 
    
    ##### > Upload data button #####
    actionBttn(
      inputId = ns("upload_data_spss_confirm"),
      label = "Upload data",
      style = "bordered",
      color = "primary",
      size = "lg",
      block = TRUE
    )
    
  )
    
}

upload_spss_server <- function(input, output, session){
  
  ##### > Update the data name #####
  observe({
    req(input$upload_data_spss_file)
    updateTextInput(
      session,
      inputId =  'upload_data_spss_text',
      label = NULL,
      value = input$upload_data_spss_file$name %>% 
        str_replace(".sav", "")
    )
  })
  
  ##### > Update the data name #####
  observe({
    req(input$upload_data_spss_file)
    updateTextInput(
      session,
      inputId =  'upload_data_spss_text',
      label = NULL,
      value = input$upload_data_spss_file$name %>% 
        str_replace(".sav", "")
    )
  })
  
  ##### > Show data #####
  output$upload_data_spss_table <- DT::renderDataTable(
    expr = {
      file <- input$upload_data_spss_file
      if(!is.null(file)){
        read_sav(
          file <- file$datapath,
          skip = input$upload_data_spss_skip, 
          n_max = input$upload_data_spss_max_rows
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
  observeEvent(input$upload_data_spss_confirm, {
    
    file <- input$upload_data_spss_file
    
    ##### >> Message of empty file #####
    if(is.null(file)) {
      sendSweetAlert(
        session = session,
        title = "Failed upload",
        text = "Select a .sav file",
        type = "error",
        btn_colors = "#3085d6"
      )
    }
    
    ##### >> Message of empty name #####
    if(input$upload_data_spss_text == ""){
      sendSweetAlert(
        session = session,
        title = "Failed upload",
        text = "Write a name for your data",
        type = "error",
        btn_colors = "#3085d6"
      )
    }
    
    ##### >> Upload file #####
    if(!is.null(file) && input$upload_data_spss_text != ""){
      Uploaded_Data[[input$upload_data_spss_text]] <- read_sav(
        file <- file$datapath,
        skip = input$upload_data_spss_skip, 
        n_max = input$upload_data_spss_max_rows
      ) 
    }
    
    ##### >> Show success message #####
    if(input$upload_data_spss_text %in% names(Uploaded_Data)){
      sendSweetAlert(
        session = session,
        title = "Done!",
        text = paste0('Your data "', 
                      input$upload_data_spss_text, 
                      '" have been uploaded'),
        type = "success"
      )
    }
    
  })

}