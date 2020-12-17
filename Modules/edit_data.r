##### UI ##############################################################################

edit_data_ui <- function(id){
  
  ns <- NS(id)
  
  fluidPage(
    
    ##### > Title ########
    h2(
      strong("Here you can edit the data you have uploaded"),
      class = "text-center"
    ),
    br(),
    
    ##### > Select data #####
    div(class = "Data_Selecter", pickerInput(
      inputId = ns("edit_data_select"),
      label = "Select data:",
      choices = NULL
    )),
    
    box(width = 12,
        tabsetPanel(
          type = "tabs",
          
          ##### >>> Variables ########################################
          
          tabPanel(
            title = "Variables",
            br(), 
            uiOutput(ns("edit_data_variables")), 
            actionBttn(
              inputId = ns("edit_data_variables_confirm"),
              label = "Upload changes",
              style = "gradient",
              color = "default",
              size = "lg",
              block = TRUE
            )
          ),
          
          ##### >>> Table ########################################
          
          tabPanel(
            title = "Table", 
            br(), 
            dataTableOutput(ns("edit_data_table"))
          )
        )
    )#, 
    
    # ##### > Footer #####
    # footer <- tags$div(p(class = "footer", "By: Carlos A. Torres Cubilla"))
    
  )
  
}

##### Server ##########################################################################

edit_data_server <- function(input, output, session){7
  
  ns <- session$ns
  
  ##### > Update data choices #####
  observe({
    updatePickerInput(
      session, 
      inputId = "edit_data_select", 
      label = NULL,
      choices = names(Uploaded_Data)
    )
  })
  
  ##### > Edit type of data #####
  output$edit_data_variables <- renderUI({
    if(is.null(input$edit_data_select)){
      bucket_list(
        header = c("Colnames of data selected"),
        add_rank_list(text = "Numeric", labels = NULL),
        add_rank_list(text = "Factor", labels = NULL),
        add_rank_list(text = "Character", labels =  NULL)
      )
    }else{
      bucket_list(
        header = c("Colnames of data"),
        add_rank_list(
          input_id = ns("edit_data_variables_numeric"),
          text = "Numeric",
          labels = Uploaded_Data[[input$edit_data_select]] %>% 
            select_if(is.numeric) %>% colnames
        ),
        add_rank_list(
          input_id = ns("edit_data_variables_factor"),
          text = "Factor",
          labels = Uploaded_Data[[input$edit_data_select]] %>% 
            select_if(is.factor) %>% colnames
        ),
        add_rank_list(
          input_id = ns("edit_data_variables_character"),
          text = "Character",
          labels = Uploaded_Data[[input$edit_data_select]] %>% 
            select_if(is.character) %>% colnames
        )
      ) 
    }
  })
  
  ##### > Upload data types #####
  observeEvent(input$edit_data_variables_confirm, {
    
    req(input$edit_data_select)
    
    ### Variables to check
    
    check_this <- intersect(
      Uploaded_Data[[input$edit_data_select]] %>% select_if(is.character) %>% colnames, 
      input$edit_data_variables_numeric
      )
    
    ### Number of NA's before edit
    
    numNAs <- Uploaded_Data[[input$edit_data_select]] %>% 
      select(check_this) %>% 
      is.na %>% 
      sum()
    
    ### Number of NA's after edit
    
    numNAs_new <- Uploaded_Data[[input$edit_data_select]] %>% 
      mutate_at(vars(input$edit_data_variables_numeric), as.numeric) %>% 
      suppressWarnings %>% 
      select(check_this) %>% 
      is.na %>% 
      sum
    
    ##### >> Avoid NA's #####
    
    if(numNAs == numNAs_new){
      
      ##### >> Upload numerics ######
      Uploaded_Data[[input$edit_data_select]] <- 
        Uploaded_Data[[input$edit_data_select]] %>% 
        mutate_at(vars(input$edit_data_variables_numeric), as.numeric)
      
      ##### >> Upload factors ######
      Uploaded_Data[[input$edit_data_select]] <- 
        Uploaded_Data[[input$edit_data_select]] %>% 
        mutate_at(vars(input$edit_data_variables_factor), as.factor)
      
      ##### >> Upload characters ######
      Uploaded_Data[[input$edit_data_select]] <- 
        Uploaded_Data[[input$edit_data_select]] %>% 
        mutate_at(vars(input$edit_data_variables_character), as.character)
      
      ##### >> Confirmation message #####
      sendSweetAlert(
        session = session,
        title = "Done!",
        text = paste0('Your data "', input$edit_data_select,  '" have been edited'),
        type = "success"
      )
      
    }else{
      
      ##### >> Error message #####
      sendSweetAlert(
        session = session,
        title = "Failed edition",
        text = "You are producing NA in your data. Probably you are wrongly trying to convert a character into numeric.",
        type = "error",
        btn_colors = "#3085d6"
      )
      
    }
    
  })
  
  ##### > Show selectet data #####
  observe({
    req(input$edit_data_select)
    output$edit_data_table <- DT::renderDataTable(
      expr = {Uploaded_Data[[input$edit_data_select]]},
      filter = 'top',
      options = list(
        scrollX = 500, deferRender = TRUE, scroller = TRUE, fixedColumns = TRUE
        ),
      editable = TRUE,
      rownames = FALSE
    )
  })
  
  
}
