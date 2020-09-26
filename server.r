shinyServer(function(input, output, session) {
    
    ##### > Sidebar #############################################################################
    
    ##### >> csv ################################################################################
    
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
    
    observe({
        if(input$upload_data_csv_text == ""){
            updateTextInput(
                session,
                'upload_data_csv_text', 
                label = NULL,
                value = input$upload_data_csv_file$name %>% str_replace(".csv", "")     
                )
            }
        })
    
    observeEvent(input$upload_data_csv_confirm, {
        
        file <- input$upload_data_csv_file
        
        if(is.null(file)) {
            sendSweetAlert(
                session = session,
                title = "Failed upload",
                text = "Select a .csv file",
                type = "error", 
                btn_colors = "#3085d6"
                )
            
        }
        
        if(input$upload_data_csv_text == ""){
            
            sendSweetAlert(
                session = session,
                title = "Failed upload",
                text = "Write a name for your data",
                type = "error", 
                btn_colors = "#3085d6"
            )
            
        }
        
        if(!is.null(file) && input$upload_data_csv_text != ""){
            
            Uploaded_Data[[input$upload_data_csv_text]] <- read.csv(
                file = file$datapath,
                header = input$upload_data_csv_first_row,
                sep = input$upload_data_csv_delimiter,
                na = input$upload_data_csv_na
            )
            
            sendSweetAlert(
                session = session,
                title = "Done!",
                text = paste0('Your data "', input$upload_data_csv_text, '" have been uploaded'),
                type = "success"
            )
            
        }
        
        
        
    })
    
##### Edit data ################################################################################
    
    observe({
        updatePickerInput(
            session, 
            inputId = "edit_data_select", 
            label = NULL,
            choices = names(Uploaded_Data)
            )
    })

})
