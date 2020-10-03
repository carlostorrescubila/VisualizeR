shinyServer(function(input, output, session) {
    
##### > Upload data ############################################################################
    
    ##### >> csv ###############################################################################
    
    callModule(upload_csv_server, "upload_csv")
    
    ##### >> txt ###############################################################################
    
    callModule(upload_csv_server, "upload_csv")
    
    ##### >> excel #############################################################################
    
    callModule(upload_csv_server, "upload_csv")
    
##### > Edit data ##############################################################################
    
    callModule(edit_data_server, "edit_data_body")
    # ### Update choices in select input data
    # observe({
    #     updatePickerInput(
    #         session, 
    #         inputId = "edit_data_select", 
    #         label = NULL,
    #         choices = names(Uploaded_Data)
    #         )
    # })
    # 
    # ### Show selectet data
    # output$edit_data_table <- DT::renderDataTable(
    #     expr = {
    #         Uploaded_Data[[input$edit_data_select]]
    #         }, 
    #     filter = 'top',
    #     options = list(scrollX = 500, deferRender = TRUE, scroller = TRUE, fixedColumns = TRUE),
    #     editable = TRUE,
    #     rownames = FALSE) 

##### > R base #################################################################################

    ##### >> Bar chart #########################################################################
    
    callModule(r_base_bar_chart_server, "r_base_bar_chart_body")
    
})
    