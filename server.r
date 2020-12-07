shinyServer(function(input, output, session) {
    
##### > Message menu ###############################################################
    
    output$messageMenu <- renderMenu({
        dropdownMenu(
            icon = icon(name = "info-circle", class = "fa-lg"), 
            headerText = strong("App Information"),
            messageItem(
                from = "GitHub Repository",
                message = "Documentation, Source",
                icon = icon(name = "github", class = "fa"),
                href = "https://github.com/carlostorrescubila/COVID-19_CyL"
            ),
            messageItem(
                from = "Issues",
                message = "Report Issues",
                icon = icon("exclamation-circle"),
                href = "https://github.com/carlostorrescubila/COVID-19_CyL/issues"
            )
        )
    })
    
##### > Home #######################################################################
    
    callModule(module = serverChangeTheme, id = "moduleChangeTheme")
    
##### > Upload data ################################################################
    
    ##### >> txt ###################################################################
    
    callModule(upload_txt_server, "upload_txt")
    
    ##### >> csv ###################################################################
    
    callModule(upload_csv_server, "upload_csv")
    
    ##### >> excel #################################################################
    
    callModule(upload_excel_server, "upload_excel")
    
    ##### >> SPSS #################################################################
    
    callModule(upload_spss_server, "upload_spss")
    
##### > Edit data ##################################################################
    
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
    #     options = list(scrollX = 500, deferRender = TRUE, scroller = TRUE, 
    #                    fixedColumns = TRUE),
    #     editable = TRUE,
    #     rownames = FALSE) 

##### > R base #####################################################################

    ##### >> Bar chart #############################################################
    callModule(r_base_bar_chart_server, "r_base_bar_chart_body")
    
    ##### >> Bar chart #############################################################
    callModule(r_base_box_plot_server, "r_base_box_plot_body")
    
    ##### >> Histogram #############################################################
    callModule(r_base_histogram_server, "r_base_histogram_body")
    
    ##### >> Scatter plot ##########################################################
    callModule(r_base_scatter_plot_server, "r_base_scatter_plot_body")

    })
    