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
    

})


# library(httr)
# library(jsonlite)
# library(magrittr) 
# AA <- httr::GET(
#     # "https://analisis.datosabiertos.jcyl.es/explore/embed/dataset/situacion-epidemiologica-coronavirus-en-castilla-y-leon/table/?disjunctive.provincia&sort=fecha"
#     "https://analisis.datosabiertos.jcyl.es/api/records/1.0/search/?dataset=situacion-epidemiologica-coronavirus-en-castilla-y-leon&q=&sort=fecha&facet=fecha&facet=provincia"
#     ) %>% 
#     content("text") %>% 
#     jsonlite::fromJSON()
# AA$records$fields
