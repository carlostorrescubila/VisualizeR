##### Global ##############################################################################

# source("./global.r", encoding = "UTF-8")

##### > Header ############################################################################

header <- dashboardHeader(
    title = "VisualizeR", 
    #     tagList(
    #     tags$span(class = "logo-mini", ""),
    #     tags$span(class = "logo-lg", "Covid-19 en Castilla y León")
    # ), 
    # titleWidth = 275,
    dropdownMenuOutput(outputId = "messageMenu")
)

##### > Sidebar ############################################################################

source("./Modules/sidebar.r", encoding = "UTF-8")

##### > Body ###############################################################################

body <- dashboardBody(

    tabItems(

        ##### >> Upload data ###############################################################

        tabItem(
            tabName = "upload_data",
            fluidPage(
                box(width = 12,
                    tabsetPanel(
                        type = "tabs",

                        ##### >>> csv ######################################################

                        tabPanel(
                            "csv",
                            upload_csv_ui("upload_csv")
                            ),

                        ##### >>> txt ######################################################

                        tabPanel(
                            "txt",
                            upload_txt_ui("upload_txt")
                        ),

                        ##### >>> excel ####################################################

                        tabPanel(
                            "excel",
                            upload_excel_ui("upload_excel")
                        )
                    )
                )
            )
        ),

        ##### >> Edit data #################################################################

        tabItem(
            tabName = "edit_data",
            edit_data_ui("edit_data_body")
        ),


        ##### >> R Base ####################################################################

        # tagList(
        # list.files("./Modules/r_base") %>%
        #     str_remove(pattern = "\\.[:lower:]")  %>%
        #     purrr::map(
        #         function(x) {
        #             tabItem(
        #                 tabName = x,
        #                 x %>%
        #                     paste0(., '_ui("', ., '_body")') %>%
        #                     parse(text = .) %>%
        #                     eval()
        #                 )
        #             }
        #         ) %>% 
        # paste(collapse="\n") %>% 
        # cat
        # )
        # )
    
        ##### >> Bar chart ##################################################################
        tabItem(
            tabName = "r_base_bar_chart",
            r_base_bar_chart_ui("r_base_bar_chart_body")
            ), 
    
        ##### >> Scatter plot ###############################################################
        tabItem(
            tabName = "r_base_scatter_plot",
            r_base_scatter_plot_ui("r_base_scatter_plot_body")
        )
    
    )

)


#### UI ####################################################################################

ui <- div(
    dashboardPage(
        header,
        sidebar,
        body
    )
)