##### Global ##############################################################################

source("./global.r", encoding = "UTF-8")

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

        ##### >> Upload data ######################################################################

        tabItem(
            tabName = "upload_data",
            fluidPage(
                box(width = 12,
                    tabsetPanel(
                        type = "tabs",

                        ##### >>> csv ##################################################################

                        tabPanel(
                            "csv",
                            upload_csv_ui("upload_csv")
                            ),

                        ##### >>> txt ##################################################################

                        tabPanel(
                            "txt",
                            upload_txt_ui("upload_txt")
                        ),

                        ##### >>> excel ################################################################

                        tabPanel(
                            "excel",
                            upload_excel_ui("upload_excel")
                        )
                    )
                )
            )
        ),

        ##### >> Edit data ###########################################################################

        tabItem(
            tabName = "edit_data",
            edit_data_ui("edit_data_body")
        ), 

        ##### >> R Base ##############################################################################

        ##### >> Bar chart
        
        tabItem(
            tabName = "r_base_bar_chart", 
            r_base_bar_chart_ui("r_base_bar_chart_body")
            )
        
    )

)


#### UI ####################################################################################

ui <- div(
    dashboardPage(
        header,
        sidebar,
        body#,
        # useShinyalert()
    )
)