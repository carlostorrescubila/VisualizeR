##### > Header ###################################################################

header <- dashboardHeaderPlus(
    title = tagList(
        tags$span(class = "logo-mini", ""),
        tags$span(class = "logo-lg", div(class = "VisualizeR", "VisualizeR", style = "font-size: 25px;"))
        ),
    # enable_rightsidebar = TRUE,
    # rightSidebarIcon = "paint-brush",
    dropdownMenuOutput(outputId = "Customize_Menu"),
    dropdownMenuOutput(outputId = "messageMenu")
)

##### > Sidebar ##################################################################

source("./Modules/sidebar.r", encoding = "UTF-8")

# rightsidebar <- rightSidebar(
#     background = "dark",
#     rightSidebarTabContent(
#         id = 1,
#         title = "Tab 1",
#         icon = "desktop",
#         active = TRUE,
#         uiChangeThemeDropdown()
#     )
# )

##### > Body #####################################################################

body <- dashboardBody(
    
    includeCSS("styles.css"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    
    ##### >> Set Theme ###########################################################
    shinyDashboardThemes(
        theme = "grey_light"
    ),
    uiChangeThemeOutput(),
    
    tabItems(
        
        ##### >> Home ############################################################
        tabItem(
            tabName = "home",
            fluidRow(column(
                width = 12,
                home_ui("home_body")
            ))
        ),

        ##### >> Upload data #####################################################

        tabItem(
            tabName = "upload_data",
            fluidPage(
                box(width = 12,
                    tabsetPanel(
                        type = "tabs",

                        ##### >>> txt ########################################

                        tabPanel(
                            title = div(
                                img(src = "https://www.flaticon.com/svg/static/icons/svg/337/337956.svg", width = "32px"),
                                "txt"
                                ),
                            upload_txt_ui("upload_txt")
                            ),
                        
                        ##### >>> csv ########################################
                        
                        tabPanel(
                            title = div(
                                img(src = "https://www.flaticon.com/svg/static/icons/svg/180/180855.svg", width = "32px"),
                                "csv"
                            ),
                            upload_csv_ui("upload_csv")
                        ),
                        
                        ##### >>> excel ##########################################
                        
                        tabPanel(
                            title = div(
                                img(src = "https://www.flaticon.com/svg/static/icons/svg/732/732220.svg", width = "32px"), 
                                "Excel"
                                ),
                            upload_excel_ui("upload_excel")
                        ), 
                        
                        ##### >>> SPSS ##########################################
                        
                        tabPanel( 
                            title = div(
                                # img(src = "https://simpleicons.org/icons/ibm.svg")
                                img(src = "https://www.flaticon.com/svg/static/icons/svg/882/882625.svg", width = "32px"), 
                                "SPSS"
                                ), 
                        upload_spss_ui("upload_spss")
                        )
                    )
                )
            )
        ),

        ##### >> Edit data #######################################################

        tabItem(
            tabName = "edit_data",
            edit_data_ui("edit_data_body")
        ),


        ##### >> R Base ##########################################################

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
    
        ##### >>> Bar chart ######################################################
        tabItem(
            tabName = "r_base_bar_chart",
            r_base_bar_chart_ui("r_base_bar_chart_body")
            ), 
        
        ##### >>> Box-Plot #######################################################
        tabItem(
            tabName = "r_base_box-plot",
            r_base_box_plot_ui("r_base_box_plot_body")
        ),
        
        ##### >>> Histogram ######################################################
        tabItem(
            tabName = "r_base_histogram",
            r_base_histogram_ui("r_base_histogram_body")
        ),
        
        ##### >>> Scatter plot ###################################################
        tabItem(
            tabName = "r_base_scatter_plot",
            r_base_scatter_plot_ui("r_base_scatter_plot_body")
        )
    
    )

)


#### UI ##########################################################################

ui <- div(
    dashboardPagePlus(
        header,
        sidebar,
        body#, 
        # rightsidebar
    )
)