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

sidebar <- dashboardSidebar(
    disable = F,
    # width = 275,
    collapsed = F, 
    sidebarMenu(
        menuItem("Upload Data", tabName = "upload_data", icon = icon("upload")), 
        menuItem("Edit Data", tabName = "edit_data", icon = icon("edit")),
        menuItem("R base", tabName = "r_base", icon = icon("r-project"))
    )
)

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
                        
##### >> csv ##################################################################################

                        tabPanel(
                            "csv",
                            h2(
                                strong("Here you can upload your data to visualize in format csv"),
                                class = "text-center"
                                ), 
                            fileInput(
                                inputId = "upload_data_csv_file", 
                                label = h4(strong("Select file to upload")),
                                accept = '.csv'
                                ),
                            renderTable("upload_data_view"), 
                            fluidRow(
                                column(4, textInput(
                                    inputId = "upload_data_csv_text",
                                    label = h4(strong("Name your data base")), 
                                    value = NULL
                                    )
                                    ),
                                column(4, pickerInput(
                                    inputId = "upload_data_csv_delimiter",
                                    label = h4(strong("Delimiter:")), 
                                    choices = c(
                                        "comma" = ",", 
                                        "semicolon" = ";", 
                                        "tab" = "\t", 
                                        "whitespace"
                                        ) 
                                    )), 
                                column(4, pickerInput(
                                    inputId = "upload_data_csv_na",
                                    label = h4(strong("Fill missing values as:")), 
                                    choices = c(
                                        "NA", 
                                        "null", 
                                        "0", 
                                        "empty"
                                        )
                                    ))
                                ), 
                            br(),  
                            awesomeCheckbox(
                                inputId = "upload_data_csv_first_row",
                                label = h4(strong("First row as colnames"), style = "vertical-align: -20px;"),
                                value = TRUE, 
                                status = "succes"
                                ), 
                            dataTableOutput("upload_data_csv_table"), 
                            br(),  
                            actionBttn(
                                inputId = "upload_data_csv_confirm", 
                                label = "Upload to VisualizeR", 
                                style = "unite",
                                size = "lg",
                                block = TRUE
                                )
                            ), 

##### >> txt ##################################################################################

                        tabPanel(
                            "txt",
                            h3(
                                "Here you can upload your data to visualize in format txt", 
                                class = "text-center"
                                )
                            ), 

##### >> excel ################################################################################

                        tabPanel(
                            "excel",
                            h3(
                                "Here you can upload your data to visualize in an excel format.", 
                                class = "text-center"
                                ), 
                            )
                        )
                    )
                )
            ),

##### >> Edit data ##########################################################################

        tabItem(
            tabName = "edit_data", 
            fluidPage(
                pickerInput(
                    inputId = "edit_data_select",
                    label = "Select data:", 
                    choices = NULL
                    )
                )
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