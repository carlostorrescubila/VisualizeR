tabItem(
  tabName = "upload_data",
  fluidPage(
    box(width = 12, 
        tabsetPanel(
          type = "tabs",
          
          ##### >> csv ##################################################################################
          
          tabPanel(
            "csv",
            h3(
              "Here you can upload your data to visualize in format csv",
              class = "text-center"
            ), 
            fileInput(
              inputId = "upload_data_csv_file", 
              label = "Select file to upload",
              accept = '.csv'
            ),
            renderTable("upload_data_view"), 
            fluidRow(
              column(4, textInput(
                inputId = "upload_data_csv_text",
                label = "Name your data base", 
                value = NULL
              )
              ),
              column(4, pickerInput(
                inputId = "upload_data_csv_delimiter",
                label = "Delimiter:", 
                choices = c(
                  "comma" = ",", 
                  "semicolon" = ";", 
                  "tab" = "\t", 
                  "whitespace"
                ) 
              )), 
              column(4, pickerInput(
                inputId = "upload_data_csv_na",
                label = "Fill missing values as:", 
                choices = c(
                  "NA", 
                  "null", 
                  "0", 
                  "empty"
                )
              ))
            ), 
            awesomeCheckbox(
              inputId = "upload_data_csv_first_row",
              label = "First row as colnames", 
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
)#,