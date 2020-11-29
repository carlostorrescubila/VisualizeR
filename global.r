##### > Libraries #################################################################

source("./Code/libraries.r", encoding = "UTF-8")

##### > Functions #################################################################

list.files("./Code") %>% 
  paste0("./Code/", .) %>% 
  # str_remove_all("libraries.r")
  sapply(source)

### > Side bar menu sub items #####################################################

# Plot_libraries <-
#   list.dirs("./Modules") %>%
#   str_replace_all(pattern = "./Modules", replacement = "") %>%
#   str_replace_all(pattern = "/", replacement = "") %>%
#   stringi::stri_remove_empty() %>%
#   str_remove_all("upload_data")

# sapply(
#   Plot_libraries,
#   function(x){
#     paste0("./Modules/", x) %>%
#       list.dirs() %>% list.files()
#   }
#   )

##### > Data ######################################################################

Uploaded_Data <- reactiveValues()

##### > Alerts ####################################################################

useSweetAlert()

##### > Call modules ##############################################################

### Scripts ###
list.files("./Modules") %>% 
  str_subset(pattern = ".r") %>% 
  paste0("./Modules/", .) %>% 
  sapply(source)

### Upload data ###
list.files("./Modules/upload_data") %>% 
  paste0("./Modules/upload_data/", .) %>% 
  sapply(source)

### R base ###
list.files("./Modules/r_base") %>% 
  paste0("./Modules/r_base/", .) %>% 
  sapply(source)
