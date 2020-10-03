##### > Libraries ##########################################################################

source("./Code/libraries.r", encoding = "UTF-8")

##### > Side bar menu sub items ############################################################

Plot_libraries <- 
  list.dirs("./Modules") %>% 
  str_replace_all(pattern = "./Modules", replacement = "") %>% 
  str_replace_all(pattern = "/", replacement = "") %>%
  stringi::stri_remove_empty()

# sapply(
#   Plot_libraries, 
#   function(x){
#     paste0("./Modules/", x) %>% 
#       list.dirs() %>% list.files()
#   } 
#   )

##### > Data ###############################################################################

Uploaded_Data <- reactiveValues()


Colors_Choices <- c(
  "tomato", 
  "salmon", 
  "orange", 
  "yellow green", 
  "dark green", 
  "sea green",
  "dark cyan",
  "steel blue", 
  "royal blue", 
  "slate blue", 
  "dark orchid", 
  "plum", 
  "light pink"
  )

##### > Alerts #############################################################################

useSweetAlert()

##### > Call modules #######################################################################

list.files("./Modules") %>% 
  str_subset(pattern = ".r") %>% 
  paste0("./Modules/", .) %>% 
  sapply(source)

list.files("./Modules/r_base") %>% 
  # str_subset(pattern = ".r") %>% 
  paste0("./Modules/r_base/", .) %>% 
  sapply(source)
