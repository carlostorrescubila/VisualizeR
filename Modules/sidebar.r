sidebar <- dashboardSidebar(
  disable = F,
  # width = 275,
  collapsed = F,
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Upload Data", tabName = "upload_data", icon = icon("file-upload")), 
    menuItem("Edit Data", tabName = "edit_data", icon = icon("edit")),
    menuItem("R base", tabName = "r_base", icon = icon("r-project"), 
             list.files("./Modules/r_base") %>% 
               str_remove_all(pattern = "\\.r") %>% 
               purrr::map(
                 function(x) {
                   menuSubItem(
                     text = x %>% 
                       str_remove(pattern = "r_base_") %>% 
                       str_replace(pattern = "_", replacement = " ") %>% 
                       str_to_title(), 
                     tabName = x
                   )
                 }
               )
             ), 
    menuItem("Lattice", tabName = "lattice", icon = icon("r-project", class = "lattice_icon"), 
             list.files("./Modules/lattice") %>% 
               str_remove_all(pattern = "\\.r") %>% 
               purrr::map(
                 function(x) {
                   menuSubItem(
                     text = x %>% 
                       str_remove(pattern = "lattice_") %>% 
                       str_replace(pattern = "_", replacement = " ") %>% 
                       str_to_title(), 
                     tabName = x
                   )
                 }
               )
             )
    )
  )