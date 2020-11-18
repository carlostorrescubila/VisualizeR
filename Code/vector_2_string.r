#' Title
#'
#' @param vector 
#'
#' @return
#' @export
#'
#' @examples

vector_2_string <- function(vector){
  
  if(length(vector) == 1){
    vector
  } else {
    vector %>% 
      glue_collapse(sep = '", "') %>% 
      paste0('c("', ., '")') #%>% cat
  }
  
}