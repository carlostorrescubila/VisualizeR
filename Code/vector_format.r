#' Title
#'
#' @param vector 
#'
#' @return
#' @export
#'
#' @examples

vector_format <- function(vector){
  
  if(length(vector) == 1){
    paste0('"', vector, '"') %>% noquote()
  } else {
    vector %>% 
      glue_collapse(sep = '\", \"') %>% 
      paste0('c("', ., '")') %>% 
      noquote()
  }
  
}