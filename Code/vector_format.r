#' @title Vector format
#' 
#' @description Convert the output of a vector into a character that contains a vector to use as input in R. 
#'
#' @param vector character; \cr
#' The output that user want to give the vector format.
#'
#' @return It returns a character that contains a vector as definied in R language. 
#' 
#' @examples
#' iris %>% colnames %>% vector_format
#' 
#' @import glue
#' 
#' @export

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