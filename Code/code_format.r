#' Chunk code format
#'
#' @param X 
#'
#' @return
#' @export
#'
#' @examples
#' my_string <- code_string("string")

code_string <- function(X){
  paste0(
    "<font color=\"#0a7113\">", 
    '"', X, '"', 
    "</font>"
  )
}

code_logical <- function(X){
  paste0(
    "<font color=\"#594bf6\">", 
    X,
    "</font>"
  )
}