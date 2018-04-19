`%+%` <- function(a, b) paste0(a, b)

viewTable <- function(table) utils::View(table)


isColExist <- function(table, colname){

  if (colname %in% names(table)) {return(TRUE)}  else {return(FALSE)}
}