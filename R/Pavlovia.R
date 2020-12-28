

#' @title Convert list in Pavlovia CSV to R vector
#' @param v Text version of Pavlovia list
#' @return A numeric R vector
#' @description 
#' does not yet allow non-numeric data types
#' @details 
#' ?
#' @examples
#' ?
#' @export
convertCellToNumVector <- function(v) {
  
  # remove opening square bracket:
  v <- gsub('\\[', replacement='', x=v)
  # remove closing square bracket:
  v <- gsub(']', replacement='', x=v)
  # split by commas:
  v <- strsplit(v, ',')
  # convert to numeric:
  v <- lapply(v, FUN=as.numeric)
  # make vector:
  v <- as.vector(unlist(v))
  
  return(v)
  
}