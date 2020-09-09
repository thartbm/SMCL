


#' @title Generate random participant IDs
#' @param checkFile CSV file with previous IDs that should not be duplicated
#' @param IDlength Number character pairs to use for IDs (3 is 6 characters)
#' @param number How many IDs to add to the existing list.
#' @param addToFile What CSV file to write the IDs to.
#' @return NULL
#' @description This function generates a set of random IDs for use in 
#' experiments where participant anonymity is important. The IDs are not
#' generated with R's random number generator, but uses openssl.
#' @details The function does not yet check if it is possible to generate
#' as many unique IDs as requested with the given length, so be careful not to
#' start an endless loop.
#' @examples
#' ?
#' @export
generateRandomIDs <- function(checkFile='randomIDs.csv', IDlength=3, number=100, addToFile=checkFile) {
  # This function generates a NUMBER of random IDs of length IDlength * 2, and 
  # checks if they already exist in file checkFile.
  
  # we use non-reproducible and cryptographically secure sequences
  # from the openssl package
  library(openssl)
  
  # we need to know which ones we already generated/used
  if (file.exists(checkFile)) {
    existingList <- read.csv(checkFile, stringsAsFactors=F)
  } else {
    existingList <- data.frame('randomIDs'=c())
  }
  
  # we count how many new ones we created
  newIDs <- c()
  
  # we generate new IDs until we have the required number of new, unique ones:
  while(length(newIDs) < number) {
    
    # get a random byte string from openssl:
    my_randcypher <- openssl::rand_bytes(n=IDlength)
    
    # convert to a string:
    my_randid <- ''
    for (idx in c(1:length(my_randcypher))) {
      my_randid <- sprintf('%s%s',my_randid,as.character(my_randcypher[idx]))
    }
    
    # check if it exists, and keep if it is new:
    if (my_randid %in% existingList$randomIDs) {
      # nothing to do here...
    } else {
      # put in list
      newIDs <- c(newIDs,my_randid)
    }
    
  }
  
  # convert list to data frame
  newList <- data.frame('randomIDs'=newIDs)
  
  # check if target file exists, read contents if true
  if (file.exists(addToFile)) {
    existingList <- read.csv(addToFile, stringsAsFactors=F)
  } else {
    existingList <- data.frame('randomIDs'=c())
  }
  
  # combine contents:
  idList <- rbind(existingList,newList)
  
  # write to file:
  write.csv(idList, file=addToFile, row.names=FALSE)
  
}