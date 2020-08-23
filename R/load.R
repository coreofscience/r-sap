#' @title Loading isi files
#' 
#' @description Load isi file (txt) and convert it to a data frame with a bibliometrix
#' function called convert2df().
#' 
#' @param file the isi file (txt) downloaded from Web of Science
#' 
#' @import bibliometrix
#' 
#' @importFrom bibliometrix convert2df
#' 
#' @return a dataframe with all variables in columns
#' 
#' @examples
#' 
isi_df <- function(file) {
  isi_file = bibliometrix::convert2df(file = file, 
                                      dbsource = "wos", 
                                      format = "plaintext")
} 
