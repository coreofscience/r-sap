#' Loading isi files
#'
#' load isi file and convert it to df
#'
#' Load isi file (txt) and convert it to a data frame with a bibliometrix
#' function called convert2df().
#'
#' @param file the isi file (txt) downloaded from Web of Science
#'
#' @author Sebastian Robledo
#'
#' @import bibliometrix
#'
#' @return a dataframe with all variables in columns
#'
#' @examples
#' \dontrun{
#' df <- sap::sap_load("scientometrics_1_1059.txt")
#' }
sap_load <- function(file) {
  isi_df = bibliometrix::convert2df(file = file,
                                    dbsource = "wos",
                                    format = "plaintext")
}
