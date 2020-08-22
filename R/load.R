#' @title read isi files
#' @description this function uses convert2df from bibliometrics package to 
#' convert the isi file (txt) in a data frame
#' @param file vector de datos cuyo histograma se va a calcular
#' @return a isi file in a dataframe format
#' @export isi_df
#' @examples
isi_df <- function(file) {
  isi_file = bibliometrix::convert2df(file = file, dbsource = "wos", format = "plaintext")
} 
