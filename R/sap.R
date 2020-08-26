#' @title sap algorithm
#' 
#' @description sap algorithm represents the metaphor of the 
#' tree sap
#'  
#' @param file the isi file (txt) downloaded from web of science
#'  
#' @author Sebastian Robledo
#'  
#' @import bibliometrix
#' @import sap_load
#' @import sap_graph
#'  
#' @return a dataframe with roots, trunk, and leaves
#'  
#' @export

sap <- function(file) {
  
  isi_file <- sap_load(file)
  
  graph_file <- sap_graph(isi_file)
  
  sap_file <- sap_process(graph_file)
}
