#' @title from graph isi to sap - Tree of Science
#' 
#' @description Load a dataframe with isi data and convert it
#' in a graph
#' 
#' @param isi_file a dataframe with wos data
#' 
#' @author Sebastian Robledo
#' 
#' @import igraph
#' @import tidyverse
#' 
#' @importFrom igraph graph.data.frame
#' @importFrom igraph delete.vertices
#' @importFrom igraph induced.subgraph
#' 
#' @importFrom tidyverse tibble
#' @importFrom tidyverse separate_rows
#' 
#' @return a graph object

sap_process <- function(graph_file) {
  
}