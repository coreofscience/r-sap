#' Creating our ToS
#'
#' from graph isi to sap - Tree of Science
#'
#' Load a dataframe with isi data and convert it
#' in a graph
#'
#' @param graph_file a dataframe with wos data
#'
#' @author Sebastian Robledo
#'
#' @import dplyr
#' @import igraph
#' @import utils
#'
#' @return a graph object
#' @importFrom utils head

sap_process <- function(graph_file) {
  
  network.metrics <-
    dplyr::tibble(
      id = igraph::V(graph_file)$name,
      indegree = igraph::degree(graph_file, mode = "in"),
      outdegree = igraph::degree(graph_file, mode = "out"),
      bet = igraph::betweenness(graph_file)
    )
  
  seminals <- network.metrics[network.metrics$outdegree == 0,
                              c("id","indegree")]
  seminals <- utils::head(seminals[with(seminals, order(-indegree)),],10)
  
  structurals <- network.metrics[network.metrics$bet > 0,
                                 c("id", "bet")]
  structurals <- utils::head(structurals[with(structurals, order(-bet)),],10)
  
  current <- network.metrics[network.metrics$indegree == 0,
                             c("id","outdegree")]
  current <- utils::head(current[with(current, order(-outdegree)),], 60)
  
  if(sum(network.metrics$bet) == 0 ) {
    stop("Your ToS does not have trunk, check the search out")} else {
      
      
      seminals$ToS <- "Raiz"
      seminals$order <- 1:length(seminals$id)
      structurals$ToS <- "Tronco"
      structurals$order <- 1:length(structurals$id)
      current$ToS <- "Hojas"
      current$order <- 1:length(current$id)
      
      tos <- rbind(seminals[,c(1,3,4)], structurals[,c(1,3,4)], current[,c(1,3,4)])
    }
}
