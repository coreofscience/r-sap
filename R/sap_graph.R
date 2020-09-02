#' @title from df isi to graph
#'
#' @description Load a dataframe with isi data and convert it
#' in a graph
#'
#' @param isi_df a dataframe with wos data
#'
#' @author Sebastian Robledo
#'
#' @import dplyr
#' @import tidyr
#' @import igraph
#'
#' @importFrom dplyr as_tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @importFrom tidyr separate_rows
#'
#' @importFrom igraph graph.data.frame
#' @importFrom igraph simplify
#' @importFrom igraph delete.vertices
#' @importFrom igraph induced.subgraph
#' @importFrom igraph clusters
#' @importFrom igraph induced.subgraph
#'
#' @return a graph object
#'
sap_graph <- function(isi_df) {
  
  isi_df$ID_WOS <- rownames(isi_df)
  
  isi_df$ID_WOS <- ifelse(!is.na(isi_df$VL),
                          paste(isi_df$ID_WOS,
                                isi_df$VL,
                                sep = ", V"),
                          isi_df$ID_WOS)
  
  isi_df$ID_WOS <- ifelse(!is.na(isi_df$BP),
                          paste(isi_df$ID_WOS,
                                isi_df$BP,
                                sep = ", P"),
                          isi_df$ID_WOS)
  
  isi_df$ID_WOS <- ifelse(!is.na(isi_df$DI),
                          paste(isi_df$ID_WOS,
                                isi_df$ID,
                                sep = ", DOI"),
                          isi_df$ID_WOS)
  edgelist <-
    dplyr::as_tibble(isi_df) %>%
    dplyr::mutate(cited_references = CR) %>%
    tidyr::separate_rows(CR, sep = ";") %>%
    dplyr::filter(!grepl(pattern = "^[0-9].*",
                         CR)) %>%
    dplyr::select(ID_WOS, CR) %>%
    dplyr::filter(CR != "" & is.na(CR) == FALSE) %>%
    dplyr::mutate(ID_WOS = str_to_upper(ID_WOS),
                  CR = str_to_upper(CR)) %>%
    unique()
  
  graph <-
    igraph::graph.data.frame(edgelist) %>%
    igraph::simplify()
  
  graph_1 <-
    igraph::delete.vertices(graph,
                            which(degree(graph,
                                         mode = "in") == 1 &
                                    degree(graph,
                                           mode = "out") == 0))
  
  giant.component <- function(graph) {
    
    cl <- igraph::clusters(graph)
    
    igraph::induced.subgraph(graph,
                             which(cl$membership == which.max(cl$csize)))
  }
  
  graph_file <- giant.component(graph_1)
  
}
