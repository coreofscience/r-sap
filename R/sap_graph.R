#' Creating a graph object
#'
#' from df isi to graph
#'
#' Load a dataframe with isi data and convert it
#' in a graph
#'
#' @param isi_df a dataframe with wos data
#'
#' @author Sebastian Robledo
#'
#' @import dplyr
#' @import tidyr
#' @import igraph
#' @import tidyverse
#' @import stringr
#' @import utils
#' @import magrittr
#' @import rlang
#'
#' @importFrom dplyr %>%
#' @importFrom magrittr %>%
#' @importFrom rlang .data
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
    dplyr::mutate(cited_references = .data$CR) %>%
    tidyr::separate_rows(.data$CR, sep = ";") %>%
    dplyr::filter(!grepl(pattern = "^[0-9].*",
                         .data$CR)) %>%
    dplyr::select(.data$ID_WOS, .data$CR) %>%
    dplyr::filter(.data$CR != "" & is.na(.data$CR) == FALSE) %>%
    dplyr::mutate(ID_WOS = stringr::str_to_upper(.data$ID_WOS),
                  CR = stringr::str_to_upper(.data$CR)) %>%
    unique()
  
  graph <-
    igraph::graph.data.frame(edgelist) %>%
    igraph::simplify()
  
  graph_1 <-
    igraph::delete.vertices(graph,
                            which(igraph::degree(graph,
                                                 mode = "in") == 1 &
                                    igraph::degree(graph,
                                                   mode = "out") == 0))
  
  giant.component <- function(graph) {
    
    cl <- igraph::clusters(graph)
    
    igraph::induced.subgraph(graph,
                             which(cl$membership == which.max(cl$csize)))
  }
  
  graph_file <- giant.component(graph_1)
  
}
