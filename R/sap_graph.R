#' @title from df isi to graph
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
#' 
sap_graph <- function(isi_file) {
  
  isi_file$ID_WOS <- rownames(isi_file)
  
  isi_file$ID_WOS <- ifelse(!is.na(isi_file$VL),
                            paste(isi_file$ID_WOS,
                                  isi_file$VL,
                                  sep = ", V"),
                            isi_file$ID_WOS)
  
  isi_file$ID_WOS <- ifelse(!is.na(isi_file$BP),
                            paste(isi_file$ID_WOS,
                                  isi_file$BP,
                                  sep = ", P"),
                            isi_file$ID_WOS)
  
  isi_file$ID_WOS <- ifelse(!is.na(isi_file$DI),
                            paste(isi_file$ID_WOS,
                                  isi_file$ID,
                                  sep = ", DOI"),
                            isi_file$ID_WOS)
  edgelist <- 
    as_tibble(isi_file) %>% 
    mutate(cited_references = CR) %>% 
    separate_rows(CR, sep = ";") %>% 
    filter(!grepl(pattern = "^[0-9].*",
                  CR)) %>% 
    select(ID_WOS, CR) %>% 
    filter(CR != "" & is.na(CR) == FALSE) %>% 
    mutate(ID_WOS = str_to_upper(ID_WOS), 
           CR = str_to_upper(CR)) %>% 
    unique()
  
  graph <- 
    graph.data.frame(edgelist) %>% 
    simplify()
  
  graph_1 <- delete.vertices(graph, 
                             which(degree(graph, mode = "in") == 1 &
                                     degree(graph, mode = "out") == 0))
  
  giant.component <- function(graph) {
    
    cl <- clusters(graph)
    
    induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
  }
  
  graph_file <- giant.component(graph_1)
  
}