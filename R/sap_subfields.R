#' Creating subflieds from
#'
#'
#'
#' @param scopus_dataframe,cited_references a dataframe with wos data
#'
#' @author Sebastian Robledo
#'
#' @import igraph
#' @import tidyverse
#' @import bibliometricx
#' @import ggplot2
#' @import CINNA
#' @import formattable
#' @import rebus
#' @import tm
#' @import lubridate
#' @import gt
#' @import dplyr
#' @import wordcloud
#'
#' @return a dataframe with cited references
#'
#'
sap_subfields <- function(graph,cited_references){
subareas <-
  as.undirected(graph, mode = "each") %>%
  cluster_louvain()

subareas_3 <-
  tibble(
    subarea = V(graph)$sub_area) %>%
  group_by(subarea) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(3) %>%
  select(subarea)

graph_subarea_1 <-
  graph %>%
  delete_vertices(V(graph)$sub_area != subareas_3$subarea[1])

graph_subarea_2 <-
  graph %>%
  delete_vertices(V(graph)$sub_area != subareas_3$subarea[2])

graph_subarea_3 <-
  graph %>%
  delete_vertices(V(graph)$sub_area != subareas_3$subarea[3])

tabla.subareas <- tibble(autores = subareas$names , subarea = V(graph)$sub_area)
grupo1         <- tabla.subareas[tabla.subareas$subarea == subareas_3$subarea[1],]
grupo1$subarea <- "Group 1"
grupo2         <- tabla.subareas[tabla.subareas$subarea == subareas_3$subarea[2],]
grupo2$subarea <- "Group 2"
grupo3         <- tabla.subareas[tabla.subareas$subarea == subareas_3$subarea[3],]
grupo3$subarea <- "Group 3"
grupos         <- rbind(grupo1,grupo2, grupo3)
referencias1   <- cited_references[cited_references$CR_SO %in% grupo1$autores,]
referencias1$grupo <- "Group 1"
referencias2   <- cited_references[cited_references$CR_SO %in% grupo2$autores,]
referencias2$grupo <- "Group 2"
referencias3   <- cited_references[cited_references$CR_SO %in% grupo3$autores,]
referencias3$grupo <- "Group 3"
referencias.grupo  <- rbind(referencias1,referencias2, referencias3)

return(referencias.grupo)
}
