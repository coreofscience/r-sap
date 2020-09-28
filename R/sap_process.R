#' Creating our ToS
#'
#' from graph .bib to sap - Tree of Science
#'
#' Load a dataframe with .bib data and convert it
#' in a graph
#'
#' @param graph_file a dataframe with wos data
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
#' @return a dataframe with Tree of science

sap_process <- function(graph) {

  # Se crean la metricas de la red

  metricas.red <- tibble(
    id        = V(graph)$name,
    indegree  = degree(graph, mode = "in"),
    outdegree = degree(graph, mode = "out"),
    bet       = betweenness(graph))

  metricas.red <- metricas.red %>%
    mutate(year = as.numeric(str_extract(id, "[0-9]{4}")))

  # Clasificacion de las raices

  Raices <- metricas.red[metricas.red$outdegree == 0, c("id","indegree")] %>%
    arrange(desc(indegree))
  Raices <- Raices[1:10,]


  # Clasificacion de las hojas
  Hojas.ext <- metricas.red[metricas.red$indegree == 0, c("id","outdegree","year")]
  act.year  <- as.numeric(format(Sys.Date(),'%Y'))
  Hojas.ext <- Hojas.ext %>%
    mutate(antiguedad = act.year - year) %>%
    arrange(antiguedad)
  Hojas     <- filter(Hojas.ext, antiguedad <= 5)

  # Se determina el numero del vertice de las Hojas
  num.vertices.hojas <- c()
  for (vertice in Hojas$id){
    num.vertices.hojas <- c(num.vertices.hojas,which(metricas.red$id == vertice))
  }

  # Se determina el numero del vertice de las raices
  num.vertices.raices <- c()
  for (vertice in Raices$id){
    num.vertices.raices <- c(num.vertices.raices,which(metricas.red$id == vertice))
  }

  # Calculo del SAP de las Hojas
  SAP_hojas <- c()
  for (vert in Hojas$id){
    h <- get.all.shortest.paths(graph,
                                from = vert,
                                to   = Raices$id,
                                mode = "out")

    SAP_hojas   <- c(SAP_hojas, length(h[[1]]))
  }
  Hojas <- Hojas %>%
    mutate(SAP = SAP_hojas) %>%
    arrange(desc(SAP))

  Hojas <- Hojas[1:60,] %>%
    filter(SAP > 0)

  Caminos   <- c()
  for (vert in Hojas$id){
    h <- get.all.shortest.paths(graph,
                                from = vert,
                                to   = Raices$id,
                                mode = "out")
    lista.nodos <- unique(unlist(h[1]))
    lista.nodos <- lista.nodos[!(lista.nodos %in% num.vertices.raices)]
    lista.nodos <- lista.nodos[!(lista.nodos %in% num.vertices.hojas)]
    Caminos     <- c(Caminos,lista.nodos)
  }



  # Seleccion del tronco

  Tronco     <- metricas.red[unique(Caminos), c("id","indegree","year")]
  mas.nuevo  <- max(Tronco$year, na.rm = TRUE)
  Tronco     <- Tronco %>%
    mutate(antiguedad = mas.nuevo - year)



  # Tree of science
  Raices$TOS <- "Raiz"
  Hojas$TOS  <- "Hojas"
  Tronco$TOS <- "Tronco"

  tos   <- rbind(Raices[,c(1,3)], Tronco[,c(1,5)], Hojas[,c(1,6)])
  return(tos)
}
