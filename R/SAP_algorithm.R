# Este programa es para la implementacion del algoritmo SAP para analisis de
# citaciones.
# ------------------------------------------------------------------------------
# Referencias:
# 1) SAP algorithm for citation analisis: an improvement to tree of science.
# Daniel S. Valencia, Sebastian Robledo, Ricardo Pinilla, Nestor D. Duque y
# Gerard Olivar Tost. Revista de Ingenieria e Investigacion. 2020.
# ------------------------------------------------------------------------------
# Elaboro: Luis Alexander Valencia - 08 septiembre 2020
# correo: lavalenciah@unal.edu.co - lavalenciah12@gmail.com

# Se cargan las librerias necesarias

library(igraph)
library(tidyverse)
library(tidyr)
library(bibliometrix)
library(ggplot2)
library(CINNA)

# Se cargan los datos

nombre.archivo  <- "savedrecs - 2020-08-29T093831.545.txt"
data_wos        <- convert2df(file     = nombre.archivo,
                              dbsource = "wos",
                              format   = "plaintext")

# Se Crea el ID_WOS para compararlo con las referencias


data_wos$ID_WOS <- rownames(data_wos)

data_wos$ID_WOS <- ifelse(!is.na(data_wos$VL),
                          paste(data_wos$ID_WOS,
                                data_wos$VL,
                                sep = ", V"),
                          data_wos$ID_WOS)

data_wos$ID_WOS <- ifelse(!is.na(data_wos$BP),
                          paste(data_wos$ID_WOS,
                                data_wos$BP,
                                sep = ", P"),
                          data_wos$ID_WOS)

data_wos$ID_WOS <- ifelse(!is.na(data_wos$DI),
                          paste(data_wos$ID_WOS,
                                data_wos$DI,
                                sep = ", DOI "),
                          data_wos$ID_WOS)

# Creando la lista de enlases

edgelist <-
  as_tibble(data_wos) %>%
  mutate(cited_references = CR) %>%
  separate_rows(CR, sep = ";") %>%
  filter(!grepl(pattern = "^[0-9].*",
                CR)) %>%
  select(ID_WOS, CR) %>%
  filter(CR != "" & is.na(CR) == FALSE) %>%
  mutate(ID_WOS = str_to_upper(ID_WOS),
         CR = str_to_upper(CR)) %>%
  unique()

# Se crea el grafo y se eliminan los bucles o ciclos y las aristas repetidas
graph <- graph.data.frame(edgelist) %>%
  simplify()

# Se eliminan los vertices con indegree = 1 y con outdegree = 0
graph_1 <- delete.vertices(graph,
                           which(degree(graph, mode = "in") == 1 &
                                   degree(graph, mode = "out") == 0))

# Se escoge el componente mas grande conectado
graph_2 <- giant_component_extract(graph_1, directed = TRUE)
graph_2 <- graph_2[[1]]

# Se determinan las metricas de la red

metricas.red <- tibble(
  id        = V(graph_2)$name,
  indegree  = degree(graph_2, mode = "in"),
  outdegree = degree(graph_2, mode = "out"),
  bet       = betweenness(graph_2))

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
Caminos   <- c()
for (vert in Hojas$id){
  h <- get.all.shortest.paths(graph_2,
                              from = vert,
                              to   = Raices$id,
                              mode = "out")

  SAP_hojas   <- c(SAP_hojas, length(h[[1]]))
  lista.nodos <- unique(unlist(h[1]))
  lista.nodos <- lista.nodos[!(lista.nodos %in% num.vertices.raices)]
  lista.nodos <- lista.nodos[!(lista.nodos %in% num.vertices.hojas)]
  Caminos     <- c(Caminos,lista.nodos)
}
Hojas <- Hojas %>%
  mutate(SAP = SAP_hojas) %>%
  arrange(desc(SAP))

Hojas <- Hojas[1:60,]


# Seleccion del tronco

Tronco     <- metricas.red[Caminos, c("id","indegree","year")]
mas.nuevo  <- max(Tronco$year, na.rm = TRUE)
Tronco     <- Tronco %>%
  mutate(antiguedad = mas.nuevo - year)
