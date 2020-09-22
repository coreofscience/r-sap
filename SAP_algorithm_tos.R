
# Se cargan las librerias necesarias

library(igraph)
library(tidyverse)
library(tidyr)
library(bibliometrix)
library(ggplot2)
library(CINNA)
library(formattable)
library(rebus)
library(tm)
library(lubridate)
library(gt)
library(dplyr)
library(wordcloud)


# Se cargan los datos 

nombre.archivo   <- "scopus3.bib"
scopus_dataframe <- convert2df(file     = nombre.archivo,
                              dbsource = "scopus",
                              format   = "bibtex")%>% 
                   as_tibble()%>% 
  mutate(SR_TOS = str_extract(SR, one_or_more(WRD) %R%
                                SPC %R% one_or_more(WRD) %R%
                                "," %R% SPC %R% 
                                one_or_more(DGT) %R% ","),
         SR_TOS = str_c(SR_TOS, " ", SO))


#-------------------------------------------------------------------------------
# ------ Se crea la funcion para la generacion de la edgelist ------------------
#-------------------------------------------------------------------------------
edgelista.bib <- function(scopus_dataframe){
  
  pattern_authors <- 
    SPC %R% 
    one_or_more(WRD) %R%
    SPC %R%
    one_or_more(or(WRD, ANY_CHAR))
  
  pattern_titles <- 
    OPEN_PAREN %R% 
    repeated(DGT, 4) %R% 
    CLOSE_PAREN %R%
    one_or_more(or(WRD,ANY_CHAR))
  
  pattern_year <- 
    OPEN_PAREN %R% 
    repeated(DGT, 4) %R% 
    CLOSE_PAREN 
  
  pattern_journal <- 
    one_or_more(or(WRD,SPC))
  
  pattern_volume <-
    one_or_more(or(WRD, SPC))
  
  pattern_pages <- 
    "PP. " %R%
    one_or_more(or(DGT, ANY_CHAR))
  
  cited_references <- 
    scopus_dataframe %>%
    separate_rows(CR, sep = "; ") %>% 
    select(SR_TOS, 
           CR) %>% 
    mutate(CR_AUTHOR = str_remove(CR, pattern_authors),
           CR_TITLE_1 = str_extract(CR, pattern_authors),
           CR_TITLE = str_remove(CR_TITLE_1, pattern_titles),
           CR_TITLE = str_trim(CR_TITLE),
           CR_YEAR_1 <- str_extract(CR_TITLE_1, pattern_titles),
           CR_YEAR = str_extract(CR_YEAR_1, repeated(DGT, 4)),
           CR_JOURNAL_1 = str_remove(CR_YEAR_1, pattern_year),
           CR_JOURNAL = str_extract(CR_JOURNAL_1, pattern_journal),
           CR_JOURNAL = str_trim(CR_JOURNAL),
           CR_VOLUME_1 = str_remove(CR_JOURNAL_1, pattern_journal),
           CR_VOLUME = str_extract(CR_VOLUME_1, pattern_volume),
           CR_PAGES = str_extract(CR_VOLUME_1, pattern_pages),
           CR_PAGES = str_remove(CR_PAGES, "PP. ")) %>% 
    select(SR_TOS, 
           CR, 
           CR_AUTHOR, 
           CR_TITLE, 
           CR_YEAR, 
           CR_JOURNAL, 
           CR_VOLUME, 
           CR_PAGES) %>% 
    mutate(lastname = sub("\\., .*", "", CR),
           lastname = sub(",", "", lastname),
           lastname = sub("\\.", "", lastname),
           CR_SO = str_c(lastname, 
                         ", ",
                         CR_YEAR,
                         ", ",
                         CR_JOURNAL)) %>% 
    select(-lastname)
  
  
  edge_list <-
    cited_references %>% 
    select(SR_TOS, 
           CR_SO) %>% 
    na.omit() %>% 
    unique()
  
  return(edge_list)
  
}

#-------------------------------------------------------------------------------
# ------ Se crea la funcion para depurar el grafo ------------------------------
#-------------------------------------------------------------------------------

crear.grafo <- function(edgelist){
  
  graph <- graph.data.frame(edgelist) %>%
    simplify()
  
  # Se eliminan los vertices con indegree = 1 y con outdegree = 0
  graph_1 <- delete.vertices(graph,
                             which(degree(graph, mode = "in") == 1 &
                                     degree(graph, mode = "out") == 0))
  
  # Se escoge el componente mas grande conectado
  graph_2 <- giant_component_extract(graph_1, directed = TRUE)
  graph_2 <- graph_2[[1]]
  
  subareas <- 
    as.undirected(graph_2,
                  mode = "each") %>% 
    cluster_louvain()
  
  graph_2 <- 
    graph_2 %>% 
    set_vertex_attr(name = "sub_area",
                    value = membership(subareas))
  
  
}

#-------------------------------------------------------------------------------
# ------ Se crea la funcion algoritmo SAP para analisis de citaciones ----------
#-------------------------------------------------------------------------------
algoritmoSAP <- function(graph_2,scopus_dataframe){
  
  # Se crean la metricas de la red 
  
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
  for (vert in Hojas$id){
    h <- get.all.shortest.paths(graph_2,
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
    h <- get.all.shortest.paths(graph_2,
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
  
  TOS       <- rbind(Raices[,c(1,3)], Tronco[,c(1,5)], Hojas[,c(1,6)])
  return(TOS)
}



#-------------------------------------------------------------------------------
# ------ Se crea la funcion para generar subareas ------------------------------
#-------------------------------------------------------------------------------
subarea.grafo <- function(graph,referencias){
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
  grupo1$subarea <- "Grupo 1"
  grupo2         <- tabla.subareas[tabla.subareas$subarea == subareas_3$subarea[2],]
  grupo2$subarea <- "Grupo 2"
  grupo3         <- tabla.subareas[tabla.subareas$subarea == subareas_3$subarea[3],]
  grupo3$subarea <- "Grupo 3"
  grupos         <- rbind(grupo1,grupo2, grupo3) 
  referencias1   <- referencias[referencias$CR_SO %in% grupo1$autores,]
  referencias1$grupo <- "Group 1"
  referencias2   <- referencias[referencias$CR_SO %in% grupo2$autores,]
  referencias2$grupo <- "Group 2"
  referencias3   <- referencias[referencias$CR_SO %in% grupo3$autores,]
  referencias3$grupo <- "Group 3"
  referencias.grupo  <- rbind(referencias1,referencias2, referencias3) 
  
  return(referencias.grupo)
}

#-------------------------------------------------------------------------------
# ------ Se crea la funcion para generar referencias citadas -------------------
#-------------------------------------------------------------------------------

cited_references <- function(scopus_dataframe) {
  
  pattern_authors <- 
    SPC %R% 
    one_or_more(WRD) %R%
    SPC %R%
    one_or_more(or(WRD, ANY_CHAR))
  
  pattern_titles <- 
    OPEN_PAREN %R% 
    repeated(DGT, 4) %R% 
    CLOSE_PAREN %R%
    one_or_more(or(WRD,ANY_CHAR))
  
  pattern_year <- 
    OPEN_PAREN %R% 
    repeated(DGT, 4) %R% 
    CLOSE_PAREN 
  
  pattern_journal <- 
    one_or_more(or(WRD,SPC))
  
  pattern_volume <-
    one_or_more(or(WRD, SPC))
  
  pattern_pages <- 
    "PP. " %R%
    one_or_more(or(DGT, ANY_CHAR))
  
  cited_references <- 
    scopus_dataframe %>%
    separate_rows(CR, sep = "; ") %>% 
    select(SR_TOS, 
           CR) %>% 
    mutate(CR_AUTHOR = str_remove(CR, pattern_authors),
           CR_TITLE_1 = str_extract(CR, pattern_authors),
           CR_TITLE = str_remove(CR_TITLE_1, pattern_titles),
           CR_TITLE = str_trim(CR_TITLE),
           CR_YEAR_1 <- str_extract(CR_TITLE_1, pattern_titles),
           CR_YEAR = str_extract(CR_YEAR_1, repeated(DGT, 4)),
           CR_JOURNAL_1 = str_remove(CR_YEAR_1, pattern_year),
           CR_JOURNAL = str_extract(CR_JOURNAL_1, pattern_journal),
           CR_JOURNAL = str_trim(CR_JOURNAL),
           CR_VOLUME_1 = str_remove(CR_JOURNAL_1, pattern_journal),
           CR_VOLUME = str_extract(CR_VOLUME_1, pattern_volume),
           CR_PAGES = str_extract(CR_VOLUME_1, pattern_pages),
           CR_PAGES = str_remove(CR_PAGES, "PP. ")) %>% 
    select(SR_TOS, 
           CR, 
           CR_AUTHOR, 
           CR_TITLE, 
           CR_YEAR, 
           CR_JOURNAL, 
           CR_VOLUME, 
           CR_PAGES) %>% 
    mutate(lastname = sub("\\., .*", "", CR),
           lastname = sub(",", "", lastname),
           lastname = sub("\\.", "", lastname),
           CR_SO = str_c(lastname, 
                         ", ",
                         CR_YEAR,
                         ", ",
                         CR_JOURNAL)) %>% 
    select(-lastname)
  
  return(cited_references)
  
}

#-------------------------------------------------------------------------------
# ------ Se crea la funcion para generar los wordclouds-------------------------
#-------------------------------------------------------------------------------

wordclouds <- function (subarea_1){
  
  # Se crea el corpus 
  corp <- Corpus(VectorSource(subarea_1$CR_TITLE %>% na.omit()))
  paperCorp <- corp
  
  # Se realiza la limpieza el corpus 
  paperCorp <- tm_map(paperCorp, removePunctuation)
  paperCorp <- tm_map(paperCorp, removeNumbers)
  paperCorp <- tm_map(paperCorp, content_transformer(tolower))
  paperCorp <- tm_map(paperCorp, removeWords, stopwords("english"))
  paperCorp <- tm_map(paperCorp, stripWhitespace)
  #paperCorp <- tm_map(paperCorp, stemDocument)
  paperCorp <- tm_map(paperCorp, removeWords, c("viral", "market"))
}



edgelista   <- edgelista.bib(scopus_dataframe)
grafo       <- crear.grafo(edgelista)
TOS         <- algoritmoSAP(grafo,scopus_dataframe)
referencias <- cited_references(scopus_dataframe)
subareas    <- subarea.grafo(grafo,referencias)
wordcloud1  <- wordclouds(subareas[subareas$grupo == "Group 1",])
wordcloud2  <- wordclouds(subareas[subareas$grupo == "Group 2",])
wordcloud3  <- wordclouds(subareas[subareas$grupo == "Group 3",])

wordcloud(wordcloud1, 
          min.freq = 20,
          max.words=50, 
          random.order=FALSE, 
          rot.per=0.35,
          colors=brewer.pal(8,"Dark2"))

wordcloud(wordcloud2, 
          min.freq = 20,
          max.words=50, 
          random.order=FALSE, 
          rot.per=0.35,
          colors=brewer.pal(8,"Dark2"))

wordcloud(wordcloud3, 
          min.freq = 20,
          max.words=50, 
          random.order=FALSE, 
          rot.per=0.35,
          colors=brewer.pal(8,"Dark2"))


dtm <- TermDocumentMatrix(wordcloud1)
m   <- as.matrix(dtm)
v   <- sort(rowSums(m),decreasing=TRUE)
d   <- data.frame(word = names(v),freq=v)
head(d, 10)

palabras <- d[1:10,1]

wordcloud(words = d$word, 
          freq = d$freq, 
          min.freq = 20,
          max.words=50, 
          random.order=FALSE, 
          rot.per=0.35,
          colors=brewer.pal(8,"Dark2"))









