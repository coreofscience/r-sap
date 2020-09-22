# Se cargan
library(shiny)
library(shinydashboard)
library(igraph)
library(tidyverse)
library(tidyr)
library(CINNA)
library(bibliometrix)

ui <- dashboardPage(
  dashboardHeader(title = "Core of Science"),
  dashboardSidebar(

    helpText("Note: this program performs a citation analysis  ",
             "based on the SAP algorithm."),

    fileInput("file1", "Choose txt File",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".bib")
    ),


    sidebarMenu(
      menuItem("Introduction", tabName = "importance", icon = icon("th")),
      menuItem("Importance"  , tabName = "importance", icon = icon("th")),
      menuItem("Evolution"   , tabName = "evolution" , icon = icon("th")),
      menuItem("Subfields"   , tabName = "subfields" , icon = icon("th"))
    ),

    selectInput("var",
                label = "Choose a subfield group",
                choices = list("Grupo 1",
                               "Grupo 2",
                               "Grupo 3"),
                selected = "Grupo 1"),

    downloadButton("downloadData", strong("Download Tree of science"))
  ),
  dashboardBody(

    tabItems(
      tabItem(tabName = "importance",h2("Articles importance"),

              fluidRow( box(title = "History publication",plotOutput("grafico1")),
                        box(title = "Most productive authors",tableOutput("tabla1"))),

              fluidRow( box(title = "Most popular journals",tableOutput("tabla2")))

      ),
      tabItem(tabName = "evolution",h2("Evolution ToS"),
              fluidRow(
                box(title = "Tree of Science ToS",
                    tableOutput("contents")
                )
              )
      ),
      tabItem(tabName = "subfields",h2("Subfields"),
              fluidRow(
                box(title = "Subfields clasification",
                    tableOutput("subareas")
                )
              )
      )

    )
  )
)


server <- function(input, output) {


  output$contents <- renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)

    #df <- read.(input$file1$datapath)
    data_wos <- convert2df(file     = input$file1$datapath,
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
    Raices$TOS <- "Root"
    Hojas$TOS  <- "Leaves"
    Tronco$TOS <- "Trunck"

    TOS   <- rbind(Raices[,c(1,3)], Tronco[,c(1,5)], Hojas[,c(1,6)])

    output$downloadData <- downloadHandler(
      filename = function() {
        paste("arbol", ".csv", sep = "")
      },
      content = function(file) {
        #write.csv(TOS, file, row.names = FALSE)
        write.table(TOS,file,sep=";")
      }
    )
    return(TOS[,c("id","TOS")])
  })

  output$grafico1 <- renderPlot({

    req(input$file1)

    #df <- read.(input$file1$datapath)
    data_wos <- convert2df(file     = input$file1$datapath,
                           dbsource = "wos",
                           format   = "plaintext")
    fecha.publicacion <- data_wos$PY                  # Año de publicacion
    N                 <- length(fecha.publicacion)   # Numero de articulos
    frec.publicacion  <- table(fecha.publicacion)
    plot(x    = frec.publicacion,  # Datos
         main = "Scientific anual production",
         ylab = "Papers",
         xlab = "Year",
         col  = "Red",
         type = "o")
  })

  output$tabla1 <- renderTable({

    req(input$file1)

    #df <- read.(input$file1$datapath)
    data_wos <- convert2df(file     = input$file1$datapath,
                           dbsource = "wos",
                           format   = "plaintext")

    autores       <- strsplit( x = data_wos$AF, split = ";", fixed = FALSE )
    Author <- c()

    for (w in autores) {
      for (w1 in w){
        Author <- c(Author,w1)
      }
    }
    frec.autores <- as.data.frame(table(Author)) %>%
      arrange(desc(Freq))


    return(frec.autores[1:10,])

  })

  output$tabla2 <- renderTable({

    req(input$file1)

    #df <- read.(input$file1$datapath)
    data_wos <- convert2df(file     = input$file1$datapath,
                           dbsource = "wos",
                           format   = "plaintext")


    Journals <- as.data.frame(table(data_wos$SO)) %>%
      arrange(desc(Freq)) %>%
      rename(Journal = "Var1")

    return(Journals[1:10,])

  })

  output$subareas <- renderTable({

    req(input$file1)

    #df <- read.(input$file1$datapath)
    data_wos <- convert2df(file     = input$file1$datapath,
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


    subareas <-
      as.undirected(graph_2,
                    mode = "each") %>%
      cluster_louvain()

    graph <-
      graph_2 %>%
      set_vertex_attr(name = "sub_area",
                      value = membership(subareas))
    subareas <-
      as.undirected(graph_2,
                    mode = "each") %>%
      cluster_louvain()

    graph <-
      graph_2 %>%
      set_vertex_attr(name = "sub_area",
                      value = membership(subareas))


    subareas_3 <-
      tibble(publication = subareas$names , subarea = V(graph)$sub_area) %>%
      group_by(subarea) %>%
      count() %>%
      arrange(desc(n))


    tabla.subareas <- tibble(Authors = subareas$names , subfield = V(graph)$sub_area)
    grupo1         <- tabla.subareas[tabla.subareas$subfield == subareas_3$subarea[1],]
    grupo1$subfield <- "Group 1"
    grupo2         <- tabla.subareas[tabla.subareas$subfield == subareas_3$subarea[2],]
    grupo2$subfield <- "Group 2"
    grupo3         <- tabla.subareas[tabla.subareas$subfield == subareas_3$subarea[3],]
    grupo3$subfield <- "Group 3"
    agrupacion     <- rbind(grupo1,grupo2,grupo3)

    if (input$var == "Grupo 1"){
      return(grupo1)
    }
    if (input$var == "Grupo 2"){
      return(grupo2)
    }
    if (input$var == "Grupo 3"){
      return(grupo3)
    }



  })


}

shinyApp(ui, server)


tabItem(
  tabName = "intento",
  fluidRow(
    tabBox(
      title = "Subfields",
      tabPanel("Group 1",
               fluidRow(
                 box()
               )
      ),
      tabPanel("Wordcloud Group 1",
               fluidRow(
                 box()
               ))
    )
  )
)





