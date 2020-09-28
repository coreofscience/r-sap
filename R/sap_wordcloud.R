#' Creating wordcloud from subfields
#'
#'
#'
#' @param subfield,min_freq,max_words a dataframe with wos data
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


sap_wordcloud <- function (subarea){

  # Se crea el corpus
  corp <- Corpus(VectorSource(subarea$CR_TITLE %>% na.omit()))
  paperCorp <- corp

  # Se realiza la limpieza el corpus
  paperCorp <- tm_map(paperCorp, removePunctuation)
  paperCorp <- tm_map(paperCorp, removeNumbers)
  paperCorp <- tm_map(paperCorp, content_transformer(tolower))
  paperCorp <- tm_map(paperCorp, removeWords, stopwords("english"))
  paperCorp <- tm_map(paperCorp, stripWhitespace)
  #paperCorp <- tm_map(paperCorp, stemDocument)
  paperCorp <- tm_map(paperCorp, removeWords, c("viral", "market"))
  return(paperCorp)

}
