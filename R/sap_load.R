#' Loading .bib files
#'
#' load .bib file and convert it to df
#'
#' Load .bib file and convert it to a data frame with a bibliometrix
#' function called convert2df().
#'
#' @param file the isi file (txt) downloaded from Web of Science
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
#' @return a dataframe with all variables in columns
#'
#'
sap_load <- function(file){
  scopus_dataframe <- convert2df(file     = file,
                                 dbsource = "scopus",
                                 format   = "bibtex") %>%
                                 as_tibble()%>%
    mutate(SR_TOS = str_extract(SR, one_or_more(WRD) %R%
                                  SPC %R% one_or_more(WRD) %R%
                                  "," %R% SPC %R%
                                  one_or_more(DGT) %R% ","),
           SR_TOS = str_c(SR_TOS, " ", SO))
  return(scopus_dataframe)
}
