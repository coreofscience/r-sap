#' Creating edge list from scopus data frame
#'
#'
#'
#' @param scopus_dataframe a dataframe with wos data
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
#' @return a dataframe with edge list for graph


sap_edgelist <- function(scopus_dataframe){

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

