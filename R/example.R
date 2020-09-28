
# Load file
file        <- "scopus.bib"
scopus_data <- sap_load(file)

# Create graph
graph_tos <- sap_graph(scopus_data)

# Create Tree of Science
ToS <- sap_process(graph_tos)

# Create data frame with cited references
cited_references <- sap_citedReferences(scopus_data)

# Create subfields dataframe
subfields <- sap_subfields(graph_tos,cited_references)
formattable(subfields)

# Create a wordcloud
wordcloud1  <- sap_wordcloud(subfields[subfields$grupo == "Group 1",])
wordcloud(wordcloud1,
          min.freq = 20,
          max.words= 50,
          random.order=FALSE,
          rot.per=0.35,
          colors=brewer.pal(8,"Dark2"))

