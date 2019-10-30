library(visNetwork)
nodes <- read.csv("id.csv", header = TRUE)
edges <- read.csv("tie.csv", header = TRUE)

visNetwork(nodes, edges) %>% 
visOptions(highlightNearest = list(degree=1,hover=T),nodesIdSelection = TRUE) %>%
visInteraction(navigationButtons = TRUE) %>%
visInteraction(hoverConnectedEdges = TRUE) %>%
visEdges(arrows = 'from') %>%
visInteraction(hover = TRUE) %>%
visLayout(randomSeed = 123) 
