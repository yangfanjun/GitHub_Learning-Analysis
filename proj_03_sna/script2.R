library(igraph)

# load edge list 
all_edge=read.csv("all_edge.csv")

# create igraph from the edge list
all_igraph <- graph_from_data_frame(d=all_edge, directed=T)
# cheeck node and edge
E(all_igraph)     
V(all_igraph)
# plot not pretty,  need further revise it later
plot(all_igraph, edge.arrow.size=.1)

## node-level analysis
igraph::degree(all_igraph)
igraph::degree(all_igraph,mode="out")
igraph::betweenness(all_igraph)
igraph::closeness(all_igraph, mode="in")
igraph::closeness(all_igraph, mode="out")
igraph::closeness(all_igraph, mode="all")
igraph::eigen_centrality(all_igraph)

## network level analysis
igraph::diameter(all_igraph, directed = TRUE)
dyad_census(all_igraph)
distances(all_igraph)
shortest_paths(all_igraph, 5)
centr_degree(all_igraph)$centralization
centr_clo(all_igraph, mode="all")$centralization
centr_eigen(all_igraph, directed=FALSE)$centralization

#### use visNetwork package########
# plot you should customize it later
library(visNetwork)
visIgraph(all_igraph, idToLabel = TRUE, layout = "layout_nicely",
          physics = FALSE, smooth = T)





