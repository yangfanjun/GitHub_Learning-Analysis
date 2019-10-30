library(sna)

getwd()

# load the matrix file
all_matrix <- read.csv("all_matrix.csv", row.names = 1)
# load the edge list file
all_edge<- read.csv("all_edge.csv")

####### use package sna, network ##########
library(sna)

# plot the network, not pretty
# 对all_matrix.csv中的矩阵画图，模式为有向图, 显示标签，标签大小为0.8，顶点的颜色暗橄榄绿色
gplot(all_matrix,gmode="digraph", displaylabels=TRUE,label.cex=0.8,vertex.col="darkolivegreen")

# convert matrix to network format for further analysis
# 构造网络对象overallnet
overallnet=network(all_matrix)
overallnet

############################
###### node level analysis#######
############################
#calculate in/out degree
id<- sna::degree(overallnet,gmode="digraph",cmode="indegree")
od<- sna::degree(overallnet,gmode="digraph",cmode="outdegree")
id
od
id+od
#betweenness
bet1=sna::betweenness(overallnet,rescale=T)
bet2=sna::betweenness(overallnet)
bet1
bet2
#closeness
clo1=sna::closeness(overallnet,rescale=T)
clo2=sna::closeness(overallnet)
clo1
clo2
#Eigenvector
eig1=sna::evcent(overallnet,rescale=T)
eig2=sna::evcent(overallnet)
eig1
eig2

#plot node size and color based on in/out degree
# vertex.cex 节点大小 无向图 无标签背景框 标签大小 标签与节点的位置关系
gplot(overallnet, vertex.cex=(id+od)^0.5/2, gmode="graph",
      boxed.labels=FALSE,label.cex=0.7, label.pos=5, label.col="grey17",
      vertex.col=rgb((id+od)/max(id+od),0,(id+od)/max(id+od)),edge.col="grey88",
      label=network.vertex.names(overallnet),edge.lwd=all_matrix/2,mode = "fruchtermanreingold")

###########################################
######## group level analysis  ######
###########################################
centralization(overallnet,degree)
centralization(overallnet,degree,cmode="outdegree")
centralization(overallnet,degree,cmode="indegree")
centralization(overallnet, betweenness)
centralization(overallnet, closeness)
centralization(overallnet, evcent)

network.size(overallnet)
gden(overallnet,mode="graph") #density
degree(overallnet)
mean(degree(overallnet)) #average degree
sum(id)/20 #average in degree
sum(od)/20 #average out degree

#transitivity(overallnet)
dyad.census(overallnet)
network.dyadcount(overallnet, na.omit = F)
network.edgecount(overallnet, na.omit = F)
grecip(overallnet, measure = "edgewise")
grecip(overallnet, measure = "dyadic")
grecip(overallnet, measure = "dyadic.nonnull")
gtrans(overallnet) #transitivity

hierarchy(overallnet, measure = "reciprocity")
hierarchy(overallnet, measure = "krackhardt")

# Returns the number of components within network
components(overallnet,connected="weak")
#returns the Krackhardt connectedness
connectedness(overallnet, g=NULL)



