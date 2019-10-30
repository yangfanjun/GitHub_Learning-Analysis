library(stats)
library(dplyr)
library(igraph)

# 读取数据表
all_code=read.csv("all_code.csv", header=TRUE)
# 筛选Week 02部分，选取vert1_id和vert2_id两列，存入week02_code，每行代表vert1_id向vert2_id回复一次
week02_code <- all_code %>% filter(week=="Week 10") %>% select(vert1_id,vert2_id)

# 将week02_code转化为igraph的数据格式，这是一张有向图
all_igraph <- graph_from_data_frame(d=week02_code, directed=T)
# 获得图中每条边和节点名称
E(all_igraph)     
V(all_igraph)

## 节点层面分析
# 计算每个节点的入度，并画出柱状图
indegree_v <- igraph::degree(all_igraph)
barplot(indegree_v, main="Indegree Bar Plot", xlab="Identity", ylab="Indegree")

# 计算每个节点的出度，并画出柱状图
outdegree_v <- igraph::degree(all_igraph,mode="out")
barplot(outdegree_v, main="Outdegree Bar Plot", xlab="Identity", ylab="Outdegree")

# 中介中心性（betweenness）这个度量很有意思。这个有点像是我们身边那种社交达人，
# 我们认识的不少朋友可能都是通过他/她认识的，这个人起到了中介的作用。
# 中介中心性指的是一个结点担任其它两个结点之间最短路的桥梁的次数。
betweenness_v <- igraph::betweenness(all_igraph)
barplot(betweenness_v, main="Betweenness Bar Plot", xlab="Identity", ylab="Betweenness")

# 特征向量中心性（Eigenvector Centrality）。一个节点的重要性既取决于其邻居节点的数量（即该节点的度），也取决于其邻居节点的重要性。
eigen_centrality_v <- igraph::eigen_centrality(all_igraph)
barplot(eigen_centrality_v$vector, main="Eigenvector Centrality Bar Plot", xlab="Identity", ylab="Eigenvector Centrality")


#igraph::closeness(all_igraph, mode="in")
#igraph::closeness(all_igraph, mode="out")
#igraph::closeness(all_igraph, mode="all")


## 网络层面的分析
# 图的直径是最长线的长度,直径采用宽度优先搜索法计算
#> diameter
#[1] 6
diameter <- igraph::diameter(all_igraph, directed = TRUE)

# 在有向图中对节点两两之间的关系进行分类。它可以分为三种状态：相互的、不对称的或不存在的。
# > dyad_census(all_igraph) $mut 13 $asym 54 $null 123  13+54+123=190=c20/2
dyad_census <- dyad_census(all_igraph)

# 节点两两之间最短距离，返回一个矩阵
distances <- distances(all_igraph)

# shortest_paths(all_igraph, 10)

# 中心化是从顶点的中心度得分进行图中心化度量的种方法
centr_degree(all_igraph)$centralization
centr_clo(all_igraph, mode="all")$centralization
centr_eigen(all_igraph, directed=FALSE)$centralization

#### use visNetwork package########
# plot you should customize it later
library(visNetwork)

# vert1_id和vert2_id单独成列，命名为nodes_v1和nodes_v2
nodes_v1 <- all_code %>% filter(week=="Week 10") %>% select(vert1_id)
nodes_v2 <- all_code %>% filter(week=="Week 10") %>% select(vert2_id)

# 将nodes_v1和nodes_v2中不重复的部分取并集，组成字符串向量，放入unique_nodes中
unique_nodes_v1 <- as.character(unique(nodes_v1$vert1_id))
unique_nodes_v2 <- as.character(unique(nodes_v2$vert2_id))
unique_nodes <- union(unique_nodes_v1, unique_nodes_v2)

# index为unique_nodes中每个元素的数字索引
index <- seq(1,length(unique_nodes))
# 将数字索引和节点放入一张数据表，意义对应
unique_char_num = data.frame(unique_nodes, index)

# 将命名为nodes_v1和nodes_v2转化为字符串向量
nodes_v1_c <- as.character(nodes_v1$vert1_id)
nodes_v2_c <- as.character(nodes_v2$vert2_id)
# 准备两个空向量，用于存放结果
nodes_v1_n <- c()
nodes_v2_n <- c()

# 依据索引对照表，将字符串向量转化为数字向量
for (i in nodes_v1_c) nodes_v1_n <- append(nodes_v1_n,(unique_char_num[which(unique_char_num$unique_nodes==i), "index"]))
for (i in nodes_v2_c) nodes_v2_n <- append(nodes_v2_n,(unique_char_num[which(unique_char_num$unique_nodes==i), "index"]))

# 依据特征向量中心性赋予节点不同的颜色，中心性越高，节点越发紫
colorchange <- c()
for (i in eigen_centrality_v$vector){
  # 将%%替换为计算后的数值，数值最终转化为字符串形式
  color_str <- "rgba(151,%%,252,1)"
  colorchange <- append(colorchange, gsub("%%", as.character(194-round(120*i^0.8)), color_str))}

#设置visNetwork节点属性，节点大小为2*(入度+出度)^0.8
nodes <- data.frame(id=unique_char_num$index, label=unique_char_num$unique_nodes, 
                    size = 2*(indegree_v+outdegree_v)^0.8, color.background=colorchange)
#设置visNetwork边属性，有向的
edges <- data.frame(from = nodes_v1_n, to = nodes_v2_n)

#visNetwork画图，节点标签30px，箭头大小0.8
visNetwork(nodes, edges, height = "1080px", width = "100%") %>%
  visNodes(font="30px") %>%
  visEdges(arrows =list(to = list(enabled = TRUE, scaleFactor = 0.8)))



