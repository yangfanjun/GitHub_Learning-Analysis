require(devtools)
library(devtools)
install_github('meefen/LagSeq')
library(LagSeq)

sa_data <-read.csv("sa_data.csv")

sa_data<-as.vector(sa_data$code)

library(dplyr)

#下面我们用1-6的数字表示这六种code类别
# case_when等同于if and else if statement
sa_data<- case_when(sa_data == "SKI" ~ 1,
                    sa_data == "MKI" ~ 2,
                    sa_data == "DKI" ~ 3,
                    sa_data == "SKC" ~ 4,
                    sa_data == "MKC" ~ 5,
                    sa_data == "DKC" ~ 6)

LagSeq(sa_data) # 我们使用LagSeq可以计算出过渡频率，Yule q 值和z 值
freq=as.matrix(LagSeq(sa_data)$freq) #过渡频率
write.csv(freq,file = "freq.csv") #把结果写入一个新的csv文件

yulesq=as.matrix(LagSeq(sa_data)$yulesq) #Yule q 值
yulesq[yulesq<0]<-0
write.csv(yulesq, file = "yulesq.csv")

adjres=as.matrix(LagSeq(sa_data)$adjres)  #z 值
adjres[adjres<1.96]<-0
write.csv(adjres,file = "adjres.csv")

# construct network freq
library(sna)

gplot(freq,gmode="digraph", displaylabels=TRUE,label.cex=0.8,vertex.col="darkolivegreen")

overallnet=network(freq)

#calculate in/out degree
id<- sna::degree(overallnet,gmode="digraph",cmode="indegree")
od<- sna::degree(overallnet,gmode="digraph",cmode="outdegree")
id
od
id+od

gplot(overallnet, vertex.cex=(id+od)^2/15, gmode="graph",
      boxed.labels=FALSE,label.cex=0.7, label.pos=5, label.col="grey17",
      vertex.col=rgb((id+od)/max(id+od),0,(id+od)/max(id+od)),edge.col="grey88",
      label=network.vertex.names(overallnet),edge.lwd=freq/2,mode = "fruchtermanreingold")






