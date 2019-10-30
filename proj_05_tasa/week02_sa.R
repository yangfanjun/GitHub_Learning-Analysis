library(devtools)
library(LagSeq)
library(dplyr)


# 读取数据表
all_code=read.csv("all_code.csv", header=TRUE)
# 筛选Week 02部分，选取participatory_role一列，存入week02_code，每行代表该角色向vert2回复，数据表本身已按事件发生先后排序
week02_code <- all_code %>% filter(week=="Week 02") %>% select(participatory_role)
# 将participatory_role转化为字符串向量role_v
role_v<-as.vector(week02_code$participatory_role)
# 取出role_v中不重复元素
as.character(unique(role_v))


#下面我们用1-6的数字表示这六种code类别
# case_when等同于if and else if statement
role_case<- case_when(role_v == "Starter" ~ 1,
                   role_v == "Leader" ~ 2,
                   role_v == "Influencer" ~ 3,
                   role_v == "Peripheral" ~ 4,
                   role_v == "Mediator" ~ 5,
                   role_v == "Regular" ~ 6)

# 我们使用LagSeq可以计算出过渡频率，Yule q 值和z 值
LagSeq(role_case) 
freq=as.matrix(LagSeq(role_case)$freq) #过渡频率
#write.csv(freq,file = "freq.csv") #把结果写入一个新的csv文件

# Yule q 值，即标准化度量，表示两个编码类别之间的关联强度，范围从-1到+1, 0表示没有关联
yulesq=as.matrix(LagSeq(role_case)$yulesq)
# yulesq[yulesq<0]<-0
# write.csv(yulesq, file = "yulesq.csv")

# z 值表示特定过渡的统计显著性(z 值大于1.96表示过渡序列达到了统计显著性即p < .05)
adjres=as.matrix(LagSeq(role_case)$adjres)  #z 值
adjres[adjres<1.96]<-0
# write.csv(adjres,file = "adjres.csv")

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






