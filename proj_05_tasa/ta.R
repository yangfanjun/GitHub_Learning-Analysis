library(ggplot2)
df<-read.csv("df8.csv", header = TRUE, stringsAsFactors=FALSE)

#create alllevels for change shape (scale_shape_manual)
df$sortedid <- 1:nrow(df) #用于x轴表示时间序列先后次序

#The function factor is used to encode a vector as a factor (the terms ‘category’ and 
# ‘enumerated type’ are also used for factors).
df$code <- factor(df$code, levels=c("IEx", "IEl", "IQE", "IQR", "IPA","GEx", "GEl-1","GEl-2","GQE", "GQR", "GPA","IML", "GML","ISP", "GSI-1","GSI-2"), 
                  labels=c("IEx", "IEl", "IQE", "IQR", "IPA","GEx", "GEl-1","GEl-2","GQE", 
                           "GQR", "GPA","IML", "GML","ISP", "GSI-1","GSI-2")) #用于y轴表示不同code

alllevels <- c(levels(df$code)) #下面会用它表示点的形状 

#using ggplot2 - ggplot to make a graph
p <- ggplot(df) + 
  geom_point(aes(x=df$sortedid, y=df$code, shape=df$code, colour=df$student)) + 
  scale_shape_manual(values=1:length(alllevels))+
  xlab("Discussion Threads") + ylab("Coding Categories")

#以下是优化graph的显示，主要针对图列的说明
p + guides(shape=FALSE) +
  guides(col=guide_legend(ncol=2,title="Students"))