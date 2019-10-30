library(stats)
library(dplyr)
library(sqldf)

# 读取数据表
all_code=read.csv("all_code.csv", header=TRUE)
# 筛选Week 02部分
week02_code <- all_code %>% filter(week=="Week 02")

# 对SKI MKI DKI SKC MKC DKC进行分类汇总
SKI_sum <- sqldf("select vert1_id, sum(SKI) as SKI_sum from week02_code group by vert1_id")
MKI_sum <- sqldf("select vert1_id, sum(MKI) as MKI_sum from week02_code group by vert1_id")
DKI_sum <- sqldf("select vert1_id, sum(DKI) as DKI_sum from week02_code group by vert1_id")
SKC_sum <- sqldf("select vert1_id, sum(SKC) as SKC_sum from week02_code group by vert1_id")
MKC_sum <- sqldf("select vert1_id, sum(MKC) as MKC_sum from week02_code group by vert1_id")
DKC_sum <- sqldf("select vert1_id, sum(DKC) as DKC_sum from week02_code group by vert1_id")

# 转换为数字向量，对向量命名，画出柱状图
# SKI
SKI_sum_v = as.numeric(SKI_sum$SKI_sum)
names(SKI_sum_v) <- SKI_sum$vert1_id
barplot(SKI_sum_v, main="SKI Bar Plot", xlab="Identity", ylab="SKI")
# MKI
MKI_sum_v = as.numeric(MKI_sum$MKI_sum)
names(MKI_sum_v) <- MKI_sum$vert1_id
barplot(MKI_sum_v, main="MKI Bar Plot", xlab="Identity", ylab="MKI")
# DKI
DKI_sum_v = as.numeric(DKI_sum$DKI_sum)
names(DKI_sum_v) <- DKI_sum$vert1_id
barplot(DKI_sum_v, main="DKI Bar Plot", xlab="Identity", ylab="DKI")
# SKC
SKC_sum_v = as.numeric(SKC_sum$SKC_sum)
names(SKC_sum_v) <- SKC_sum$vert1_id
barplot(SKC_sum_v, main="SKC Bar Plot", xlab="Identity", ylab="SKC")
# MKC
MKC_sum_v = as.numeric(MKC_sum$MKC_sum)
names(MKC_sum_v) <- MKC_sum$vert1_id
barplot(MKC_sum_v, main="MKC Bar Plot", xlab="Identity", ylab="MKC")
# DKC
DKC_sum_v = as.numeric(DKC_sum$DKC_sum)
names(DKC_sum_v) <- DKC_sum$vert1_id
barplot(DKC_sum_v, main="DKC Bar Plot", xlab="Identity", ylab="DKC")


# student to instructor including codes
# sum = SKI*1+MKI*2+DKI*3
fsi_code <- all_code %>% filter(week=="Week 02")  %>%
  select(vert1_id,vert2_id, SKI, MKI, DKI) %>% mutate(si_codes = SKI*1+MKI*2+DKI*3)%>%
  group_by(vert1_id)%>% summarise(si_codes=sum(si_codes))
si_codes_v = as.numeric(fsi_code$si_codes)
names(si_codes_v) <- fsi_code$vert1_id
barplot(si_codes_v, main="Sum Bar Plot", xlab="Identity", ylab="Sum")

# 描述性统计分析
# the number of values (nbr.val), the number of null values (nbr.null), the number of missing values (nbr.na)
# the minimal value (min), the maximal value (max), the range (range, that is, max-min),  sum of all non-missing values (sum)
# the median (median), the mean (mean), the standard error on the mean (SE.mean), the confidence interval of the mean (CI.mean) at the p level
# the variance (var), the standard deviation (std.dev) and the variation coefficient (coef.var)
library(pastecs)
stat.desc(fsi_code$si_codes, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)




