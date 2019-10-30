library(stats)

all_code=read.csv("all_code.csv", header=TRUE)

# correlations between SKI and MKI 
fit01 <- aov(SKI~MKI, data=all_code)
summary(fit01)

fit02 <- aov(SKI~week, data=all_code)
summary(fit02)

aov_SKI = aov(SKI~week * is_reply * participatory_role, data=all_code)  
summary(aov_SKI)

fit_m = manova(cbind(SKI,MKI,DKI) ~ week * is_reply, data=all_code)
summary(fit_m)

