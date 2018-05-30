vr_count <- read_csv("vr_count.csv")
head(vr_count)

qplot(vr_count$count_vr, geom="histogram", binwidth = 0.5,main = "Histogram for VR visits in FY1617", 
      xlab = "Number of VR visits") 

# library(ggplot2)
# ggplot(data=vr_count, aes(vr_count$count_vr)) + geom_histogram()

mean(vr_count$count_vr)
var(vr_count$count_vr) #sign of some over dispersion... not valid poisson assumption


fit.poisson=glm(count_vr~as.factor(defer_flag)+age+offset(log(stay_mckc_days)),family=quasipoisson(link='log'),data=vr_count)

summary(fit.poisson)
exp(0.0889948)
exp(-0.0043891)

p<-predict(fit.poisson,type='response')
plot(p)

qplot(defer_flag,vr_count$rate,data =vr_count,colour =factor(defer_flag))
