#Load ToothGrowth data and any relevant packages
data <- ToothGrowth
attach(data)
library(lattice)
library (latticeExtra)

#Exploratory analysis
#Scatter Plots

xyplot(len ~ dose, xlab = "Dose", ylab = "Tooth Length", 
       main = "Scatterplot")

xyplot(len ~ dose | supp, xlab = "Dose", ylab = "Tooth Length", 
       main = "Tooth length by dose for a given supplement")

#Histogram with density overlay
histogram(len, type = "density", xlab = "Tooth length") + densityplot(len)

histogram(len[supp == "OJ"], type = "density", xlab = "Tooth length: OJ") +
  densityplot(len[supp == "OJ"])

histogram(len[supp == "VC"], type = "density", xlab = "Tooth length: VC") + 
  densityplot(len[supp == "VC"])

qqnorm(len)
qqline(len, probs=c(0.25,0.75))

#We will assume a normal distribution for the purposes of this analysis.

#Using alpha == .05, we therefore assume the distribution of tooth lengths is not normal.

#Data summary
#Quantiles
summary(data)

#Variance
datavarsupp <- c(var(len[supp == "OJ"]), var(len[supp == "VC"]))

datavardose <- c(var(len[dose == 0.5]), var(len[dose == 1.0]), var(len[dose == 2.0]))
datavarsupp
datavardose

#For the analysis comparing differences by supplement, we will not assume equal variances
#For the analysis comparing differences between 0.5 doses and 1.0 doses, we will assume equal variances

#Standard Deviation
sqrt(datavar)

#Hypothesis tests
#T-test comparing tooth lenth and supplement
tsupp <- t.test(len~supp, paired = FALSE, var.equal = FALSE, data = ToothGrowth)
tsupp

#T-test comparing tooth length under 0.5 and 1.0 doses
tdose.0.5.1 <- t.test(len[dose == 0.5], len[dose == 1.0], paired = FALSE,                                              var.equal = TRUE)
tdose.0.5.1

#T-test comparing tooth length under 0.5 and 2.0 doses
tdose.0.5.2 <- t.test(len[dose == 0.5], len[dose == 2.0], paired = FALSE, 
                      var.equal = FALSE)
tdose.0.5.2 

#T-test comparing tooth length under 1.0 and 2.0 doses
tdose.1.2 <- t.test(len[dose == 1.0], len[dose == 2.0], paired = FALSE, 
                      var.equal = FALSE)
tdose.1.2


