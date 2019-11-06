setwd("C:/Users/ryanl/OneDrive/Documents/Python Scripts")
getwd()

sp<- read.csv(file="spfinal.csv", header=TRUE, sep=",")

sp

mylm = lm(AnnReturn.1 ~ ExcessMkR, data = sp)

summary(mylm)

xs_stock <- sp["AnnReturn.1"]-sp[""]

AnnReturn <- sp["AnnReturn"]
AnnReturn.1 <- sp["AnnReturn.1"]

AnnReturn
AnnReturn.1

covar = cov(AnnReturn,AnnReturn.1)

vari = var(AnnReturn)

beta = (covar/vari)

###############
## New Feature
> xx = sp$AnnReturn.1 - sp$US1T/100.0
> length(xx)
[1] 246
> xx[1:5]
[1] -0.0314676644 -0.0418982021 -0.0334337685 -0.0390032656 -0.0008406127
> fit.xx = lm(xx ~ sp$AnnReturn.1)
> summary(fit.xx)

###############

x = c(2,4,6,8,10)
y = c(1,2,3,4,5)

mylm1 <- lm(x~y)
mylm1
summary(mylm1)
