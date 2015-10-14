setwd("/users/cwillis/dev/uiucGSLIS/ecir-2016/analysis")
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5) # Cost function used for cv.glm


# For each collection, read the topic/code matrix. 
# This includes the ACF/DPS values for the true-relevant
# document distrbution.
# Use safeBinaryRegression to identify problem variables

# Dakka et al, TREC 6-8 (301-450), LATimes
d <- read.csv("dakka-latimes-trec678-rel-acf.csv", header=T)
d <- d[,3:ncol(d)]          # Remove topic and file fields
d[is.na(d)] <- 0            # Replace NA with 0
d[,1:10][d[,1:10] > 0] <- 1

library(safeBinaryRegression)
m <- glm(Temporal ~ . - ACF - DPS, d, family=binomial(link=logit))
summary(m)
m <- step(m, direction=c("both"))
summary(m)
detach("package:safeBinaryRegression", unload=TRUE)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)
# No effect for Dakka/LATimes

# Logistic model with ACF/DPS, stepwise selection, 10-fold cross-validation
m <- glm(Temporal ~ . - ExplicitDate - FutureEvent - SpecificEvent, d, family="binomial")
summary(m)
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)
# DPS (p < 0.05)
# -0.91718 + 0.39331*DPS (p<0.001) R^2=0.263

# Dakka et al, TREC 6-8 (301-450), Financial Times
d <- read.csv("dakka-ft-trec678-rel-acf.csv", header=T)
d <- d[,3:ncol(d)]
d[is.na(d)] <- 0
d[,1:10][d[,1:10] > 0] <- 1

library(safeBinaryRegression)
m <- glm(Temporal ~ . - ACF - DPS - ExplicitDate - SpecificEvent - FutureEvent, d, family="binomial")
summary(m)
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)
# No DPS
# 0.1335 + 0.8781*Place (p < 0.1) R^2 = 0.019

m <- glm(Temporal ~ . - ExplicitDate - SpecificEvent - FutureEvent, d, family=binomial(link=logit))
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)
# -0.91718 + 0.39331*DPS (p < 0.001) R^2 = 0.


# Efron & Golovchinsky, TREC 6-8 (301-450), Financial Times
d <- read.csv("efron-ft-trec678-rel-acf.csv", header=T)
d <- d[,3:ncol(d)]
d[is.na(d)] <- 0
d[,1:10][d[,1:10] > 0] <- 1

library(safeBinaryRegression)
m <- glm(Temporal ~ . - DPS - ACF - FutureEvent - PeriodicEvent - IndirectEventReference, d, family=binomial)
m <- step(m, direction=c("both"))
summary(m)
detach("package:safeBinaryRegression", unload=TRUE)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)
# -1.7648 + 1.4099*Other (p<0.05) + 2.3528*Place (p<0.001)  R^2 = 0.1814

m <- glm(Temporal ~ . - FutureEvent - PeriodicEvent - IndirectEventReference, d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)
# Efron FT
# -2.72677 + 1.78668*Other (p<0.01) + 1.96489*Place (p<0.001) + 0.16313*DPS (p<0.001) R^2=0.377

# Efron & Golovchinsky, TREC 6-8 (301-450), LA Times
d <- read.csv("efron-latimes-trec678-rel-acf.csv", header=T)
d <- d[,3:ncol(d)]
d[is.na(d)] <- 0
d[,1:10][d[,1:10] > 0] <- 1

m <- glm(Temporal ~ . - DPS - ACF - FutureEvent - PeriodicEvent, d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)
#Efron LATimes
# -1.7798 + 1.2487*Other (p<0.05) + 2.2541*Place (p<0.001) + 1.7363*Indirect R^2=0.1944

m <- glm(Temporal ~ . - FutureEvent - PeriodicEvent, d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
detach("package:safeBinaryRegression", unload=TRUE)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)
# Efron LATimes w/ DPS
# -0.5517 + 2.0907*Place (p<0.001) + 4.5274*Indriect (p <0.1) + 5.3570*ACF (p<0.10) - -0.8975*DPS (p<0.001)
# R^2 = 0.3453


# Peetz et al, Blog06-08 (900-1050), Blog 06
d <- read.csv("peetz-blog0608-rel-acf.csv", header=T)
d <- d[,3:ncol(d)]
d[is.na(d)] <- 0
d[,1:10][d[,1:10] > 0] <- 1

m <- glm(Temporal ~ . - ACF - DPS - ExplicitDate - FutureEvent - GenericEvent, d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)
# Peetz no DPS
# -0.3360+ -0.6175*Org  + 0.6717*Person + 0.9823*Period (p<0.10) + 1.6824*Specific (p<0.05)
# R^2 = 0.1268

m <- glm(Temporal ~ . - ExplicitDate - FutureEvent - GenericEvent, d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)
# Peetz with DPS
# -1.246838 + 0.796674*Period + 1.218178*Specific + 2.837967*ACF (p<0.05) + 0.002163*DPS (p<0.10)
#R^2 = 0.2236

# Novelty 03-04.  Predict whether the topic is an "Event" (1) or "Opinion" (0)
d <- read.csv("novelty-0304-rel-acf.csv", header=T)
d <- d[,3:ncol(d)]
d[is.na(d)] <- 0
d[,1:10][d[,1:10] > 0] <- 1
d[d$ACF < 0,]$ACF <- 0

library(safeBinaryRegression)
m <- glm(Temporal ~ . -ACF - DPS -FutureEvent - IndirectEventReference, d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)
# Novelty no DPS
# -3.854 + 2.170*Explicit (p<0.10) + 2.097*Other + 5.290*Specific (p<0.001)
# R^2 = 0.7014

library(safeBinaryRegression)
m <- glm(Temporal ~ . -FutureEvent - IndirectEventReference, d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)
# Novelty no DPS
# -3.637 + 2.272 Explicit (p<0.10) + 2.285*Other (p<0.10) + 6.477*Specific (p<0.001) -7.878*ACF (p<0.05)
# R^2 = 0.7367

m <- glm(Temporal ~ . - ACF - DPS - FutureEvent - ExplicitDate, d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)

library(safeBinaryRegression)
m <- glm(Temporal ~ . - FutureEvent - ExplicitDate, d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
detach("package:safeBinaryRegression", unload=TRUE)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)


# KL v KDE
d <- read.csv("kl-kde-tweets-acf.out", header=T)
d <- d[,3:ncol(d)]
d[is.na(d)] <- 0
d[,1:10][d[,1:10] > 0] <- 1
library(safeBinaryRegression)
m <- glm(Temporal ~ .  - ExplicitDate - FutureEvent, d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
detach("package:safeBinaryRegression", unload=TRUE)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)


d <- read.csv("kl-kde-novelty-0304-acf.out", header=T)
d <- d[,3:ncol(d)]
d[is.na(d)] <- 0
d[,1:10][d[,1:10] > 0] <- 1
library(safeBinaryRegression)
m <- glm(Temporal ~ .   - FutureEvent, d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
detach("package:safeBinaryRegression", unload=TRUE)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)


d <- read.csv("kl-kde-ap-acf.out", header=T)
d <- d[,3:ncol(d)]
d[is.na(d)] <- 0
d[,1:10][d[,1:10] > 0] <- 1
d <- subset(d, select=-c(PeriodicEvent, SpecificEvent, IndirectEventReference, PersonEntity, OrganizationEntity, OtherEntity))
library(safeBinaryRegression)
m <- glm(Temporal ~ .   - FutureEvent, d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
detach("package:safeBinaryRegression", unload=TRUE)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)



d <- read.csv("kl-kde-blog06-acf.out", header=T)
d <- d[,3:ncol(d)]
d[is.na(d)] <- 0
d[,1:10][d[,1:10] > 0] <- 1
library(safeBinaryRegression)
m <- glm(Temporal ~ .   - ExplicitDate - FutureEvent - GenericEvent, d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
detach("package:safeBinaryRegression", unload=TRUE)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)

# Linear model to predict diff
d <- read.csv("kl-kde-blog06-acf-raw.out", header=T)
d <- d[,3:ncol(d)]
d[is.na(d)] <- 0
d[,1:10][d[,1:10] > 0] <- 1
m <- lm(Temporal ~ . , d)
m <- step(m, direction=c("both"))
summary(m)


d <- read.csv("kl-kde-latimes-acf.out", header=T)
d <- d[,3:ncol(d)]
d[is.na(d)] <- 0
d[,1:10][d[,1:10] > 0] <- 1
library(safeBinaryRegression)
m <- glm(Temporal ~ .  - OrganizationEntity - PersonEntity - FutureEvent, d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
detach("package:safeBinaryRegression", unload=TRUE)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)

d <- read.csv("kl-kde-ft-acf.out", header=T)
d <- d[,3:ncol(d)]
d[is.na(d)] <- 0
d[,1:10][d[,1:10] > 0] <- 1
library(safeBinaryRegression)
m <- glm(Temporal ~ . - ExplicitDate - PersonEntity -FutureEvent, d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
detach("package:safeBinaryRegression", unload=TRUE)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)


d <- read.csv("kl-kde-tweets-acf.out", header=T)
d <- d[,3:ncol(d)]
d[is.na(d)] <- 0
d[,1:10][d[,1:10] > 0] <- 1
library(safeBinaryRegression)
m <- glm(Temporal ~ ., d, family="binomial")
m <- glm(Temporal ~ . - ExplicitDate - FutureEvent - GenericEvent, d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
detach("package:safeBinaryRegression", unload=TRUE)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)
# 0.01 = ~ ACF ** cv=0.05 1=71/111
# 0.02 = ~ ACF * cv=0.05  1=33/111
# 0.03 = ~ PeriodicEvent + ACF ** cv=0.12 1=19/111
# 0.04 = ~ PeriodicEvent + ACF *** cv=0.154 1=17/111

# Linear model to predict diff
d <- read.csv("kl-kde-tweets-acf-raw.out", header=T)
d <- d[,3:ncol(d)]
d[is.na(d)] <- 0
d[,1:10][d[,1:10] > 0] <- 1
m <- lm(Temporal ~ . , d)
m <- step(m, direction=c("both"))
summary(m)




d <- read.csv("kl-mix-tweets-acf.out", header=T)
d <- d[,3:ncol(d)]
d[is.na(d)] <- 0
d[,1:10][d[,1:10] > 0] <- 1
library(safeBinaryRegression)
m <- glm(Temporal ~ ., d, family="binomial")
m <- glm(Temporal ~ . - ExplicitDate - FutureEvent, d, family="binomial")
m <- step(m, direction=c("both"))
m <- glm(Temporal ~ PeriodicEvent*ACF*OtherEntity, d, family="binomial")
summary(m)
detach("package:safeBinaryRegression", unload=TRUE)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)


library(MASS)
x <- data.matrix(d[,1:12])
y <- d$Temporal
m <- glmnet(x, y, family="binomial")
cv <- cv.glmnet(x, y, family="binomial", type.measure = "auc")
coef(cv)
plot(cv)
coef(cv, s="lambda.min")
coef(cv, s="lambda.1se")


