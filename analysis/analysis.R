setwd("/users/cwillis/dev/uiucGSLIS/ecir-2016/analysis")
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)


# Temporality
library(irr)

d <- read.csv("excerpts-temporal-20151001-table.txt", header=T)
d0 <- d
d2 <- d

d0[which(d0[,3] == 1), 3] <- 0
d0[which(d0[,4] == 1), 4] <- 0
d0[which(d0[,3] == 2), 3] <- 1
d0[which(d0[,4] == 2), 4] <- 1

d2[which(d2[,3] == 2), 3] <- 1
d2[which(d2[,4] == 2), 4] <- 1

kappa2(d[,c(3,4)], "unweighted")
kappa2(d0[,c(3,4)], "unweighted")
kappa2(d2[,c(3,4)], "unweighted")

trec678 <- d0[d0$Topic > 300 & d0$Topic <= 450,]
blog <- d0[d0$Topic > 800 & d0$Topic <= 1050,]
novelty <- d0[d0$Track == "novelty" & d0$Topic >=1 & d0$Topic <= 100,]

# Efron
kappa2(trec678[,c(3,5)], "unweighted") # Coder 1
kappa2(trec678[,c(4,5)], "unweighted") # Coder 2

# Dakka
kappa2(trec678[,c(3,6)], "unweighted") # Coder 1 
kappa2(trec678[,c(4,6)], "unweighted") # Coder 2

# Peetz
kappa2(blog[,c(3,7)], "unweighted") # Coder 1 
kappa2(blog[,c(4,7)], "unweighted") # Coder 2

# Novelty
kappa2(novelty[,c(3,8)], "unweighted") # Coder 1 
kappa2(novelty[,c(4,8)], "unweighted") # Coder 2



# Just me
d <- read.csv("excerpts-temporal-20151001-table-willis8.txt", header=T)
d0 <- d
d2 <- d

d0[which(d0[,3] == 1), 3] <- 0
d0[which(d0[,3] == 2), 3] <- 1

d2[which(d2[,3] == 2), 3] <- 1

trec678 <- d0[d0$Topic > 300 & d0$Topic <= 450,]
blog <- d0[d0$Topic > 800 & d0$Topic <= 1050,]
novelty <- d0[d0$Track == "novelty" & d0$Topic >=1 & d0$Topic <= 100,]

# Efron
kappa2(trec678[,c(3,4)], "unweighted")

# Dakka
kappa2(trec678[,c(3,5)], "unweighted")

# Peetz
kappa2(blog[,c(3,6)], "unweighted")

# Novelty
kappa2(novelty[,c(3,7)], "unweighted")

efron# Summaries
d <- read.csv("all-re-acf.csv", header=T)
d <- d[,3:ncol(d)]          # Remove topic and file fields
d[is.na(d)] <- 0            # Replace NA with 0
d[,1:10][d[,1:10] > 0] <- 1
m <- glm(Temporal ~ . - FutureEvent - SpecificEvent, d, family=binomial(link=logit))
summary(m)
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)



# Dakka et al, TREC 6-8 (301-450), LATimes
d <- read.csv("dakka-latimes-trec678-rel-acf.csv", header=T)
d <- d[,3:ncol(d)]          # Remove topic and file fields
d[is.na(d)] <- 0            # Replace NA with 0
d[,1:10][d[,1:10] > 0] <- 1
m <- glm(Temporal ~ . - ACF - DPS - ExplicitDate - FutureEvent - PeriodicEvent - SpecificEvent, d, family=binomial(link=logit))
summary(m)
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)

# Logistic model with ACF/DPS, stepwise selection, 10-fold cross-validation
m <- glm(Temporal ~ . - ExplicitDate - OrganizationEntity - OtherEntity - 
           PersonEntity - IndirectEventReference - PeriodicEvent - SpecificEvent, d, family="binomial")
summary(m)
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)




# Dakka et al, TREC 6-8 (301-450), Financial Times
d <- read.csv("dakka-ft-trec678-rel-acf.csv", header=T)
d <- d[,3:ncol(d)]
d[is.na(d)] <- 0
d[,1:10][d[,1:10] > 0] <- 1


# Logistic model without ACF/DPS, stepwise selection, 10-fold cross-validation
m <- glm(Temporal ~ . - ACF - DPS - ExplicitDate - SpecificEvent, d, family="binomial")
summary(m)
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)


# Logistic model with ACF/DPS, stepwise selection, 10-fold cross-validation
m <- glm(Temporal ~ . - ExplicitDate - SpecificEvent, d, family=binomial(link=logit))
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)



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


m <- glm(Temporal ~ PlaceEntity, d, family=binomial)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)

m <- glm(Temporal ~ . - FutureEvent - PeriodicEvent - IndirectEventReference, d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)


# Efron & Golovchinsky, TREC 6-8 (301-450), LA Times
d <- read.csv("efron-latimes-trec678-rel-acf.csv", header=T)
d <- d[,3:ncol(d)]
d[is.na(d)] <- 0
d[,1:10][d[,1:10] > 0] <- 1

m <- glm(Temporal ~ . - DPS - ACF, d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)


m <- glm(Temporal ~ ., d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)



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


m <- glm(Temporal ~ . - ExplicitDate - FutureEvent - GenericEvent, d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)


# Novelty 03-04.  Predict whether the topic is an "Event" (1) or "Opinion" (0)
d <- read.csv("novelty-0304-rel-acf.csv", header=T)
d <- d[,3:ncol(d)]
d[is.na(d)] <- 0
d[,1:10][d[,1:10] > 0] <- 1
d[d$ACF < 0,]$ACF <- 0

library(safeBinaryRegression)
m <- glm(Temporal ~ . -FutureEvent - IndirectEventReference, d, family="binomial")
m <- step(m, direction=c("both"))
summary(m)
cv.glm(d, m, cost, K=10)$delta
1-(m$deviance/m$null.deviance)


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


