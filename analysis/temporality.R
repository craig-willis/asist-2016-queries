library(irr)

setwd("/users/cwillis/dev/uiucGSLIS/ecir-2016/analysis")

# This isn't currently used for the ECIR paper
# Read the master table of topic/temporality classification
# This includes our manual classification (which, as of 10/5/2015 
# is still considered a little suspect).

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

trec678 <- d2[d2$Topic > 300 & d2$Topic <= 450,]

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



# Unified
d <- read.csv("excerpts-temporal-20151001-table-all.txt", header=T)
d0 <- d
d2 <- d

d0[which(d0[,3] == 1), 3] <- 0
d0[which(d0[,3] == 2), 3] <- 1

d2[which(d2[,3] == 2), 3] <- 1

trec678 <- d0[d0$Topic > 300 & d0$Topic <= 450,]
# Efron
kappa2(trec678[,c(3,4)], "unweighted")

# Dakka
kappa2(trec678[,c(3,5)], "unweighted") # Coder 1 
nrow(trec678[trec678$Coder == 1 & trec678$Dakka == 0,])
# 336 342 351 363 397 429 430 445
nrow(trec678[trec678$Coder == 0 & trec678$Dakka == 1,])

# try with 1 => 2
trec678 <- d2[d2$Topic > 300 & d2$Topic <= 450,]
kappa2(trec678[,c(3,5)], "unweighted") # Coder 1 
nrow(trec678[trec678$Coder == 1 & trec678$Dakka == 0,])
nrow(trec678[trec678$Coder == 0 & trec678$Dakka == 1,])

# Dakka v Efron
kappa2(trec678[,c(4,5)], "unweighted") # Coder 1 

# Dakka v DPS
tmp <- read.csv("dakka-latimes-trec678-rel-acf.csv", header=T)
tmp <- read.csv("dakka-ft-trec678-rel-acf.csv", header=T)

tmp <- data.frame(tmp$Topic, tmp$DPS, tmp$ACF)
colnames(tmp) <- c("Topic", "DPS", "ACF")
tmp <- merge(trec678, tmp, by="Topic")
tmp <- tmp[,-c(1,2,3,4,6,7)]
cor(tmp)
m <- glm(Dakka ~ DPS, tmp, family=binomial)
summary(m)
1-(m$deviance/m$null.deviance)



# Peetz
blog <- d0[d0$Topic > 800 & d0$Topic <= 1050,]
kappa2(blog[,c(3,6)], "unweighted") # Coder 1 
blog <- d2[d2$Topic > 800 & d2$Topic <= 1050,]
kappa2(blog[,c(3,6)], "unweighted") # Coder 1 

# Novelty
novelty <- d0[d0$Track == "novelty" & d0$Topic >=1 & d0$Topic <= 100,]
kappa2(novelty[,c(3,7)], "unweighted") # Coder 1 
novelty <- d2[d2$Track == "novelty" & d2$Topic >=1 & d2$Topic <= 100,]
kappa2(novelty[,c(3,7)], "unweighted") # Coder 1 



# Just me
setwd("/users/cwillis/dev/uiucGSLIS/events/dedoose")
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

# Efron & Dakka
kappa2(trec678[,c(4,5)], "unweighted")



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
