
setwd("/Users/cwillis/dev/uiucGSLIS/ecir-2016/analysis/reldocdist/")
library(dplyr)
library(irr)

ap <- read.csv("ap-rd-temp.csv", header=T)
ap.judged <- ap %>% subset(willis8 > -1)
ap.judged <- ap.judged[,-c(1,2)]
kappa2(ap.judged)
kappa2(ap.judged, weight="equal")

ap0 <- ap.judged
ap0$willis8[which(ap0$willis8 == 1)] <- 0
ap0$gsherma2[which(ap0$gsherma2 == 1)] <- 0
kappa2(ap0)

ap2 <- ap.judged
ap2$willis8[which(ap2$willis8 == 1)] <- 2
ap2$gsherma2[which(ap2$gsherma2 == 1)] <- 2
kappa2(ap2)

blog <- read.csv("blog-rd-temp.csv", header=T)
blog.judged <- blog %>% subset(willis8 > -1)
blog.judged <- blog.judged[,-c(1,2)]
kappa2(blog.judged)
kappa2(blog.judged, weight="equal")

blog0 <- blog.judged
blog0$willis8[which(blog0$willis8 == 1)] <- 0
blog0$gsherma2[which(blog0$gsherma2 == 1)] <- 0
kappa2(blog0)

blog2 <- blog.judged
blog2$willis8[which(blog2$willis8 == 1)] <- 2
blog2$gsherma2[which(blog2$gsherma2 == 1)] <- 2
kappa2(blog2)

mb <- read.csv("mb-rd-temp.csv", header=T)
mb.judged <- mb %>% subset(willis8 > -1)
mb.judged <- mb.judged[,-c(1,2)]
kappa2(mb.judged)
kappa2(mb.judged, weight="equal")

mb0 <- mb.judged
mb0$willis8[which(mb0$willis8 == 1)] <- 0
mb0$gsherma2[which(mb0$gsherma2 == 1)] <- 0
kappa2(mb0)

mb2 <- mb.judged
mb2$willis8[which(mb2$willis8 == 1)] <- 2
mb2$gsherma2[which(mb2$gsherma2 == 1)] <- 2
kappa2(mb2)

# Compare to DPS
setwd("/Users/cwillis/dev/uiucGSLIS/ecir-2016/analysis/qrel-acf/")

ap.reldist <- read.csv("qrels-acf-ap.out", header=T);
ap.reldist <- merge(ap, ap.reldist, by="topic")
cor(ap.reldist)

blog.reldist <- read.csv("qrels-acf-blog.out", header=T);
blog.reldist <- merge(blog, blog.reldist, by="topic")
cor(blog.reldist)

mb.reldist <- read.csv("qrels-acf-tweets.out", header=T);
mb.reldist <- merge(mb, mb.reldist, by="topic")
mb.reldist <- mb.reldist[,-c(1)]
cor(mb.reldist)

