
setwd("/Users/cwillis/dev/uiucGSLIS/ecir-2016/analysis/reldocdist/")
library(dplyr)
library(irr)

ap <- read.csv("ap-rd-temp.out", header=T)
ap <- ap[,-c(1,2)]
ap.judged <- ap %>% subset(willis8 > -1)

kappa2(ap.judged)

ap0 <- ap.judged
ap0$willis8[which(ap0$willis8 == 1)] <- 0
ap0$gsherma2[which(ap0$gsherma2 == 1)] <- 0
kappa2(data.frame(ap0$willis8, ap0$gsherma2))

ap2 <- ap.judged
ap2$willis8[which(ap2$willis8 == 1)] <- 2
ap2$gsherma2[which(ap2$gsherma2 == 1)] <- 2
kappa2(data.frame(ap2$willis8, ap2$gsherma2))


blog <- read.csv("blog-rd-temp.out", header=T)
blog <- blog[,-c(1,2)]
blog.judged <- blog %>% subset(willis8 > -1)
kappa2(blog.judged)

blog0 <- blog.judged
blog0$willis8[which(blog0$willis8 == 1)] <- 0
blog0$gsherma2[which(blog0$gsherma2 == 1)] <- 0
kappa2(data.frame(blog0$willis8, blog0$gsherma2))

blog2 <- blog.judged
blog2$willis8[which(blog2$willis8 == 1)] <- 2
blog2$gsherma2[which(blog2$gsherma2 == 1)] <- 2
kappa2(data.frame(blog2$willis8, blog2$gsherma2))

mb <- read.csv("mb-rd-temp.out", header=T)
mb <- mb[,-c(1,2)]
mb.judged <- mb %>% subset(willis8 > -1)
kappa2(mb.judged)

mb0 <- mb.judged
mb0$willis8[which(mb0$willis8 == 1)] <- 0
mb0$gsherma2[which(mb0$gsherma2 == 1)] <- 0
kappa2(data.frame(mb0$willis8, mb0$gsherma2))

mb2 <- mb.judged
mb2$willis8[which(mb2$willis8 == 1)] <- 2
mb2$gsherma2[which(mb2$gsherma2 == 1)] <- 2
kappa2(data.frame(mb2$willis8, mb2$gsherma2))



