
setwd("/users/cwillis/tmp")

par(mfrow=c(1,1))

d1 <- read.csv("301.out", header=F)
d4 <- read.csv("301.la", header=F)
d3 <- read.csv("301.ft", header=F)
d2 <- read.csv("301.fbis", header=F)

pdf("/Users/cwillis/dev/uiucGSLIS/ecir-2016/analysis/301/301-trec8.pdf")
plot(density(d1$V4), xlim=c(0, 2000), xlab="", ylab="", yaxt="n", main="")
dev.off()

pdf("/Users/cwillis/dev/uiucGSLIS/ecir-2016/analysis/301/301-la.pdf")
plot(density(d4$V4), xlim=c(0, 2000), col="orange", ylim=c(0, 0.02), xlab="", ylab="", yaxt="n", main="" )
dev.off()

pdf("/Users/cwillis/dev/uiucGSLIS/ecir-2016/analysis/301/301-ft.pdf")
plot(density(d3$V4), xlim=c(0, 2000), col="red", ylim=c(0, 0.01), xlab="", ylab="", yaxt="n", main="")
dev.off()

pdf("/Users/cwillis/dev/uiucGSLIS/ecir-2016/analysis/301/301-fbis.pdf")
plot(density(d2$V4), xlim=c(0, 2000), col="blue", xlab="", ylab="", yaxt="n", main="")
dev.off()

