options(width=80)

input <- function(inputfile) {
hnrnp.cnts <<- read.csv(inputfile)
}

run <- function() {}

output <- function(outputfile) {
cnts=hnrnp.cnts
dim(cnts)
sel.rn=rowSums(cnts) != 0
cnts=cnts[sel.rn,]
dim(cnts)
libsizes=colSums(cnts)
size.factor=libsizes/exp(mean(log(libsizes)))
cnts.norm=t(t(cnts)/size.factor)
range(cnts.norm)
cnts.norm=log2(cnts.norm+8)
range(cnts.norm)

#optional MA plot
pdf(outputfile, width=8, height=10)
op=par(lwd=2, cex.axis=1.5, cex.lab=1.5, mfrow=c(2,1))
plot((cnts.norm[,6]+cnts.norm[,5])/2, (cnts.norm[,6]-cnts.norm[,5]),
main="(a) Control vs Control", xlab="mean", ylab="change",
ylim=c(-5,5), xlim=c(0,20), lwd=1)
abline(h=0, lwd=2, col="red", lty="dashed")
plot((cnts.norm[,1]+cnts.norm[,5])/2, (cnts.norm[,1]-cnts.norm[,5]),
main="(b) Knockdown vs Control", xlab="mean", ylab="change",
ylim=c(-5,5), xlim=c(0,20), lwd=1)
abline(h=0, lwd=2, col="red", lty="dashed")
write.csv(cnts.norm, paste(outputfile, "norm", "csv", sep="."))
saveRDS(cnts, paste(outputfile, "cnts", "rds", sep="."))
}
