
install.packages("HSAUR2")
install.packages("MVA")
library(MVA)


path="/path/"
fname="IceBreakupCanada66-95_fill.csv"
mydata=read.csv(paste(path, fname, sep=""))
rownames(mydata)=mydata[,1]

mydata.pc=prcomp(mydata[, -1], center=TRUE, scale=FALSE)
summary(mydata.pc)
E=mydata.pc$rotation
ev=mydata.pc$sdev^2
print(data.frame(eigenvalues=ev, proportion=ev/sum(ev), row.names=paste("PC", 1:30, sep="")))



screeplot(mydata.pc,type='line')



f2name="BreakupLakeCoordinates.csv"
mydata2=read.csv(paste(path, f2name, sep=""))
rownames(mydata2)=mydata2[,1]

M=cbind(E[,c(1, 2, 3)], mydata2[,-1])

plotfile=paste(path, "scatterplot.pdf", sep="")
pdf(file=plotfile)
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}

pairs(M, upper.panel = panel.cor)
graphics.off()



pc.use <- 7 
trunc <- mydata.pc$x[,1:pc.use] %*% t(mydata.pc$rotation[,1:pc.use])
trunc <- scale(trunc, center = FALSE , scale=1/mydata.pc$scale)
trunc <- scale(trunc, center = -1 * mydata.pc$center, scale=FALSE)
plot(mydata[, 1], mydata[, 2], type="l", col="black", main="Original and Reconstructed", xlab="year", ylab="days")
lines(mydata[, 1], trunc[, 1], type="l", col="red")




