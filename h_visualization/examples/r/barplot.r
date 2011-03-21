# -*- ess -*-

#univariate


x = c(3,5,2,1)
barplot(x)

names(x) = c("cat 1", "cat 2", "cat 3", "cat x")


#bivariate

y=matrix(c(3, 5, 2 ,1), nrow=2)


#* @Meta                           27  -------> (2.6%)
#stat: L:63.0%, F:18.5%, O:18.5%
#* @PastFuture                     47  -------> (4.5%)
#stat: L:40.4%, F:42.6%, O:17.0%

catos= c("L","F","O")
meta = c(63.0, 18.5, 18.5)
names(meta) = catos
pastfuture = c(40.4, 42.6, 17.0)
names(pastfuture) = catos

composed = data.frame(meta, pastfuture)
barplot(as.matrix(composed),beside=TRUE,col=1:3, xlab="OS", ylab="%",ylim=c(0,70))
legend("topright", catos, cex=0.6, bty="n", fill=1:3);



barplot(as.matrix(topcat_os),beside=TRUE,col=1:3, xlab="OS", ylab="%",ylim=c(0,70))
