#usage:
# R CMD BATCH postscript.r; gv /tmp/xxx.ps

postscript("/tmp/xxx.ps"
#           ,width = 6.0,
           ,height = 3.0
           )
           #horizontal = FALSE, onefile = FALSE, paper = "special")



par(mfrow=c(1, 4))

par(mai=c(0, 0.2, 0.2, 0.2))

x9 = c(6, 3, 95, 21, 11, 45, 1, 28)
names(x9) = c("@Meta", "@PastFuture", "@Explanation", "@Type", "@Interface", "@Relationship", "@Extra", "@Other")

pie(x9, col=NULL, main="/")
x10 = c(3, 1, 36, 3, 1, 15, 0, 11)
names(x10) = c("@Meta", "@PastFuture", "@Explanation", "@Type", "@Interface", "@Relationship", "@Extra", "@Other")

pie(x10, col=NULL, main="//dir1:linux")
x11 = c(2, 1, 32, 12, 5, 6, 1, 11)
names(x11) = c("@Meta", "@PastFuture", "@Explanation", "@Type", "@Interface", "@Relationship", "@Extra", "@Other")

pie(x11, col=NULL, main="//dir1:freebsd")
x12 = c(1, 1, 27, 6, 5, 24, 0, 6)
names(x12) = c("@Meta", "@PastFuture", "@Explanation", "@Type", "@Interface", "@Relationship", "@Extra", "@Other")

pie(x12, col=NULL, main="//dir1:opensolaris")

par("din","fin","pin","mai")
$din
$fin
$pin
$mai
