# -*- ess -*-

x = c(3,5,2,1)
pie(x)
names(x) = c("cat 1", "cat 2", "cat 3", "cat x")
pie(x)
?pie
pie(x, main="head title")
pie(x, sub="sub title")
pie(x, col=rainbow(10))
