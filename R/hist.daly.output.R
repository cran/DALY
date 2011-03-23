hist.daly.output <-
function(x, ...){
hist(x$DALY, breaks=20, col="#dfdfdf", freq=FALSE, xlab="DALY", ylab="density", main=x$name)
lines(density(x$DALY), col="red", lwd=2)
abline(v=quantile(x$DALY, c(.025,.975)), lwd=2)
}

