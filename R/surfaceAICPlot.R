surfaceAICPlot<-
function(fwd, bwd, ...){
	aics<-data.frame(k=as.numeric(c(sapply(fwd,function(x)x$n_regimes[2]),sapply(bwd,function(x)x$n_regimes[2])[-1])),AIC=as.numeric(c(sapply(fwd,function(x)x$aic), sapply(bwd,function(x)x$aic)[-1])))
	plot(aics[,1],aics[,2],type="n",xlab="Number of Regimes", ylab=expression(paste("AIC"["c"])), ...)
	points(aics[,1],aics[,2],type="l", ...)
	points(aics[,1],aics[,2], pch=21, bg="black", ...)
}
