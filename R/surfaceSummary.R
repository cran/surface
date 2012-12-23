surfaceSummary <-
function(obj){
	k<-length(obj)
	aics<-sapply(obj,function(x)x$aic);names(aics)<-1:k
	obj<-obj[[k]]
	shifts<-obj$savedshifts
	n_regimes<-round(obj$n_regimes,2)
	obj<-obj$fit
	alpha<-sapply(obj,function(x)summary(x)$alpha)
	sigma_squared<-sapply(obj,function(x)summary(x)$sigma.squared)
	theta<-sapply(obj,function(x)summary(x)$optima[[1]])

	list(n_steps=k,aics=aics,shifts=shifts,n_regimes=n_regimes, alpha=alpha,sigma_squared=sigma_squared,theta=theta)
	}
