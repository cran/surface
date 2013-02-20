pmcSurface<-
function(model1,model2,otree,odata,nboot=20,prob=0.95){

	if(!require(pmc)) stop("'pmc' package must be installed to run 'pmcSurface'")

	alphas1<-sapply(model1$fit,function(x)as.numeric(sqrt(summary(x)$alpha)))
	alphas2<-sapply(model2$fit,function(x)as.numeric(sqrt(summary(x)$alpha)))
	sigmas1<-sapply(model1$fit,function(x)as.numeric(sqrt(summary(x)$sigma)))
	sigmas2<-sapply(model2$fit,function(x)as.numeric(sqrt(summary(x)$sigma)))
	r1<-repaint(otree,regshifts=model1$savedshifts)
	r2<-repaint(otree,regshifts=model2$savedshifts)
	
	nt<-dim(odata)[2]
	bootlist<-list()
	for(i in 1:nt){
	print(i,quote=F)
	odat<-odata[,i];names(odat)<-otree@nodes
	bootlist[[i]]<-pmc(otree,odat,modelA="hansen",modelB="hansen",optionsA= list(regimes=r1, sqrt.alpha=alphas1[i],sigma=sigmas1[i],fit=F), optionsB= list(regimes=r2, sqrt.alpha=alphas2[i],sigma=sigmas2[i],fit=F),nboot=nboot)
	}

	nullL<-sapply(bootlist,function(x)x$null)
	testL<-sapply(bootlist,function(x)x$test)
	obsL<-sapply(bootlist,function(x)x$lr)

	critval<-quantile(apply(nullL,1,sum),prob,type=1)
	pval<-sum(c(sum(obsL),apply(nullL,1,sum))>=sum(obsL))/(nboot+1)
	power<-sum(c(sum(obsL),apply(testL,1,sum))>critval)/(nboot+1)

	addL<-list(null=apply(nullL,1,sum),test=apply(testL,1,sum),lr=sum(obsL));class(addL)<-c("list","pmc")
	
	return(list(bootlist=bootlist,pmc=addL,nullL=nullL,testL=testL,obsL=obsL,critval=critval,pval=pval,power=power))
}