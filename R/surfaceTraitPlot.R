surfaceTraitPlot<-function(dat,hansenfit,whattraits=c(1,2),cols=NULL, convcol=TRUE,pch.pt=21,pch.opt=21,cex.opt=2.5,optellipses=FALSE, ellipsescale=1,y.lim=NULL,x.lim=NULL,y.lab=NULL,x.lab=NULL,...){

	fit<-hansenfit$fit
	if(length(fit)>1|class(fit)=="list")fit<-fit[[1]]
	otree<-as(fit,"data.frame")
	otree<-data.frame(otree,shifts=rep(NA,length(otree$nodes)))
	otree$shifts[match(names(hansenfit$savedshifts),otree$nodes)]<-1:length(hansenfit$savedshifts)
	ntip<-(dim(otree)[1]+1)/2;nnode<-ntip-1
	otree2<-otree[match(rownames(dat),otree$labels),]

if(length(cols)==1)cols<-rep(cols,length(unique(hansenfit$savedshifts)))
if(is.null(cols)){
		xx<-summary(factor(hansenfit$savedshifts))
	if(convcol){
		cols<-character(length(xx))
		cols[xx>1]<-rainbow(sum(xx>1))
		cols[xx==1]<-c("black",grey(seq(0.7,0.3,length.out=sum(xx==1)-1)))
	}else{
		cols<-c("black",rainbow(length(xx)-1))
	}	}

	if(pch.pt[1]%in%(21:25)){
		datbg<-cols[as.numeric(factor(otree2[,5]))]
		datcols<-rep("black",length(datbg))
	}else{
		datcols<-cols[as.numeric(factor(otree2[,5]))]
		datbg<-rep("black",length(datcols))
	}
	if(pch.opt%in%(21:25)){
		optcols<-rep("black",length(cols))
		optbg<-cols
	}else{
		optcols<-cols
		optbg<-rep("black",length(cols))
	}
	optima<-sapply(hansenfit$fit,function(x)summary(x)$optima[[1]])
	if(optellipses){
		alphas<-sapply(hansenfit$fit,function(x)summary(x)$alpha)
		sigsqs<-sapply(hansenfit$fit,function(x)summary(x)$sigma.squared)
		widths<-0.5*sqrt((sigsqs/(2*alphas)))#width is one half of ellipse
		ellipsescale<-sort(ellipsescale,decreasing=TRUE)
	}else{
		widths<-rep(0,dim(dat)[2])
	}

	x<-whattraits[1];y<-whattraits[2]
	if(is.null(y.lim))
		y.lim<-range(c(dat[,y],optima[,y]+widths[y]*ellipsescale[1],optima[,y]-widths[y]*ellipsescale[1]))
	if(is.null(x.lim))
		x.lim<-range(c(dat[,x],optima[,x]+widths[x]*ellipsescale[1],optima[,x]-widths[x]*ellipsescale[1]))
	if(is.null(x.lab)) x.lab<-names(dat)[x]
	if(is.null(y.lab)) y.lab<-names(dat)[y]
	plot(NA,xlim=x.lim,ylim=y.lim,xlab=x.lab,ylab=y.lab,...)
	if(optellipses){
		ae<-seq(0,2*pi,length=100)
		for(i in 1:dim(optima)[1]){
			for(j in 1:length(ellipsescale)){
			xe<-optima[i,x]+ellipsescale[j]*widths[x]*cos(ae)
			ye<-optima[i,y]+ellipsescale[j]*widths[y]*sin(ae)
			polygon(x=xe,y=ye,col=cols[i])
	}}	}else{
	points(optima[,x],optima[,y],pch=pch.opt,col=optcols,bg=optbg,cex=cex.opt)
		}
	points(dat[,x],dat[,y],col=datcols,bg=datbg,pch=pch.pt,...)
}