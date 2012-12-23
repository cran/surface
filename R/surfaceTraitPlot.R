surfaceTraitPlot<-
function(dat,hansenfit,whattraits=c(1,2),cols=NULL, convcol=TRUE,pchs=c(21,21),cex.opt=2.5,...){

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

	if(pchs[1]%in%(21:25)){
		datbg<-cols[as.numeric(factor(otree2[,5]))]
		datcols<-rep("black",length(datbg))
	}else{
		datcols<-cols[as.numeric(factor(otree2[,5]))]
		datbg<-rep("black",length(datcols))
	}
	if(pchs[2]%in%(21:25)){
		optcols<-rep("black",length(cols))
		optbg<-cols
	}else{
		optcols<-cols
		optbg<-rep("black",length(cols))
	}
	optima<-sapply(hansenfit$fit,function(x)summary(x)$optima[[1]])
	x<-whattraits[1];y<-whattraits[2]
	plot(NA,xlim=range(c(dat[,x],optima[,x])),ylim=range(c(dat[,y],optima[,y])),xlab=names(dat)[x],ylab=names(dat)[y],...)
	points(optima[,x],optima[,y],pch=pchs[2],col=optcols,bg=optbg,cex=cex.opt)
	points(dat[,x],dat[,y],col=datcols,bg=datbg,pch=pchs[1],...)
}