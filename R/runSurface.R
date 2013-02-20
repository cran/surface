runSurface<-
function(tree,dat,exclude=0,aic_threshold=0,max_steps=NULL, verbose=FALSE, plotaic = FALSE,error_skip=FALSE,only_best=FALSE,sample_shifts=FALSE, sample_threshold=2){

if(class(tree)!="phylo") stop("'tree' must by a 'phylo' formatted phylogenetic tree")

if(only_best==FALSE) if(!require(igraph)) stop("The 'igraph' package must be loaded to use the option 'only_best=FALSE'")

	tree<-nameNodes(tree)
	olist<-convertTreeData(tree,dat)
	otree<-olist[[1]];odata<-olist[[2]]

	fwd<-surfaceForward(otree, odata, exclude=exclude, aic_threshold=aic_threshold, max_steps=max_steps, verbose=verbose, plotaic=plotaic, error_skip=error_skip, sample_shifts=sample_shifts, sample_threshold=sample_threshold)
	bwd<-surfaceBackward(otree,odata,fwd[[length(fwd)]], aic_threshold=aic_threshold, verbose=verbose, plotaic=plotaic, error_skip=error_skip, only_best=only_best, sample_shifts=sample_shifts, sample_threshold=sample_threshold)
	
list(fwd=fwd,bwd=bwd)
}
