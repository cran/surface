runSurface<-
function(tree,dat,exclude=0,aic_threshold=0,max_steps=NULL, verbose=FALSE, plotaic = FALSE,error_skip=FALSE,only_best=FALSE,sample_shifts=FALSE, sample_threshold=2){

	tree<-nameNodes(tree)
	olist<-convertTreeData(tree,dat)
	otree<-olist[[1]];odata<-olist[[2]]

	fwd<-surfaceForward(otree,odata,exclude=exclude,max_steps=max_steps, verbose=verbose,plotaic=plotaic,error_skip=error_skip,sample_shifts=sample_shifts, sample_threshold=sample_threshold)
	bwd<-surfaceBackward(otree,odata,fwd[[length(fwd)]],verbose=verbose, plotaic=plotaic, error_skip=error_skip,only_best=only_best, sample_shifts=sample_shifts,sample_threshold=sample_threshold)
	
list(fwd=fwd,bwd=bwd)
}
