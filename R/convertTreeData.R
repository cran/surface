convertTreeData <-
function(tree,dat){
	otree<-ape2ouch(tree,scale=F)
	onames<-as.character(as(otree,"data.frame")$labels)
	odata<-dat[as.character(as(otree,"data.frame")$labels),,drop=F]
	rownames(odata)<-otree@nodes
	return(list(otree,odata))
	}
