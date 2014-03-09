#################################
# Processes distance-related measures.
#################################
measures[["distance-average"]] <- list(
	type=numeric(),
	bounds=c(1,NA),
	foo=function(graph) 
	{	average.path.length(graph=graph, directed=FALSE, unconnected=TRUE)
	}
)
#measures[["girth"]] <- list(		# cycle of maximal length
#	type=integer(),
#	bounds=c(1,NA),
#	foo=function(graph) 
#	{	girth(graph=graph, circle=FALSE)$girth
#	}
#)
