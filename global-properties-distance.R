#################################
# Processes distance-related measures.
#################################
properties[["distance-average"]] <- list(
	type=numeric(),
	bounds=c(1,NA),
	foo=function(graph) 
	{	average.path.length(graph=graph, directed=FALSE, unconnected=TRUE)
	}
)
#properties[["girth"]] <- list(		# cycle of maximal length
#	type=integer(),
#	bounds=c(1,NA),
#	foo=function(graph) 
#	{	girth(graph=graph, circle=FALSE)$girth
#	}
#)
