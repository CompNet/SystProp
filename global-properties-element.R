#################################
# Processes node/link-related measures.
#################################
properties[["node-count"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	vcount(graph=graph)
	}
)
properties[["link-count"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	ecount(graph=graph)
	}
)
properties[["density"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	graph.density(graph=graph, loops=FALSE)
	}
)
