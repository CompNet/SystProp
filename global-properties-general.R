#################################
# general properties
#################################
properties[["multiplex"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	has.multiple(graph=graph)
	}
)
properties[["directed"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	is.directed(graph=graph)
	}
)
properties[["weighted"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	is.weighted(graph)
	}
)
properties[["bipartite"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	is.bipartite(graph=graph)
	}
)
properties[["connected"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	is.connected(graph=graph,mode="weak")
	}
)
