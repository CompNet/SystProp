#################################
# degree-related
#################################
properties[["degree average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	if(length(cache$degree)==0)
			cache$degree <<- degree(graph=graph, mode="all",loops=FALSE, normalized=FALSE)
		mean(cache$degree)
	}
)
properties[["degree stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	if(length(cache$degree)==0)
			cache$degree <<- degree(graph=graph, mode="all",loops=FALSE, normalized=FALSE)
		sd(cache$degree)
	}
)
properties[["degree min"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	if(length(cache$degree)==0)
			cache$degree <<- degree(graph=graph, mode="all",loops=FALSE, normalized=FALSE)
		min(cache$degree)
	}
)
properties[["degree max"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	if(length(cache$degree)==0)
			cache$degree <<- degree(graph=graph, mode="all",loops=FALSE, normalized=FALSE)
		max(cache$degree)
	}
)
properties[["degree assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	assortativity.degree(graph=graph, directed=FALSE)
	}
)
