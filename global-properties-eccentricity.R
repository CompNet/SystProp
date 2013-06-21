#################################
# eccentricity-related
#################################
properties[["eccentricity average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	if(length(cache$eccentricity)==0)
			cache$eccentricity <<- eccentricity(graph=graph, mode="all")
		mean(cache$eccentricity,na.rm=TRUE)
	}
)
properties[["eccentricity stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	if(length(cache$eccentricity)==0)
			cache$eccentricity <<- eccentricity(graph=graph, mode="all")
		sd(cache$eccentricity,na.rm=TRUE)
	}
)
properties[["radius"]] <- list(		# aka minimal eccentricity
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	radius(graph=graph, mode="all")
	}
)
properties[["diameter"]] <- list(	# aka maximal eccentricity, maximal distance
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	diameter(graph=graph, directed=FALSE, unconnected=TRUE, weights=NULL)
	}
)
properties[["eccentricity assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	if(length(cache$eccentricity)==0)
			cache$eccentricity <<- eccentricity(graph=graph, mode="all")
		assortativity(graph=graph, types1=cache$eccentricity, types2=NULL, directed=FALSE)
	}
)
