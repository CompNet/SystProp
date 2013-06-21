#################################
# component-related
#################################
properties[["component-count"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	if(length(cache$componentsizes)==0)
			cache$componentsizes <<- clusters(graph=graph, mode="weak")$csize
		length(cache$componentsizes)
	}
)
properties[["component-average-size"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	if(length(cache$componentsizes)==0)
			cache$componentsizes <<- clusters(graph=graph, mode="weak")$csize
		mean(cache$componentsizes,na.rm=TRUE)
	}
)
properties[["component-stdev-size"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	if(length(cache$componentsizes)==0)
			cache$componentsizes <<- clusters(graph=graph, mode="weak")$csize
		sd(cache$componentsizes)
	}
)
properties[["component-min-size"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	if(length(cache$componentsizes)==0)
			cache$componentsizes <<- clusters(graph=graph, mode="weak")$csize
		min(cache$componentsizes)
	}
)
properties[["component-max-size"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	if(length(cache$componentsizes)==0)
			cache$componentsizes <<- clusters(graph=graph, mode="weak")$csize
		max(cache$componentsizes)
	}
)
