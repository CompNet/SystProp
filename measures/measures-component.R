#################################
# Processes component-related measures.
#################################
# identifies components
process.components <- function(graph)
{	if(length(cache$componentsizes)==0)
	{	prop.file <- paste(net.folder,"component-sizes.txt",sep="")
		if(file.exists(prop.file))
			cache$componentsizes <<- as.matrix(read.table(prop.file))
		else
		{	cache$componentsizes <<- clusters(graph=graph, mode="weak")$csize
			write.table(cache$componentsizes,prop.file,row.names=FALSE,col.names=FALSE)
		}
	}
}

measures[["component-count"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.components(graph)
		length(cache$componentsizes)
	}
)
measures[["component-average-size"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.components(graph)
		mean(cache$componentsizes,na.rm=TRUE)
	}
)
measures[["component-stdev-size"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.components(graph)
		sd(cache$componentsizes)
	}
)
measures[["component-min-size"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.components(graph)
		min(cache$componentsizes)
	}
)
measures[["component-max-size"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.components(graph)
		max(cache$componentsizes)
	}
)
