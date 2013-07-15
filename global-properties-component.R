#################################
# component-related
#################################
# identifies components
process.components <- function(graph)
{	if(length(cache$componentsizes)==0)
	{	prop.file <- paste(net.folder,"component-sizes.txt",sep="")
		if(file.exists(prop.file))
			cache$componentsizes <<- as.matrix(read.table(prop.file))
		else
		{	cache$componentsizes <<- clusters(graph=graph, mode="weak")$csize
			write.tabble(cache$componentsizes,prop.file,row.names=FALSE,col.names=FALSE)
		}
	}
}

properties[["component-count"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.components(graph)
		length(cache$componentsizes)
	}
)
properties[["component-average-size"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.components(graph)
		mean(cache$componentsizes,na.rm=TRUE)
	}
)
properties[["component-stdev-size"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.components(graph)
		sd(cache$componentsizes)
	}
)
properties[["component-min-size"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.components(graph)
		min(cache$componentsizes)
	}
)
properties[["component-max-size"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.components(graph)
		max(cache$componentsizes)
	}
)
