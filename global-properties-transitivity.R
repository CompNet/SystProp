#################################
# Processes transitivity-related measures.
#################################
# processes local transitivies
process.local.transitivity <- function(graph)
{	if(length(cache$localtransitivity)==0)
	{	prop.file <- paste(net.folder,"transitivity-local.txt",sep="")
		if(file.exists(prop.file))
			cache$localtransitivity <<- as.matrix(read.table(prop.file))
		else
		{	cache$localtransitivity <<- transitivity(graph, type="localundirected",weights=NULL,isolates="zero")
			write.table(cache$localtransitivity,prop.file,row.names=FALSE,col.names=FALSE)
		}
	}
}

properties[["transitivity-global"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	transitivity(graph, type="globalundirected",weights=NULL,isolates="zero")
	}
)
properties[["transitivity-local-average"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	process.local.transitivity(graph)
		mean(cache$localtransitivity,na.rm=TRUE)
	}
)
properties[["transitivity-local-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	process.local.transitivity(graph)
		sd(cache$localtransitivity,na.rm=TRUE)
	}
)
properties[["transitivity-local-min"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	process.local.transitivity(graph)
		min(cache$localtransitivity,na.rm=TRUE)
	}
)
properties[["transitivity-local-max"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	process.local.transitivity(graph)
		max(cache$localtransitivity,na.rm=TRUE)
	}
)
properties[["transitivity-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	process.local.transitivity(graph)
		assortativity(graph=graph, types1=cache$localtransitivity, types2=NULL, directed=FALSE)
	}
)
