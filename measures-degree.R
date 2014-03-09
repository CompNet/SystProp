#################################
# Processes degree-related measures.
#################################
# processes degrees
process.degree <- function(graph)
{	if(length(cache$degree)==0)
	{	prop.file <- paste(net.folder,"degree-all.txt",sep="")
		if(file.exists(prop.file))
			cache$degree <<- as.matrix(read.table(prop.file))
		else
		{	cache$degree <<- degree(graph=graph, mode="all",loops=FALSE, normalized=FALSE)
			write.table(cache$degree,prop.file,row.names=FALSE,col.names=FALSE)
		}
	}
}

measures[["degree-all-centralization"]] <- list(
	type=integer(),
	bounds=c(0,1),
	foo=function(graph) 
	{	process.degree(graph)
		# formula taken from Freeman L. C., "Centrality in Social Networks I: Conceptual Clarification", Social Networks, 1(3):215-239, 1978.
		n <- vcount(graph)
		(max(cache$degree)*n - sum(cache$degree)) / (n^2 - 3*n +2)
	}
)
measures[["degree-all-average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.degree(graph)
		mean(cache$degree,na.rm=TRUE)
	}
)
measures[["degree-all-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.degree(graph)
		sd(cache$degree,na.rm=TRUE)
	}
)
measures[["degree-all-min"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.degree(graph)
		min(cache$degree,na.rm=TRUE)
	}
)
measures[["degree-all-max"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.degree(graph)
		max(cache$degree,na.rm=TRUE)
	}
)
measures[["degree-all-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	assortativity.degree(graph=graph, directed=FALSE)
	}
)
