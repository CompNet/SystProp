#################################
# Processes spectral measures.
#################################
# processes eigenvector centrality
process.evcent <- function(graph)
{	if(length(cache$eigenvectorcentrality)==0)
	{	prop.file <- paste(net.folder,"eigenvector-centrality.txt",sep="")
		if(file.exists(prop.file))
			cache$eigenvectorcentrality <<- as.matrix(read.table(prop.file))
		else
		{	cache$eigenvectorcentrality <<- evcent(graph=graph, directed=FALSE, scale=FALSE, weights=NULL)$vector
			write.table(cache$eigenvectorcentrality,prop.file,row.names=FALSE,col.names=FALSE)
		}
	}
}
	
measures[["eigenvector-centralization"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	centralization.evcent(graph=graph, directed=FALSE, scale=FALSE, normalized=TRUE)$centralization
	}
)
measures[["eigenvector-centrality-average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.evcent(graph)
		mean(cache$eigenvectorcentrality,na.rm=TRUE)
	}
)
measures[["eigenvector-centrality-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.evcent(graph)
		sd(cache$eigenvectorcentrality,na.rm=TRUE)
	}
)
measures[["eigenvector-centrality-min"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.evcent(graph)
		#print(cache$eigenvectorcentrality)		
		min(cache$eigenvectorcentrality,na.rm=TRUE)
	}
)
measures[["eigenvector-centrality-max"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.evcent(graph)
		#print(cache$eigenvectorcentrality)		
		max(cache$eigenvectorcentrality,na.rm=TRUE)
	}
)
measures[["eigenvector-centrality-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	process.evcent(graph)
		assortativity(graph=graph, types1=cache$eigenvectorcentrality, types2=NULL, directed=FALSE)
	}
)
