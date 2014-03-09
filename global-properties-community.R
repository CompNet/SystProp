#################################
# Processes community-related measures.
#################################
# identifies community structure
process.communities <- function(graph)
{	if(length(cache$communities)==0)
	{	prop.file <- paste(net.folder,"communities.txt",sep="")
		if(file.exists(prop.file))
			cache$communities <<- as.matrix(read.table(prop.file))
		else
		{	cache$communities <<- multilevel.community(graph=as.undirected(graph), weights=NULL)$membership
			write.table(cache$communities,prop.file,row.names=FALSE,col.names=FALSE)
		}
	}
}

properties[["modularity"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph)
	{	process.communities(graph)
		modularity(x=graph, membership=cache$communities, weights=NULL)
	}
)
properties[["community-number"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph)
	{	process.communities(graph)
		length(unique(cache$communities))
	}
)
