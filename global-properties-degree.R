#################################
# degree-related
#################################
properties[["degree-centralization"]] <- list(
	type=integer(),
	bounds=c(0,1),
	foo=function(graph) 
	{	if(length(cache$degree)==0)
		{	prop.file <- paste(net.folder,"degree.txt",sep="")
			if(file.exists(prop.file))
				cache$degree <<- as.matrix(read.table(prop.file))
			else
			{	cache$degree <<- degree(graph=graph, mode="all",loops=FALSE, normalized=FALSE)
				write.tabble(cache$degree,prop.file,row.names=FALSE,col.names=FALSE)
			}
		}
		# formula taken from Freeman L. C., "Centrality in Social Networks I: Conceptual Clarification", Social Networks, 1(3):215-239, 1978.
		n <- vcount(graph)
		(max(cache$degree)*n - sum(cache$degree)) / (n^2 - 3*n +2)
	}
)
properties[["degree-average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	if(length(cache$degree)==0)
		{	prop.file <- paste(net.folder,"degree.txt",sep="")
			if(file.exists(prop.file))
				cache$degree <<- as.matrix(read.table(prop.file))
			else
			{	cache$degree <<- degree(graph=graph, mode="all",loops=FALSE, normalized=FALSE)
				write.tabble(cache$degree,prop.file,row.names=FALSE,col.names=FALSE)
			}
		}
		mean(cache$degree,na.rm=TRUE)
	}
)
properties[["degree-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	if(length(cache$degree)==0)
		{	prop.file <- paste(net.folder,"degree.txt",sep="")
			if(file.exists(prop.file))
				cache$degree <<- as.matrix(read.table(prop.file))
			else
			{	cache$degree <<- degree(graph=graph, mode="all",loops=FALSE, normalized=FALSE)
				write.tabble(cache$degree,prop.file,row.names=FALSE,col.names=FALSE)
			}
		}
		sd(cache$degree,na.rm=TRUE)
	}
)
properties[["degree-min"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	if(length(cache$degree)==0)
		{	prop.file <- paste(net.folder,"degree.txt",sep="")
			if(file.exists(prop.file))
				cache$degree <<- as.matrix(read.table(prop.file))
			else
			{	cache$degree <<- degree(graph=graph, mode="all",loops=FALSE, normalized=FALSE)
				write.tabble(cache$degree,prop.file,row.names=FALSE,col.names=FALSE)
			}
		}
		min(cache$degree,na.rm=TRUE)
	}
)
properties[["degree-max"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	if(length(cache$degree)==0)
		{	prop.file <- paste(net.folder,"degree.txt",sep="")
			if(file.exists(prop.file))
				cache$degree <<- as.matrix(read.table(prop.file))
			else
			{	cache$degree <<- degree(graph=graph, mode="all",loops=FALSE, normalized=FALSE)
				write.tabble(cache$degree,prop.file,row.names=FALSE,col.names=FALSE)
			}
		}
		max(cache$degree,na.rm=TRUE)
	}
)
properties[["degree-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	assortativity.degree(graph=graph, directed=FALSE)
	}
)
