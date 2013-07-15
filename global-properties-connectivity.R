#################################
# connectivity-related
#################################
# processes the link-connectivities
process.link.connectivity <- function(graph)
{	if(length(cache$linkconnectivity)==0)
	{	prop.file <- paste(net.folder,"link-connectivity.txt",sep="")
		if(file.exists(prop.file))
			cache$linkconnectivity <<- as.matrix(read.table(prop.file))
		else
		{	cache$linkconnectivity <<- edge.connectivity(graph=graph, source=V(g), target=V(g), checks=TRUE)
			write.tabble(cache$linkconnectivity,prop.file,row.names=FALSE,col.names=FALSE)
		}
	}
}
# processes the node-connectivities
process.node.connectivity <- function(graph)
{	if(length(cache$nodeconnectivity)==0)
	{	prop.file <- paste(net.folder,"node-connectivity.txt",sep="")
		if(file.exists(prop.file))
			cache$nodeconnectivity <<- as.matrix(read.table(prop.file))
		else
		{	cache$nodeconnectivity <<- vertex.connectivity(graph=graph, source=V(g), target=V(g), checks=TRUE)
			write.tabble(cache$nodeconnectivity,prop.file,row.names=FALSE,col.names=FALSE)
		}
	}
}

properties[["link-connectivity-average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.link.connectivity(graph)
		mean(cache$linkconnectivity,na.rm=TRUE)
	}
)
properties[["link-connectivity-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.link.connectivity(graph)
		sd(cache$linkconnectivity,na.rm=TRUE)
	}
)
properties[["adhesion"]] <- list(		# aka minimal link connectivity
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	## old version >> not efficient
		##graph.adhesion(graph=graph, checks=TRUE)
		process.link.connectivity(graph)
		min(cache$linkconnectivity,na.rm=TRUE)
	}
)
properties[["link-connectivity-max"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.link.connectivity(graph)
		max(cache$linkconnectivity,na.rm=TRUE)
	}
)
properties[["node-connectivity-average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.node.connectivity(graph)
		mean(cache$nodeconnectivity,na.rm=TRUE)
	}
)
properties[["node-connectivity-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.node.connectivity(graph)
		sd(cache$nodeconnectivity,na.rm=TRUE)
	}
)
properties[["cohesion"]] <- list(		# aka minimal node connectivity
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	## old version >> not efficient
		##graph.cohesion(graph=graph, checks=TRUE)
		process.node.connectivity(graph)
		min(cache$nodeconnectivity,na.rm=TRUE)
	}
)
properties[["node-connectivity-max"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	process.node.connectivity(graph)
		max(cache$nodeconnectivity,na.rm=TRUE)
	}
)
