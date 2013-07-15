#################################
# eccentricity-related
#################################
properties[["eccentricity-average"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	if(length(cache$eccentricity)==0)
		{	prop.file <- paste(net.folder,"eccentricity.txt",sep="")
			if(file.exists(prop.file))
				cache$eccentricity <<- as.matrix(read.table(prop.file))
			else
			{	cache$eccentricity <<- eccentricity(graph=graph, mode="all")
				write.tabble(cache$eccentricity,prop.file,row.names=FALSE,col.names=FALSE)
			}
		}
		mean(cache$eccentricity,na.rm=TRUE)
	}
)
properties[["eccentricity-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	if(length(cache$eccentricity)==0)
		{	prop.file <- paste(net.folder,"eccentricity.txt",sep="")
			if(file.exists(prop.file))
				cache$eccentricity <<- as.matrix(read.table(prop.file))
			else
			{	cache$eccentricity <<- eccentricity(graph=graph, mode="all")
				write.tabble(cache$eccentricity,prop.file,row.names=FALSE,col.names=FALSE)
			}
		}
		sd(cache$eccentricity,na.rm=TRUE)
	}
)
properties[["radius"]] <- list(		# aka minimal eccentricity
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	## old version >> not efficient
		##radius(graph=graph, mode="all")
		if(length(cache$eccentricity)==0)
		{	prop.file <- paste(net.folder,"eccentricity.txt",sep="")
			if(file.exists(prop.file))
				cache$eccentricity <<- as.matrix(read.table(prop.file))
			else
			{	cache$eccentricity <<- eccentricity(graph=graph, mode="all")
				write.tabble(cache$eccentricity,prop.file,row.names=FALSE,col.names=FALSE)
			}
		}
		max(cache$eccentricity,na.rm=TRUE)
	}
)
properties[["diameter"]] <- list(	# aka maximal eccentricity, maximal distance
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	## old version >> not efficient
		##diameter(graph=graph, directed=FALSE, unconnected=TRUE, weights=NULL)
		if(length(cache$eccentricity)==0)
		{	prop.file <- paste(net.folder,"eccentricity.txt",sep="")
			if(file.exists(prop.file))
				cache$eccentricity <<- as.matrix(read.table(prop.file))
			else
			{	cache$eccentricity <<- eccentricity(graph=graph, mode="all")
				write.tabble(cache$eccentricity,prop.file,row.names=FALSE,col.names=FALSE)
			}
		}
		min(cache$eccentricity,na.rm=TRUE)
	}
)
properties[["eccentricity-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	if(length(cache$eccentricity)==0)
		{	prop.file <- paste(net.folder,"eccentricity.txt",sep="")
			if(file.exists(prop.file))
				cache$eccentricity <<- as.matrix(read.table(prop.file))
			else
			{	cache$eccentricity <<- eccentricity(graph=graph, mode="all")
				write.tabble(cache$eccentricity,prop.file,row.names=FALSE,col.names=FALSE)
			}
		}
		assortativity(graph=graph, types1=cache$eccentricity, types2=NULL, directed=FALSE)
	}
)
