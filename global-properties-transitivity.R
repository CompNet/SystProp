#################################
# transitivity-related
#################################
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
	{	if(length(cache$localtransitivity)==0)
		{	prop.file <- paste(net.folder,"transitivity-local.txt",sep="")
			if(file.exists(prop.file))
				cache$localtransitivity <<- as.matrix(read.table(prop.file))
			else
			{	cache$localtransitivity <<- transitivity(graph, type="localundirected",weights=NULL,isolates="zero")
				write.tabble(cache$localtransitivity,prop.file,row.names=FALSE,col.names=FALSE)
			}
		}
		mean(cache$localtransitivity,na.rm=TRUE)
	}
)
properties[["transitivity-local-stdev"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	if(length(cache$localtransitivity)==0)
		{	prop.file <- paste(net.folder,"transitivity-local.txt",sep="")
			if(file.exists(prop.file))
				cache$localtransitivity <<- as.matrix(read.table(prop.file))
			else
			{	cache$localtransitivity <<- transitivity(graph, type="localundirected",weights=NULL,isolates="zero")
				write.tabble(cache$localtransitivity,prop.file,row.names=FALSE,col.names=FALSE)
			}
		}
		sd(cache$localtransitivity,na.rm=TRUE)
	}
)
properties[["transitivity-local-min"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	if(length(cache$localtransitivity)==0)
		{	prop.file <- paste(net.folder,"transitivity-local.txt",sep="")
			if(file.exists(prop.file))
				cache$localtransitivity <<- as.matrix(read.table(prop.file))
			else
			{	cache$localtransitivity <<- transitivity(graph, type="localundirected",weights=NULL,isolates="zero")
				write.tabble(cache$localtransitivity,prop.file,row.names=FALSE,col.names=FALSE)
			}
		}
		min(cache$localtransitivity,na.rm=TRUE)
	}
)
properties[["transitivity-local-max"]] <- list(
	type=numeric(),
	bounds=c(0,1),
	foo=function(graph) 
	{	if(length(cache$localtransitivity)==0)
		{	prop.file <- paste(net.folder,"transitivity-local.txt",sep="")
			if(file.exists(prop.file))
				cache$localtransitivity <<- as.matrix(read.table(prop.file))
			else
			{	cache$localtransitivity <<- transitivity(graph, type="localundirected",weights=NULL,isolates="zero")
				write.tabble(cache$localtransitivity,prop.file,row.names=FALSE,col.names=FALSE)
			}
		}
		max(cache$localtransitivity,na.rm=TRUE)
	}
)
properties[["transitivity-assortativity"]] <- list(
	type=numeric(),
	bounds=c(-1,1),
	foo=function(graph) 
	{	if(length(cache$localtransitivity)==0)
		{	prop.file <- paste(net.folder,"transitivity-local.txt",sep="")
			if(file.exists(prop.file))
				cache$localtransitivity <<- as.matrix(read.table(prop.file))
			else
			{	cache$localtransitivity <<- transitivity(graph, type="localundirected",weights=NULL,isolates="zero")
				write.tabble(cache$localtransitivity,prop.file,row.names=FALSE,col.names=FALSE)
			}
		}
		assortativity(graph=graph, types1=cache$localtransitivity, types2=NULL, directed=FALSE)
	}
)
