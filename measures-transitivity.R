#################################
# Processes transitivity-related measures.
#################################
source("SystProp/rewire-network.R")

# processes local transitivities
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

#measures[["transitivity-global"]] <- list(
#	type=numeric(),
#	bounds=c(0,1),
#	foo=function(graph) 
#	{	transitivity(graph, type="globalundirected", weights=NULL, isolates="zero")
#	}
#)
measures[["transitivity-global-latt"]] <- list(	# global transitivity in the latticized version of the network
	type=numeric(),
	bounds=c(1,NA),
	foo=function(graph) 
	{	# init
		update.trans <- TRUE # whether or not to update transitivity for each iteration (TRUE=slower)
		tolerance <- 0.005
		i <- 0
		best.res <- 0
		go.on <- TRUE
		g2 <- graph
		
		# we may perform several rewirings to improve transitivity
		while(i<3 | (update.trans & go.on))
		{	# latticize the network (the more iterations, the closer to a lattice)
			g2 <- latticize.network(g=g2, iterations=10)
			
			if(update.trans)
			{	# process its global transitivity
				cur.res <- transitivity(g2, type="globalundirected", weights=NULL, isolates="zero")
				# compare with current best 
				go.on <- cur.res-best.res > tolerance
				if(cur.res>best.res)
					best.res <- cur.res
			}
			
			cat("i=",i," cur.res=",cur.res," best.res=",best.res,"\n",sep="")
			i <- i + 1
		}
		
		if(!update.trans)
			best.res <- transitivity(g2, type="globalundirected", weights=NULL, isolates="zero")
		return(best.res)
	}
)
#measures[["transitivity-local-average"]] <- list(
#	type=numeric(),
#	bounds=c(0,1),
#	foo=function(graph) 
#	{	process.local.transitivity(graph)
#		mean(cache$localtransitivity,na.rm=TRUE)
#	}
#)
#measures[["transitivity-local-stdev"]] <- list(
#	type=numeric(),
#	bounds=c(0,1),
#	foo=function(graph) 
#	{	process.local.transitivity(graph)
#		sd(cache$localtransitivity,na.rm=TRUE)
#	}
#)
#measures[["transitivity-local-min"]] <- list(
#	type=numeric(),
#	bounds=c(0,1),
#	foo=function(graph) 
#	{	process.local.transitivity(graph)
#		min(cache$localtransitivity,na.rm=TRUE)
#	}
#)
#measures[["transitivity-local-max"]] <- list(
#	type=numeric(),
#	bounds=c(0,1),
#	foo=function(graph) 
#	{	process.local.transitivity(graph)
#		max(cache$localtransitivity,na.rm=TRUE)
#	}
#)
#measures[["transitivity-assortativity"]] <- list(
#	type=numeric(),
#	bounds=c(-1,1),
#	foo=function(graph) 
#	{	process.local.transitivity(graph)
#		assortativity(graph=graph, types1=cache$localtransitivity, types2=NULL, directed=FALSE)
#	}
#)
