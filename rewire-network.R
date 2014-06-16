# Rewires an existing network in a more or
# less random way. One function is completely
# random, the other is a latticization.
#
# setwd("~/eclipse/workspaces/Networks")
# setwd("C:/eclipse/workspaces/Networks")
#
# source("SystProp/rewire-network.R")
###################################################

#################################
# load dependencies
#################################
library(igraph)


#################################
# Returns the adjacency matrix when
# considering only the specified nodes
# vs. the whole network.
# The result is an k*n matrix, where
# n is the total number of nodes in the
# network and k the number of nodes
# of interest.
#
# g: network
# nodes: nodes of interest.
#################################
get.partial.matrix <- function(g, nodes)
{	result <- matrix(data=FALSE,ncol=vcount(g),nrow=length(nodes))
	cat("!!!!!!!!!!!!!!\n")
	print(nodes)
	for(i in 1:length(nodes))
	{	neigh <- neighbors(graph=g,v=nodes[i],mode="all")
		print(neigh)
		neigh <- sort(unique(neigh))
		result[i,neigh] <- TRUE
	}
	
	return(result)
}

#################################
# Randomly rewires the network,
# while preserving the degree distribution.
# Adapted from function randmio_und_connected from BCT
# https://sites.google.com/site/bctnet
#
# g: network to be rewired.
# iterations: number of times a link is rewired (approximately)
#################################
randomize.network <- function(g, iterations)
{	# init
	n <- vcount(g)
	m <- ecount(g)
	iter <- m*iterations
	# maximal number of rewiring attempts per iter
	max.attempts <- round(n*m/(n*(n-1.0)))
	# actual number of successful rewirings
	eff <- 0
	
	# repeat process
	for(it in 1:iter)
	{	att <- 0
		rewire <- FALSE
		
		# while not rewired
		while(!rewire & att<=max.attempts)
		{	rewire <- TRUE
			
			# randomly draw 2 links
			es <- igraph.sample(1,m,2)
			temp <- get.edges(graph=g,es=es)
			a <- temp[1,1]
			b <- temp[1,2]
			c <- temp[2,1]
			d <- temp[2,2]
			
			# check if they involve different nodes
			if(length(intersect(c(a,b),c(c,d)))==0)
			{	# possibly flip the first link
				p <- runif(1)
				if(p>0.5)
				{	a <- temp[1,2]
					b <- temp[1,1]
				}
				
				# check if some of the 2 new links already exist
				if(length(E(g)[a %--% d])==0 & length(E(g)[c %--% b])==0)
				{	
					# check if the rewiring is going to split the network
					if(length(E(g)[a %--% c])==0 & length(E(g)[b %--% d])==0)
					{	p0 <- get.partial.matrix(g,c(a,d))
						p0[1,b] <- FALSE
						p0[2,c] <- FALSE
						p1 <- p0
						p1[,a] <- TRUE
						p1[,d] <- TRUE
						
						while(rewire & !any(p0[,c(b,c)]))
						{	p0[1,] <- apply(get.partial.matrix(g,which(p0[1,])),2,any)
							p0[2,] <- apply(get.partial.matrix(g,which(p0[2,])),2,any)
							p0 <- p0 & (!p1)
							if(!all(apply(p0,1,any)))
								rewire <- FALSE
							p1 <- p1 + p0
						}
					}
				
					# possibly rewire the links
					if(rewire)
					{	g <- delete.edges(graph=g, edges=es)
						g <- add.edges(graph=g, edges=c(a,d,b,c))
						eff <- eff + 1
					}
				}
			}
			
			# increment the number of attempts
			att <- att + 1
		}
	}
	
	return(g)
}