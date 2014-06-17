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
	#cat("!!!!!!!!!!!!!!\n")
	#print(nodes)
	if(length(nodes)>0)
	{	for(i in 1:length(nodes))
		{	neigh <- neighbors(graph=g,v=nodes[i],mode="all")
			if(length(neigh)>0)
				result[i,neigh] <- TRUE
		}
	}
	return(result)
}


#################################
# Checks if the specified rewiring is
# going to split the network, i.e.
# lead to the apparition of two components.
# Before the rewiring the existing links
# are (a,b) and (c,d). After the rewiring,
# they are (a,d) and (b,c).
#
# g: network
# a,b,c,d: nodes concerned by the rewiring 
#################################
is.splitting.network <- function(g, a, b, c, d)
{	result <- FALSE
	
	if(!are.connected(g, a, c) & !are.connected(g, b, d))
	{	# original method
#		# we take both adjacency matrix rows for a and d
#		p0 <- get.partial.matrix(g,c(a,d))
#		# remove their respective links to b and c
#		p0[1,b] <- FALSE
#		p0[2,c] <- FALSE
#		# copy as p1, add the new respective links to d and a
#		p1 <- p0
#		p1[,d] <- TRUE
#		p1[,a] <- TRUE
#		
#		while(!result & !any(p0[,c(b,c)]))
#		{	# get the neighborhood of the reachable nodes in p0 
#			p0[1,] <- apply(get.partial.matrix(g,which(p0[1,])),2,any)
#			p0[2,] <- apply(get.partial.matrix(g,which(p0[2,])),2,any)
#			p0 <- p0 & (!p1)
#			if(!all(apply(p0,1,any)))
#				result <- TRUE
#			p1 <- p1 | p0
#		}
		
		# igraph-based method
		# modify and check the graph
		g <- delete.edges(graph=g, edges=c(E(g)[a %--% b],E(g)[c %--% d]))
		g <- add.edges(graph=g, edges=c(a,d,b,c))
		result <- !is.connected(graph=g)
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
		{	rewire <- FALSE
			
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
				if(!are.connected(g,a,d) & !are.connected(g,c,b))
				{	
					# check if the rewiring is going to split the network
					rewire <- !is.splitting.network(g,a,b,c,d)
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

#################################
# Randomly rewires the specified
# network, so that it becomes more
# similar to a lattice.
# Adapted from function latmio_und_connected from BCT
# https://sites.google.com/site/bctnet
#
# g: network
# iterations: number of times a link is rewired (approximately)
#################################
latticize.network <- function(g, iterations)
{	# init
	n <- vcount(g)
	# randomize node order
	ind_rp <- sample(1:n)
	
	# create distance-to-diagonal matrix
	if(nargin<3) #if D is not specified by user
	{	D <- zeros(n)
		u <- [0 min([mod(1:n-1,n);mod(n-1:-1:1,n)])]
		for(v in 1:ceil(n/2))
		{	D(n-v+1,:) <- u([v+1:n 1:v])
			D(v,:) <- D(n-v+1,n:-1:1)
		}
	}
		
	[i j] <- find(tril(R))
	K <- length(i)
	ITER <- K*ITER

	# maximal number of rewiring attempts per iteration
	maxAttempts <- round(n*K/(n*(n-1)/2))
	# actual number of successful rewirings
	eff = 0

	for(iter=1:ITER)
	{	att=0
		# while not rewired
		while (att<=maxAttempts)    
		{	rewire <- 1
			while(TRUE)
			{	e1 <- ceil(K*rand)
				e2 <- ceil(K*rand)
				while(e2==e1)
					e2=ceil(K*rand)
				a=i(e1); b=j(e1)
				c=i(e2); d=j(e2)
				
				# all four vertices must be different
				if(all(a~=[c d]) && all(b~=[c d]))
					break
			}

			if(rand>0.5)
			{	# flip edge c-d with 50% probability
				i(e2) <- d
				j(e2) <- c
				c <- i(e2)
				d <- j(e2)
			}

			# rewiring condition
			if(~(R(a,d) || R(c,b)))
			{	# lattice condition
				if((D(a,b)*R(a,b)+D(c,d)*R(c,d))>=(D(a,d)*R(a,b)+D(c,b)*R(c,d)))
				{	# connectedness condition
					if(~(R(a,c) || R(b,d)))
					{	P <- R([a d],:)
						P(1,b) <- 0
						P(2,c) <- 0
						PN <- P
						PN(:,d) <- 1
						PN(:,a) <- 1
						while(TRUE)
						{	P(1,:) <- any(R(P(1,:)~=0,:),1)
							P(2,:) <- any(R(P(2,:)~=0,:),1)
							P <- P.*(~PN)
							if(~all(any(P,2)))
							{	rewire <- 0
								break
							}
							else if(any(any(P(:,[b c]))))
								break
							PN <- PN+P
						}
					}
		
					# reassign edges
					if(rewire)
					{	R(a,d)=R(a,b); R(a,b)=0;
						R(d,a)=R(b,a); R(b,a)=0;
						R(c,b)=R(c,d); R(c,d)=0;
						R(b,c)=R(d,c); R(d,c)=0;

						# reassign edge indices
						j(e1) = d;
						j(e2) = b;
						eff = eff+1;
						break;
					}
				}
			}
			att <- att + 1
		}
	}
		
	# lattice in node order used for latticization
	Rrp <- R
	# reverse random permutation of nodes
	[~,ind_rp_reverse] <- sort(ind_rp)
	Rlatt <- Rrp(ind_rp_reverse,ind_rp_reverse)
}
