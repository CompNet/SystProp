#################################
# Processes general measures.
#################################
measures[["directed"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	is.directed(graph=graph)
	}
)
measures[["weighted"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	#is.weighted(graph)
		# we just check whether a link attribute of a certain name exists
		ref.att <- c("weight"
						#,"Weight"
					)
		g.att <- list.edge.attributes(graph)
		inter <- intersect(ref.att, g.att)
		length(inter) > 0
	}
)
measures[["connected"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	is.connected(graph=graph,mode="weak")
	}
)
measures[["bipartite"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	#is.bipartite(graph=graph)
		# we just check whether a node attribute of a certain name exists
		ref.att <- c("type"
						#,"Type"
					)
		g.att <- list.vertex.attributes(graph)
		inter <- intersect(ref.att, g.att)
		length(inter) > 0
	}
)
measures[["multiplex"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	# we just check whether a link attribute of a certain name exists
		ref.att <- c("type"
						#,"Type"
					)
		g.att <- list.edge.attributes(graph)
		inter <- intersect(ref.att, g.att)
		length(inter) > 0
	}
)
measures[["multiple-links"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	has.multiple(graph=graph)
	}
)
measures[["loop"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	values <- is.loop(graph=graph)
		!all(!values)
	}
)
measures[["format"]] <- list(
	type=character(),
	bounds=c(NA,NA),
	foo=function(graph) 
	{	return(format)
	}
)
