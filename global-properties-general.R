#################################
# Processes general measures.
#################################
properties[["directed"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	is.directed(graph=graph)
	}
)
properties[["weighted"]] <- list(
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
properties[["connected"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	is.connected(graph=graph,mode="weak")
	}
)
properties[["bipartite"]] <- list(
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
properties[["multiplex"]] <- list(
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
properties[["multiple-links"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	has.multiple(graph=graph)
	}
)
properties[["loop"]] <- list(
		type=logical(),
		bounds=c(FALSE,TRUE),
		foo=function(graph) 
		{	values <- is.loop(graph=graph)
			!all(!values)
		}
)
properties[["format"]] <- list(
		type=character(),
		bounds=c(NA,NA),
		foo=function(graph) 
		{	return(format)
		}
)
