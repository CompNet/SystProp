#################################
# attribute-related properties
#################################
properties[["node-attributes-total"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	att.names <- list.vertex.attributes(graph)
		att.names <- att.names[att.names!="id"]	# automatically inserted by igrapgh
		length(att.names)
	}
)
properties[["node-attributes-numeric"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	att.names <- list.vertex.attributes(graph)
		att.names <- att.names[att.names!="id"]	# automatically inserted by igrapgh
		result <- 0
		for(att.name in att.names)
		{	values <- get.vertex.attribute(graph=graph, name=att.name)
			if(is.numeric(values))
				result <- result + 1
		}
		return(result)
	}
)
properties[["node-attributes-categorical"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	att.names <- list.vertex.attributes(graph)
		att.names <- att.names[att.names!="id"]	# automatically inserted by igrapgh
		result <- 0
		for(att.name in att.names)
		{	values <- get.vertex.attribute(graph=graph, name=att.name)
			values <- values[which(!is.na(values))]
			# we consider only non-numeric attributes, with repeating values
			if(!is.numeric(values) && length(values)>length(unique(values)))
				result <- result + 1
		}
		return(result)
	}
)
properties[["node-attributes-unique"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	att.names <- list.vertex.attributes(graph)
		att.names <- att.names[att.names!="id"]	# automatically inserted by igrapgh
		result <- 0
		for(att.name in att.names)
		{	values <- get.vertex.attribute(graph=graph, name=att.name)
			values <- values[which(!is.na(values))]
			# we consider only non-numeric attributes, without repeating values
			if(!is.numeric(values) && length(values)==length(unique(values)))
				result <- result + 1
		}
		return(result)
	}
)

properties[["link-attributes-total"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	att.names <- list.edge.attributes(graph)
		length(att.names)
	}
)
properties[["link-attributes-numeric"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	att.names <- list.edge.attributes(graph)
		result <- 0
		for(att.name in att.names)
		{	values <- get.edge.attribute(graph=graph, name=att.name)
			if(is.numeric(values))
				result <- result + 1
		}
		return(result)
	}
)
properties[["link-attributes-categorical"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	att.names <- list.edge.attributes(graph)
		result <- 0
		for(att.name in att.names)
		{	values <- get.edge.attribute(graph=graph, name=att.name)
			values <- values[which(!is.na(values))]
			# we consider only non-numeric attributes, with repeating values
			if(!is.numeric(values) && length(values)>length(unique(values)))
				result <- result + 1
		}
		return(result)
	}
)
properties[["link-attributes-unique"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	att.names <- list.edge.attributes(graph)
		result <- 0
		for(att.name in att.names)
		{	values <- get.edge.attribute(graph=graph, name=att.name)
			values <- values[which(!is.na(values))]
			# we consider only non-numeric attributes, without repeating values
			if(!is.numeric(values) && length(values)==length(unique(values)))
				result <- result + 1
		}
		return(result)
	}
)
