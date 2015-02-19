#################################
# Processes attribute-related measures
#################################
measures[["node-attributes-total"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	att.names <- list.vertex.attributes(graph)
		att.names <- att.names[att.names!="id"]	# automatically inserted by igrapgh
		length(att.names)
	}
)
measures[["node-attributes-numeric"]] <- list(
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
measures[["node-attributes-categorical"]] <- list(
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
measures[["node-attributes-unique"]] <- list(
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

measures[["link-attributes-total"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	att.names <- list.edge.attributes(graph)
		length(att.names)
	}
)
measures[["link-attributes-numeric"]] <- list(
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
measures[["link-attributes-categorical"]] <- list(
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
measures[["link-attributes-unique"]] <- list(
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
