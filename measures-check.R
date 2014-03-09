#################################
# Performs various tests used to identify problems
# in the dataset.
#################################
measures[["file-name"]] <- list(
	type=integer(),
	bounds=c(NA,NA),
	foo=function(graph) 
	{	dirname(data.file)
	}
)
measures[["file-size"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	info <- file.info(data.file)
		return(info$size)
	}
)
measures[["weight-vs-weights"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	ref.att <- c("weights","Weights")
		g.att <- list.edge.attributes(graph)
		inter <- intersect(ref.att, g.att)
		length(inter) > 0
	}
)
measures[["weight-vs-Weight"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	g.att <- list.edge.attributes(graph)
		any(g.att=="Weight")
	}
)
measures[["type-vs-Type"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	g.att <- c(list.vertex.attributes(graph),list.edge.attributes(graph))
		any(g.att=="Type")
	}
)
measures[["link-name"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	ref.att <- c("name","Name")
		g.att <- list.edge.attributes(graph)
		inter <- intersect(ref.att, g.att)
		length(inter) > 0
	}
)
measures[["node-mode"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	ref.att <- c("mode","Mode")
		g.att <- list.vertex.attributes(graph)
		inter <- intersect(ref.att, g.att)
		length(inter) > 0
	}
)
