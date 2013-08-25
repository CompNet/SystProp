#################################
# various tests used to debug the dataset
#################################
properties[["file-name"]] <- list(
	type=integer(),
	bounds=c(NA,NA),
	foo=function(graph) 
	{	dirname(data.file)
	}
)
properties[["file-size"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	info <- file.info(data.file)
		return(info$size)
	}
)
properties[["weight-vs-weights"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	ref.att <- c("weights","Weights")
		g.att <- list.edge.attributes(graph)
		inter <- intersect(ref.att, g.att)
		length(inter) > 0
	}
)
properties[["weight-vs-Weight"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	g.att <- list.edge.attributes(graph)
		any(g.att=="Weight")
	}
)
properties[["type-vs-Type"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	g.att <- c(list.vertex.attributes(graph),list.edge.attributes(graph))
		any(g.att=="Type")
	}
)
properties[["link-name"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	ref.att <- c("name","Name")
		g.att <- list.edge.attributes(graph)
		inter <- intersect(ref.att, g.att)
		length(inter) > 0
	}
)
properties[["node-mode"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	ref.att <- c("mode","Mode")
		g.att <- list.vertex.attributes(graph)
		inter <- intersect(ref.att, g.att)
		length(inter) > 0
	}
)
