#################################
# various tests used to debug the dataset
#################################
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
properties[["file-size"]] <- list(
	type=integer(),
	bounds=c(0,NA),
	foo=function(graph) 
	{	info <- file.info(data.file)
		return(info$size)
	}
)
