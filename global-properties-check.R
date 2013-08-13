#################################
# various tests used to debug the dataset
#################################
properties[["weight-vs-weights"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	ref.att <- c("weights","Weights")
		g.att <- list.edge.attributes(g)
		inter <- intersect(ref.att, g.att)
		length(inter) > 1
	}
)
properties[["link-name"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	ref.att <- c("name","Name")
		g.att <- list.edge.attributes(g)
		inter <- intersect(ref.att, g.att)
		length(inter) > 1
	}
)
properties[["node-mode"]] <- list(
	type=logical(),
	bounds=c(FALSE,TRUE),
	foo=function(graph) 
	{	ref.att <- c("mode","Mode")
		g.att <- list.vertex.attributes(g)
		inter <- intersect(ref.att, g.att)
		length(inter) > 1
	}
)
