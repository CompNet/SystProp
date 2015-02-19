# Generates some random data, for testing purposes.
#
# setwd("~/eclipse/workspaces/Networks")
# setwd("D:/eclipse/workspaces/Networks")
#
# source("preprocess/generate-data.R")
###################################################
library(igraph)

data.folder <- "c:/Temp/"
folders <- 1:5

for(f in folders)
{	prefix <- sprintf("%04d",f)
	net.folder <- paste(data.folder,prefix,".Xxxx",sep="")
	dir.create(path=net.folder)
	
	net.file <- paste(net.folder,"/network.graphml",sep="")
	g <- barabasi.game(1000)
	write.graph(g,net.file,format="graphml")
}
