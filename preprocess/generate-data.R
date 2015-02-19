# Generates some random data, for testing purposes.
#
# setwd("~/eclipse/workspaces/Networks/SystProp")
# setwd("D:/eclipse/workspaces/Networks/SystProp")
#
# source("preprocess/generate-data.R")
###################################################
library(igraph)

#data.folder <- "c:/Temp/"
data.folder <- "/var/data/networks/"

folders <- 1:5
#folders <- 58

for(f in folders)
{	prefix <- sprintf("%04d",f)
	net.folder <- paste(data.folder,prefix,".Xxxx",sep="")
	dir.create(path=net.folder,showWarnings=FALSE)
	
	net.file <- paste(net.folder,"/network.graphml",sep="")
	g <- barabasi.game(n=1000,m=3,directed=FALSE)
	write.graph(g,net.file,format="graphml")
}
