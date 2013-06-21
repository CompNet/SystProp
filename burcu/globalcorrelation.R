library(igraph)
path <- "/home/gorman/Desktop/Networks/"

file.counter <- 1
file.number <- 2
dosyalar=c(1,10,100,101,106,107,108,109,110,112,113,114,115,119,123,124,125,126,127,128,129,13,130,131,132,133,134,135,136,137,138,139,14,140,146,147,148,15,152,153,154,155,159,166,167,168,17,171,172,173,174,175,178,179,180,181,182,2,20,201,202,203,204,205,206,207,208,209,21,210,211,212,213,214,215,216,227,228,229,23,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,26,29,3,30,31,32,33,34,35,36,38,41,43,44,45,46,47,48,49,5,50,51,52,53,54,55,6,7,73,75,76,77,78,79,8,80,81,82,83,84,9,90,91,92,93,94,95,96,97,98,99)
colmatrix <- matrix(ncol=7,nrow=length(dosyalar))
j <- 1
for (file.counter in dosyalar)
{
	m <- read.table((paste(paste(path,"/",file.counter,sep = ""),"/globals.txt", sep= "")))
	colmatrix[j,1] <- m[,1]	
	colmatrix[j,2] <- m[,2]
	colmatrix[j,3] <- m[,3]
	colmatrix[j,4] <- m[,4]
	colmatrix[j,5] <- m[,5]
	colmatrix[j,6] <- m[,6]
	colmatrix[j,7] <- m[,7]
	j <- j +1
}	

res <-  matrix(ncol=7,nrow=7)
for(i in 1:7)
{
	for(j in 1:7)
	{
		res[i,j] <- cor(colmatrix[,i],colmatrix[,j],method="pearson")
	
	}

}
