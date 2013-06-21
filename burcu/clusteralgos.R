
n <- read.table("C:/Users/bkantarci/Desktop/networks/distmatrix.txt",sep= "")
	
library(cluster)
ag <- agnes(n, diss = TRUE, stand = FALSE ,method = "complete")
si3 <- silhouette(cutree(ag, k = 2), n)
max <- 2
m <- summary(si3)
prev <- m$avg.width
for(i in 3:152)
{
	si3 <- silhouette(cutree(ag, k = i), n)
	if(summary(si3)$avg.width > prev)
	{
		max <- i
		prev <- summary(si3)$avg.width
	}
}
maxag <- max




dv <- diana(n,diss = TRUE, stand = FALSE)  
si4 <- silhouette(cutree(dv, k = 2), n)
max <- 2
m <- summary(si4)
prev <- m$avg.width
for(i in 2:151)
{
	si4 <- silhouette(cutree(dv, k = i), n)
	if(summary(si4)$avg.width > prev)
	{
		max <- i
		prev <- summary(si4)$avg.width
	}
}
maxdiana <- max





library(fpc)
dbs <- dbscan(n, 0.1)    
si5 <- silhouette(dbs$cluster, n)
max <- 0.1
m <- summary(si5)
prev <- m$avg.width
v <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
for(i in v)
{
	dbs <- dbscan(n, i)
	si5 <- silhouette(dbs$cluster, n)
	if(summary(si5)$avg.width > prev)
	{
		max <- i
		prev <- summary(si5)$avg.width
	}
}

maxdbscan <- max






library(stats)
pam<- pam(n, 2) 
si6 <- silhouette(pam$cluster, n)
max <- 1
m <- summary(si6)
prev <- m$avg.width
for(i in 2:151)
{
	pam<- pam(n, 2)
	si6 <- silhouette(pam$cluster, n)
	if(summary(si6)$avg.width > prev)
	{
		max <- i
		prev <- summary(si6)$avg.width
	}
}
maxpam <- pam




library(mclust)
ag <- agnes(n, diss = TRUE, stand = FALSE ,method = "complete") 
si3 <- silhouette(cutree(ag, k = maxag), n)
dv <- diana(n,diss = TRUE, stand = FALSE)  
si4 <- silhouette(cutree(dv, k = maxdiana), n)
dbs <- dbscan(n, maxdbscan)    
si5 <- silhouette(dbs$cluster, n)
pam<- pam(n, maxpam)
si6 <- silhouette(pam$cluster, n)
v <- c(si3,si4,si5,si6)
mat <- matrix(ncol = 4, nrow= 4)


mat[1,2] <- adjustedRandIndex(si3[,1], si4[,1])
mat[1,3] <- adjustedRandIndex(si3[,1], si5[,1])
mat[1,4] <- adjustedRandIndex(si3[,1], si6[,1])
mat[2,3] <- adjustedRandIndex(si4[,1], si5[,1])
mat[2,4] <- adjustedRandIndex(si4[,1], si6[,1])
mat[3,4] <- adjustedRandIndex(si5[,1], si6[,1])

mat






