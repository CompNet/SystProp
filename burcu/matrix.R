library(igraph)

path <- "/home/gorman/Desktop/Networks/"
file.count <- 1
file.number <- 2

dosyalar=c(1,10,100,101,106,107,108,109,110,112,113,114,115,119,123,124,125,126,127,128,129,13,130,131,132,133,134,135,136,137,138,139,14,140,146,147,148,15,152,153,154,155,159,166,167,168,17,171,172,173,174,175,178,179,180,181,182,2,20,201,202,203,204,205,206,207,208,209,21,210,211,212,213,214,215,216,227,228,229,23,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,26,29,3,30,31,32,33,34,35,36,38,41,43,44,45,46,47,48,49,5,50,51,52,53,54,55,6,7,73,75,76,77,78,79,8,80,81,82,83,84,9,90,91,92,93,94,95,96,97,98,99)
j <- 1
colmatrix <- matrix(ncol=6,nrow=length(dosyalar))
for (file.counter in dosyalar)
{	

	m <- read.table((paste(paste(path,file.counter,sep = ""),"/globals.txt", sep= "")))
	colmatrix[j,1] <- m[1,1]	
	colmatrix[j,2] <- m[1,2] 
	colmatrix[j,3] <- m[1,3]   
	colmatrix[j,4] <- m[1,4]
	colmatrix[j,5] <- m[1,5]
	colmatrix[j,6] <- m[1,6]
	j <- j+1
}	


res <- matrix(ncol=6,nrow=length(dosyalar))
for(i in 1:length(dosyalar))
{
	
	for(j in 1:6)
	{
		v <- colmatrix[,j]
		res[i,j] <- (colmatrix[i,j]-min(v))/(max(v)-min(v))	
	}
}

resultglobal <- matrix(ncol=length(dosyalar),nrow=length(dosyalar))
for(i in 1:length(dosyalar))
{
	for(j in 1:length(dosyalar))
	{
		resultglobal[i,j] <- dist(rbind(res[i,], res[j,]), method = "manhattan")
	
	}	
}




library(emdist)

dosyalar=c(1,10,100,101,106,107,108,109,110,112,113,114,115,119,123,124,125,126,127,128,129,13,130,131,132,133,134,135,136,137,138,139,14,140,146,147,148,15,152,153,154,155,159,166,167,168,17,171,172,173,174,175,178,179,180,181,182,2,20,201,202,203,204,205,206,207,208,209,21,210,211,212,213,214,215,216,227,228,229,23,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,26,29,3,30,31,32,33,34,35,36,38,41,43,44,45,46,47,48,49,5,50,51,52,53,54,55,6,7,73,75,76,77,78,79,8,80,81,82,83,84,9,90,91,92,93,94,95,96,97,98,99)


reslocal <- matrix(ncol=length(dosyalar),nrow=length(dosyalar))
i <- 1
j<- 1
for (file.counter in dosyalar)
{	

	dosyalar2 <- dosyalar
	m <- read.table((paste(paste(path,file.counter,sep = ""),"/localnodenormalized.txt", sep= "")),header=T,sep= ";")
	
	m.degree<- cbind(matrix(m[,1],ncol=1,nrow=20),1:length(m[,1]))
if(!all(is.nan(m.degree)==FALSE)) print("A")
	m.betweenness.centrality <- cbind(matrix(m[,2] ,ncol=1,nrow=20),1:length(m[,1]))
if(!all(is.nan(m.betweenness.centrality)==FALSE)) print("B")
	m.closeness.centrality <- cbind(matrix(m[,3],ncol=1,nrow=20),1:length(m[,1]))
if(!all(is.nan(m.closeness.centrality)==FALSE)) print("C")
	m.local.transitivity <- cbind(matrix(m[,4],ncol=1,nrow=20),1:length(m[,1]))
if(!all(is.nan(m.local.transitivity)==FALSE)) print("D")
	m.degree.centrality <-  cbind(matrix(m[,5],ncol=1,nrow=20),1:length(m[,1]))
if(!all(is.nan(m.degree.centrality)==FALSE)) print("E")
	m.eccentricity  <- cbind(matrix(m[,6],ncol=1,nrow=20),1:length(m[,1]))
if(!all(is.nan(m.eccentricity)==FALSE)) print("F")
	m.edge.betweenness <- cbind(matrix(m[,7],ncol=1,nrow=20),1:length(m[,1]))

	for (file.counter2 in dosyalar)
	{
		n <- read.table((paste(paste(path,file.counter2,sep = ""),"/localnodenormalized.txt", sep= "")),header=T,sep= ";")
	
		n.degree<-cbind(matrix(n[,1],ncol=1,nrow=20),1:length(m[,1]))
		n.betweenness.centrality <- cbind(matrix(n[,2],ncol=1,nrow=20) ,1:length(m[,1]))
		n.closeness.centrality <- cbind(matrix(n[,3],ncol=1,nrow=20),1:length(m[,1]))
		n.local.transitivity <- cbind(matrix(n[,4],ncol=1,nrow=20),1:length(m[,1]))
		n.degree.centrality <-  cbind(matrix(n[,5],ncol=1,nrow=20),1:length(m[,1]))
		n.eccentricity  <- cbind(matrix(n[,6],ncol=1,nrow=20),1:length(m[,1]))
		n.edge.betweenness <- cbind(matrix(n[,7],ncol=1,nrow=20),1:length(m[,1]))
		
		a <- emd(m.degree, n.degree, dist="manhattan")
		if(all(m.degree[,1]==0) && all(n.degree[,1]==0))
		{
			a <- 0
		}
		if ((all(m.degree[,1]==0) && !all(n.degree[,1]==0)) || (!all(m.degree[,1]==0) && all(n.degree[,1]==0)))
		{
			a <- 1
		}
		
		
		b <- emd(m.betweenness.centrality, n.betweenness.centrality, dist="manhattan")
		if(all(m.betweenness.centrality[,1]==0) && all(n.betweenness.centrality[,1]==0))
		{	
			b <- 0
		}	
		if ((all(m.betweenness.centrality[,1]==0) && !all(n.betweenness.centrality[,1]==0)) || (!all(m.betweenness.centrality[,1]==0) && all(n.betweenness.centrality[,1]==0)))
		{
			b <- 1
		}	
		
		
		c <- emd(m.closeness.centrality, n.closeness.centrality, dist="manhattan")
		if(all(m.closeness.centrality[,1]==0) && all(m.closeness.centrality[,1]==0))
		{	
			c <- 0
		}

		if ((all(m.closeness.centrality[,1]==0) && !all(n.closeness.centrality[,1]==0)) || (!all(m.closeness.centrality[,1]==0) && all(n.closeness.centrality[,1]==0)))
		{	
			c <- 1
		}	
		
		
		d <- emd(m.degree.centrality, n.degree.centrality, dist="manhattan")	
		if(all(m.degree.centrality[,1]==0) && all(m.degree.centrality[,1]==0))
		{	d <- 0
		}
		if ((all(m.degree.centrality[,1]==0) && !all(n.degree.centrality[,1]==0)) || (!all(m.degree.centrality[,1]==0) && all(n.degree.centrality[,1]==0)))
		{	d <- 1
		}

	
		e <- emd(m.local.transitivity, n.local.transitivity, dist="manhattan")	
		if(all(m.local.transitivity[,1]==0) && all(m.local.transitivity[,1]==0))
		{	
			e <- 0
		}
		if ((all(m.local.transitivity[,1]==0) && !all(n.local.transitivity[,1]==0)) || (!all(m.local.transitivity[,1]==0) && all(n.local.transitivity[,1]==0)))
		{
			e <- 1
		}	
		
		f <- emd(m.eccentricity, n.eccentricity,  dist="manhattan")		
		if(all(m.eccentricity[,1]==0) && all(m.eccentricity[,1]==0))
		{
			f <- 0	
		}
		if ((all(m.eccentricity[,1]==0) && !all(m.eccentricity[,1]==0)) || (!all(m.eccentricity[,1]==0) && all(n.eccentricity[,1]==0)))
		{
			f <- 1
		}
	
		reslocal[i,j] <- (a+b+c+d+e+f)/(length(m.degree)-1)
		reslocal[j,i] <- (a+b+c+d+e+f)/(length(m.degree)-1)
		j <- j+1
		
	}	
	i <- i+1
	j <- 1
	
}
print(file.counter2)
print(file.counter)


result <- matrix(ncol=length(dosyalar),nrow=length(dosyalar))

result <- resultglobal + reslocal
result <- result / 13


colnames(result) <- c("1","10","100","101","106","107","108","109","110","112","113","114","115","119","123","124","125","126","127","128","129","13","130","131","132","133","134","135","136","137","138","139","14","140","146","147","148","15","152","153","154","155","159","166","167","168","17","171","172","173","174","175","178","179","180","181","182","2","20","201","202","203","204","205","206","207","208","209","21","210","211","212","213","214","215","216","227","228","229","23","230","231","232","233","234","235","236","237","238","239","240","241","242","243","244","245","246","247","248","249","250","26","29","3","30","31","32","33","34","35","36","38","41","43","44","45","46","47","48","49","5","50","51","52","53","54","55","6","7","73","75","76","77","78","79","8","80","81","82","83","84","9","90","91","92","93","94","95","96","97","98","99")
	
write.table(result,paste(path,"/distmatrix.txt",sep=""))
	
	

