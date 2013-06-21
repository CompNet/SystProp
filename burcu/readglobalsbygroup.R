library(igraph)
path <- "C:/Users/bkantarci/Desktop/networks/networks/"

file.counter <- 1
file.number <- 2
#dosyalar=c(1,10,100,101,106,107,108,109,110,112,113,114,115,119,123,124,125,126,127,128,129,13,130,131,132,133,134,135,136,137,138,139,14,140,146,147,148,15,152,153,154,155,159,166,167,168,17,171,172,173,174,175,178,179,180,181,182,2,20,201,202,203,204,205,206,207,208,209,21,210,211,212,213,214,215,216,227,228,229,23,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,26,29,3,30,31,32,33,34,35,36,38,41,43,44,45,46,47,48,49,5,50,51,52,53,54,55,6,7,73,75,76,77,78,79,8,80,81,82,83,84,9,90,91,92,93,94,95,96,97,98,99)
dosyalar=c(232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,1,229,44,181,214,51,52,53)
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

bio.density <- c(colmatrix[,1])
bio.diameter <- c(colmatrix[,2])
bio.transitivity <- c(colmatrix[,3])
bio.modularity <- c(colmatrix[,4])
bio.average.distance <- c(colmatrix[,5])
bio.average.degree <- c(colmatrix[,6])
bio.radius <- c(colmatrix[,7])


file.counter <- 1
file.number <- 2
#dosyalar=c(1,10,100,101,106,107,108,109,110,112,113,114,115,119,123,124,125,126,127,128,129,13,130,131,132,133,134,135,136,137,138,139,14,140,146,147,148,15,152,153,154,155,159,166,167,168,17,171,172,173,174,175,178,179,180,181,182,2,20,201,202,203,204,205,206,207,208,209,21,210,211,212,213,214,215,216,227,228,229,23,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,26,29,3,30,31,32,33,34,35,36,38,41,43,44,45,46,47,48,49,5,50,51,52,53,54,55,6,7,73,75,76,77,78,79,8,80,81,82,83,84,9,90,91,92,93,94,95,96,97,98,99)
dosyalar=c(101,153,17,171,172,173,205,206,207,208,21,210,212)
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

citation.density <- c(colmatrix[,1])
citation.diameter <- c(colmatrix[,2])
citation.transitivity <- c(colmatrix[,3])
citation.modularity <- c(colmatrix[,4])
citation.average.distance <- c(colmatrix[,5])
citation.average.degree <- c(colmatrix[,6])
citation.radius <- c(colmatrix[,7])



file.counter <- 1
file.number <- 2
#dosyalar=c(1,10,100,101,106,107,108,109,110,112,113,114,115,119,123,124,125,126,127,128,129,13,130,131,132,133,134,135,136,137,138,139,14,140,146,147,148,15,152,153,154,155,159,166,167,168,17,171,172,173,174,175,178,179,180,181,182,2,20,201,202,203,204,205,206,207,208,209,21,210,211,212,213,214,215,216,227,228,229,23,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,26,29,3,30,31,32,33,34,35,36,38,41,43,44,45,46,47,48,49,5,50,51,52,53,54,55,6,7,73,75,76,77,78,79,8,80,81,82,83,84,9,90,91,92,93,94,95,96,97,98,99)
dosyalar=c(216,227,45,48,50,29,31,32,34,36,54,55,146,35)
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

compt.density <- c(colmatrix[,1])
compt.diameter <- c(colmatrix[,2])
compt.transitivity <- c(colmatrix[,3])
compt.modularity <- c(colmatrix[,4])
compt.average.distance <- c(colmatrix[,5])
compt.average.degree <- c(colmatrix[,6])
compt.radius <- c(colmatrix[,7])

file.counter <- 1
file.number <- 2
#dosyalar=c(1,10,100,101,106,107,108,109,110,112,113,114,115,119,123,124,125,126,127,128,129,13,130,131,132,133,134,135,136,137,138,139,14,140,146,147,148,15,152,153,154,155,159,166,167,168,17,171,172,173,174,175,178,179,180,181,182,2,20,201,202,203,204,205,206,207,208,209,21,210,211,212,213,214,215,216,227,228,229,23,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,26,29,3,30,31,32,33,34,35,36,38,41,43,44,45,46,47,48,49,5,50,51,52,53,54,55,6,7,73,75,76,77,78,79,8,80,81,82,83,84,9,90,91,92,93,94,95,96,97,98,99)
dosyalar=c(119,123,124,125,126,127,128,129,130,132,133,134,135,136,137,138,139,152)
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

eco.density <- c(colmatrix[,1])
eco.diameter <- c(colmatrix[,2])
eco.transitivity <- c(colmatrix[,3])
eco.modularity <- c(colmatrix[,4])
eco.average.distance <- c(colmatrix[,5])
eco.average.degree <- c(colmatrix[,6])
eco.radius <- c(colmatrix[,7])



file.counter <- 1
file.number <- 2
#dosyalar=c(1,10,100,101,106,107,108,109,110,112,113,114,115,119,123,124,125,126,127,128,129,13,130,131,132,133,134,135,136,137,138,139,14,140,146,147,148,15,152,153,154,155,159,166,167,168,17,171,172,173,174,175,178,179,180,181,182,2,20,201,202,203,204,205,206,207,208,209,21,210,211,212,213,214,215,216,227,228,229,23,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,26,29,3,30,31,32,33,34,35,36,38,41,43,44,45,46,47,48,49,5,50,51,52,53,54,55,6,7,73,75,76,77,78,79,8,80,81,82,83,84,9,90,91,92,93,94,95,96,97,98,99)
dosyalar=c(215,8,9,13,97)
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

engi.density <- c(colmatrix[,1])
engi.diameter <- c(colmatrix[,2])
engi.transitivity <- c(colmatrix[,3])
engi.modularity <- c(colmatrix[,4])
engi.average.distance <- c(colmatrix[,5])
engi.average.degree <- c(colmatrix[,6])
engi.radius <- c(colmatrix[,7])



file.counter <- 1
file.number <- 2
#dosyalar=c(1,10,100,101,106,107,108,109,110,112,113,114,115,119,123,124,125,126,127,128,129,13,130,131,132,133,134,135,136,137,138,139,14,140,146,147,148,15,152,153,154,155,159,166,167,168,17,171,172,173,174,175,178,179,180,181,182,2,20,201,202,203,204,205,206,207,208,209,21,210,211,212,213,214,215,216,227,228,229,23,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,26,29,3,30,31,32,33,34,35,36,38,41,43,44,45,46,47,48,49,5,50,51,52,53,54,55,6,7,73,75,76,77,78,79,8,80,81,82,83,84,9,90,91,92,93,94,95,96,97,98,99)
dosyalar=c(10,100,106,107,108,109,110,155,159,166,2,7,81,82,84,98,99)
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

person.density <- c(colmatrix[,1])
person.diameter <- c(colmatrix[,2])
person.transitivity <- c(colmatrix[,3])
person.modularity <- c(colmatrix[,4])
person.average.distance <- c(colmatrix[,5])
person.average.degree <- c(colmatrix[,6])
person.radius <- c(colmatrix[,7])


file.counter <- 1
file.number <- 2
#dosyalar=c(1,10,100,101,106,107,108,109,110,112,113,114,115,119,123,124,125,126,127,128,129,13,130,131,132,133,134,135,136,137,138,139,14,140,146,147,148,15,152,153,154,155,159,166,167,168,17,171,172,173,174,175,178,179,180,181,182,2,20,201,202,203,204,205,206,207,208,209,21,210,211,212,213,214,215,216,227,228,229,23,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,26,29,3,30,31,32,33,34,35,36,38,41,43,44,45,46,47,48,49,5,50,51,52,53,54,55,6,7,73,75,76,77,78,79,8,80,81,82,83,84,9,90,91,92,93,94,95,96,97,98,99)
dosyalar=c(112,113,114,115,178,179,201,204,180,213,3,33,38,43,90)
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
social.density <- c(colmatrix[,1])
social.diameter <- c(colmatrix[,2])
social.transitivity <- c(colmatrix[,3])
social.modularity <- c(colmatrix[,4])
social.average.distance <- c(colmatrix[,5])
social.average.degree <- c(colmatrix[,6])
social.radius <- c(colmatrix[,7])






