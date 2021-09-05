# Iteratively applies the rewiring functions
# to certain network, processing transitivity
# and average distance each time, in order
# to check how the theoretical estimations
# compare to the experimental ones (obtained
# through rewiring).
#
# Vincent Labatut 02/2015
# 
# setwd("~/eclipse/workspaces/Networks/SystProp")
# setwd("D:/eclipse/workspaces/Networks/SystProp")
#
# source("preprocess/test-rewiring.R")
###################################################

#################################
# load dependencies
#################################
library(igraph)

library(compiler)	# activate compilation
enableJIT(3)

source("tools/rewire-network.R")


#################################
# setup parameters
#################################
measure <- "TRANSITIVITY"	# AVG_DIST TRANSITIVITY
max.iter <- 10 #25	#100	# maximal number of rewiring iterations
os <- .Platform$OS.type
if(os=="windows")
{	data.folder <- "q:/networks/_cleaned/"
#	data.folder <- "c:/Temp/"
#	folders <- 1:5
	# all possible folders, ordered by file size
	folders <- c(
#		107,34,160,108,571,166,159,568,111,113,569,112,570,
#		585,168,164,491,167,152,586,538,177,539,581,580,178,
#		583,582,584,576,604,599,531,502,595,530,598,605,606,
#		100,601,557,559,558,607,603,602,536,535,546,600,610,
#		537,562,596,563,597,609,578,593,93,589,591,567,50,564,
#		560,579,608,532,533,561,594,524,556,96,503,588,522,
#		555,574,29,566,114,286,565,90,590,554,115,39,86,523,
#		592,511,575,501,500,52,552,551,553,81,508,587,101,545,
#		529,547,506,573,507,572,509,550,526,525,549,527,510,
#		505,520,162,528,57,95,577,548,118,516,519,542,543,131,
#		544,534,521,517,540,514,91,515,518,4,5,541,513,145,512,
#		142,92,490,51,110,169,30,94,504,31,20,127,106,414,283,
#		3,117,124,499,282,44,123,10,98,85,33,84,40,83,97,204,2,
#		73,120,119,496,116,165,121,130,122,128,451,35,46,498,42,
#		89,279,141,87,153,47,179,228,230,132,129,495,180,170,45,
#		37,26,497,88,103,242,136,135,236,38,488,255,415,243,494,
#		49,266,56,174,276,252,271,489,234,137,138,125,161,194,
#		82,12,460,196,226,198,289,48,61,259,356,9,235,156,383,384,
#		477,261,263,459,357,75,139,140,144,270,241,248,105,240,
#		250,60,232,262,258,247,486,251,254,231,349,158,257,268,
#		253,8,264,492,269,32,215,481,134,126,133,171,287,239,181,
#		182,358,475,175,217,233,143,227,238,350,277,212,273,157,
#		249,244,338,493,256,325,326,267,148,7,348,364,347,19,176,
#		208,265,416,15,173,319,260,6,216,346,188,237,245,76,246,
#		272,163,411,433,340,104,378,331,476,352,487,202,147,210,
#		151,155,351,278,1,102,214,229,207,285,379,77,432,353,146,
#		13,473,410,195,206,21,288,23,197,199,203,154,78,382,14,354,
#		25,213,337,24,339,187,201,11,43,205,80,225,417,336,79,22,
#		368,332,189,452,62,63,64,53,280,211,74,403,426,172,485,404,
#		183,69,70,200,402,296,437,360,209,436,361,423,381,467,362,
#		222,463,464,465,281,28,483,363,380,218,413,324,186,366,322,
#		193,184,457,484,221,342,41,224,284,223,292,294,291,67,68,377,
#		36,275,71,185,290,16,367,17,431,27,369,370,371,304,334,391,
#		318,611,65,66,311,312,315,479,313,390,372,482,301,192,299,
#		373,478,321,343,317,109,314,344,345,293,385,220,428,412,59,
#		462,355,453,472,323,333,302,480,341,149,320,376,190,466,307,
#		365,295,425,72,450,455,150,438,439,298,427,447,407,274,359,
#		398,399,400,401,395,421,468,422,406,405,303,434,454,300,397,
#		394,429,430,393,388,335,435,396,469,470,389,297,18,58,420,55,
#		392,458,99,309,449,375,219,444,374,445,316,418,424,443,446,
#		54,408,441,308,471,409,305,386,419,306,442,387,461,329,456,
#		440,191,330,448,327,474,328,310
	)
	# remove missing files (not converted yet)
	folders <- folders[!(folders %in% c(182,312,326,399,400,401,439,464,465))]
	# remove large files to speed up calculations
#	folders <- folders[!(folders %in% c(18,54:55,58,71:72,99,109,149:150,182,190:192,200,218:221,274:275,293:294:296,298:305,307:318,320,323,326:330,332:333,335,341:343:345,358:359,365,367,369,371:372,374:377,385:387:401,405,406,408,409,412,413,418,419,427,429:431,434:435,438:450,453:456,458,461,463:467,470,472,474))]
	# remove multipartite networks which could not be projected
	folders <- folders[!(folders %in% c(308,310,330))]
}else
{	
#	data.folder <- "/var/data/networks/"
#	data.folder <- "/media/Samsung/networks/_cleaned/"
	data.folder <- "/media/vlabatut/Samsung/networks/_cleaned/"
	# all possible folders
	folders <- c(
#		107,34,160,108,571,166,159,568,111,113,569,112,570,
#		585,168,164,491,167,152,586,538,177,539,581,580,178,
#		583,582,584,576,604,599,531,502,595,530,598,605,606,
#		100,601,557,559,558,607,603,602,536,535,546,600,610,
#		537,562,596,563,597,609,578,593,93,589,591,567,50,564,
#		560,579,608,532,533,561,594,524,556,96,503,588,522,
#		555,574,29,566,114,286,565,90,590,554,115,39,86,523,
#		592,511,575,501,500,52,552,551,553,81,508,587,101,545,
#		529,547,506,573,507,572,509,550,526,525,549,527,510,
#		505,520,162,528,57,95,577,548,118,516,519,542,543,131,
#		544,534,521,517,540,514,91,515,518,4,5,541,513,145,512,
#		142,92,490,51,110,169,30,94,504,31,20,127,106,414,283,
#		3,117,124,499,282,44,123,10,98,85,33,84,40,83,97,204,2,
#		73,120,119,496,116,165,121,130,122,128,451,35,46,498,42,
#		89,279,141,87,153,47,179,228,230,132,129,495,180,170,45,
#		37,26,497,88,103,242,136,135,236,38,488,255,415,243,494,
#		49,266,56,174,276,252,271,489,234,137,138,125,161,194,
#		82,12,460,196,226,198,289,48,61,259,356,9,235,156,383,384,
#		477,261,263,459,357,75,139,140,144,270,241,248,105,240,
#		250,60,232,262,258,247,486,251,254,231,349,158,257,268,
#		253,8,264,492,269,32,215,481,134,126,133,171,287,239,181,
#		182,358,475,175,217,233,143,227,238,350,277,212,273,157,
#		249,244,338,493,256,325,326,267,148,7,348,364,347,19,176,
#		208,265,416,15,173,319,260,6,216,346,188,237,245,76,246,
#		272,163,411,433,340,104,378,331,476,352,487,202,147,210,
#		151,155,351,278,1,102,214,229,207,285,379,77,432,353,146,
#		13,473,410,195,206,21,288,23,197,199,203,154,78,382,14,354,
#		25,213,337,24,339,187,201,11,43,205,80,225,417,336,
#		79,22,368,332,189,452,62,63,64,53,280,211,74,403,426,172,
#		485,404,183,69,70,200,402,
		296,437,360,209,436,361,423,381,467,362,
		222,463,464,465,281,28,483,363,380,218,413,324,186,366,322,
		193,184,457,484,221,342,41,224,284,223,292,294,291,67,68,377,
		36,275,71,185,290,16,367,17,431,27,369,370,371,304,334,391,
		318,611,65,66,311,312,315,479,313,390,372,482,301,192,299,
		373,478,321,343,317,109,314,344,345,293,385,220,428,412,59,
		462,355,453,472,323,333,302,480,341,149,320,376,190,466,307,
		365,295,425,72,450,455,150,438,439,298,427,447,407,274,359,
		398,399,400,401,395,421,468,422,406,405,303,434,454,300,397,
		394,429,430,393,388,335,435,396,469,470,389,297,18,58,420,55,
		392,458,99,309,449,375,219,444,374,445,316
#		#418,
#		424,443,446,
#		54,408,441,308,471,409,305,386,419,306,442,387,461,329,456,
#		440,191,330,448,327,		
#		#474,
#		328,310,
	)
	# remove missing files (not converted yet)
	folders <- folders[!(folders %in% c(182,312,326,399,400,401,439,464,465))]
	# remove large files to speed up calculations
#	folders <- folders[!(folders %in% c(18,54:55,58,71:72,99,109,149:150,182,190:192,200,218:221,274:275,293:294:296,298:305,307:318,320,323,326:330,332:333,335,341:343:345,358:359,365,367,369,371:372,374:377,385:387:401,405,406,408,409,412,413,418,419,427,429:431,434:435,438:450,453:456,458,461,463:467,470,472,474))]
}
out.folder <- paste(data.folder,"_rewired/",sep="")
dir.create(out.folder,showWarnings=FALSE)


#################################
# get existing folder list
#################################
paths <- list.files(path=data.folder, pattern="\\d{4}\\..*", all.files=TRUE, full.names=FALSE, recursive=FALSE, ignore.case = FALSE)
paths <- sort(paths)


#################################
# clean each network
#################################
j <- 1
for(f in folders)
{	# check for file name existence
	filename <- NA
	prefix0 <- sprintf("%04d",f)
	i <- 1
	while(is.na(filename) && i<=length(paths))
	{	prefix1 <- substr(paths[i], 1, 4)
		if(prefix0==prefix1)
			filename <- paths[i]
		i <- i + 1
	}
	
	# no network available
	if(is.na(filename))
		cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] WARNING: No network could be found for ",filename,"\n",sep="")
	
	# network available
	else
	{	start.time <- Sys.time();
		
		# load network
		net.folder <- paste(data.folder,filename,"/",sep="")
		data.file <- paste(net.folder,"network.graphml",sep="")
		format <- "graphml"
		if(!file.exists(data.file))
		{	data.file <- paste(net.folder,"network.net",sep="")
			format <- "pajek"
		}
		if(!file.exists(data.file))
		{	cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] WARNING: No network file could be found for ",filename,"\n",sep="")
			format <- NULL
		}else
		{	cat("[",format(start.time,"%a %d %b %Y %X"),"] Loading network #",f,": '",data.file,"'\n",sep="")
			g <- read.graph(data.file,format=format)
			end.time <- Sys.time();
			total.time <- end.time - start.time;
			cat("[",format(end.time,"%a %d %b %Y %X"),"] Loading (",vcount(g)," nodes and ",ecount(g)," links) completed in ",format(total.time),"\n",sep="")
			
			# process measure in the original network
			if(measure=="AVG_DIST")
				values <- average.path.length(graph=g, directed=FALSE, unconnected=TRUE)
			else if(measure=="TRANSITIVITY")
				values <- transitivity(graph=g, type="globalundirected", weights=NULL, isolates="zero")
			cat("[",format(Sys.time(),"%a %d %b %Y %X"),"] Values: ",values,sep="")
			
			# rewire multiple times
			for(i in 1:max.iter)
			{	# process the measure after each rewiring
				if(measure=="AVG_DIST")
				{	g <- randomize.network(g, iterations=1)
					value <- average.path.length(graph=g, directed=FALSE, unconnected=TRUE)
				}
				else if(measure=="TRANSITIVITY")
				{	g <- latticize.network(g, iterations=1)
					value <- transitivity(graph=g, type="globalundirected", weights=NULL, isolates="zero")
				}
				values <- c(values,value)
				cat(",",value)				
				
				# record the resulting series of measures
				net.folder <- paste(out.folder,filename,"/",sep="")
				dir.create(net.folder,showWarnings=FALSE)
				data.file <- paste(net.folder,measure,".txt",sep="")
				write.table(x=values,file=data.file,col.names=FALSE,row.names=FALSE)
				
				# generate PDF
				if(measure=="AVG_DIST")
					ylim <- c(0,max(values))
				else if(measure=="TRANSITIVITY")
					ylim <- c(0,1)
				plot.file <- paste(net.folder,measure,".PDF",sep="")
				pdf(file=plot.file,bg="white")
					plot(0:i,values,xlab="Iteration",ylab=measure,ylim=ylim)
				dev.off()
			}
			cat("\n")
		}
		
		end.time <- Sys.time();
		total.time <- end.time - start.time;
		cat("[",format(end.time,"%a %d %b %Y %X"),"] Processing completed in ",format(total.time),"\n",sep="")
	}
}
