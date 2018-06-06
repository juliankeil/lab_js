# Read in SQL-Database from Lab.js
# The current R-Script is work in Progress.
# It may contain errors and is far from finished
# Questions and Comments should be directed to julian.keil@gmail.com

# 0. Load packages
library("RSQLite")
library("DBI")
library("data.table")
library("ggplot2")

# 1. Set Working Directory
setwd('E:/LabJS/Experiments/MultiRace/Live/data')

# 2. Connect to Database
con = dbConnect(dbDriver("SQLite"), dbname="data_20180605.sqlite")

# 3. Get a List of all tables
alltables = dbListTables(con)

# 3. Select Column
p1 <- dbSendQuery(con, 'SELECT data FROM labjs')
data <- fetch(p1, n = -1)
p2 <- dbSendQuery(con, 'SELECT session FROM labjs')
session <- fetch(p2, n = -1)
dbDisconnect(con)

# 4. Loop through messy character data
# 4.1. Find breaks between participants
starti = NULL
Meta = NULL
j = 0

for (i in 1:nrow(data)) {
	# Unwrap the text data in the SQL-database
	sp1<-gsub('"|[{]|[}]','', data[i,])
	sp2<-gsub('\\\\','', sp1)
	sp3<-unlist(strsplit(sp2,"[,]"))
	sp4<-unlist(strsplit(sp3,"[:]"))
	
	# Extract Meta Data
	# First Intro Screen
	if (sp4[1]=="meta") {
		j = j+1
		Meta$Session[j]<-session[i,]
		Meta$PC[j]<-sp4[9] # What kind of computer was used? 
		Meta$ht[j]<-sp4[27] # How high was the experiment display
		Meta$wd[j]<-sp4[29] # How wide was the experiment display
		tmp<-c(sp4[49],sp4[50]) # At what date and time was the experiment performed
		Meta$ts[j]<-paste(tmp,collapse="")
	}
	# Consent Screen
	if (sp4[1]=="f_vname") {
		tmp<-c(sp4[2],sp4[4]) # What are the participants initials
		Meta$name[j]<-paste(tmp,collapse="")
	}
	# Demographics Screen
	if (sp4[1]=="Geschlecht") {
		Meta$gend[j]<-sp4[2] # Gender
		Meta$birth[j]<-sp4[4] # Birthday
		Meta$sick[j]<-sp4[6] # Neurological diseases
		Meta$drug[j]<-sp4[8] # Taking drugs
		Meta$sight[j]<-sp4[10] # Visual impairments
		Meta$hear[j]<-sp4[12] # Auditory impairments
		starti[j]<-i
	}
	# Final Feedback Screen
	if (sp4[1]=="visual") {
		Meta$fb_v[j]<-sp4[2] # Were all visual stimuli seen
		Meta$fb_a[j]<-sp4[4] # Were all auditory stimuli heard
		Meta$fb_del[j]<-sp4[6] # Was there a delay between stimuli
		Meta$fb_brow[j]<-sp4[8] # What kind of browser was used
	}
	# Open Feedback Question
	if (sp4[1]=="feedback") {
	  Meta$fb_open[j]<-sp4[2] # Open Text for Feedback
	}
}

# 4.2. Now read in the data for one session
alldat <- list()
for (v in 1:length(starti)) {
	# Define where the actual data starts
	start <- starti[v]+1
	stop  <- nrow(data)
	
	# Account for the case of end-of-file
	if (is.na(stop)){
		stop<-nrow(data)
	}
	
	# Check again if the new start value is after the end of file
	if (!is.na(start) & start>nrow(data)){
		tmp<-data.frame()
	}	
	
	# If all is good, go ahead and count responses
	if (!is.na(start) & start<nrow(data)){
		# Preallocate Variables
		resp = NULL
		labl = NULL
		RT = NULL
		j = 0;
	
		# Actually read in the data	
		for (i in start:stop) {
		  # Check if we are within one session
		  if (session[i,]==Meta$Session[v]){
			# Unwrap the text data
			x<-gsub('"|[{]|[}]','', data[i,])
			y<-unlist(strsplit(x,"[,]"))
			z<-unlist(strsplit(y,"[:]"))
	
			# Extract the labels, responses and RTs
			if (z[1]=="response" & z[2]!="Weiter") {
				j<-j+1
				resp[j]<-z[2] # What was the response label -> trivial as there is only one
				labl[j]<-z[12] # What was the Trial label (A,V or VA)
				RT[j]<-as.numeric(z[26]) # Reaction Time
			}
			tmp<-data.frame(labl,resp,RT)
		  }
		}
	}
	
	# Store in common data frame
	alldat<-c(alldat,list(tmp))
}

# Now clean up the dataset
# 4.2.1. Remove empty elements
iszero = NULL
# Find non-zero datasets
for (v in 1:length(alldat)){
	iszero[v]<-nrow(alldat[[v]])>1
}
# Keep non-zero datasets
alldat<-alldat[iszero]
for (i in 1:length(Meta)){
	Meta[[i]]<-Meta[[i]][iszero]
}

# 4.2.2. Remove incomplete sets
isincomp = NULL
# Find datasets with less than 30 trials
for (v in 1:length(alldat)){
	isincomp[v]<-nrow(alldat[[v]])>30
}
# Keep only datasets with more than 30 trials
alldat<-alldat[isincomp]
for (i in 1:length(Meta)){
	Meta[[i]]<-Meta[[i]][isincomp]
}

# 4.2.3. Remove sets with negative values
isneg = NULL
# Find datasets with negative values
for (v in 1:length(alldat)){
  isneg[v]<-alldat[[v]][1,3]>0
}
# Keep only datasets with positive values
alldat<-alldat[isneg]
for (i in 1:length(Meta)){
  Meta[[i]]<-Meta[[i]][isneg]
}

# 4.2.3. Remove dulicated rows
dups<-!duplicated(alldat)
alldat<-alldat[dups]
for (i in 1:length(Meta)){
  Meta[[i]]<-Meta[[i]][dups]
}

# 5. Compute response time
A_RT <- vector("list", length(alldat))
V_RT <- vector("list", length(alldat))
AV_RT <- vector("list", length(alldat))

A_Mean <- NA
V_Mean <- NA
AV_Mean <- NA

for (v in 1:length(alldat)){
	tmp<-alldat[[v]]
	A_RT[[v]]<-(tmp[tmp$labl=="A",]$RT)
	V_RT[[v]]<-(tmp[tmp$labl=="V",]$RT)
	AV_RT[[v]]<-(tmp[tmp$labl=="VA",]$RT)
	
	A_Mean[v]<-mean(tmp[tmp$labl=="A",]$RT)
	V_Mean[v]<-mean(tmp[tmp$labl=="V",]$RT)
	AV_Mean[v]<-mean(tmp[tmp$labl=="VA",]$RT)
}

# 6. Plot
# Very bad and priliminary bar plots
error.bar<-function(x,y,upper,lower=upper,length=0.1) {
	arrows(x,y+upper,x,y-lower,angle=90,code=3,length=length)
}

barx<-barplot(c(mean(A_Mean),mean(V_Mean),mean(AV_Mean)),ylim=c(0,550),names.arg=c("Audio","Visual","Audiovisual"),ylab="Mean RT")
error.bar(barx,c(mean(A_Mean),mean(V_Mean),mean(AV_Mean)), c(sd(A_Mean)/sqrt(length(A_Mean)),sd(V_Mean)/sqrt(length(V_Mean)),sd(AV_Mean)/sqrt(length(AV_Mean))))

# 7. Anova
sub_v <- 1:length(A_Mean)
cond <- rep(1,length(A_Mean))
indat <- rbind(cbind(A_Mean,cond*1,sub_v),cbind(V_Mean,cond*2,sub_v),cbind(AV_Mean,cond*3,sub_v))
colnames(indat)[2] <- "cond"
indat <- as.data.frame(indat)
indat$cond <- as.factor(indat$cond)
indat$sub_v <- as.factor(indat$sub_v)

aov1 <- aov(A_Mean~cond+Error(sub_v/cond),data = indat)
summary(aov1)    
# 7.1 Post-Hoch T-Tests
A_V <- t.test(A_Mean,V_Mean,paired=TRUE,var.equal=TRUE)
A_AV <- t.test(A_Mean,AV_Mean,paired=TRUE,var.equal=TRUE)
AV_V <- t.test(AV_Mean,V_Mean,paired=TRUE,var.equal=TRUE)

# 8. Race Model
source('GetPercentile.R')
source('ties.R')
source('cdf.Ulrich.R')
source('probSpace.R')

A_p <- vector("list", length(A_Mean))
V_p <- vector("list", length(V_Mean))
AV_p <- vector("list", length(AV_Mean))
B_p <- vector("list", length(A_Mean))

psq <- probSpace(10); psq

for (v in 1:length(A_Mean)) {
  dfx <- ties(A_RT[[v]])
  dfy <- ties(V_RT[[v]])
  dfz <- ties(AV_RT[[v]])
  tmax <- max(A_RT[[v]],V_RT[[v]],AV_RT[[v]])
  
  gx <- cdf.ulrich(data=dfx, maximum=tmax)
  gy <- cdf.ulrich(data=dfy, maximum=tmax)
  gz <- cdf.ulrich(data=dfz, maximum=tmax)
  
  b <- gx + gy
  A_p[[v]] <- GetPercentile(psq, gx, tmax);
  V_p[[v]] <- GetPercentile(psq, gy, tmax);
  AV_p[[v]] <- GetPercentile(psq, gz, tmax);
  B_p[[v]] <- GetPercentile(psq, b, tmax);
}


# Plot
gdf <- data.frame(RT =c(xp,yp,zp,bp), Probability =rep(psq, 4),
                  Condition =rep(c("gx(t)", "gy(t)","gz(t)","gx(t)+gy(t)"), each=length(xp)))
panelf <- ggplot(gdf, aes(x = RT, y = Probability, group=Condition,
                          colour=Condition, shape=Condition)) + 
  geom_point() + geom_line() 
panelf + coord_cartesian(xlim = c(200, 500), ylim=c(-.01,1.01)) +
  theme(legend.position= c(.85, .20),  
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))
