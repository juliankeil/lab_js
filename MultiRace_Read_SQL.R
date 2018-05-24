# Read in SQL-Database from Lab.js
# The current R-Script is work in Progress.
# It may contain errors and is far from finished
# Questions and Comments should be directed to julian.keil@gmail.com

# 0. Load packages
library("RSQLite")
library("DBI")
library("data.table")

# 1. Set Working Directory
setwd('~/Documents/Arbeit/lab_js/Experiments/MultiRace/Data')

# 2. Connect to Database
con = dbConnect(dbDriver("SQLite"), dbname="data_20180524.sqlite")

# 3. Get a List of all tables
alltables = dbListTables(con)

# 3. Select Column
p1 <- dbSendQuery(con, 'SELECT data FROM labjs')
data <- fetch(p1, n = -1)
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
}

# 4.2. Now read in the data between breaks
alldat <- list()
for (v in 1:length(starti)) {
	# Define where the actual data starts
	start <- starti[v]+1
	stop  <- starti[v+1]-1
	
	# Account for the case of end-of-file
	if (is.na(stop)){
		stop<-nrow(data)
	}
	
	# Check again if the new start value is after the end of file
	if (start>nrow(data)){
		tmp<-data.frame()
	}	
	
	# If all is good, go ahead and count responses
	if (start<nrow(data)){
		# Preallocate Variables
		resp = NULL
		labl = NULL
		RT = NULL
		j = 0;
	
		# Actually read in the data	
		for (i in start:stop) {
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
	
	# Store in common data frame
	alldat<-c(alldat,list(tmp))
}

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

# 5. Compute response time
A_RT = NA
V_RT = NA
AV_RT = NA

for (v in 1:length(alldat)){
	tmp<-alldat[[v]]
	
	A_RT[v]<-mean(tmp[tmp$labl=="A",]$RT)
	V_RT[v]<-mean(tmp[tmp$labl=="V",]$RT)
	AV_RT[v]<-mean(tmp[tmp$labl=="VA",]$RT)
}

# 6. Plot
# Very bad and priliminary bar plots
error.bar<-function(x,y,upper,lower=upper,length=0.1) {
	arrows(x,y+upper,x,y-lower,angle=90,code=3,length=length)
}

barx<-barplot(c(mean(A_RT),mean(V_RT),mean(AV_RT)),ylim=c(0,400),names.arg=c("Audio","Visual","Audiovisual"),ylab="Mean RT")
error.bar(barx,c(mean(A_RT),mean(V_RT),mean(AV_RT)), c(sd(A_RT)/sqrt(length(A_RT)),sd(V_RT)/sqrt(length(V_RT)),sd(AV_RT)/sqrt(length(AV_RT))))