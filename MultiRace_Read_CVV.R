# Read in CSV-Database from Lab.js
# The current R-Script is work in Progress.
# It may contain errors and is far from finished
# Questions and Comments should be directed to julian.keil@gmail.com

# 0. Load packages
library("data.table")

# 1. Set Working Directory
setwd('~/Documents/Arbeit/lab_js/Experiments/MultiRace/Data/Offline')

# 2. Get the list of files in the Directory
indat <- list.files(pattern='.csv')
Meta <- NULL
alldat <- NULL

# 3. Loop through files to read in data
for (v in 1:length(indat)) {
	data <- read.csv(indat[v])
	
	# 3.1. Get the meta data
	Meta$ts[v] = levels(data$timestamp)[1] # Timestamp
	tmp<-c(levels(data$f_vname)[2],levels(data$f_zname)[2])
	Meta$name[v] <- paste(tmp,collapse="") # Initials
	Meta$gend[v] <- levels(data$Geschlecht)[2] # Gender
	Meta$birth[v] <- levels(data$f_gebdat)[2] # Birthday
	Meta$sick[v] <- levels(data$erkrankt)[2] # Neurological diseases
	Meta$drug[v] <- levels(data$drugs)[2] # Taking drugs
	Meta$sight[v] <- levels(data$Sehschwäche)[2] # Visual impairments
	Meta$hear[v] <- levels(data$Hörstörung)[2] # Auditory impairments
	Meta$fb_v[v] <- levels(data$visual)[2] # Were all visual stimuli seen
	Meta$fb_a[v] <- levels(data$auditory)[2] # Were all auditory stimuli heard
	Meta$fb_del[v] <- levels(data$Verzögerung)[2] # Was there a delay between stimuli
	if (length(levels(data$feedback))==0) {
		Meta$fb_open[v] <- "None"
	}
	if (length(levels(data$feedback))!=0) {
		Meta$fb_open[v] <- levels(data$feedback)[2] # Open Text for Feedback
	}
	
	resp = NULL
	labl = NULL
	RT = NULL
	
	for (i in 1:length(data)) {
	# 3.2 Get the sections with the reactions 
	index <- data$response=="react"
	# 3.2.1 Cut the whole dataset according to this
	# label is 2=A,3=V,4=AV
	resp <-data$respons[index]
	labl <- data$label[index]
	RT <- data$duration[index]
	}
	tmp <- data.frame(labl,resp,RT)	
	alldat <- c(alldat,list(tmp))
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
gdf <- data.frame(RT =c(mean(A_p),mean(V_p),mean(AV_p),mean(B_p)), Probability =rep(psq, 4),
                  Condition =rep(c("gx(t)", "gy(t)","gz(t)","gx(t)+gy(t)"), each=length(xp)))
panelf <- ggplot(gdf, aes(x = RT, y = Probability, group=Condition,
                          colour=Condition, shape=Condition)) + 
  geom_point() + geom_line() 
panelf + coord_cartesian(xlim = c(200, 500), ylim=c(-.01,1.01)) +
  theme(legend.position= c(.85, .20),  
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))
