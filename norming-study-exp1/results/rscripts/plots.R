# evaluative adjectives, norming study of projection experiment
# plots

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')
library(grid)
library(ggplot2)
library(plyr)

# load cleaned, melded data file
load("../data/xcd.RData") 

names(xcd)
nrow(xcd) #3220 (data from 230 Turkers, 10 target stimuli, 4 fillers (all correctly answered))

# create subset of target data

target = subset(xcd, xcd$Adj != "true")
target <- droplevels(target)
table(target$Adj) #230 responses per Adj

# create a column that identifies whether an item is consonant (C) or dissonant (D)
target$ConDis = "C"
target[(target$Condition == "CnD" | target$Condition == "CxD"),]$ConDis = "D"
table(target$ConDis)

# subset the data by Cn and Cx condition
cx = subset(target, target$Condition2 == "Cx")
cx = droplevels(cx)
nrow(cx) #1150

cn = subset(target, target$Condition2 == "Cn")
cn = droplevels(cn)
nrow(cn) #1150

# all individual items (ID) have between 7 and 13 responses, mean number of responses: 9.6
t = table(target$ID)
as.vector(t)
min(t)
max(t)
mean(t)

#### calculate means 

# mean response for Consonant and Dissonant items (across Cx and Cn)
ConDisMeans = ddply(target, .(ConDis), summarize, CMean = mean(Response), CSD=sd(Response))
ConDisMeans
#ConDis    CMean      CSD
#1      C 6.132189 1.210508
#2      D 1.852863 1.392987

# mean response for Consonant and Dissonant items in Cx
CxMeans = ddply(cx, .(ConDis), summarize, CMean = mean(Response), CSD=sd(Response))
CxMeans
#ConDis    CMean      CSD
#1      C 6.025510 1.256409
#2      D 2.151246 1.580938

# mean response for Consonant and Dissonant items in Cn
CnMeans = ddply(cn, .(ConDis), summarize, CMean = mean(Response), CSD=sd(Response))
CnMeans
#ConDis    CMean      CSD
#1      C 6.240901 1.152831
#2      D 1.560209 1.105590

# the means in the Cn part are further apart than in the Cx part
names(target)
# mean response for all items (ID)
id.means = ddply(target, .(ID), transform, IDMean = mean(Response))
head(id.means)
nrow(id.means) #2300
myvars <- c("ID","Adj","Condition","Condition2","IDMean")
id.means <- id.means[myvars]
id.means <- unique(id.means)
write.csv(id.means, file = "id.means.csv",row.names=FALSE, na="")

# I made a copy of id.means.csv where I hand-coded whether an ID was used or not
dataUsedNotUsed <- read.csv(file="id.meansUsedNotUsed.csv",head=TRUE,sep=",")
summary(dataUsedNotUsed)
head(dataUsedNotUsed)
nrow(dataUsedNotUsed) #240 (i.e. 24 items, 12 pairs, per adjective)

# I then subsetted the data to those that were actually used
used.data <- subset(dataUsedNotUsed, Used == "y")
used.data = droplevels(used.data)
nrow(used.data) #120 (i.e. 12 items, 6 pairs, per adjective)
head(used.data)

# graphical representation of the means of the used stimuli
ggplot(used.data, aes(x=ID,y=IDMean,color=ID,label=IDMean)) +
  geom_point() +
  geom_text(aes(label=IDMean),hjust=0, vjust=0) +
  theme(legend.position = "none") +
  facet_wrap(~Condition2, scales = "free") +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=90, vjust=1)) + 
  xlab("ID") +
  ylab("Response mean")
ggsave(f="graphs/used.data.pdf",width=10, height=12)

# characterize the used data
names(used.data)
used.data$ConDis = "C"
used.data[(used.data$Condition == "CnD" | used.data$Condition == "CxD"),]$ConDis = "D"
table(used.data$ConDis)
head(used.data)

# means of the consonant and dissonant items in the Cn and Cx conditions
used.means = ddply(used.data, .(Condition2,ConDis), summarize, UsedMean = mean(IDMean), UsedSD=sd(IDMean))
used.means
#       Condition2 ConDis UsedMean    UsedSD
# 1         Cn      C 6.503473 0.3746432
# 2         Cn      D 1.266353 0.3142057
# 3         Cx      C 6.150700 0.5241618
# 4         Cx      D 1.736657 0.6948430

# identify maximum and minimum mean
id.means2 <- used.data[c("ID","IDMean")]
rownames(id.means2) <- NULL
id.means2
print(id.means2, row.names = FALSE)

### to make sure that the means and SDs of the used stimuli are right, 
### I redid the calculation based on the original data frame (with responses
### instead of means)

# add the information about which stimuli were used to the target data
used.data$ID
target$Used <- ifelse(target$ID %in% used.data$ID,"y", "n")
table(target$Used)
# n    y 
# 1136 1164

# subset the data to the used data
used.target <- subset(target, target$Used == "y")
used.target <- droplevels(used.target)
nrow(used.target) #1164

# calculate means and SDs per Condition and ConDis
names(used.target)
ut = ddply(used.target, .(Condition2,ConDis), summarize, UsedMean = mean(Response), UsedSD=sd(Response))
ut
# Condition2 ConDis UsedMean    UsedSD
# 1         Cn      C 6.498258 0.8563811
# 2         Cn      D 1.275510 0.7721334
# 3         Cx      C 6.148649 1.1665674
# 4         Cx      D 1.735192 1.2174066

# calculate means and SDs by ConDis only
ut2 = ddply(used.target, .(ConDis), summarize, UsedMean = mean(Response), UsedSD=sd(Response))
ut2
# ConDis UsedMean   UsedSD
# 1      C 6.320755 1.039610
# 2      D 1.502582 1.041589

### How to talk about which data was (not) selected

# Identify the means for the used and not-used items
target$Used <- ifelse(target$ID %in% used.data$ID,"y", "n")
target2 = ddply(target, .(ID), transform,  IDMean = mean(Response), IDSD=sd(Response))
head(target2)
table(target$Used)

# plot those items to describe cut-off line for used and not-used items
ggplot(target2, aes(x=ID,y=IDMean,color=ID,label=IDMean)) +
  geom_point() +
  geom_text(aes(label=IDMean),hjust=0, vjust=0) +
  theme(legend.position = "none") +
  facet_wrap(~Used+Condition2, scales = "free") +
  theme(text = element_text(size=8), axis.text.x = element_text(angle=90, vjust=1)) + 
  xlab("ID") +
  ylab("Response mean")
ggsave(f="graphs/used-not-used-stimuli-comparison.pdf",width=14, height=16)

# calculate the difference in the means for each pair of used items
# create a subset of the target data with the relevant info
# make it unique, i.e. each line is one ID with its mean and info about whether it was
# used or not
myvars <- c("ID","Adj","Condition","Condition2","Used","IDMean")
target3 <- target2[myvars]
target3 <- unique(target3)
nrow(target3) #240
target3

# subset the data to include only the used data
target3 <- subset(target3, target3$Used == "y")
target3 <- droplevels(target3)
nrow(target3) #120

target3
target3$Diff <- c(diff(target3$IDMean),NA)
target3 <- subset(target3, target3$Diff < 0)
target3 <- droplevels(target3)
nrow(target3) #60

table(target3$Diff,target3$Condition2)

target4 = ddply(target3, .(Condition), summarize,  Mean = mean(Diff), SD=sd(Diff))
target4
# Condition      Mean        SD
# 1       CnC -5.237120 0.5064686
# 2       CxC -4.414043 0.9738995

### Could I have chosen better stimuli in the Cx condition?

myvars <- c("ID","Adj","Condition","Condition2","Used","IDMean")
target3 <- target2[myvars]
target3 <- unique(target3)
nrow(target3) #240
target3

# subset the data to include only the not-used data
target3 <- subset(target3, target3$Used == "n")
target3 <- droplevels(target3)
nrow(target3) #120

target3
target3$Diff <- c(diff(target3$IDMean),NA)
target3 <- subset(target3, target3$Condition2 == "Cx")
target3 <- droplevels(target3)
nrow(target3) #30
target3

target3 <- subset(target3, target3$Diff < 0)
target3 <- subset(target3, target3$ID != "4-smart-CxD")
target3 <- droplevels(target3)
nrow(target3) #60



#### Stuff done but not used (made no sense)

# subset the data by adjective

brave = subset(means, means$Adj == "brave")
brave = droplevels(brave)
nrow(brave) #230

ggplot(brave, aes(x=ID,y=IDMean,color=ID,label=IDMean)) +
  geom_point() +
  geom_text(aes(label=IDMean),hjust=0, vjust=0) +
  theme(legend.position = "none") +
  facet_wrap(~Condition2, scales = "free") +
  theme(text = element_text(size=20), axis.text.x = element_text(angle=90, vjust=1)) + 
  xlab("brave") +
  ylab("Response mean")
ggsave(f="graphs/brave.pdf",width=8, height=10)




###### Previous, bad analysis of norming data

# do all items have at least 8 responses? yes: all have between 8 and 14 responses, average 10.5
responses = table(cd$ID)
responses
write.csv(responses, file = "responses.csv",row.names=FALSE, na="")

## create data subsets to compare means across the conditions
table(cd$Condition)

cnc <- subset(cd, cd$Condition == "CnC")
cnd <- subset(cd, cd$Condition == "CnD")
cxc <- subset(cd, cd$Condition == "CxC")
cxd <- subset(cd, cd$Condition == "CxD")

# check if it was done right
nrow(cnc)
nrow(cnd)
nrow(cxc)
nrow(cxd)

# drop unused levels
cnc <- droplevels(cnc)
cnd <- droplevels(cnd)
cxc <- droplevels(cxc)
cxd <- droplevels(cxd)

# calculate means for the individual items in the four conditions
agr.cnc = aggregate(Response ~ ID, data=cnc, FUN=mean)
agr.cnd = aggregate(Response ~ ID, data=cnd, FUN=mean)
agr.cxc = aggregate(Response ~ ID, data=cxc, FUN=mean)
agr.cxd = aggregate(Response ~ ID, data=cxd, FUN=mean)

# write to separate files (and then merge the relevant ones by hand)
write.csv(agr.cnc, file = "cnc.csv",row.names=FALSE, na="")
write.csv(agr.cnd, file = "cnd.csv",row.names=FALSE, na="")
write.csv(agr.cxc, file = "cxc.csv",row.names=FALSE, na="")
write.csv(agr.cxd, file = "cxd.csv",row.names=FALSE, na="")
