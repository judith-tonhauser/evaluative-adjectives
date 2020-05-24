# evaluative adjectives, norming study of projection experiment
# analysis

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')
library(grid)
library(ggplot2)
library(tidyverse)

xcd = read_csv("../data/xcd.csv")
nrow(xcd)

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

nrow(t) #240 items

#### calculate means 

# mean response for Consonant and Dissonant items (across Cx and Cn)
ConDisMeans = ddply(target, .(ConDis), summarize, CMean = mean(Response), CSD=sd(Response))
ConDisMeans
#ConDis    CMean      CSD
#1      C 6.132189 1.210508
#2      D 1.852863 1.392987

# data for Table 1 in Supplement

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

# now I look at individual items to select the best ones

# the means in the Cn part are further apart than in the Cx part
names(target)
# mean response for all items (ID)
id.means = ddply(target, .(ID), transform, IDMean = mean(Response))
head(id.means)
nrow(id.means) #2300
myvars <- c("ID","Adj","Condition","Condition2","IDMean")
id.means <- id.means[myvars]
id.means <- unique(id.means)
View(id.means)
write.csv(id.means, file = "id.means.csv",row.names=FALSE, na="")

# JT in 2016: I made a copy of id.means.csv where I hand-coded whether an ID was used or not
# maximizing how far apart the means of the less likely/more likely conditions were
# JT in 2020: UGH
dataUsedNotUsed <- read.csv(file="../data/id.meansUsedNotUsed.csv",head=TRUE,sep=",")
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

# characterize the used data
names(used.data)
used.data$ConDis = "C"
used.data[(used.data$Condition == "CnD" | used.data$Condition == "CxD"),]$ConDis = "D"
table(used.data$ConDis)
head(used.data)

# not reported in paper (see below for Table 2 data)

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

# Table 2 in Supplement

# calculate means and SDs per Condition and ConDis
names(used.target)
ut = ddply(used.target, .(Condition2,ConDis), summarize, UsedMean = mean(Response), UsedSD=sd(Response))
ut
# Condition2 ConDis UsedMean    UsedSD
# 1         Cn      C 6.498258 0.8563811
# 2         Cn      D 1.275510 0.7721334
# 3         Cx      C 6.148649 1.1665674
# 4         Cx      D 1.735192 1.2174066


