# evaluative adjectives
# Exp 2: at-issueness of prejacent

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')
library(reshape2)

# load data
d = read.csv("../data/raw-parsed-results.csv")
summary(d)
table(d$workerid)
nrow(d) #75 Turkers

# reshape the data
d = reshapeNAIEAS(d)

save(d, file="../data/d.RData")
nrow(d)
names(d)
head(d)

# find the column numbers with the 16 trials in them
sort(names(d))

sort(names(d))[148]
sort(names(d))[163]
sort(names(d))[148:163]


# melt the data based on Trial number position in d 
md = melt(d, id.vars=c("workerid", "assignmentid","Answer.listNumber","Answer.age","Answer.language","Answer.americanenglish"),measure=sort(names(d))[148:163])
summary(md)
head(md)
names(md)
nrow(md) #1200 rows / 75 Turkers = 16 ratings per Turker
tail(md)


#melt
#d$List0,d$ID0,d$AIness0,d$Target0,d$Responseutt0,d$Response0,d$Adj0

md$Trial = gsub("Trial","",md$variable)
md$Trial
table(md$Trial)

md$Trial = as.numeric(md$Trial)
table(md$Trial) #every trial has 75 responses

# List of the item
md$List = sapply(strsplit(as.character(md$value)," ",fixed=T), "[", 1)
md$List = as.factor(as.character(md$List))
table(md$List) #6 lists, fillers (450 = 75 x 6 fillers)

# ID of the item (unique)
md$ID = sapply(strsplit(as.character(md$value)," ",fixed=T), "[", 2)
md$ID = as.factor(as.character(md$ID))
table(md$ID)

# item type
md$AIness = sapply(strsplit(as.character(md$value)," ",fixed=T), "[", 3)
md$AIness = as.factor(as.character(md$AIness))
table(md$AIness)

# Target sentence of the item
md$Target = sapply(strsplit(as.character(md$value)," ",fixed=T), "[", 4)
md$Target = as.factor(as.character(md$Target))
table(md$Target)

# Responseutt(erance)  of the item
md$Responseutt = sapply(strsplit(as.character(md$value)," ",fixed=T), "[", 5)
md$Responseutt = as.factor(as.character(md$Responseutt))
table(md$Responseutt)

# Response given for the item (1-7)
md$Response = sapply(strsplit(as.character(md$value)," ",fixed=T), "[", 6)
md$Response = as.numeric(as.character(md$Response))
table(md$Response)

#1   2 
#537 663
#1=Yes, 2=No

# Adjective of the item
md$Adj = sapply(strsplit(as.character(md$value)," ",fixed=T), "[", 7)
md$Adj = as.factor(md$Adj)
str(md$Adj)
table(md$Adj)

# ages of participants
str(md$Answer.age)
md$Answer.age <- gsub("\"", "", md$Answer.age)
md$Answer.age <- as.numeric(md$Answer.age)

table(md$Answer.age)
mean(md$Answer.age)

save(md,file="../data/md.RData")

# exclude Turkers
cd = md
names(cd)
nrow(cd) #1200

### currently 1=Yes, 2=No
### recode 1=No, 0=Yes, so that higher responses = higher not-at-issueness
table(cd$Response)
cd$Response <- gsub("1", "0", cd$Response) #Yes: turn 1 into 0
cd$Response <- gsub("2", "1", cd$Response) #No: turn 2 into 1

table(cd$Response)
#0   1 
#537 663

# exclude data from Turkers who either said "no" to question about American English
# or who didn't include English in the question about native language

# language
table(cd$Answer.language)
cd <- droplevels(subset(cd, (cd$Answer.language != "\"Chinese\"" & cd$Answer.language != "\"Vietnamese\"")))
nrow(cd) #1168 (= data from 2 Turkers excluded, 73 remaining Turkers)

# American English
table(cd$Answer.americanenglish)
#"Yes" 
#1168 
#no turkers excluded

### exclude Turkers based on controls (two main clauses)
names(cd)
table(cd$ID)

cd.MC <- subset(cd, cd$ID == "mainclause1" | cd$ID == "mainclause2")
cd.MC = droplevels(cd.MC)
nrow(cd.MC) #146 = 73 Turkers x 2 main clause controls

# Turkers that gave "No" (1), i.e. wrong response to main clause
table(cd.MC$Response)

MCWrong <- subset(cd.MC, cd.MC$Response == "1")
MCWrong = droplevels(MCWrong)
nrow(MCWrong) #6 wrong judgments

table(MCWrong$workerid) #5 Turkers

cd <- droplevels(subset(cd, !cd$workerid %in% MCWrong$workerid))
nrow(cd) #1088 / 16 = 68 (5 Turkers excluded)

#5 Turkers excluded for getting one or both of the main clauses wrong

# age of remaining participants
table(cd$Answer.age)
mean(cd$Answer.age)

table(cd$AIness)
#FAI  SAI VPAI 
#408  340  340 
#In the target utterances, there was the same amount of SAI and VPAI items (just a check)

##create new columns for the analysis and plots

# SentenceType distinguishes the two types of EAS
cd$SentenceType <- cd$AIness
table(cd$SentenceType)
str(cd$SentenceType)
cd$SentenceType <- as.character(cd$SentenceType)
cd$ID <- as.character(cd$ID)
str(cd$ID)
table(cd$ID)

cd$SentenceType[cd$ID=="compAnnoyed"]<-"Comp-of-Annoyed"
cd$SentenceType[cd$ID=="compDiscover"]<-"Comp-of-Discover"
cd$SentenceType[cd$ID=="nomapp"]<-"NomApp"
cd$SentenceType[cd$ID=="mainclause1"]<-"MainClause1"
cd$SentenceType[cd$ID=="mainclause2"]<-"MainClause2"
cd$SentenceType[cd$ID=="possNP"]<-"PossNP"
cd$SentenceType <- gsub("SAI", "EAS_n", cd$SentenceType) #generalization is neutral, doesn't follow from CG
cd$SentenceType <- gsub("VPAI", "EAS_f", cd$SentenceType) #prejacent is ai, generalization follows from CG

table(cd$SentenceType)

nrow(cd) #1088 / 16 = 68 remaining Turkers

cd <- within(cd,  ID2 <- paste(Adj, AIness, sep=""))
table(cd$ID2)

# save data
save(cd,file="../data/cd.RData") #cd = clean data

