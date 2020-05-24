# evaluative adjectives
# Exp 2: at-issueness of prejacent

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')
library(tidyverse)

# load anonymized data
d = read_csv("../data/md.csv")
summary(d)

#create a file for the cleaned up data (cd)
cd = md
names(cd)

# Now exclude Turkers

#create a file for the cleaned up data (cd)
cd = md
names(cd)
nrow(cd) #1200

# ages of participants
str(cd$Answer.age)
cd$Answer.age <- gsub("\"", "", cd$Answer.age)
cd$Answer.age <- as.numeric(cd$Answer.age)

table(cd$Answer.age)
mean(cd$Answer.age)

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

# exclude data where answer to "language?" does not include "English" (or a spelling
# variant thereof)
table(cd$Answer.language)
cd <- subset(cd, (cd$Answer.language != "\"Chinese\"" & cd$Answer.language != "\"Vietnamese\""))
#cd <- subset(cd, (cd$Answer.language != "\"Arabic\""))
cd = droplevels(cd)
nrow(cd) #1168 (= data from 2 Turkers excluded, 73 remaining Turkers)

# find out if somebody said "no" to the question about American English
table(cd$Answer.americanenglish)
#"Yes" 
#1168 

# exclude data where answer to "American English?" is "no"
str(cd$Answer.americanenglish)
cd <- subset(cd, cd$Answer.americanenglish == "\"Yes\"")
cd = droplevels(cd)
nrow(cd) #no turkers excluded

### how did the Turkers do on the controls (two main clauses, nominal appositive)?
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

cd <- subset(cd, !cd$workerid %in% MCWrong$workerid)
cd = droplevels(cd)
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

#I need a column like AIness but where the 6 controls aren't just "none" but distinguished
#In SentenceType the two types of EAS are distinguished
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
cd$SentenceType <- gsub("SAI", "EAS_n", cd$SentenceType) #generalization is at-issue, doesn't follow from wk
cd$SentenceType <- gsub("VPAI", "EAS_f", cd$SentenceType) #VP is ai, generalization follows from world knowledge

table(cd$SentenceType)

nrow(cd) #1088 / 16 = 68 remaining Turkers

cd <- within(cd,  ID2 <- paste(Adj, AIness, sep=""))
table(cd$ID2)

# save cleaned-up data (only American English speakers who got controls right)
write_csv(cd, path="../data/cd.csv")

