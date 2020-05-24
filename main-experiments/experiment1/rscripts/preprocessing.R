# evaluative adjectives, reshape of context/content projectivity experiment

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')
library(tidyverse)

# load anonymized data
d = read_csv("../data/md.csv")
summary(d)

# information about Turkers

# what ages are there? (ages are currently factors)
table(d$Answer.age)
mean(d$Answer.age)


d[as.character(d$Answer.comments) != "\"\"",c("Answer.comments")]

#cd is clean data
cd = md
names(cd)

# exclude data from Turkers who either said "no" to question about American English
# or who didn't include English in the question about native language

# find out if somebody said "no" to the question about American English
nrow(cd)
# 2432
table(cd$Answer.americanenglish)

# exclude data where answer to "American English?" is "no"
str(cd$Answer.americanenglish)
cd <- droplevels(subset(cd, cd$Answer.americanenglish == "\"Yes\""))


nrow(cd)
# 2400 (2432 - 2400 = 32, data from 2 Turker(s) excluded, 150 remaining Turkers)

# exclude data where answer to "language?" does not include "English" (or a spelling
# variant thereof)
table(cd$Answer.language)

# "english"            "English" 
# 576                 1696 
# "ENGLISH" "english%2C+spanish" 
# 48                   16 
# "English%2C+Spanish"              "HINDI" 
# 16                   16 
# "Russian%2C+French"               "Thai" 
# 16                   16 

# Three Turkers didn't include English as their native language

#cd <- subset(cd, (cd$Answer.language != "\"Chinese\"" & cd$Answer.language != "\"spanish\"" & cd$Answer.language != "\"Vietnamese\""))
str(cd$Answer.language)

cd <- subset(cd, (cd$Answer.language != "\"HINDI\""
                  & cd$Answer.language != "\"Thai\""
                  & cd$Answer.language != "\"Russian%2C+French\""))
cd = droplevels(cd)
nrow(cd)
# 2352 (2352 / 16 = 147, i.e. 3 Turkers excluded)

# throw out Turkers that get any of the six fillers wrong
names(cd)
table(cd$ConDissonant) # 882 fillers
str(cd$ConDissonant)

filler.data <- subset(cd, cd$ConDissonant == "filler") #these are all the fillers
filler.data <- droplevels(filler.data)
nrow(filler.data)
# 882

table(filler.data$ID,filler.data$Response)
#             1   2   3   4   5   6   7
#filler1No  134   6   2   0   1   2   2
#filler1Yes   2   0   3   1  12  18 111
#filler2No  139   5   0   1   0   1   1
#filler2Yes   0   0   0   1   0   3 143
#filler3No  141   2   1   1   1   0   1
#filler3Yes   0   0   1   0   1   3 142

# wrong response is <5 to fillerYes and >3 to fillerNo
fillerWRONG <- subset(filler.data, (filler.data$ID == "filler1No" & filler.data$Response > 3) |
                      (filler.data$ID == "filler2No" & filler.data$Response > 3) |
                      (filler.data$ID == "filler3No" & filler.data$Response > 3) |
                      (filler.data$ID == "filler1Yes" & filler.data$Response < 5) |
                      (filler.data$ID == "filler2Yes" & filler.data$Response < 5) |
                      (filler.data$ID == "filler3Yes" & filler.data$Response < 5))
fillerWRONG <- droplevels(fillerWRONG)                   
nrow(fillerWRONG) #19 wrong judgments

table(fillerWRONG$ID,fillerWRONG$Response)
#           1 3 4 5 6 7
#filler1No  0 0 0 1 2 2
#filler1Yes 2 3 1 0 0 0
#filler2No  0 0 1 0 1 1
#filler2Yes 0 0 1 0 0 0
#filler3No  0 0 1 1 0 1
#filler3Yes 0 1 0 0 0 0
# wrong responses are nicely distributed across fillers

# exclude Turkers who got a filler or more wrong
cd <- subset(cd, !cd$workerid %in% fillerWRONG$workerid)
cd = droplevels(cd)
nrow(cd) #2144 (2144 / 16 = 134 Turkers; 13 Turkers excluded)

# Divide fillers into good and bad ones

table(cd$ID) #shows which fillers are Yes and which are No
table(cd$Condition) #lumps all fillers together
head(cd$Condition)

cd$ConditionYN = cd$ID

cd$ConditionYN = gsub("filler1No","FillerNo",cd$ConditionYN) 
cd$ConditionYN = gsub("filler2No","FillerNo",cd$ConditionYN) 
cd$ConditionYN = gsub("filler3No","FillerNo",cd$ConditionYN) 
cd$ConditionYN = gsub("filler1Yes","FillerYes",cd$ConditionYN) 
cd$ConditionYN = gsub("filler2Yes","FillerYes",cd$ConditionYN) 
cd$ConditionYN = gsub("filler3Yes","FillerYes",cd$ConditionYN)

table(cd$ConditionYN)
str(cd$ConditionYN)

# make new column ConditionYN2 filled with values from Condition and ID
cd$ConditionYN2 <- "NA"
cd$Condition = as.character(cd$Condition)
cd$ConditionYN2 <- ifelse(cd$ConditionYN == "FillerNo","FillerNo",ifelse(cd$ConditionYN == "FillerYes","FillerYes",cd$Condition)) 

cd$ConditionYN2

# cleaned-up data (only American English speakers who got enough fillers right)
write_csv(cd, path="../data/cd.csv")
