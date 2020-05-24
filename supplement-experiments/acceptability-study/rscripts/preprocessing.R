# evaluative adjectives, reshape of acceptability judgment experiment

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')
library(reshape2)

# load required packages
library(tidyverse)

md = read_csv("../data/md.csv")

#create a file for the cleaned up data (cd)
cd = md
names(cd)

# exclude data from Turkers who either said "no" to question about American English
# or who didn't include English in the question about native language

table(cd$Answer.americanenglish)

# exclude data where answer to "American English?" is "no"
cd <- subset(cd, cd$Answer.americanenglish == "\"Yes\"")
cd = droplevels(cd)

nrow(cd)
# 2394 (= data from 1 Turker(s), 133 remaining Turkers)

# exclude data where answer to "language?" does not include "English" (or a spelling
# variant thereof)
table(cd$Answer.language)
# cd <- subset(cd, (cd$Answer.language != "\"Chinese\"" & cd$Answer.language != "\"spanish\"" & cd$Answer.language != "\"Vietnamese\""))
cd <- subset(cd, (cd$Answer.language != "\"Arabic\""))
cd = droplevels(cd)
nrow(cd) # 2376 (= data from 1 Turker(s), 132 remaining Turkers)

# throw out Turkers that got more than any of the 8 controls wrong
# where "wrong" means a response higher than 3 (4+ means that they thought the controls were acceptable)
table(cd$Enough) # "na" are the fillers
filler.data <- subset(cd, cd$Enough == "na") 
filler.data <- droplevels(filler.data)
nrow(filler.data) # 1056

FillerWrong <- subset(filler.data, (filler.data$Enough == "na" & filler.data$Response > 3))
FillerWrong = droplevels(FillerWrong)
nrow(FillerWrong) #71 wrong judgments (correct, as established through CSV file)

cd <- subset(cd, !cd$workerid %in% FillerWrong$workerid)
cd = droplevels(cd)
nrow(cd) #1692 rows (2376 - 1692 = 684 / 18 = 38 Turkers excluded; 1692 / 18 = 94 Turkers remain)

## create some more columns needed for the analysis

# create a column in which each pair of examples (with enough and without) has the same label
# e.g. I want 9-stupid-CxC-E and 9-stupid-CxC-noE to both be called 9-stupid-CxC

table(cd$ID)
table(cd$Adj)
str(cd$Adj)

cd$AdjPure = cd$Adj
cd$AdjPure = as.character(cd$AdjPure)
str(cd$AdjPure)

cd$AdjPure = gsub("_enough","",cd$AdjPure)
table(cd$AdjPure)

# cleaned-up data (only American English speakers who got enough fillers right)
write.csv(cd,file="../data/cd.csv",row.names=F,quote=F)

