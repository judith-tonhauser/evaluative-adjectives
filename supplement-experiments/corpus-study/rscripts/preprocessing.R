# evaluative adjectives, corpus study, preprocessing

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')
library(reshape2)

# read the data
md = read.csv("../data/md.csv")
summary(md)
nrow(md) # 2600 

#create a file for the cleaned up data (cd)
cd = md
names(cd)

# age 
str(cd$Answer.age)
cd$Answer.age <- gsub("\"", "", cd$Answer.age)
cd$Answer.age <- as.numeric(cd$Answer.age)

table(cd$Answer.age) #18-83
median(cd$Answer.age) #33
mean(cd$Answer.age) #35.6

# exclude data from Turkers who either said "no" to question about American English
# or who didn't include English in the question about native language

# find out if somebody said "no" to the question about American English
table(cd$Answer.americanenglish)

# exclude data where answer to "American English?" is "no"
str(cd$Answer.americanenglish)
cd <- subset(cd, cd$Answer.americanenglish == "\"Yes\"")
cd = droplevels(cd)

nrow(cd)
# 2550 (5 Turkers excluded)

# exclude data where answer to "language?" does not include "English" (or a spelling
# variant thereof)
table(cd$Answer.language)

# "Eglish"          "Eng%3Bish" 
# 10                   10 
# "english"            "English" 
# 830                 1590 
# "ENGLISH" "English%2C+Spanish" 
# 40                   10 
# "ENGLISH%2C+SPANISH"           "English+" 
# 10                   30 
# "enlish"            "spanish" 
# 10                   10

cd <- subset(cd, (cd$Answer.language != "\"spanish\""))
cd = droplevels(cd)
nrow(cd)
# 2540 (1 Turker excluded, 254 remain)

# throw out Turkers that get one or both of the fillers wrong
names(cd)
table(cd$Index) 

filler.data <- subset(cd, cd$Index == "filler1No" | cd$Index == "filler3Yes") 
filler.data <- droplevels(filler.data)
nrow(filler.data)
# 508 / 2 = 254

# names used in fillers
table(filler.data$Adj,filler.data$Question)

table(filler.data$Index,filler.data$Response)
#             1   2   3   4   5   6   7
# filler1No  201  17  11   3   6   6  10
# filler3Yes   0   0   3   3   8  22 218

# wrong response is lower than 5 to fillerYes and higher than 3 to fillerNo
fillerWRONG <- subset(filler.data, (filler.data$Index == "filler1No" & filler.data$Response > 3) |
                      (filler.data$Index == "filler3Yes" & filler.data$Response < 5))
fillerWRONG <- droplevels(fillerWRONG)                   
nrow(fillerWRONG) #31 wrong judgments

table(fillerWRONG$Index,fillerWRONG$Response)
#             3  4  5  6  7
# filler1No   0  3  6  6 10
# filler3Yes  3  3  0  0  0

# exclude Turkers who got a filler or more wrong
cd <- subset(cd, !cd$workerid %in% fillerWRONG$workerid)
cd = droplevels(cd)
nrow(cd) #2260 (28 Turkers excluded)

# age
table(cd$Answer.age) # 18-83
median(cd$Answer.age) #33
mean(cd$Answer.age) #35.6

# cleaned-up data (only American English speakers who both fillers right)
write_csv(cd, path="../data/cd.csv")

