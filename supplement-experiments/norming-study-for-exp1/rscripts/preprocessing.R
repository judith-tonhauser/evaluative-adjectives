# evaluative adjectives, norming study of projection experiment
# preprocessing

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

md <- read.csv(file="../data/md.csv", header=TRUE, sep=",")
nrow(md)

# create the clean data (cd)
cd = md

# exclude data where answer to "language?" does not include "English" (or a spelling
# variant thereof)
table(cd$Answer.language)
# "++english"                       "chinese" 
# 14                              14 
# "Chinese"                       "Englidh" 
# 28                              14 
# "english"                       "eNGLISH" 
# 840                              14 
# "English" "English%2C+Chinese%2C+Spanish" 
# 2688                              14 
# "english%2Fspanish"             "English%2FSpanish" 
# 14                              14 
# "English+"         "English+and+Cantonese" 
# 14                              14 
# "English+and+Lao"                        "ENLISH" 
# 14                              14 
# "Mandarin"                       "spanish" 
# 14                              14 
# "Spanish"                    "vietnamese" 
# 14                              14 
# "Vietnamese" 
# 14 

cd <- subset(cd, (cd$Answer.language != "\"Chinese\"" & 
                    cd$Answer.language != "\"chinese\"" &
                    cd$Answer.language != "\"Mandarin\"" &
                    cd$Answer.language != "\"spanish\"" & 
                    cd$Answer.language != "\"Spanish\"" &
                    cd$Answer.language != "\"vietnamese\"" &
                    cd$Answer.language != "\"Vietnamese\""))
nrow(cd)
# 3668 (= 3780 - 3668 = 112, 112 / 14 = 8, data from 8 Turkers excluded)
cd = droplevels(cd)
nrow(cd) #3668

# exclude data where answer to "American English?" is "no"
str(cd$Answer.americanenglish)
table(cd$Answer.americanenglish)
#"No" "Yes" 
#14  3654

cd <- subset(cd, cd$Answer.americanenglish == "\"Yes\"")
cd = droplevels(cd)

nrow(cd)
# 3654 (3668 - 3654 = 14, , i.e. data from 1 Turker excluded)

# data from 261 Turkers remain

# xcd = extra-clean data (Turkers excluded for getting one filler wrong)
xcd = cd

### throw out Turkers that got one or more fillers wrong

table(xcd$Adj) # "true" is the adjective used in the fillers

# create subset of data that only contains filler data

filler.data <- droplevels(subset(xcd, xcd$Adj == "true"))
nrow(filler.data) #1044 (261 Turkers x 4 fillers = 1044)

# identify how many of the fillers were answered wrongly
# to do this I need to create a new column that codes whether a filler was a "good" or a "bad" one
names(filler.data)
table(filler.data$ID)

filler.data$fillerType = "GoodFiller"
filler.data[(filler.data$ID == "filler1No"),]$fillerType = "BadFiller"
table(filler.data$fillerType)


table(filler.data$fillerType,filler.data$Response)
#             1   2   3   4   5   6   7
#BadFiller  435  35  25   9  12   2   4
#GoodFiller   1   2   6   8  33  39 433

# now identify the fillers that were answered wrongly

filler.dataWRONG <- droplevels(subset(filler.data, (filler.data$fillerType == "BadFiller" & filler.data$Response > 3) |
                                  (filler.data$fillerType == "GoodFiller" & filler.data$Response < 5)))
nrow(filler.dataWRONG) #44 judgments that lead to exclusions

table(filler.dataWRONG$fillerType,filler.dataWRONG$Response)
#           1  2  3  4  5  6  7
#BadFiller   0  0  0  9 12  2  4
#GoodFiller  1  2  6  8  0  0  0

# exclude the Turkers that got one or more of the four fillers wrong

xcd <- subset(xcd, !xcd$workerid %in% filler.dataWRONG$workerid)
xcd = droplevels(xcd)
nrow(xcd) #3220 (3654 - 3220 = 31 Turkers excluded)

table(xcd$Answer.age)
str(xcd$Answer.age)
mean(xcd$Answer.age)

# cleaned-up data (only American English speakers who got all fillers right)
write.csv(xcd,file="../data/xcd.csv",row.names=F,quote=F)
