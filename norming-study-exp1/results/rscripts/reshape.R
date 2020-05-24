# evaluative adjectives, norming study of projection experiment
# reshape the data

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')
library(reshape2)

# d = columns from results file downloaded from AMT plus Response0-14, List0-14, ID0-14
# Condition0-14, Adj0-14, Context0-14, Target0-14; 270 lines total because 270 Turkers; 
# each responded to 14 items
d = read.csv("../parsed-experiment-results.csv",header = TRUE, stringsAsFactors = FALSE, quote = "\"", sep="\t")
summary(d)
head(d)
nrow(d) #270 

# Turkers ages
# what ages are there? (ages are currently factors)
table(d$Answer.age)
str(d$Answer.age)
d$Answer.age <- gsub("\"\\.0\"","32",d$Answer.age) # set to mean age
d$Answer.age <- gsub("\"", "", d$Answer.age)
d$Answer.age <- as.numeric(d$Answer.age)
mean(d$Answer.age)

# reshaping the data

d = reshapeNormingData(d)

save(d, file="../data/d.RData")
nrow(d) #270
names(d)
head(d)
summary(d)

# find the column numbers with the 14 trials in them
sort(names(d))

sort(names(d))[107]
sort(names(d))[120]
sort(names(d))[107:120]


# melt the data based on Trial number position in d 
md = melt(d, id.vars=c("workerid", "assignmentid","Answer.listNumber","Answer.age","Answer.language","Answer.americanenglish"),measure=sort(names(d))[107:120])
summary(md)
head(md)
names(md)
nrow(md)
# 3780 (which is right because 3780 / 270 Turkers = 14 assignments per Turker)
tail(md)

# Trial 
table(md$variable)
md$Trial = gsub("Trial","",md$variable)
md$Trial = as.numeric(md$Trial)
table(md$Trial)

# melt 
# paste(d$List0,d$ID0,d$Condition0,d$Context0,d$Target0,d$Response0,d$Adj0,sep = " ")

# List of the item (one of 24 lists)
table(md$Answer.listNumber) #24 lists with varying number of ratings
# because the next 3 lines give the wrong results, I used the Answer.listNumber info to get me the lists
#head(md$value)
#md$List = sapply(strsplit(as.character(md$value)," ",fixed=T), "[", 1)
#md$List = as.factor(as.character(md$List))
md$List <- md$Answer.listNumber+1
table(md$List)

# ID of the item (unique)
md$ID = sapply(strsplit(as.character(md$value)," ",fixed=T), "[", 2)
md$ID = as.factor(as.character(md$ID))
table(md$ID)
# the two 'no' fillers were both coded as 'filler1No'

# Condition of the item (four conditions: CnC, CnD, CxC, CxD, na = fillers)
md$Condition = sapply(strsplit(as.character(md$value)," ",fixed=T), "[", 3)
md$Condition = as.factor(as.character(md$Condition))
table(md$Condition)
# fillers are "na"

# Context sentence of the item
md$Context = sapply(strsplit(as.character(md$value)," ",fixed=T), "[", 4)
md$Context = as.factor(as.character(md$Context))
table(md$Context)

# Target sentence of the item
md$Target = sapply(strsplit(as.character(md$value)," ",fixed=T), "[", 5)
md$Target = as.factor(as.character(md$Target))
table(md$Target)

# Response given for the item (1-7)
md$Response = sapply(strsplit(as.character(md$value)," ",fixed=T), "[", 6)
md$Response = as.numeric(as.character(md$Response))
table(md$Response)

# most responses are 1 or 7
# 1    2    3    4    5    6    7 
# 1263  216  230  141  427  302 1201 

# Adjective of the item
md$Adj = sapply(strsplit(as.character(md$value)," ",fixed=T), "[", 7)
md$Adj = as.factor(md$Adj)
str(md$Adj)
table(md$Adj)
# adj is "true" for fillers

# create new columns for the condition without the 'C' or 'D' at the end (i.e. just "Cx" for
#the data in the context conditions and "Cn" for the data in the content conditions)

# first make a column that's a copy of the ID column into ID2
names(md)
table(md$Condition)
md$Condition2 = md$Condition
# then make F1, F2 into FG (good-filler) and F3, F4 into FB (bad-fillers)
md$Condition2 = gsub("CnC","Cn",md$Condition2)
md$Condition2 = gsub("CnD","Cn",md$Condition2)
md$Condition2 = gsub("CxC","Cx",md$Condition2)
md$Condition2 = gsub("CxD","Cx",md$Condition2)

head(md$Condition2)
table(md$Condition2)

nrow(md)
save(md,file="../data/md.RData")

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

# cleaned-up data (only American English speakers who all fillers right)
save(xcd,file="../data/xcd.RData") #xcd = extra-clean data

names(xcd)

# target data
table(xcd$Adj)
t <- droplevels(subset(xcd,xcd$Adj != "true"))
nrow(t) #2300 = 230 Turkers x 10 ratings
  
names(t)
# number of responses per item
head(t)
length(unique(t$ID)) #240
count <- count(t, ID)
as.data.frame(count)
min(count$n)
max(count$n)
mean(count$n)

agr = aggregate(Response ~ Condition, data=t, FUN="mean")
agr$sd = aggregate(Response ~ Condition, data=t, FUN="sd")
agr
