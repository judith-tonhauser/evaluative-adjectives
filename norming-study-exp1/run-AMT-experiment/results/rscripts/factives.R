# created by jdegen, 05/07/2014

setwd('/Users/judith/Documents/current-research-topics/NSF-NAI/prop-att-experiments/different-factive-verbs/experiment-set-up/58_factivespilot/results/')
source('rscripts/helpers.R')
library(reshape2)
# 
#
d = read.csv("../scripts/result.csv",sep="\t")
summary(d)
nrow(d)
names(d)

# information about Turkers' language 
table(d$Answer.language)
table(d$workerid,d$Answer.language)
table(d$Answer.language,d$Answer.americanenglish)
table(d$Answer.americanenglish)
prop.table(table(d$Answer.americanenglish))

d[as.character(d$Answer.comments) != "\"\"",c("Answer.comments")]
table(d$Answer.language)
nrow(d)

ggplot(d, aes(x=Answer.age, fill=Answer.gender)) +
  geom_histogram(position="dodge")

d = getGender(d)

# reshaping the data

d = reshapeData(d)

save(d, file="data/d.RData")
nrow(d)
names(d)
head(d)

sort(names(d))

names(d)[135]
names(d)[148]
names(d)[135:148]
sort(names(d))[135:148]

# melt the data based on Trial number position in d 
md = melt(d, id.vars=c("assignmentid","Answer.listNumber"),measure=sort(names(d))[105:118])
summary(md)
head(md)
names(md)
nrow(md)
tail(md)

table(md$Trial)

md$Trial = gsub("Trial","",md$variable)
md$Trial = as.numeric(md$Trial)
md$Name = sapply(strsplit(as.character(md$value)," ",fixed=T), "[", 1)
md$Name = as.factor(as.character(md$Name))
md$ID = sapply(strsplit(as.character(md$value)," ",fixed=T), "[", 2)
md$ID = as.factor(as.character(md$ID))
md$Verb = sapply(strsplit(as.character(md$value)," ",fixed=T), "[", 3)
md$Verb = as.factor(as.character(md$Verb))
md$Underlined = sapply(strsplit(as.character(md$value)," ",fixed=T), "[", 4)
md$Underlined = as.factor(as.character(md$Underlined))
md$Pronoun = sapply(strsplit(as.character(md$value)," ",fixed=T), "[", 5)
md$Pronoun = as.factor(as.character(md$Pronoun))
md$Response = sapply(strsplit(as.character(md$value)," ",fixed=T), "[", 6)
md$Response = as.numeric(as.character(md$Response))
md$Modal = sapply(strsplit(as.character(md$value)," ",fixed=T), "[", 7)
md$Modal = as.factor(as.character(md$Modal))

# create new columns for the (actual) verb and the embedding from the ID column

# first make a column that's a copy of the ID column into ID2
md$ID2 = md$ID
tail(md$ID2)
# then make F1, F2 into FG (good-filler) and F3, F4 into FB (bad-fillers)
md$ID3 = gsub("F1","FG",md$ID2)
md$ID4 = gsub("F2","FG",md$ID3)
md$ID5 = gsub("F3","FB",md$ID4)
md$ID6 = gsub("F4","FB",md$ID5)
head(md$ID6)
tail(md$ID6)
table(md$ID6)

# then strip all the numbers from the values in that column; this is ID7 column
md$ID7 = gsub("[0-9]","",md$ID6)
tail(md$ID7)

head(md$ID)
head(md)
tail(md)
summary(md)

save(md,file="data/md.RData")

nrow(md)



### MODIFIED UP TO HERE ### the rest is for a study Judith Degen did
pd = melt(md, id.vars=c("assignmentid","Answer.age","Answer.gender","Gender","Container","Speaker","Quantifier","Trial","Object","Mode","Verb"),measure=sort(names(md))[9:11],variable.name="Slider",value.name="SliderValue")
summary(pd)
# normalize by participant/trial

# compute normalized probabilities
pnormValue = ddply(pd, .(assignmentid,Trial), summarize, normValue=SliderValue/(sum(SliderValue)),assignmentid=assignmentid,Slider=Slider)
row.names(pnormValue) = paste(pnormValue$assignmentid,pnormValue$Trial,pnormValue$Slider)
pd$normValue = pnormValue[paste(pd$assignmentid,pd$Trial,pd$Slider),]$normValue

# test: all sums should be 1, except for trials which are missing because the toal number of marbles was smaller
sums = ddply(pd, .(assignmentid,Trial), summarize, sum(normValue))
colnames(sums) = c("workerid","trial","sum")
sums

pd = pd[order(pd$assignmentid),]
pd$prevQuantifier = c(NA,as.character(pd$Quantifier)[1:(length(pd$Quantifier)-1)])
pd[pd$Trial == 1,]$prevQuantifier = NA
pd$CompScore = pd$normValue*pd$SliderValue # create a composite score of normalized and raw slider value
pd[is.na(pd$prevQuantifier),]$prevQuantifier = "first"
pd$prevQuantifier = as.factor(as.character(pd$prevQuantifier))

pd$NumObjects = factor(x=ifelse(pd$Slider == "Slider0","none",ifelse(pd$Slider == "Slider1","some-not-all","all")),levels=c("none","some-not-all","all"))
save(pd,file="data/pd.RData")

prior = pd[pd$Trial < 13,]
prior = droplevels(prior)
interpretation = pd[pd$Trial > 12,]
interpretation = droplevels(interpretation)
nrow(prior)
nrow(interpretation)
row.names(prior) = paste(prior$assignmentid,prior$Verb,prior$Object,prior$Slider)
interpretation$PriorProbability = prior[paste(interpretation$assignmentid,interpretation$Verb,interpretation$Object,interpretation$Slider),]$normValue

# compute mean prior probability of all-state for each combination of object/verb
agr = aggregate(normValue ~ Object + Verb + NumObjects, data=prior[prior$NumObjects == "all",], FUN=mean)
row.names(agr) = paste(agr$Object, agr$Verb)
interpretation$MeanPrior = agr[paste(interpretation$Object, interpretation$Verb),]$normValue
#sanity check:
unique(paste(interpretation$Object, interpretation$Verb,interpretation$MeanPrior))
save(prior, file="data/prior.RData")
save(interpretation, file="data/interpretation.RData")

# TODO - EXCLUDE PEOPLE BASED ON CLEARLY INATTENTIVE BEHAVIORAL PATTERNS?

# # get a feel for the variation
# porder = pd[ order(pd[ ,c("Prior")]), ]
# pd$Subject = factor(x=as.character(pd$assignmentid),unique(as.character(porder$assignmentid)))
# ggplot(pd, aes(x=Slider,y=SliderValue,color=as.factor(Trial),group=as.factor(Trial))) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(~Subject)
# ggsave(file="graphs/variation.pdf",width=35,height=20)
# 
# 
# # todo: do the exclusion automatically
# 
# # exclude people if they just left all sliders where they were
#  badsliderusers = c("3BQU611VFPJHIBAZQCAUFWXD7Y699R")
#  
#  pd = subset(pd, ! assignmentid %in% badsliderusers)
#  pd = droplevels(pd)
# 
