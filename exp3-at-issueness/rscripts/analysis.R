# evaluative adjectives
# Exp 2: at-issueness of prejacent

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load clean data 
load("../data/cd.RData")
nrow(cd) #1088 = 68 Turkers x 16 ratings

# load libraries
library(lme4)
packageVersion("lme4")
library(brms)
citation("brms")

# recode responses so that 1 = yes (ai) and 0 = no (nai)
table(cd$Response)
cd$Response <- gsub("0","2",cd$Response) #change 1 (no) to 2
cd$Response <- gsub("1","0",cd$Response) #change 0 (yes) to 1
cd$Response <- gsub("2","1",cd$Response) #now change 2 (no) to 0

str(cd$Response)
cd$Response <- as.factor(cd$Response)

# target data are the data where the Adj is not "none", i.e everything but the control sentences
cd.target <- droplevels(subset(cd, cd$Adj != "none"))
nrow(cd.target) #680 (680 / 10 adj = 68 Turkers)

# ID = item
names(cd.target)
table(cd.target$ID)

# how many responses per stimulus?
t <- as.data.frame(table(cd.target$ID))
t
min(t$Freq) #10
max(t$Freq) #14
mean(t$Freq) #11.3

# 680 ratings for 60 target sentences in 2 groups: 340 ratings in each group
table(cd.target$SentenceType,cd.target$Adj)
table(cd.target$SentenceType) 
str(cd.target$SentenceType)
cd.target$SentenceType <- as.factor(cd.target$SentenceType)

# ID is the unique identifier of each target stimulus
table(cd.target$ID)
length(unique(cd.target$ID)) #60 sentences = 10 adjectives x 6 sentences

# every Turker saw both types of evalutive adjective sentences
table(cd.target$AIness,cd.target$workerid)

# Table 1
# more 1/yes answers for EAS where generalization follows, i.e., prejacent more at-issue
t <- table(cd.target$Response,cd.target$SentenceType)
print(t)
prop.table(t,2)

str(cd.target$Response)
# make EAS_n the reference level
table(cd.target$SentenceType,cd.target$Adj)
str(cd.target$SentenceType)
cd.target$SentenceType = relevel(cd.target$SentenceType, ref="EAS_n")

# predict response from whether generalization follows
m = glmer(Response ~ SentenceType +  (1 + SentenceType | Adj) + (1 + SentenceType | workerid), data = cd.target, family=binomial(link="logit"), 
          control=glmerControl(optimizer="bobyqa"))
summary(m) #does not converge

m = glmer(Response ~ SentenceType +  (1 + SentenceType | Adj) + (1 | workerid), data = cd.target, family=binomial(link="logit"), 
          control=glmerControl(optimizer="bobyqa"))
summary(m) #does not converge

m = glmer(Response ~ SentenceType +  (1 | Adj) + (1 | workerid), data = cd.target, family=binomial(link="logit"), 
          control=glmerControl(optimizer="bobyqa"))
summary(m) #does not converge

m = glmer(Response ~ SentenceType +  (1 | Adj), data = cd.target, family=binomial(link="logit"), 
          control=glmerControl(optimizer="bobyqa"))
summary(m)

m.1 = glmer(Response ~ 1 +  (1 | Adj), data = cd.target, family=binomial(link="logit"), 
            control=glmerControl(optimizer="bobyqa"))
summary(m.1)

anova(m,m.1)

## random by-participant effect
m = glmer(Response ~ SentenceType +  (1 | workerid), data = cd.target, family=binomial(link="logit"), 
          control=glmerControl(optimizer="bobyqa"))
summary(m)

m.1 = glmer(Response ~ 1 +  (1 | workerid), data = cd.target, family=binomial(link="logit"), 
            control=glmerControl(optimizer="bobyqa"))
summary(m.1)

anova(m,m.1)

# Bayesian regression analysis reported in paper
cd.target$NumResponse = as.numeric(ifelse(cd.target$Response == "0",0,1))
m = brm(NumResponse ~ SentenceType +  (1 | ID) + (1 + SentenceType | workerid) + (1 + SentenceType | workerid), 
        data = cd.target, seed=42, family=bernoulli())
summary(m)
plot(m, pars = c("SentenceType"))
plot(m, pars = c("workerid"))
plot(m, pars = c("ID"))

# sentence type effect:
fixef(m)["SentenceTypeEAS_f", ]
# posterior probability of sentence type beta > 0
mean(posterior_samples(m, pars = "b_SentenceType") > 0)
