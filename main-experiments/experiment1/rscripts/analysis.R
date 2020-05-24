# evaluative adjectives, content/context experiment, analysis

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load packages
source('helpers.R')
library(tidyverse)
library(ordinal)

theme_set(theme_bw())

# load data
cd = read.csv(file="../data/cd.csv")
       
nrow(cd) #2144 (134 Turkers, each gave 16 judgments)
summary(cd)

# factors
str(cd$Condition)
cd$Condition = as.factor(cd$Condition)
str(cd$Response) #numerical
cd$Response = as.factor(cd$Response)

# target data
cd.target <- droplevels(subset(cd, cd$Condition != "filler"))
nrow(cd.target) #1340 = 134 Turkers x 10 judgments

table(cd.target$ConDissonant)
str(cd.target$ConDissonant)
cd.target$ConDissonant <- as.factor(cd.target$ConDissonant)

table(cd.target$Condition)
#CnC CnD CxC CxD 
#338 330 332 340

# make new item column
names(cd.target)
table(cd.target$Adj)
table(cd.target$VPinf)
table(cd.target$Context)
cd.target$item <- paste(cd.target$Context,cd.target$Adj,cd.target$VPinf,"-")
table(cd.target$item)

# create subsets for the content and context conditions

cd.target.content <- droplevels(subset(cd.target, cd.target$Condition == "CnC" | cd.target$Condition == "CnD"))
nrow(cd.target.content) #668 = CnC + CnD

cd.target.context <- droplevels(subset(cd.target, cd.target$Condition == "CxC" | cd.target$Condition == "CxD"))
nrow(cd.target.context) #672 = CxC + CxD

# models
content <- clmm(Response ~ ConDissonant + (1+ConDissonant|workerid) + (1 | item), data=cd.target.content)
summary(content)

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# ConDissonantD   3.2877     0.3422   9.608   <2e-16 ***
  
content.2 <- clmm(Response ~ 1  + (1+ConDissonant|workerid) + (1 | item), data=cd.target.content)
anova(content,content.2)

# Likelihood ratio tests of cumulative link models:
#   
#   formula:                                                            
#   content.2 Response ~ 1 + (1 + ConDissonant | workerid) + (1 | item)           
# content   Response ~ ConDissonant + (1 + ConDissonant | workerid) + (1 | item)
# link: threshold:
#   content.2 logit flexible  
# content   logit flexible  
# 
# no.par    AIC  logLik LR.stat df Pr(>Chisq)    
# content.2     10 2094.6 -1037.3                          
# content       11 2023.6 -1000.8  73.042  1  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# context models
context <- clmm(Response ~ ConDissonant + (1+ConDissonant|workerid) + (1 | item), data=cd.target.context)
summary(context)

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# ConDissonantD   2.1037     0.3674   5.726 1.03e-08 ***

context.2 <- clmm(Response ~ 1 + (1+ConDissonant|workerid) + (1 | item), data=cd.target.context)
anova(context,context.2)

# Likelihood ratio tests of cumulative link models:
#   
#   formula:                                                            
#   context.2 Response ~ 1 + (1 + ConDissonant | workerid) + (1 | item)           
# context   Response ~ ConDissonant + (1 + ConDissonant | workerid) + (1 | item)
# link: threshold:
#   context.2 logit flexible  
# context   logit flexible  
# 
# no.par    AIC  logLik LR.stat df Pr(>Chisq)    
# context.2     10 2096.1 -1038.0                          
# context       11 2068.4 -1023.2  29.675  1  5.109e-08 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

