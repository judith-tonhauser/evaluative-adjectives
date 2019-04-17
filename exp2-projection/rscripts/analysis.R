# evaluative adjectives, content/context experiment, analysis

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load packages
source('helpers.R')
library(tidyverse)
library(ordinal)

# load data
load("../data/cd.RData")

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

cd.target.content <- droplevels(subset(cd.target, cd.target$Condition == "CnC" | cd.target$Condition == "CnD"))
nrow(cd.target.content) #668 = CnC + CnD

cd.target.context <- droplevels(subset(cd.target, cd.target$Condition == "CxC" | cd.target$Condition == "CxD"))
nrow(cd.target.context) #672 = CxC + CxD

# content models
content <- clmm(Response ~ ConDissonant + (1+ConDissonant|workerid) + (1 | item), data=cd.target.content)
summary(content)
content.2 <- clmm(Response ~ 1  + (1+ConDissonant|workerid) + (1 | item), data=cd.target.content)
anova(content,content.2)

# context models
context <- clmm(Response ~ ConDissonant + (1+ConDissonant|workerid) + (1 | item), data=cd.target.context)
summary(context)
context.2 <- clmm(Response ~ 1 + (1+ConDissonant|workerid) + (1 | item), data=cd.target.context)
anova(context,context.2)

#################

# Predict Response from ConDissonant in Context data
str(cd.target.context$ConDissonant)
cd.target.context$ConDissonant = as.factor(cd.target.context$ConDissonant)

# full model
context <- clmm(Response ~ ConDissonant + (1+ConDissonant|workerid) + (1+ConDissonant|Adj) + (1|ID), data=cd.target.context)
summary(context)
# model without slopes
context.1 <- clmm(Response ~ ConDissonant + (1|workerid) + (1|Adj) + (1|ID), data=cd.target.context)
summary(context.1)
# comparison shows that full model is better
anova(context,context.1)

## Does Age matter?

str(cd.target.content$ConDissonant)
str(cd.target.content$OldYoung)
cd.target.content$ConDissonant = as.factor(cd.target.content$ConDissonant)
cd.target.content$OldYoung = as.factor(cd.target.content$OldYoung)
contentOldYoung <- clmm(Response ~ ConDissonant*OldYoung + (1|workerid) + (1|ID), data=cd.target.content)
summary(contentOldYoung)

# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: Response ~ ConDissonant * OldYoung + (1 | workerid) + (1 | ID)
# data:    cd.target.content
# 
# link  threshold nobs logLik   AIC     niter     max.grad cond.H 
# logit flexible  720  -1106.09 2234.17 930(4493) 1.68e-03 3.6e+02
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# workerid (Intercept) 2.2852   1.5117  
# ID       (Intercept) 0.4218   0.6494  
# Number of groups:  workerid 145,  ID 60 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# ConDissonantD                 3.2660     0.3223  10.133   <2e-16 ***
#   OldYoungYoung                 0.4331     0.3453   1.254   0.2097    
# ConDissonantD:OldYoungYoung  -0.7460     0.3269  -2.282   0.0225 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# 
# Threshold coefficients:
#   Estimate Std. Error z value
# 1|2   0.1452     0.2738   0.530
# 2|3   0.7295     0.2764   2.639
# 3|4   1.5454     0.2840   5.442
# 4|5   1.9667     0.2894   6.795
# 5|6   2.8691     0.3042   9.433
# 6|7   3.4803     0.3150  11.048
# (7 observations deleted due to missingness)

str(cd.target.content$OldMiddleYoung)
cd.target.content$OldMiddleYoung = as.factor(cd.target.content$OldMiddleYoung)
contentOldMiddleYoung <- clmm(Response ~ ConDissonant*OldMiddleYoung + (1|workerid) + (1|ID), data=cd.target.content)
summary(contentOldMiddleYoung)

# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: Response ~ ConDissonant * OldMiddleYoung + (1 | workerid) + (1 |      ID)
# data:    cd.target.content
# 
# link  threshold nobs logLik   AIC     niter      max.grad cond.H 
# logit flexible  720  -1106.23 2238.47 1268(6158) 4.01e-04 6.3e+02
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# workerid (Intercept) 2.2448   1.4983  
# ID       (Intercept) 0.4141   0.6435  
# Number of groups:  workerid 145,  ID 60 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# ConDissonantD                      3.31230    0.38269   8.655   <2e-16 ***
#   OldMiddleYoungOld                 -0.20426    0.51351  -0.398   0.6908    
# OldMiddleYoungYoung                0.04307    0.39505   0.109   0.9132    
# ConDissonantD:OldMiddleYoungOld   -0.15407    0.49997  -0.308   0.7580    
# ConDissonantD:OldMiddleYoungYoung -0.72434    0.38016  -1.905   0.0567 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# 
# Threshold coefficients:
#   Estimate Std. Error z value
# 1|2 -0.08486    0.34328  -0.247
# 2|3  0.49660    0.34475   1.440
# 3|4  1.30986    0.34948   3.748
# 4|5  1.73033    0.35321   4.899
# 5|6  2.63312    0.36464   7.221
# 6|7  3.24533    0.37341   8.691
# (7 observations deleted due to missingness)

str(cd.target.context$OldYoung)
cd.target.context$ConDissonant = as.factor(cd.target.context$ConDissonant)
cd.target.context$OldYoung = as.factor(cd.target.context$OldYoung)
contextOldYoung <- clmm(Response ~ ConDissonant*OldYoung + (1|workerid) + (1|ID), data=cd.target.context)
summary(contextOldYoung)

# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: Response ~ ConDissonant * OldYoung + (1 | workerid) + (1 | ID)
# data:    cd.target.context
# 
# link  threshold nobs logLik   AIC     niter     max.grad cond.H 
# logit flexible  730  -1145.25 2312.51 956(4688) 4.69e-04 5.9e+02
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# workerid (Intercept) 2.7384   1.6548  
# ID       (Intercept) 0.7402   0.8604  
# Number of groups:  workerid 145,  ID 60 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# ConDissonantD                1.77595    0.32193   5.517 3.46e-08 ***
#   OldYoungYoung               -0.05568    0.36385  -0.153    0.878    
# ConDissonantD:OldYoungYoung -0.16327    0.32557  -0.501    0.616    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# 
# Threshold coefficients:
#   Estimate Std. Error z value
# 1|2  -0.3555     0.3024  -1.176
# 2|3   0.3289     0.3027   1.086
# 3|4   1.0228     0.3061   3.341
# 4|5   1.3033     0.3084   4.226
# 5|6   2.0617     0.3162   6.520
# 6|7   2.6544     0.3238   8.197
# (3 observations deleted due to missingness)

str(cd.target.context$OldMiddleYoung)
cd.target.context$OldMiddleYoung = as.factor(cd.target.context$OldMiddleYoung)
contextOldMiddleYoung <- clmm(Response ~ ConDissonant*OldMiddleYoung + (1|workerid) + (1|ID), data=cd.target.context)
summary(contextOldMiddleYoung)

# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: Response ~ ConDissonant * OldMiddleYoung + (1 | workerid) + (1 |      ID)
# data:    cd.target.context
# 
# link  threshold nobs logLik   AIC     niter      max.grad cond.H 
# logit flexible  730  -1143.68 2313.36 1193(5800) 2.29e-03 1.0e+03
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# workerid (Intercept) 2.675    1.6354  
# ID       (Intercept) 0.739    0.8597  
# Number of groups:  workerid 145,  ID 60 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# ConDissonantD                       1.9123     0.3759   5.087 3.63e-07 ***
#   OldMiddleYoungOld                  -0.3493     0.5282  -0.661    0.508    
# OldMiddleYoungYoung                -0.4375     0.4182  -1.046    0.296    
# ConDissonantD:OldMiddleYoungOld    -0.3016     0.4596  -0.656    0.512    
# ConDissonantD:OldMiddleYoungYoung  -0.3234     0.3709  -0.872    0.383    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# 
# Threshold coefficients:
#   Estimate Std. Error z value
# 1|2 -0.62795    0.37527  -1.673
# 2|3  0.05686    0.37480   0.152
# 3|4  0.75162    0.37649   1.996
# 4|5  1.03245    0.37779   2.733
# 5|6  1.79212    0.38277   4.682
# 6|7  2.38552    0.38802   6.148
# (3 observations deleted due to missingness)

## Predicting response from age alone

age1 <- clmm(Response ~ OldYoung + (1|workerid) + (1|ID), data=cd.target)
summary(age1)

# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: 
#   Response ~ OldMiddleYoung + (1 | workerid) + (1 | ID)
# data:    cd.target
# 
# link  threshold nobs logLik   AIC    
# logit flexible  1450 -2263.66 4547.32
# niter     max.grad cond.H 
# 957(4695) 5.57e-03 9.7e+02
# 
# Random effects:
#   Groups   Name        Variance
# workerid (Intercept) 2.388   
# ID       (Intercept) 2.128   
# Std.Dev.
# 1.545   
# 1.459   
# Number of groups:  workerid 145,  ID 120 
# 
# Coefficients:
#   Estimate
# OldMiddleYoungOld    -0.3323
# OldMiddleYoungYoung  -0.3384
# Std. Error z value
# OldMiddleYoungOld       0.4170  -0.797
# OldMiddleYoungYoung     0.3272  -1.034
# Pr(>|z|)
# OldMiddleYoungOld      0.425
# OldMiddleYoungYoung    0.301
# 
# Threshold coefficients:
#   Estimate Std. Error z value
# 1|2  -1.6150     0.2998  -5.386
# 2|3  -0.9884     0.2982  -3.315
# 3|4  -0.2407     0.2970  -0.810
# 4|5   0.1060     0.2969   0.357
# 5|6   0.9239     0.2978   3.102
# 6|7   1.5186     0.2995   5.070
# (10 observations deleted due to missingness)

age2 <- clmm(Response ~ OldMiddleYoung + (1|workerid) + (1|ID), data=cd.target)
summary(age2)

# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: Response ~ OldMiddleYoung + (1 | workerid) + (1 | ID)
# data:    cd.target
# 
# link  threshold nobs logLik   AIC     niter     max.grad cond.H 
# logit flexible  1450 -2263.66 4547.32 957(4695) 5.57e-03 9.7e+02
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# workerid (Intercept) 2.388    1.545   
# ID       (Intercept) 2.128    1.459   
# Number of groups:  workerid 145,  ID 120 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# OldMiddleYoungOld    -0.3323     0.4170  -0.797    0.425
# OldMiddleYoungYoung  -0.3384     0.3272  -1.034    0.301
# 
# Threshold coefficients:
#   Estimate Std. Error z value
# 1|2  -1.6150     0.2998  -5.386
# 2|3  -0.9884     0.2982  -3.315
# 3|4  -0.2407     0.2970  -0.810
# 4|5   0.1060     0.2969   0.357
# 5|6   0.9239     0.2978   3.102
# 6|7   1.5186     0.2995   5.070
# (10 observations deleted due to missingness)

age3 <- clmm(Response ~ ReallyOldYoung + (1|workerid) + (1|ID), data=cd.target)
summary(age3)

# Cumulative Link Mixed Model fitted with the Laplace approximation
# 
# formula: Response ~ ReallyOldYoung + (1 | workerid) + (1 | ID)
# data:    cd.target
# 
# link  threshold nobs logLik   AIC     niter     max.grad cond.H 
# logit flexible  1450 -2264.18 4546.36 825(4042) 1.94e-03 2.2e+03
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# workerid (Intercept) 2.412    1.553   
# ID       (Intercept) 2.132    1.460   
# Number of groups:  workerid 145,  ID 120 
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# ReallyOldYoungYoung   0.1699     0.4637   0.366    0.714
# 
# Threshold coefficients:
#   Estimate Std. Error z value
# 1|2  -1.2233     0.4627  -2.644
# 2|3  -0.5963     0.4620  -1.291
# 3|4   0.1515     0.4617   0.328
# 4|5   0.4981     0.4618   1.079
# 5|6   1.3160     0.4629   2.843
# 6|7   1.9106     0.4645   4.113
# (10 observations deleted due to missingness)

### How consistent are the Turkes / testing the dialect claim 

table(cd.target$Condition)
table(cd.target$Response)
table(cd.target$Condition,cd.target$Response)

#       1   2   3   4   5   6   7
# CnC 186  40  52  15  26   8  39
# CnD  44  17  28  25  57  45 145
# CxC 164  42  38  13  32  19  56
# CxD  71  29  35  16  44  36 138

table(cd.target$ConDissonant,cd.target$Response)

#     1   2   3   4   5   6   7
# C 350  82  90  28  58  27  95
# D 115  46  63  41 101  81 283

# code Response as 'Factive' (5,6,7), 'Implicative' (1,2,3), or 'Neither' (4)

cd.target$FactOrImpl = cd.target$Response #make copy of Response column
cd.target$FactOrImpl <- c("Impl", "Neither", "Fact")[ findInterval(cd.target$Response, c(-Inf,4,5,Inf)) ]
table(cd.target$FactOrImpl)
table(cd.target$Response,cd.target$FactOrImpl) # check to see if responses split in right way
nrow(cd.target) #1460 (good)

table(cd.target$FactOrImpl)

# subset the data by whether the response was to a consonant or to a dissonant item (across both studies)
cd.target.consonant = subset(cd.target, cd.target$ConDissonant == "C")
cd.target.dissonant = subset(cd.target, cd.target$ConDissonant == "D")
cd.target.consonant = droplevels(cd.target.consonant)
cd.target.dissonant = droplevels(cd.target.dissonant)
nrow(cd.target.consonant) #730 (good)
nrow(cd.target.dissonant) #730 (good)

# Look at how many factive, implicative and neither responses each Turker gave

con <- table(cd.target.consonant$workerid,cd.target.consonant$FactOrImpl)
colnames(con) <- c("FactiveResponseConsontItem","ImplicativeResponseConsonantItem","NeitherConsonantItem")
con
dis <- table(cd.target.dissonant$workerid,cd.target.dissonant$FactOrImpl)
colnames(dis) <- c("FactiveResponseDissonantItem","ImplicativeResponseDissonantItem","NeitherDissonantItem")
dis

consistent = cbind(con,dis)
consistent
write.csv(consistent, file = "consistent.csv",row.names=FALSE, na="")

# Looking at responses to "enough" items
table(cd.E$Response)
cd.E$Impl = cd.E$Response #make copy of Response column
cd.E$Impl <- c("Fact", "Neither", "Impl")[ findInterval(cd.E$Response, c(-Inf,4,5,Inf)) ] #split at 4
table(cd.E$Impl)
table(cd.E$Response,cd.E$Impl)
nrow(cd.E)
#625

# More implicative than factive responses to these items
table(cd.noE$Impl)

#Fact    Impl Neither 
#244     334      45 

# Chi-square analysis to see if significantly more implicative responses
chisq.test(table(cd.noE$Impl))

# Chi-squared test for given probabilities
# 
# data:  table(cd.noE$Impl) 
# X-squared = 210.6292, df = 2, p-value < 2.2e-16

# jdegen:
# Response ~ (1|Subject)
# Imp: 1, Fac: 0
# If coefficient significant & positive -> more implicative than factive readings
lmer(Response ~ (1|workerid), data = cd.noE, family="binomial")

# Intercept is significant; shows that ???
# Generalized linear mixed model fit by the Laplace approximation 
# Formula: Response ~ (1 | workerid) 
# Data: cd.noE 
# AIC   BIC logLik deviance
# 547.3 556.1 -271.6    543.3
# Random effects:
#   Groups   Name        Variance Std.Dev.
# workerid (Intercept) 3.2223   1.7951  
# Number of obs: 623, groups: workerid, 125
# 
# Fixed effects:
#   Estimate Std. Error z value
# (Intercept)   2.2950     0.2156   10.64
# Pr(>|z|)    
# (Intercept)   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 


enough = table(cd.E$workerid,cd.E$Impl)
enough
write.csv(enough, file = "enough.csv",row.names=FALSE, na="")

names(cd)
str(cd$Response)
cd$Response = as.factor(cd$Response)

clmm1 <- clmm(Response ~ Enough + (Enough+1|workerid) + (Enough+1|ID), data=cd)
summary(clmm1)

cd.target$Response = as.factor(cd.target$Response)
cd.target$OldYoung = as.factor(cd.target$OldYoung)
cd.target$OldMiddleYoung = as.factor(cd.target$OldMiddleYoung)

cd$Response = as.factor(cd$Response)
cd$Enough = as.factor(cd$Enough)

