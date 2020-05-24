# created by jdegen, 03/19/2014

setwd('~/webprojects/57_sinkingmarbles/results/') 
source('rscripts/helpers.R')
library(grid)
library(ggplot2)

# load any or all of the following datasets:
load("data/d.RData") # data with only minimal preprocessing; eg non-native subjects excluded (won't be very helpful)
load("data/pd.RData") 
load("data/prior.RData") # only prior (first 12 trials)
load("data/interpretation.RData") # only interpretation (second 12 trials)

summary(pd)

length(unique(pd$assignmentid)) # 99 total

## NORMALIZED DATA
agr = aggregate(Response ~ ID, data=md, FUN=mean)
agr$CILow = aggregate(Response ~ ID, data=md, FUN=ci.low)$Response
agr$CIHigh = aggregate(Response ~ ID, data=md, FUN=ci.high)$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh

ggplot(agr, aes(x=ID,y=Response,color=ID)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) 
ggsave(f="graphs/test.pdf")

agr = aggregate(Response ~ Verb, data=md, FUN=mean)
agr$CILow = aggregate(Response ~ Verb, data=md, FUN=ci.low)$Response
agr$CIHigh = aggregate(Response ~ Verb, data=md, FUN=ci.high)$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh

ggplot(agr, aes(x=Verb,y=Response,color=Verb)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  theme(axis.text.x=element_text(vjust=1,hjust=1,angle=45))
ggsave(f="graphs/test.pdf")

all = agr[agr$NumObjects == "all",]
ggplot(all, aes(x=Object,y=normValue)) +
  geom_point() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_text(aes(label=as.character(round(normValue,2)),y = normValue + CIHigh+0.05)) +
  facet_wrap(~Verb)
ggsave(f="graphs/all_prior.pdf",width=6,height=4)

## RAW DATA
agr = aggregate(SliderValue ~ Object + Verb + NumObjects, data=prior, FUN=mean)
agr$CILow = aggregate(SliderValue ~ Object + Verb + NumObjects, data=prior, FUN=ci.low)$SliderValue
agr$CIHigh = aggregate(SliderValue ~ Object + Verb + NumObjects, data=prior, FUN=ci.high)$SliderValue
agr$YMin = agr$SliderValue - agr$CILow
agr$YMax = agr$SliderValue + agr$CIHigh

broke = agr[agr$Verb == "broke" & agr$NumObjects == "all",]
sank = agr[agr$Verb == "sank" & agr$NumObjects == "all",]

broke[ order(broke$SliderValue, decreasing=T),]
sank[ order(sank$SliderValue, decreasing=T),]

ggplot(agr, aes(x=NumObjects,y=SliderValue,color=Verb,group=Verb)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  facet_wrap(~Object)
ggsave(f="graphs/raw_priors.pdf",width=6,height=4)

## NORMALIZED DATA
all = interpretation[interpretation$NumObjects == "all",]
nrow(all)

# agr = aggregate(normValue ~ Object + Verb + NumObjects, data=all, FUN=mean)
# agr$CILow = aggregate(normValue ~ Object + Verb + NumObjects, data=all, FUN=ci.low)$normValue
# agr$CIHigh = aggregate(normValue ~ Object + Verb + NumObjects, data=all, FUN=ci.high)$normValue
# agr$YMin = agr$normValue - agr$CILow
# agr$YMax = agr$normValue + agr$CIHigh

ggplot(all, aes(x=PriorProbability,y=normValue,color=Verb,shape=Object,group=Verb)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Quantifier)
ggsave(f="graphs/posterior.pdf",width=6,height=4)

# these are the people who don't take the literal interpretation of "all" seriously (ie who have a lower than .85 normalized value on all objects):
alldeniers = as.data.frame(table(all[all$normValue < .85 & all$Quantifier == "all",]$assignmentid))
alldeniers = alldeniers[alldeniers$Freq > 0,]
alldeniers # there seems to be one exception, "33F859I566CTLA7FM8CIJJQRGY9BHX", who is only below threshold once

# how do they behave on "some" trials?




agr = aggregate(normValue ~ Object + Verb + NumObjects + Quantifier, data=interpretation, FUN=mean)
agr$CILow = aggregate(normValue ~ Object + Verb + NumObjects + Quantifier, data=interpretation, FUN=ci.low)$normValue
agr$CIHigh = aggregate(normValue ~ Object + Verb + NumObjects + Quantifier, data=interpretation, FUN=ci.high)$normValue
agr$YMin = agr$normValue - agr$CILow
agr$YMax = agr$normValue + agr$CIHigh

broke = agr[agr$Verb == "broke" & agr$NumObjects == "all",]
sank = agr[agr$Verb == "sank" & agr$NumObjects == "all",]

broke[ order(broke$normValue, decreasing=T),]
sank[ order(sank$normValue, decreasing=T),]

ggplot(agr, aes(x=NumObjects,y=normValue,color=Verb,group=Verb)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  facet_grid(Quantifier~Object)
ggsave(f="graphs/posteriors_byobject_byverb.pdf",width=12,height=4)


agr = aggregate(normValue ~ Object + Verb + NumObjects + Quantifier, data=pd, FUN=mean)
agr$CILow = aggregate(normValue ~ Object + Verb + NumObjects + Quantifier, data=pd, FUN=ci.low)$normValue
agr$CIHigh = aggregate(normValue ~ Object + Verb + NumObjects + Quantifier, data=pd, FUN=ci.high)$normValue
agr$YMin = agr$normValue - agr$CILow
agr$YMax = agr$normValue + agr$CIHigh
levels(agr$Quantifier) = c(levels(agr$Quantifier),"prior")
agr[agr$Quantifier == "none",]$Quantifier = "prior"
agr = droplevels(agr)

ggplot(agr, aes(x=NumObjects,y=normValue,color=Quantifier,group=Quantifier)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  facet_grid(Verb~Object)
ggsave(f="graphs/posteriors_byobject_byverb_withprior.pdf",width=12,height=4)


agr = aggregate(normValue ~ Object + Quantifier + Verb + MeanPrior, data=interpretation[interpretation$NumObjects == "all",], FUN=mean)
agr$CILow = aggregate(normValue ~ Object + Quantifier + Verb + MeanPrior, data=interpretation[interpretation$NumObjects == "all",], FUN=ci.low)$normValue
agr$CIHigh = aggregate(normValue ~ Object + Quantifier + Verb +MeanPrior, data=interpretation[interpretation$NumObjects == "all",], FUN=ci.high)$normValue
agr$YMin = agr$normValue - agr$CILow
agr$YMax = agr$normValue + agr$CIHigh

ggplot(agr, aes(x=MeanPrior,y=normValue,color=Quantifier,linetype=Verb)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.05)
ggsave(f="graphs/posteriors_byobject_byverb_withprior.pdf",width=12,height=4)

