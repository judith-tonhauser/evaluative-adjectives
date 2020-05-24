# evaluative adjectives, content/context experiment, graphs

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load packages
source('helpers.R')
library(tidyverse)

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


## paper figure 5 ----

nrow(cd.target) #1340
cd.target = cd.target %>%
  mutate(condition = case_when(
    Condition %in% c("CnC","CnD") ~ "Content",
    Condition %in% c("CxC","CxD") ~ "Context",
    TRUE ~ "other"
  ))
summary(cd.target)
table(cd.target$condition)

agr = cd.target %>%
  mutate(Response = as.numeric(as.character(Response))) %>%
  group_by(condition,ConDissonant) %>%
  summarise(Mean=mean(Response),CILow=ci.low(Response),CIHigh=ci.high(Response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
agr

agr_item = cd.target %>%
  mutate(Response = as.numeric(as.character(Response))) %>%
  group_by(condition,ConDissonant,Adj) %>%
  summarise(Mean=mean(Response))
agr_item

agr_item_left = agr_item %>%
  filter(ConDissonant == "C", Adj %in% c("polite","rude","wise","stupid","foolish"))

agr_item_right = agr_item %>%
  filter(ConDissonant == "D", Adj %in% c("brave","lucky","mean","smart","fortunate"))

ggplot(agr, aes(x=ConDissonant,y=Mean)) +
  geom_bar(stat="identity") +
  geom_bar(stat="identity",fill="grey90") +
  theme(legend.position="none") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.1) +
  geom_line(data=agr_item, aes(x=ConDissonant,y=Mean,group=Adj,colour=Adj)) +
  geom_text(data=agr_item_left, aes(label=Adj, colour = Adj),hjust=1.2,size=3) +
  geom_text(data=agr_item_right, aes(label=Adj, colour = Adj),hjust=-.2,size=3) +
  scale_x_discrete(name="Truth of generalization follows from common ground",labels=c("more likely","less likely")) +
  scale_y_continuous(name="Mean projectivity rating") +
  facet_wrap(~condition)
ggsave(f="../graphs/exp1-mean-certainty-ratings.pdf",height=4.5,width=6)
ggsave(f="../../../../paper/final-non-anon-formatted-paper/figures/exp1-mean-certainty-ratings.pdf",height=4.5,width=6)
ggsave(f="../../../../paper/final-non-anon-formatted-paper/figures/exp1-mean-certainty-ratings.eps",height=4.5,width=6)

# paper figure 8 ----

# target items
cd.target <- droplevels(subset(cd, cd$Condition != "filler"))
nrow(cd.target) #1340 = 134 Turkers x 10 items
names(cd.target)

# consistency score
table(cd.target$ConDissonant)

table(cd.target$Response)
str(cd.target$Response)

cd.target$Response <- as.numeric(as.character(cd.target$Response))

agr = aggregate(data=cd.target, Response ~ ConDissonant + workerid, FUN=mean)
agr$Follows = as.factor(ifelse(agr$ConDissonant == "C","generalization more likely","generalization less likely"))
agr

cons1 <- agr[ which(agr$ConDissonant=="C" & agr$Response <= 1.5),]
nrow(cons1) #35
cons2 <- agr[ which(agr$ConDissonant=="C" & agr$Response <= 2.5),]
nrow(cons2) #71

dis1 <- agr[ which(agr$ConDissonant=="D" & agr$Response >= 6.5),]
nrow(dis1) #23
dis2 <- agr[ which(agr$ConDissonant=="D" & agr$Response >= 5.5),]
nrow(dis2) #55

intersect(cons1$workerid,dis1$workerid) #1 Turker
intersect(cons2$workerid,dis2$workerid) #19 Turkers

# make histogram that shows what kinds of participants there are
ggplot(agr, aes(x=Response,fill=ConDissonant)) +
  geom_histogram(binwidth = 1,position="dodge",color="black") +
  scale_x_continuous(name = "Mean projectivity rating",breaks = seq(1.5,6.5,1)) +
  scale_y_continuous(name = "Number of participants") +
  labs(y="count") +
  #scale_fill_grey(name="ConDissonant",breaks=levels(agr$ConDissonant),labels=c("consonant","dissonant")) +
  scale_fill_manual(values=c("lightgray","lightgray")) +
  #theme(legend.title = element_blank()) +
  theme(legend.position="none") +
  facet_grid(. ~ Follows)
ggsave(f="../graphs/count-of-participants.pdf",height=2.5,width=5)
ggsave(f="../../../../paper/final-non-anon-formatted-paper/figures/exp1-count-of-participants.pdf",height=2.5,width=5)
ggsave(f="../../../../paper/final-non-anon-formatted-paper/figures/exp1-count-of-participants.eps",height=2.5,width=5)

