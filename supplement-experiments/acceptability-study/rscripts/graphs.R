# evaluative adjectives, acceptability rating study
# R file to create figures provided in Glossa paper

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')

# load required packages
library(tidyverse)
library(dplyr)
library(dichromat)
library(ggrepel)

theme_set(theme_bw())

# load clean data
cd <- read.csv(file="../data/cd.csv", header=TRUE, sep=",")

nrow(cd) #1692 / 18 = 94 Turkers
names(cd)
head(cd)

# target items
d = droplevels(subset(cd, Condition != "filler"))
head(d)

# mean response to noE and E items
agr = aggregate(data=d, Response ~ Enough, FUN=mean)
agr

str(d$Response)
d$Response <- as.numeric(d$Response)

# Supplement figure 1

# count of responses to target items
ggplot(d, aes(x=Response, fill=Enough)) +
  geom_histogram(position="dodge",binwidth = .5) +
  labs(x = "Response", y = "count") +
  scale_x_discrete(limits=c(1,2,3,4,5,6,7)) +
  # for colored bars in talks
  # scale_fill_discrete(name="",breaks=levels(d$Enough),labels=c("adjective enough","NEAS")) +
  # for grey scale bars in paper:
  scale_fill_grey(name="",breaks=levels(d$Enough),labels=c("adjective enough","NEAS")) +
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position="top",
        panel.background = element_blank())
ggsave("../graphs/acc-histogram.pdf",height=3,width=4)

# Supplement figure 2

# participants' acceptability scores
acc = d %>% 
  #filter(ExpPart == "accept" & Condition %in% c("factive","implicative")) %>%
  dplyr::select(workerid,Response,Enough) %>%
  group_by(workerid,Enough) %>%
  summarise(Mean = mean(Response)) %>%
  spread(Enough, Mean)
acc = as.data.frame(acc)
head(acc)
acc$Diff = acc$noE - acc$E
row.names(acc) = acc$workerid
#acc$AccMean = imp[acc$workerid,]$Mean
head(acc)

# participants for whom 'adjective' at least as good as 'adjective enough'
nrow(acc[acc$Diff >= 0, ]) #20
# participants for whom 'adjective' just about as good as 'adjective enough'
nrow(acc[acc$Diff >= -0.25, ]) #23
nrow(acc[acc$Diff >= -0.5, ]) #27

acc$workerid2 <-factor(acc$workerid, levels=acc[order(acc$Diff), "workerid"])

ggplot(acc, aes(x=workerid2,y=Diff)) +
  geom_point() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        panel.background = element_blank()) +
  geom_segment(y=0,xend=0,y=0,yend=0,color='red') +
  scale_x_discrete(name="Participant") +
  scale_y_continuous(name="Acceptability score")
ggsave("../graphs/acc-acceptability-rating-difference.pdf",width=4.5,height=3)

