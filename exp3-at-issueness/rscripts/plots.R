# evaluative adjectives
# Exp 2: at-issueness of prejacent

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('helpers.R')
library(tidyverse)
theme_set(theme_bw())

# load clean data
load("../data/cd.RData") 
nrow(cd) #1088 = 68 Turkers x 16 ratings

# recode responses so that 1 = yes (ai) and 0 = no (nai)
table(cd$Response)
cd$Response <- gsub("0","2",cd$Response) #change 1 (no) to 2
cd$Response <- gsub("1","0",cd$Response) #change 0 (yes) to 1
cd$Response <- gsub("2","1",cd$Response) #now change 2 (no) to 0

## exclude main clauses from plotting (since they are 100% at-issue, given exclusion of Turkers)
t <- droplevels(subset(cd, cd$ID != "mainclause1" & cd$ID != "mainclause2"))
nrow(t) #952 = 68 Turkers x 14 ratings (10 EAS, 4 projective content)

str(t$Response)
t$Response <- as.numeric(t$Response)
str(t$SentenceType)
t$SentenceType <- as.factor(t$SentenceType)

table(t$SentenceType)

# rename factor levels
t$SentenceType <- factor(t$SentenceType, 
                            levels=c("Comp-of-Annoyed", "Comp-of-Discover","NomApp","PossNP","EAS_n","EAS_f"),
                            labels=c("be_annoyed","discover","NomApp","possNP","neutral","follows from common ground"))

table(t$Adj)

# Figure 7
# target items only: proportion of 'yes' responses (ai) by condition, with error bars
# mean for each adjective overlaid
cd.target <- droplevels(subset(t, t$Adj != "none"))
nrow(cd.target) #680 = 68 Turkers x 10 adjectives

table(cd.target$SentenceType)
str(cd.target$Response)
table(cd.target$Response)

agr = aggregate(Response ~ SentenceType, data=cd.target, FUN="mean")
agr$CILow = aggregate(Response ~ SentenceType, data=cd.target, FUN="ci.low")$Response
agr$CIHigh = aggregate(Response ~ SentenceType, data=cd.target, FUN="ci.high")$Response
agr$YMin = agr$Response - agr$CILow
agr$YMax = agr$Response + agr$CIHigh
dodge = position_dodge(.9)
agr

agr2 = aggregate(Response ~ Adj + SentenceType, data=cd.target, FUN="mean")
agr2

ggplot(agr, aes(x=SentenceType,y=Response,fill=SentenceType)) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",fill="grey90",position=dodge) +
  theme(legend.position="none") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  geom_line(data=agr2, aes(x=SentenceType,y=Response,group=Adj,colour=Adj)) +
  geom_text(data = subset(agr2, SentenceType == "neutral" & (Adj != "polite" & Adj != "wise" & Adj != "fortunate" & Adj != "foolish")), 
            aes(label = Adj, size=12, colour = Adj),hjust=1.3) +
  geom_text(data = subset(agr2, SentenceType == "follows from common ground" & (Adj == "polite" | Adj == "wise" | Adj == "fortunate" | Adj == "foolish")), 
            aes(label = Adj, size=12, colour = Adj),hjust=-.2) +
  scale_x_discrete(name="Generalization") +
  scale_y_continuous(name="Proportion of `yes' responses") 
  #theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
ggsave(f="../graphs/proportion-ai-by-condition.pdf",height=4,width=3.8)

# old figure 7
ggplot(agr, aes(x=SentenceType,y=Response,fill=SentenceType)) +
  geom_bar(stat="identity",position=dodge) +
  geom_bar(stat="identity",color="black",position=dodge) +
  guides(fill=guide_legend(title="generalization")) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  scale_fill_manual(values=c("gray20","grey60")) +
  scale_x_discrete(name="Condition") +
  scale_y_continuous(name="Proportion of `yes' / at-issue responses") +
  facet_grid(. ~ Adj) +
  theme(legend.position="top") +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
ggsave(f="../graphs/proportion-ai-by-condition-and-adj.pdf",height=4,width=10)

