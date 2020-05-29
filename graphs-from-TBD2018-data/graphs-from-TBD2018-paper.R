
# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
library(tidyverse)
library(ggrepel)

# load helper functions
source('../../helpers.R')

# set black and white plot background
theme_set(theme_bw())

d = read.csv("../data/data_preprocessed.csv")
names(d)

# spread responses over separate columns for projectivity and at-issueness
t = d %>%
  mutate(block_ai = ifelse(question_type == "ai", ifelse(block == "block1", "block1", "block2"), ifelse(block == "block1", "block2", "block1"))) %>%
  dplyr::select(workerid,content,short_trigger,question_type,response,block_ai) %>%
  spread(question_type,response)

# exclude main clauses (fillers)
t_nomc = droplevels(subset(t, short_trigger != "MC"))

# reorder trigger levels
t_nomc$Trigger = factor(x=as.character(t_nomc$short_trigger),levels=c("only","discover","know","stop","stupid","NRRC","annoyed","NomApp","possNP"))

# eval adj paper figure 2
agr = t_nomc %>%
  group_by(Trigger) %>%
  summarise(mean_ai = mean(ai), ci.low.ai=ci.low(ai), ci.high.ai=ci.high(ai), mean_proj = mean(projective), ci.low.proj=ci.low(projective),ci.high.proj=ci.high(projective))
agr = as.data.frame(agr)
agr$YMin = agr$mean_proj - agr$ci.low.proj
agr$YMax = agr$mean_proj + agr$ci.high.proj
agr$XMin = agr$mean_ai - agr$ci.low.ai
agr$XMax = agr$mean_ai + agr$ci.high.ai

# collapsed correlation coefficient reported in paper
cor(agr$mean_ai,agr$mean_proj)

ggplot(agr, aes(x=mean_ai,y=mean_proj,group=1)) +
  geom_text_repel(aes(label=Trigger),alpha=.5,color="blue",size=3) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),color="gray50",alpha=.5) +
  geom_errorbarh(aes(xmin=XMin,xmax=XMax),color="gray50",alpha=.5) +
  geom_point() +
  scale_color_discrete(name="Target expression") +
  xlab("Mean not-at-issueness rating ('asking whether')") +
  ylab("Mean projectivity rating") +
  xlim(0.65,1) +
  ylim(0.65,1)
ggsave(file="../graphs/Fig2-ai-proj-bytrigger-labels.pdf",width=4.2,height=3.5)
ggsave(f="/Users/tonhauser.1/Documents/papers+talks/papers/eval-adj/paper/final-non-anon-formatted-paper/figures/Fig2-ai-proj-bytrigger-labels.pdf",width=4.2,height=3.5)
ggsave(f="/Users/tonhauser.1/Documents/papers+talks/papers/eval-adj/paper/final-non-anon-formatted-paper/figures/Fig2-ai-proj-bytrigger-labels.png",width=4.2,height=3.5)

# overall correlation coefficient for evaluative adjectives
stupid <- droplevels(subset(t_nomc,t_nomc$Trigger == "stupid"))
stupid
cor(stupid$projective,stupid$ai) #.57

# collapsed correlation coefficient for evaluative adjectives 
stupid <- droplevels(subset(agr,agr$Trigger == "stupid"))
stupid
cor(stupid$mean_ai,stupid$mean_proj) #.91

# eval adj paper Figure 3
t_nomc$Item = as.factor(paste(t_nomc$short_trigger, t_nomc$content))
examples = t_nomc %>%
  filter(Item %in% c("stupid cheat", "stupid kids", "stupid nails", "stupid stuntman"))
ggplot(examples, aes(x=ai,y=projective)) +
  # geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_smooth(method="lm") +
  geom_point(size=1) +
  xlab("Not-at-issueness rating") +
  ylab("Projectivity rating") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c(0,.5,1)) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c(0,.5,1)) +
  #xlim(0,1) +
  #ylim(0,1) +
  facet_wrap(~Item,ncol=4)
ggsave("../graphs/Fig3-subject-projai-stupid.pdf",width=6,height=2)
ggsave(f="/Users/tonhauser.1/Documents/papers+talks/papers/eval-adj/paper/final-non-anon-formatted-paper/figures/Fig3-subject-projai-stupid.pdf",width=6,height=2)
ggsave(f="/Users/tonhauser.1/Documents/papers+talks/papers/eval-adj/paper/final-non-anon-formatted-paper/figures/Fig3-subject-projai-stupid.png",width=6,height=2)

