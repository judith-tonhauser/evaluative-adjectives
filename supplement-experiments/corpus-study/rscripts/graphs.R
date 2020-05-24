# evaluative adjective, corpus study, plots

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load packages 
source('helpers.R')
library(tidyverse)
theme_set(theme_bw())

# load clean data
cd = read.csv("../data/cd.csv")

names(cd)
table(cd$Context)
nrow(cd) #2260

# get rid of fillers
cd.target <- subset(cd, cd$Index != "filler1No" & cd$Index != "filler3Yes")
cd.target = droplevels(cd.target)
nrow(cd.target) #1808 (= 226 turkers x 8 items)

# how many target stimuli?
length(unique(cd.target$Context)) #59

# context sentences
context <- unique(cd.target$Context)
context

# names
unique(cd.target$Question)

# how many lists
length(unique(cd.target$List)) #8

# How many responses per item?
names(cd.target)
tmp <- as.data.frame(table(cd.target$Index))
tmp
# items that appeared on more than one list have higher number of responses
tmp1 <- subset(tmp,tmp$Freq > 45)
tmp1
mean(tmp1$Freq)
# items that appeared on only one list
tmp2 <- subset(tmp,tmp$Freq <= 45)
tmp2
length(tmp2$Freq)
mean(tmp2$Freq)
min(tmp2$Freq)
max(tmp2$Freq)


head(cd.target)
summary(cd.target)
str(cd.target$Response)
table(cd.target$Index,cd.target$Response)

# Paper figure 1
# Mean responses to the sentences, by adjective
agr = cd.target %>%
  group_by(Index,Adj) %>%
  summarize(Mean = mean(Response), CILow = ci.low(Response), CIHigh = ci.high(Response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)
agr
nrow(agr) #59

myjit <- ggproto("fixJitter", PositionDodge,
                   width = 0.3,
                   dodge.width = 0.3,
                   jit = NULL,
                   compute_panel =  function (self, data, params, scales) 
                   {
                     
                     #Generate Jitter if not yet
                     if(is.null(self$jit) ) {
                       self$jit <-jitter(rep(0, nrow(data)), amount=self$dodge.width)
                     }
                     
                     data <- ggproto_parent(PositionDodge, self)$compute_panel(data, params, scales)
                     
                     data$x <- data$x + self$jit
                     #For proper error extensions
                     if("xmin" %in% colnames(data)) data$xmin <- data$xmin + self$jit
                     if("xmax" %in% colnames(data)) data$xmax <- data$xmax + self$jit
                     data
                   } )

ggplot(agr, aes(x=Adj,y=Mean,color=Adj)) +
  geom_point(position=myjit,size=2) + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),alpha=.5,width=0.2,position=myjit) +
  theme(panel.background = element_blank()) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10)) +
  theme(legend.position="none") +
  scale_x_discrete(name="Adjective of negated evaluative adjective sentence") +
  scale_y_discrete(limits = c(1,2,3,4,5,6,7),name="Mean certainty rating")
ggsave("../graphs/mean-response-by-item-and-adj.pdf",width=5,height=2)

