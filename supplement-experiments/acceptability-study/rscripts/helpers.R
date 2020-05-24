reshapeAcceptabilityData <- function(d)
{
  d$Trial1 = paste(d$List0,d$ID0,d$Condition0,d$Context0,d$Target0,d$Response0,d$Adj0,d$Contradict0,d$VPinf0,d$Enough0,sep = " ")
  d$Trial2 = paste(d$List1,d$ID1,d$Condition1,d$Context1,d$Target1,d$Response1,d$Adj1,d$Contradict1,d$VPinf1,d$Enough1,sep = " ")
  d$Trial3 = paste(d$List2,d$ID2,d$Condition2,d$Context2,d$Target2,d$Response2,d$Adj2,d$Contradict2,d$VPinf2,d$Enough2,sep = " ")
  d$Trial4 = paste(d$List3,d$ID3,d$Condition3,d$Context3,d$Target3,d$Response3,d$Adj3,d$Contradict3,d$VPinf3,d$Enough3,sep = " ")
  d$Trial5 = paste(d$List4,d$ID4,d$Condition4,d$Context4,d$Target4,d$Response4,d$Adj4,d$Contradict4,d$VPinf4,d$Enough4,sep = " ")
  d$Trial6 = paste(d$List5,d$ID5,d$Condition5,d$Context5,d$Target5,d$Response5,d$Adj5,d$Contradict5,d$VPinf5,d$Enough5,sep = " ")
  d$Trial7 = paste(d$List6,d$ID6,d$Condition6,d$Context6,d$Target6,d$Response6,d$Adj6,d$Contradict6,d$VPinf6,d$Enough6,sep = " ")
  d$Trial8 = paste(d$List7,d$ID7,d$Condition7,d$Context7,d$Target7,d$Response7,d$Adj7,d$Contradict7,d$VPinf7,d$Enough7,sep = " ")
  d$Trial9 = paste(d$List8,d$ID8,d$Condition8,d$Context8,d$Target8,d$Response8,d$Adj8,d$Contradict8,d$VPinf8,d$Enough8,sep = " ")
  d$Trial10 = paste(d$List9,d$ID9,d$Condition9,d$Context9,d$Target9,d$Response9,d$Adj9,d$Contradict9,d$VPinf9,d$Enough9,sep = " ")
  d$Trial11 = paste(d$List10,d$ID10,d$Condition10,d$Context10,d$Target10,d$Response10,d$Adj10,d$Contradict10,d$VPinf10,d$Enough10,sep = " ")
  d$Trial12 = paste(d$List11,d$ID11,d$Condition11,d$Context11,d$Target11,d$Response11,d$Adj11,d$Contradict11,d$VPinf11,d$Enough11,sep = " ")
  d$Trial13 = paste(d$List12,d$ID12,d$Condition12,d$Context12,d$Target12,d$Response12,d$Adj12,d$Contradict12,d$VPinf12,d$Enough12,sep = " ")
  d$Trial14 = paste(d$List13,d$ID13,d$Condition13,d$Context13,d$Target13,d$Response13,d$Adj13,d$Contradict13,d$VPinf13,d$Enough13,sep = " ")
  d$Trial15 = paste(d$List14,d$ID14,d$Condition14,d$Context14,d$Target14,d$Response14,d$Adj14,d$Contradict14,d$VPinf14,d$Enough14,sep = " ")
  d$Trial16 = paste(d$List15,d$ID15,d$Condition15,d$Context15,d$Target15,d$Response15,d$Adj15,d$Contradict15,d$VPinf15,d$Enough15,sep = " ")
  d$Trial17 = paste(d$List16,d$ID16,d$Condition16,d$Context16,d$Target16,d$Response16,d$Adj16,d$Contradict16,d$VPinf16,d$Enough16,sep = " ")
  d$Trial18 = paste(d$List17,d$ID17,d$Condition17,d$Context17,d$Target17,d$Response17,d$Adj17,d$Contradict17,d$VPinf17,d$Enough17,sep = " ")
  return(d) 
} 

reshapeNormingData <- function(d)
{
  d$Trial1 = paste(d$List0,d$ID0,d$Condition0,d$Context0,d$Target0,d$Response0,d$Adj0,sep = " ")
  d$Trial2 = paste(d$List1,d$ID1,d$Condition1,d$Context1,d$Target1,d$Response1,d$Adj1,sep = " ")
  d$Trial3 = paste(d$List2,d$ID2,d$Condition2,d$Context2,d$Target2,d$Response2,d$Adj2,sep = " ")
  d$Trial4 = paste(d$List3,d$ID3,d$Condition3,d$Context3,d$Target3,d$Response3,d$Adj3,sep = " ")
  d$Trial5 = paste(d$List4,d$ID4,d$Condition4,d$Context4,d$Target4,d$Response4,d$Adj4,sep = " ")
  d$Trial6 = paste(d$List5,d$ID5,d$Condition5,d$UContext5,d$Target5,d$Response5,d$Adj5,sep = " ")
  d$Trial7 = paste(d$List6,d$ID6,d$Condition6,d$Context6,d$Target6,d$Response6,d$Adj6,sep = " ")
  d$Trial8 = paste(d$List7,d$ID7,d$Condition7,d$Context7,d$Target7,d$Response7,d$Adj7,sep = " ")
  d$Trial9 = paste(d$List8,d$ID8,d$Condition8,d$Context8,d$Target8,d$Response8,d$Adj8,sep = " ")
  d$Trial10 = paste(d$Lis90,d$ID9,d$Condition9,d$Context9,d$Target9,d$Response9,d$Adj9,sep = " ")
  d$Trial11 = paste(d$List10,d$ID10,d$Condition10,d$Context10,d$Target10,d$Response10,d$Adj10,sep = " ")
  d$Trial12 = paste(d$List11,d$ID11,d$Condition11,d$Context11,d$Target11,d$Response11,d$Adj11,sep = " ")
  d$Trial13 = paste(d$List12,d$ID12,d$Condition12,d$Context12,d$Target13,d$Response12,d$Adj12,sep = " ")
  d$Trial14 = paste(d$List13,d$ID13,d$Condition13,d$Context13,d$Target13,d$Response13,d$Adj13,sep = " ")
  return(d) 
} 

reshapeData <- function(d)
{
  d$Trial1 = paste(d$Name0,d$ID0,d$Verb0,d$Underlined0,d$Pronoun0,d$Response0,d$Modal0)
  d$Trial2 = paste(d$Name1,d$ID1,d$Verb1,d$Underlined1,d$Pronoun1,d$Response1,d$Modal1)
  d$Trial3 = paste(d$Name2,d$ID2,d$Verb2,d$Underlined2,d$Pronoun2,d$Response2,d$Modal2)
  d$Trial4 = paste(d$Name3,d$ID3,d$Verb3,d$Underlined3,d$Pronoun3,d$Response3,d$Modal3)
  d$Trial5 = paste(d$Name4,d$ID4,d$Verb4,d$Underlined4,d$Pronoun4,d$Response4,d$Modal4)
  d$Trial6 = paste(d$Name5,d$ID5,d$Verb5,d$Underlined5,d$Pronoun5,d$Response5,d$Modal5)
  d$Trial7 = paste(d$Name6,d$ID6,d$Verb6,d$Underlined6,d$Pronoun6,d$Response6,d$Modal6)
  d$Trial8 = paste(d$Name7,d$ID7,d$Verb7,d$Underlined7,d$Pronoun7,d$Response7,d$Modal7)
  d$Trial9 = paste(d$Name8,d$ID8,d$Verb8,d$Underlined8,d$Pronoun8,d$Response8,d$Modal8)
  d$Trial10 = paste(d$Name9,d$ID9,d$Verb9,d$Underlined9,d$Pronoun9,d$Response9,d$Modal9)
  d$Trial11 = paste(d$Name10,d$ID10,d$Verb10,d$Underlined10,d$Pronoun10,d$Response10,d$Modal10)
  d$Trial12 = paste(d$Name11,d$ID11,d$Verb11,d$Underlined11,d$Pronoun11,d$Response11,d$Modal11)
  d$Trial13 = paste(d$Name12,d$ID12,d$Verb12,d$Underlined12,d$Pronoun12,d$Response12,d$Modal12)
  d$Trial14 = paste(d$Name13,d$ID13,d$Verb13,d$Underlined13,d$Pronoun13,d$Response13,d$Modal13)
  return(d) 
} 


getGender <- function(dd) {
  genders = data.frame(Name = c("Alex", "Ben", "Calvin", "Dan", "Ted", "Max","Ann", "Liz", "Diane","Amy", "Marie", "Jane"), Gender = c(rep("male",6),rep("female",6)))
  row.names(genders) = genders$Name
  for (i in seq(0, 23)) {
    dd[,paste("Gender",i,sep="")] = genders[as.character(dd[,paste("Speaker",i,sep="")]),]$Gender
  }
  return(dd)
}

getQUD <- function(qud) {
  #print(qud)
  if (length(grep("How many", qud)) > 0) {
    return("HowMany?")
  } else {
    if (length(grep("all", qud)) > 0) {
      return("All?")
    } else {
      if (length(grep("Are any", qud)) > 0) {
        return("Any?")
      } else {
        return("ERROR!")
      }
    }
  }
}

myCenter <- function(x) {
  if (is.numeric(x)) { return(x - mean(x)) }
  if (is.factor(x)) {
    x <- as.numeric(x)
    return(x - mean(x))
  }
  if (is.data.frame(x) || is.matrix(x)) {
    m <- matrix(nrow=nrow(x), ncol=ncol(x))
    colnames(m) <- paste("c", colnames(x), sep="")
    for (i in 1:ncol(x)) {
      if (is.factor(x[,i])) {
        y <- as.numeric(x[,i])
        m[,i] <- y - mean(y, na.rm=T)
      }
      if (is.numeric(x[,i])) {
        m[,i] <- x[,i] - mean(x[,i], na.rm=T)
      }
    }
    return(as.data.frame(m))
  }
}

se <- function(x)
{
  y <- x[!is.na(x)] # remove the missing values, if any
  sqrt(var(as.vector(y))/length(y))
}

zscore <- function(x){
  ## Returns z-scored values
  x.mean <- mean(x)
  x.sd <- sd(x)
  
  x.z <- (x-x.mean)/x.sd
  
  return(x.z)
}

zscoreByGroup <- function(x, groups){ 
  #Compute zscores within groups
  out <- rep(NA, length(x))
  
  for(i in unique(groups)){
    out[groups == i] <- zscore(x[groups == i])
  }
  return(out)
}

## for bootstrapping 95% confidence intervals
library(bootstrap)
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - mean(x,na.rm=na.rm)}

