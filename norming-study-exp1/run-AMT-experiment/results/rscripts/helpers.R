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

