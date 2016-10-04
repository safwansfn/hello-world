
##LOAD THE DATA FROM AN URL

  mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
  
## view the first few rows of the data

  summary(mydata)
  head(mydata)
  class(mydata)
  
## CHANGE THE DATA TYPE TO FACTOR
  
  mydata$rank <- factor(mydata$rank)
  str(mydata)
  
## BUILD YOUR MODEL
  mylogit <- lm(admit ~ gre + gpa + rank, data = mydata)
  
  mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
  mylogit <- glm(admit ~ gpa + rank, data = mydata, family = "binomial")

## SEE MODEL STATS
  
  summary(mylogit)

## PREDICT THROUGH YOUR MODEL
    
Predictlogit = predict(mylogit, newdata = mydata,type = "response")

## cHANGES pROBABLITIES TO BINARY OUTCOMES

  prob_threshold=0.5
  classes=rep(0,length(Predictlogit))
  for(i in seq(length(Predictlogit))){
    if(Predictlogit[i]>prob_threshold){
      classes[i]=1
    }else{
      classes[i]=0
    }
  }
  
##CREATE CONFUSION MATRIX

  table(actual=mydata$admit, predictions= classes)
  
write.csv(mydata,"C:/Users/lap/Desktop/LogitModel.csv")

install.packages("rattle")
library(rattle)
rattle()
