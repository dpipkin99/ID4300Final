library(tidyverse)
HilmanDataCSV = asdf
HD<- as.data.frame(HilmanDataCSV)

#Cleaning the Data
  #Classification
HD$Classification<-factor(HD$Classification, levels = c("Freshman", "Sophomore", "Junior","Senior","Graduate"))
  #Socio-Politico-Economic Leaning
HD$PoliticalLean <-factor(HD$PoliticalLean, levels="Very Liberal", "Somewhat Liberal", "Independent", "Somewhat Conservative", "Very Conservative")
HD$SocialLean <-factor(HD$SocialLean, levels="Very Liberal", "Somewhat Liberal", "Independent", "Somewhat Conservative", "Very Conservative")
HD$EconomicLean <-factor(HD$EconomicLean, levels="Very Liberal", "Somewhat Liberal", "Independent", "Somewhat Conservative", "Very Conservative")
  #Initial News Opinions
HD$WaPoCredible<-factor(HD$WaPoCredible, levels=c("Strongly agree", "Somewhat agree","Somewhat disagree","Strongly disagree"))
HD$WaPoWhole<-factor(HD$WaPoWhole, levels=c("Strongly agree", "Somewhat agree","Somewhat disagree","Strongly disagree"))
HD$FoxCredible<-factor(HD$FoxCredible, levels=c("Strongly agree", "Somewhat agree","Somewhat disagree","Strongly disagree"))
HD$FoxWhole<-factor(HD$FoxWhole, levels=c("Strongly agree", "Somewhat agree","Somewhat disagree","Strongly disagree"))
  #News Rating
    #cleaning
    for(i in length(HD$WaPoRate1))
    {
      if(HD$WaPoRate1[i] == "FALSE")
      {
        HD$WaPoRate1[i] = "False"
      }
      else if(HD$WaPoRate1 == "TRUE")
      {
        HD$WaPoRate1[i] == "True"
      }
      if(HD$WaPoRate2[i] == "FALSE")
      {
        HD$WaPoRate2[i] = "False"
      }
      else if(HD$WaPoRate2 == "TRUE")
      {
        HD$WaPoRate2[i] == "True"
      }
      if(HD$FoxRate1[i] == "FALSE")
      {
        HD$FoxRate1[i] = "False"
      }
      else if(HD$FoxRate1 == "TRUE")
      {
        HD$FoxRate1[i] == "True"
      }
      if(HD$FoxRate2[i] == "FALSE")
      {
        HD$FoxRate2[i] = "False"
      }
      else if(HD$FoxRate2 == "TRUE")
      {
        HD$FoxRate2[i] == "True"
      }
    }
    #Converting To Factor
HD$WaPoRate1<-factor(HD$WaPoRate1, levels=c("TRUE", "Mostly True", "Mostly False", "FALSE"))