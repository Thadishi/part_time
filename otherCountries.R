##Individual countries
#TS analysis of countries with missing data
#


library(readxl)
library(xts)
library(dint)
library(vars)
library(astsa)
library(quantmod)
library(TSclust)
library(forecast)
library(imputeTS)
library(tseries)

##Bulgaria has missing implied volatility
Bulgaria <- as.data.frame(read_xlsx("individual_countries/Bulgaria_implied_volatility.xlsx"))
Bulgaria <- xts(Bulgaria[,-1], order.by = as.Date(Bulgaria[,1], "%Y-%m-%d"))
Bulgaria <- scale(Bulgaria, center = T, scale = T)
#Croatia has missing reserves to import
Croatia <- as.data.frame(read_xlsx("individual_countries/Croatia_reserves_to_import.xlsx"))
Croatia <- xts(Croatia[,-1], order.by = as.Date(Croatia[,1], "%Y-%m-%d"))
Croatia <- scale(Croatia, center = T, scale = T)
#India has a lot of things missing
India <- as.data.frame(read_xlsx("individual_countries/India_everything.xlsx"))
India <- xts(India[,-1], order.by = as.Date(India[,1], "%Y-%m-%d"))
India <- scale(Croatia, center = T, scale = T)
#Panama has reserves to import miaaing
Panama <- as.data.frame(read_xlsx("individual_countries/Panama_reserves_to_import.xlsx"))
Panama <- xts(Panama[,-1], order.by = as.Date(Panama[,1], "%Y-%m-%d"))
Panama <- scale(Panama, center = T, scale = T)
#Peru has Budget balance missing
Peru <- as.data.frame(read_xlsx("individual_countries/Peru_budgetBalance.xlsx"))
Peru <- xts(Peru[,-1], order.by = as.Date(Peru[,1], "%Y-%m-%d"))
Peru <- scale(Peru, center = T, scale = T)
#Venezuela has outliers and missing implied volatility.
Venezuela <- as.data.frame(read_xlsx("individual_countries/Venezuela_implied_volatility.xlsx"))
Venezuela <- xts(Venezuela[,-1], order.by = as.Date(Venezuela[,1], "%Y-%m-%d"))
Venezuela <- scale(Venezuela, center = T, scale = T)




##test for stationarity
Test_forStationarity <- function(state){
  variables <- names(state)
  state <- ts(state, start = 2005, frequency = 4)
  state <- na.seadec(state, algorithm = "interpolation")
  p_val <- c()
  for(i in 1:length(variables)){
    test <- adf.test(state[,i])
    p_val[i] <- test$p.value
  }
  
  return(p_val)
}
Bulgaria_diff <- diff(Bulgaria, differences = 2)
Croatia_diff <- diff(Croatia, differences = 2)
India_diff <- diff(India, differences = 2)
Panama_diff <- diff(Panama, differences = 2)
Peru_diff <- diff(Peru, differences = 2)
Venezuela_diff <- diff(Venezuela, differences = 2)
##deocompose the data
#To view whether there is trend or seasonality

##Use the variables that end wth _diff
decomposition <- function(state){
  variables <- names(state)
  state <- ts(state, start = 2005, frequency = 4)
  state <- na.seadec(state, algorithm = "interpolation")
  
  for(i in 1:length(variables)){
    decompo <- decompose(state[,i])
    plot(decompo)
  }
}

##perform an OLS
library(leaps)
library(ggplot2)
library(pracma)

OLS_selected <- function(state){
  variables <- names(state)
  state <- ts(state, start = 2005, frequency = 4)
  state <- na.seadec(state, algorithm = "interpolation")
  Y <- state[,1]
  X <- state[,2:length(variables)]
  
  model1 <- lm(Y~X, data = state)
  selectedMod <- step(model1)
  
  
  return(summary(selectedMod))
}


##Best subsets
bestSub <- function(state){
  variables <- names(state)
  state <- ts(state, start = 2005, frequency = 4)
  state <- na.seadec(state, algorithm = "interpolation")

  Y <- state[,1]
  X <- state[,2:length(variables)]
  bestSubsetsObj <- regsubsets(X, Y, nbest=1, nvmax = NULL)
  
  
  return(plot(bestSubsetsObj, scale="bic"))
  
  #return(summary(bestSubsetsObj))
}


##Cluster analysisi
#DTWARP

DTWARP <- function(state){
  variables <- names(state)
  state <- ts(state, start = 2005, frequency = 4)
  state <- na.seadec(state, algorithm = "interpolation")
  
  Transpose <- t(as.matrix(state))
  ##dismilarity
  dtwarp <- diss(Transpose, "DTWARP")
  
  cluster <- hclust(dtwarp)
  
  return(plot(cluster))
}

##CORR clustering
corr_cluster <- function(state){
  variables <- names(state)
  state <- ts(state, start = 2005, frequency = 4)
  state <- na.seadec(state, algorithm = "interpolation")
  
  Transpose <- t(as.matrix(state))
  ##dismilarity
  corrD <- diss(Transpose, "COR")
  
  cluster <- hclust(corrD)
  
  return(plot(cluster))
}

