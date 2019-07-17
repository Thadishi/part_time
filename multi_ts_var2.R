# WE ARE goign to create an varMax
#Load all the needed libraries


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
#library(aTSA)


system("python test.py")
fiveYears = read.csv("fiveYearCDS.csv")
quarters <- fiveYears[,1]
fiveYears<- xts(fiveYears[,-1], order.by=as.Date(fiveYears[,1], "%Y-%m-%d"))
countries = names(fiveYears)
plot(fiveYears$Argentina)




#read csv files from the directory
path = "Countries/"
temp <- container::dict()
for (i in 1:length(countries)){
  nuString <- paste(path, countries[i], sep="")
  #as.name(countries[i]) <- xts(countries[i][,-1], order.by=as.Date(countries[i][,1], "%Y-%m-%d"))
  temp[countries[i]] <- assign(countries[i], read.csv(paste(nuString, ".csv", sep="")))
}
places <- container::dict()
for (i in 1:length(countries)){
  places[countries[i]] <- xts(temp[countries[i]][,-1], order.by=as.Date(temp[countries[i]][,1], "%Y-%m-%d"))
  #places$set(countries[i], scale(places[countries[i]], center = T, scale = T))
}
variables <- colnames(places[countries[1]])


##impute missing values:
##Standardize the data
scaled_imp <- container::dict()
library(TTR)
for (i in 1:length(countries)){
  
  #convert to ts
  scaled_imp[countries[i]] <- ts(places[countries[i]], start = 2005, frequency = 4)
  ##impute data using seasdec
  scaled_imp$set(countries[i], na.seadec(scaled_imp[countries[i]],algorithm = 'interpolation'))
  
  state <- scaled_imp[countries[i]]
  
  ##change back to 1:55
  scaleDF <- data.frame(row.names = 1:55)
  scaleDF['quarters'] <- quarters
  
  for (j in 1:length(variables)){
    vari <- (state[,j] - min(state[,j]))/(max(state[,j] - min(state[,j])))
    
    scaleDF[variables[j]] <- vari
  }
  ##convert the dataframe to xts then to ts
  scaleDF<- xts(scaleDF[,-1], order.by=as.Date(scaleDF[,1], "%Y-%m-%d"))
  scaleDF <- ts(scaleDF, start = 2005, frequency = 4)
  
  scaled_imp$set(countries[i], scaleDF)
  
  
}




##Variable dictionary for countries
library(container)
var_inf <- container::dict()
for(i in 1:length(variables)){
  var_inf[variables[i]] <- as.data.frame(read_xlsx("EM_Risk_Monitor.xlsx", sheet = variables[i]))
  var_inf$set(variables[i], xts(var_inf[variables[i]][,-1], order.by = as.Date(var_inf[variables[i]][,1], "%Y-%m-%d")))
  var_inf$set(variables[i], scale(var_inf[variables[i]], center = T, scale = T))
}


##Is the data stationary?
##decompose the data 
for(i in 1:length(countries)){
  print(countries[i])
  
  for(j in 1:22){
    placesTS <- scaled_imp[countries[i]][,j]
    places_decompose <- decompose(placesTS)
    plot(places_decompose)
    #plot.ts(placesTS, title(countries[i]))
  }
}

##Compound plot
library(ggplot2)
library(reshape2)

for(i in 1:length(countries)){
  state <- scaled_imp[countries[i]]
  
  print(autoplot(state, facets = T) +
          xlab("Year") + ylab("") +
          ggtitle(countries[i]))
}


##Augmented Dickey Fuller.
##Test whether it is stationary - constant mean and variace, 
##The residuals move around mean of 0  
for(i in 1:length(countries)){
  print(countries[i])
  state <- na.seadec(diff(scaled_imp[countries[i]]))
  for(j in 1:22){
    print(variables[j])
    test <- adf.test(scaled_imp[countries[i]][,j])
    print(test$p.value)
  }
}





library(leaps)
library(pracma)
library(broom)
library(xlsx)
library(lmtest)

###OLS best subsets
par(mfrow=(c(2,2)))
for(i in 1:length(countries)){
  
  ##Make stationary - difference the data twice
  datum <- na.seadec(diff(scaled_imp[countries[i]],lag = 2,differences = 2))
  Y = datum[,1]
  X = datum[,2:length(colnames(datum))]
  
  exhaust <- regsubsets(X,Y,nbest=1,nvmax = NULL)
  summ <- summary(exhaust)
  
  ##list of 5 lower bic models
  bestBic <- sort(summ$bic)[1:5]
  list_vars1 <- summ$which[which(bestBic[1]==summ$bic),]
  list_vars2 <- summ$which[which(bestBic[3]==summ$bic),]
  list_vars3 <- summ$which[which(bestBic[5]==summ$bic),]
  ##variable positions
  varNums1 <- grep(TRUE, as.vector(list_vars1))
  varNums1 <- as.vector(varNums1[2:length(varNums1)])
  #2
  varNums2 <- grep(TRUE, as.vector(list_vars2))
  varNums2 <- as.vector(varNums2[2:length(varNums2)])
  #3
  varNums3 <- grep(TRUE, as.vector(list_vars3))
  varNums3 <- as.vector(varNums3[2:length(varNums3)])
  
  selectedMod <- step(lm(Y~X, data = datum))
  modSum <- summary(selectedMod)
  tidy_step <- tidy(modSum$coef)
  ##new subset model
  new_Mod <- lm(Y~datum[,varNums1])
  bestStep <- summary(new_Mod)
  tidy_Best <- tidy(bestStep$coef)
  #bestp2
  bestStep2 <- summary(lm(Y~datum[,varNums2]))
  tidy_B2 <- tidy(bestStep2$coef)
  #besttp3
  bestStep3 <- summary(lm(Y~datum[,varNums3]))
  tidy_B3 <- tidy(bestStep3$coef)
  
  #write to excel
  "write.xlsx(tidy_step, paste('model_selection', countries[i],'.xlsx',sep = '_'),sheetName ='stepwise',append = T )
  write.xlsx(tidy_Best, paste('model_selection', countries[i],'.xlsx',sep = '_'),sheetName = 'BestSubset',append = T )
  write.xlsx(tidy_B2, paste('model_selection', countries[i],'.xlsx',sep = '_'),sheetName = 'BestSubset2',append = T )
  write.xlsx(tidy_B3, paste('model_selection', countries[i],'.xlsx',sep = '_'),sheetName = 'BestSubset3',append = T )"
  
  print(bestStep)
  
  plot(selectedMod, main=countries[i])
  
  


  "plot(exhaust, scale='bic', main=paste(countries[i], 'exhaustive',sep = ' '))
  plot(forw, scale='bic', main=paste(countries[i], 'forward',sep = ' '))
  plot(back, scale='bic', main=paste(countries[i], 'backward',sep = ' '))
  plot(seqp, scale='bic', main=paste(countries[i], 'seqrep',sep = ' '))
  checkresiduals(regsubsetsObj)"
  
  #print(summary(bestSubS))

}
par(mfrow=(c(1,1)))




##Test whether its's stationary, after differncing
for (i in 1:length(countries)){

  datum <- na.seadec(diff(scaled_imp[countries[i]]))
  
  ##which lag
  ##select BIC15
  lagSel <- VARselect(datum[,c(varNums3)])$selection
  
  
  #model <- VAR(datum)
  ##lagSel[3],slects criterrion based on BIC
  model <- VAR(datum[,c(1,3,5)], p=1, type = 'trend',season = 4)
  "print(forecast(model) %>%
  autoplot() +xlab('Year'))"
  
  print(summary(model))
}






###Clustering clustering
#cluster the countries
#library(dendextend)
for(i in 1:length(variables)){
  varTS <- ts(var_inf[variables[i]], start = 2005, frequency = 4)
  
  varTS <- na.seadec(varTS)
  
  varTS <- t(as.matrix(varTS))
  
  varTDW <- diss(varTS, "DTWARP")
  
  ##clustr countries
  dtw_clust <- hclust(varTDW)
  plot(dtw_clust,main=variables[i])
}

##you can cut the clusters if you want
#with
#cutree(hclust)


##Correlation clustering
for (i in 1:length(countries)){
  state <- scaled_imp[countries[i]]
  
  
  state <- t(as.matrix(state))
  #print(countries[i])
  #print(statsNA(as.vector(state)))
  #correlation dissimilarity
  cor_rel <- diss(state, "COR")
  #cluster
  corr_clust <- hclust(cor_rel)
  plot(corr_clust, main =countries[i])
}



##DTW clustering 
for (i in 1:length(countries)){
  state <- scaled_imp[countries[i]]
  
  seasadj <- na.seadec(state)
  
  state <- t(as.matrix(seasadj))
  
  #disimilarity
  dtw_dis <- diss(state, "DTWARP")
  #cluster
  plot(hclust(dtw_dis), main=countries[i])
  
}

##Intergrated Periodgram Distance
for (i in 1:length(countries)){
  state <- as.matrix(scaled_imp[countries[i]])
  seasadj <- na.seadec(state)
  
  state <- t(as.matrix(seasadj))
  
  ##disimilarity
  int_per <- diss(state, "INT.PER")
  ##cluster
  plot(hclust(int_per, "mcquitty"), main=countries[i])
}









##Model with time series components - #trend and seasonality
##The CDS affected by Trend and seasonality
for(i in 1:length(countries)){
  tsOb <- ts(places[countries[i]], start = 2005, frequency = 4)
  fillna <- na.seadec(tsOb, algorithm = "interpolation")
  ##difference
  #fillna <- na.seadec(diff(fillna))
  SCDS <- window(fillna[,1], start = 2005)
  SCDS <- na.seadec(diff(SCDS))
  fit.cds <- tslm(SCDS ~trend+season)
  
  fcast <- forecast(fit.cds,h = 8)
  
  #checkresiduals(fcast)
  print(autoplot(fcast) +
          ggtitle(paste("Forecasts of CDS using regression ", countries[i])) +
          xlab("Year") + ylab("CDS"))
  
}

##model CDS as a predicted
for(i in 1:length(countries)){
  print(countries[i])
  tsData <- scaled_imp[countries[i]]
  seaData <- na.seadec(diff(tsData), algorithm = "interpolation")
  Y <- seaData[,1]
  X <- seaData[,2:22]
  
  
  model_fit <- tslm(Y ~ X, data = seaData)
  
  #f4cast <- forecast(model_fit)
  #print(summary(model_fit))
  checkresiduals(model_fit, main=countries[i])
  print(dwtest(model_fit))
}


for(i in 1:length(countries)){
  state <- scaled_imp[countries[i]][10:51,]
  
  c <- cov(as.matrix(state))
  
  print(det(c))
  
}


