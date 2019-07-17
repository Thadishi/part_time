# WE ARE goign to create an varMax
#Load all the needed libraries


#lapply(packages, require, character.only = T)
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c('readxl', 'xts', 'dint', 'vars', 'astsa', 'quantmod', 'TSclust', 'forecast', 'imputeTS', 'tseries',
              'TTR', 'ggplot2', 'reshape2', 'leaps', 'pracma', 'broom', 'lmtest', 'xlsx', 'lmtest', 'tibble')
check.packages(packages)

python <- "python test.py EM_Risk_Monitor.xlsx"
system(python)

allSheets <- lapply(excel_sheets('EM_Risk_Monitor.xlsx'), read_excel, path='EM_Risk_Monitor.xlsx')
xl_data <- loadWorkbook('EM_Risk_Monitor.xlsx')
sheetNames <- names(getSheets(xl_data))

#get all regions 
regions <- colnames(allSheets[[1]])
regions <- regions[2:length(regions)]

fiveYears = read.csv("fiveYearCDS.csv")
quarters <- fiveYears[,1]
plot(fiveYears$Argentina)



##dictionary of variables by their countries
var_dict <- container::dict()
for(i in 1:length(allSheets)){
  var_dict[sheetNames[i]] <- as.data.frame(allSheets[[i]])
}

##create a dictionary of countries
count_dict <- container::dict()
for(i in 1:length(regions)){
  newDF <- data.frame(row.names = 1:nrow(var_dict[sheetNames[1]]))
  newDF['quarter'] <- var_dict[sheetNames[1]][1]
  
  count_dict[regions[i]] <- newDF
}

##add variables to the dictionary
for( i in 1:length(regions)){
  
  country <- data.frame(row.names = 1:nrow(var_dict[sheetNames[1]]))
  region <- count_dict[regions[i]]
  for(j in 1:length(sheetNames)){
    datum <- var_dict[sheetNames[j]]
    #datum <- datum[2:length(datum)]
    count_names <- colnames(datum)
    country['quarter'] <- datum[1]
    country[sheetNames[j]]  <- datum[regions[i]]
  }
  
  #nuDF <- cbind(region, country)
  #count_dict$set(regions[i], xts(country[-1], order.by = as.(country[1], '%Y-%m-%d %I:%M:%S')))
  count_dict$set(regions[i], country)
  
}



#read csv files from the directory
path = "Countries/"
temp <- container::dict()
for (i in 1:length(regions)){
  nuString <- paste(path, regions[i], sep="")
  #as.name(regions[i]) <- xts(regions[i][,-1], order.by=as.Date(regions[i][,1], "%Y-%m-%d"))
  temp[regions[i]] <- assign(regions[i], read.csv(paste(nuString, ".csv", sep='')))
}
places <- container::dict()
for (i in 1:length(regions)){
  places[regions[i]] <- xts(temp[regions[i]][,-1], order.by=as.Date(temp[regions[i]][,1], "%Y-%m-%d"))
  #places$set(regions[i], scale(places[regions[i]], center = T, scale = T))
}
variables <- colnames(places[regions[1]])


##impute missing values:
##Standardize the data
scaled_imp <- container::dict()

###standardize
##ask input about whether it is monthly or quartely data and the year it starts
for (i in 1:length(regions)){
  #convert to ts
  scaled_imp[regions[i]] <- ts(places[regions[i]], start = 2005, frequency = 4)
  ##impute data using seasdec
  scaled_imp$set(regions[i], na.seadec(scaled_imp[regions[i]],algorithm = 'interpolation'))
  
  state <- scaled_imp[regions[i]]
  
  scaleDF <- data.frame(row.names = 1:55)
  scaleDF['quarters'] <- quarters
  
  for (j in 1:length(variables)){
    vari <- (state[,j] - min(state[,j]))/(max(state[,j] - min(state[,j])))
    
    scaleDF[variables[j]] <- vari
  }
  ##convert the dataframe to xts then to ts
  scaleDF<- xts(scaleDF[,-1], order.by=as.Date(scaleDF[,1], "%Y-%m-%d"))
  scaleDF <- ts(scaleDF, start = 2005, frequency = 4)
  
  scaled_imp$set(regions[i], scaleDF)
  
  
}
##create dictionaries
dataFrame_creator <- function(firstContainer){
  regions <- colnames(firstContainer)
  scaled_imp <- container::dict()
  
  
  ##convert places to a timeseries data
  for(i in 1:length(regions)){
    scaled_imp[regions[i]] <- ts(places[regions[i]], start = 2005, frequency = 4)
    
    ##Impute values on missing data
    scaled_imp$set(regions[i], na.seadec(scaled_imp[regions[i]], algorithm = 'interpolation'))
    
    #normalize values
    state <- scaled_imp[regions[i]]
    scaleDF <- data.frame(row.names = 1:length(quarters))
    scaleDF['quarters'] <- quarters
    
    for (j in 1:length(variables)){
      vari <- (state[,j] - min(state[,j]))/(max(state[,j] - min(state[,j])))
      
      scaleDF[variables[i]] <- vari
    }
    
    ###convert new dataframe to xts then to ts
    scaleDF<- xts(scaleDF[,-1], order.by=as.Date(scaleDF[,1], '%Y-%m-%d'))
    scaleDF <- ts(scaleDF, start = 2005, frequency = 4)
    
    scaled_imp$set(regions[i], scaleDF)
  }
  
  return(scaled_imp)
}



##Variable dictionary for regions
var_inf <- container::dict()
for(i in 1:length(variables)){
  var_inf[variables[i]] <- as.data.frame(read_xlsx("EM_Risk_Monitor.xlsx", sheet = variables[i]))
  var_inf$set(variables[i], xts(var_inf[variables[i]][,-1], order.by = as.Date(var_inf[variables[i]][,1], "%Y-%m-%d")))
  var_inf$set(variables[i], scale(var_inf[variables[i]], center = T, scale = T))
}


##Is the data stationary?
##decompose the data 
decomposition <- function(states){
  for(i in 1:length(states)){
    print(states[i])
    
    for(j in 1:22){
      placesTS <- scaled_imp[states[i]][,j]
      places_decompose <- decompose(placesTS)
      plot(places_decompose)
      #plot.ts(placesTS, title(states[i]))
    }
  }
}

##compound plot
for(i in 1:length(regions)){
  state <- scaled_imp[regions[i]]
  
  print(autoplot(state, facets = T) +
          xlab("Year") + ylab("") +
          ggtitle(regions[i]))
}


##Augmented Dickey Fuller.
##Test whether it is stationary - constant mean and variace, 
##The residuals move around mean of 0  
for(i in 1:length(regions)){
  print(regions[i])
  state <- na.seadec(diff(scaled_imp[regions[i]]))
  for(j in 1:22){
    print(variables[j])
    test <- adf.test(scaled_imp[regions[i]][,j])
    print(test$p.value)
  }
}
##check for stationarity'
##asses whether you need to make stationary
check_Unit_Roots <- function(regions){
  for(i in 1:length(regions)){
    print(regions[i])
    for(j in 1:22){
      print(variables[j])
      test <- adf.test(scaled_imp[regions[i]][,j])
      print(test$p.value)
    }
  }
}

##Stepwise and bestSUbsets for one sheet
stepwise <- function(datum){
  variables <- names(datum)
  datum <- ts(datum, start = 2005, frequency = 4)
  datum <- na.seadec(datum, algorithm = 'interpolation')
  
  Y <- datum[,1]
  X <- datum[,2:length(variables)]
  
  
  model1 <- lm(Y~X, data = datum)
  
  selectedMod <- step(model1)
  bestsubsets <- regsubsets(X,Y,nbest = 1, nvmax = NULL)
  ##summary of bestSubset
  summ <- summary(bestsubsets)
  modSum <- summary(selectedMod)
  ##list of models with low bic
  bestBic <- sort(summ$bic)[1:5]
  list_vars1 <- summ$which[which(bestBic[1]==summ$bic),]
  varNums1 <- grep(TRUE, as.vector(list_vars1))
  varNums1 <- as.vector(varNums1[2:length(varNums1)])
  
  ##new model
  new_Mod <- lm(Y~datum[,varNums1])
  
  plot(bestsubsets, scale = 'bic')
  
  ##
  ##write to excel
  tidy_step <- tidy(modSum$coef)
  tidy_Best <- tidy(summary(new_Mod)$coef)
  
  ##hey hey hey 
  write.xlsx(tidy_step, paste('model_selection', regions[i],'.xlsx',sep = '_'),sheetName ='stepwise',append = T )
  write.xlsx(tidy_Best, paste('model_selection', regions[i],'.xlsx',sep = '_'),sheetName = 'BestSubset',append = T )
  
  return(summary(selectedMod))
}


###OLS best subsets
par(mfrow=(c(2,2)))
for(i in 1:length(regions)){
  
  ##Make stationary - difference the data twice
  datum <- na.seadec(diff(scaled_imp[regions[i]],lag = 2,differences = 2))
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
  write.xlsx(tidy_step, paste('model_selection', regions[i],'.xlsx',sep = '_'),sheetName ='stepwise',append = T )
  write.xlsx(tidy_Best, paste('model_selection', regions[i],'.xlsx',sep = '_'),sheetName = 'BestSubset',append = T )
  "write.xlsx(tidy_B2, paste('model_selection', regions[i],'.xlsx',sep = '_'),sheetName = 'BestSubset2',append = T )
  write.xlsx(tidy_B3, paste('model_selection', regions[i],'.xlsx',sep = '_'),sheetName = 'BestSubset3',append = T )"
  
  print(bestStep)
  
  plot(selectedMod, main=regions[i])
  
  


  "plot(exhaust, scale='bic', main=paste(regions[i], 'exhaustive',sep = ' '))
  plot(forw, scale='bic', main=paste(regions[i], 'forward',sep = ' '))
  plot(back, scale='bic', main=paste(regions[i], 'backward',sep = ' '))
  plot(seqp, scale='bic', main=paste(regions[i], 'seqrep',sep = ' '))
  checkresiduals(regsubsetsObj)"
  
  #print(summary(bestSubS))

}
par(mfrow=(c(1,1)))




##Test whether its's stationary, after differncing
for (i in 1:length(regions)){

  datum <- na.seadec(diff(scaled_imp[regions[i]]))
  
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
#cluster the regions
#library(dendextend)
for(i in 1:length(variables)){
  varTS <- ts(var_inf[variables[i]], start = 2005, frequency = 4)
  
  varTS <- na.seadec(varTS)
  
  varTS <- t(as.matrix(varTS))
  
  varTDW <- diss(varTS, 'DTWARP')
  
  ##clustr regions
  dtw_clust <- hclust(varTDW,method = 'average')
  plot(dtw_clust,main=variables[i])
}

##you can cut the clusters if you want
#with
#cutree(hclust)


##Correlation clustering
for (i in 1:length(regions)){
  state <- scaled_imp[regions[i]]
  
  ##select items to include in cluster
  state1 <- state[,c(2:6, 8:length(colnames(state)))]
  
  state2 <- t(as.matrix(state1))
  #print(regions[i])
  #print(statsNA(as.vector(state)))
  #correlation dissimilarity
  cor_rel <- diss(state2, 'COR')
  #cluster
  corr_clust <- hclust(cor_rel,method = 'average')
  plot(corr_clust, main =regions[i])
  
  partOfTree <-  cutree(corr_clust, k=5)
  print(regions[i])
  print(sort(partOfTree))
}



##DTW clustering 
for (i in 1:length(regions)){
  state <- scaled_imp[regions[i]]
  
  seasadj <- na.seadec(state)
  
  state <- t(as.matrix(seasadj))
  
  #disimilarity
  dtw_dis <- diss(state, 'DTWARP')
  #cluster
  plot(hclust(dtw_dis,method = 'average'), main=regions[i])
  
}

##Intergrated Periodgram Distance
for (i in 1:length(regions)){
  state <- as.matrix(scaled_imp[regions[i]])
  seasadj <- na.seadec(state)
  
  state <- t(as.matrix(seasadj))
  
  ##disimilarity
  int_per <- diss(state, 'INT.PER')
  ##cluster
  plot(hclust(int_per, 'average'), main=regions[i])
}




##Model with time series components - #trend and seasonality
##The CDS affected by Trend and seasonality
for(i in 1:length(regions)){
  tsOb <- ts(places[regions[i]], start = 2005, frequency = 4)
  fillna <- na.seadec(tsOb, algorithm = "interpolation")
  ##difference
  #fillna <- na.seadec(diff(fillna))
  SCDS <- window(fillna[,1], start = 2005)
  SCDS <- na.seadec(diff(SCDS))
  fit.cds <- tslm(SCDS ~trend+season)
  
  fcast <- forecast(fit.cds,h = 8)
  
  #checkresiduals(fcast)
  print(autoplot(fcast) +
          ggtitle(paste("Forecasts of CDS using regression ", regions[i])) +
          xlab("Year") + ylab("CDS"))
  
}

##model CDS as a predicted
for(i in 1:length(regions)){
  print(regions[i])
  tsData <- scaled_imp[regions[i]]
  seaData <- na.seadec(diff(tsData), algorithm = "interpolation")
  Y <- seaData[,1]
  X <- seaData[,2:22]
  
  
  model_fit <- tslm(Y ~ X, data = seaData)
  
  #f4cast <- forecast(model_fit)
  #print(summary(model_fit))
  checkresiduals(model_fit, main=regions[i])
  print(dwtest(model_fit))
}


for(i in 1:length(regions)){
  state <- scaled_imp[regions[i]][10:51,]
  
  c <- cov(as.matrix(state))
  
  print(det(c))
  
}

