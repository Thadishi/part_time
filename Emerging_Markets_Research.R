" CTRL + ALT + R"
##then type main() in the console
if('openxlsx' %in% (.packages())){
  detach('package:openxlsx', unload = T)
}
#lapply(packages, require, character.only = T)
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


packages <- c('readxl','rJava', 'xts','container', 'dint', 'vars', 'astsa', 'quantmod', 'TSclust',
              'forecast', 'imputeTS', 'tseries', 'TTR', 'ggplot2', 'reshape2', 'leaps', 'pracma', 
              'broom', 'xlsx', 'lmtest', 'tibble', 'outreg')
check.packages(packages)

"Reads an excel file"
read.excel.file <- function(nufile){
  check.packages('readxl')
  #data = read_excel(file)
  #if(length(which(is.na(fairValue) == TRUE)) == 0) {print('jesus')}
  
  return (read_excel(nufile))
}
"reads an excel file and converts the date column into Date indices

the output is an XTS extensible time series object
"
convertToXTS <- function(excel_file){
  fileRead = read.excel.file(excel_file)
  fileRead = as.data.frame(fileRead)
  
  check.packages('xts')
  fileRead <- xts(fileRead[,-1], order.by = as.Date.POSIXct(fileRead[,1], "%Y-%m-%d"))
  
  return(fileRead)
}

"this reads an XTS objecxt to convert it to a time series object
takes in start row position, start year, and frequency
"
convertToTimeSeries <- function(xtsObject, startPos, startYear, freq ){
  timeObject <- ts(xtsObject[startPos:nrow(xtsObject),], start = startYear, frequency = freq)
  
  return (timeObject)
}

"if your data has missing values, fills in using seasonality and trend from available data "
ImputeData <- function(tsObject){
  tsFile <- data.frame()
  if(length(which(is.na(tsObject) == TRUE)) == 0){
    tsFile = tsObject
  } else {
    check.packages('imputeTS')
    imputed <- na.seadec(tsObject, algorithm = 'interpolation')
    
    tsFile = imputed
    
  }
  
  return(tsFile)
}


glsModel <- function(data_frame, variables){
  check.packages('nlme')
  "data <- data.frame(row.names = 1:nrow(data_frame))
  colNames <- colnames(data_frame)
  for(i in 1:variables){
  data[colNames[i]] <- data[,i]
  }"
  
  model <- gls(data_frame[,5] ~ data_frame[,1]+data_frame[,2]+data_frame[,3]+data_frame[,4])
  
  return(model)
}


print("Enter the excel file name")
file_in = F
while(file_in == F){
  excel_file <- readline('Enter the name of the excel file in question: ')
  if(file.exists(excel_file) == T){
    file_in = T
  } else{
    print('Error: file not in folder')
  }
}
print("Does your excel file have multiple sheets?")
howManySheet <- readline("IS there more than one sheet?(Yes/No): ")


##corellation function
get_lower_tri <- function(matrice){
  matrice[upper.tri(matrice)] <- NA
  
  return (matrice)
}
##Decompose the time series data  into seasonally, trend
decomposition <- function(states){
  for(i in 1:ncol(states)){
    print(headings_[i])
    decomposed <- decompose(states[,i])
    plot(decomposed)
  }
}
##Ditance matrix
diss_mat <- function(dataF){
  mat_stat <- t(as.matrix(dataF))
  
  dtw_diss <- diss(mat_stat, 'DTWARP')
  
  mat <- as.matrix(dtw_diss)
  return(get_lower_tri(mat))
}
##corellation matrix for an individual country
corr_matrix <- function(dataF){
  xx <- cor(dataF)
  
  xx <- get_lower_tri(xx)
  
  return(round(xx, 2))
}
##This is a heatmap of a correlation matrix
gg_heatMap <- function(place){
  xx <- round(cor(place),2)
  
  lower_part <- get_lower_tri(xx)
  
  melted <- melt(lower_part, na.rm = T)
  
  ggheatmap <- ggplot(melted, aes(x=Var1, y=Var2, fill=value,)) + geom_tile(color = 'white') +
    scale_fill_gradient2(low='blue', high='red', mid='yellow',
                         midpoint = 0, limit = c(-1,1), space = 'Lab',
                         name='Pearson\nCorrelation')+ 
    geom_raster() + theme(axis.text.x = element_text(angle = 90)) + geom_text(aes(x=Var1, y=Var2, label=value), size = 2.5)
  
  #return(ggheatmap)
  plot(ggheatmap)
}

##compound plot, plots files 
compound_ts <- function(DF){
  
  return (autoplot(DF, facets = T)+
            xlab("Year") + ylab(""))
}
##Least squares regression
##pass in dataframe as an argument plus workbook object
bestSubsets <- function(DF, variable){
  
  Y <- DF[,1]
  X <- DF[,2:ncol(DF)]
  
  ##BEsr susbets
  exhaust <- regsubsets(X,Y,nbest = 1, nvmax = NULL)
  summ <- summary(exhaust)
  
  return(summ)
}

stepWise <- function(DF, variable){
  Y <- DF[,1]
  X <- DF[,2:ncol(DF)]
  
  ##Stepwise
  mod <- lm(Y~X)
  selectMod <- step(mod)
  modSum <- summary(selectMod)
  
  return (modSum)
}
OLS_step_best <- function(DF, wb, variable, y_variable){
  #wb <- createWorkbook()

  print('Which of the following variables do you want to use as your
        Y variable (making the rest as X variables)?')
  col_names <- colnames(DF)
  #y_variable <- readline('Type name of variable to use as Y: ')
  
  var_to_delete <- which(y_variable == col_names)
  col_names = col_names[-var_to_delete]
  print(col_names)
  
  ##greet
  Y <- DF[,var_to_delete]
  X <- DF[,c(col_names)]
  
  exhaust <- regsubsets(X,Y,nbest = 1, nvmax = NULL)
  summ <- summary(exhaust)
  
  mod <- lm(Y~X)
  selectMod <- step(mod)
  modSum <- summary(selectMod)
  
  
  ##prepare for output
  tidy_step <- tidy(modSum$coef)
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
  "varNums3 <- grep(TRUE, as.vector(list_vars3))
  varNums3 <- as.vector(varNums3[2:length(varNums3)])"
  ##model information
  extra_step <- outreg(selectMod)
  len <- length(extra_step$`Model 1`)
  newDF <- data.frame(row.names = 1)
  newDF['N'] <- as.numeric(extra_step$`Model 1`[len-3])
  newDF['R2'] <- as.numeric(extra_step$`Model 1`[len-2])
  newDF['adj-R2'] <- as.numeric(extra_step$`Model 1`[len-1])
  newDF['AIC'] <- as.numeric(extra_step$`Model 1`[len])
  ##new subset model
  
  new_Mod <- lm(Y~DF[,c(varNums1)])
  bestStep <- summary(new_Mod)
  tidy_Best <- tidy(bestStep$coef)
  
  #extra_1
  subset_extra <- outreg(new_Mod)
  len1 <- length(subset_extra$`Model 1`)
  mod_DF <- data.frame(row.names = 1)
  mod_DF['N'] <- as.numeric(subset_extra$`Model 1`[len1-3])
  mod_DF['R2'] <- as.numeric(subset_extra$`Model 1`[len1-2])
  mod_DF['adj-R2'] <- as.numeric(subset_extra$`Model 1`[len1-1])
  mod_DF['AIC'] <- as.numeric(subset_extra$`Model 1`[len1])
  #bestp2
  new_Mod1 <- lm(Y~DF[,varNums2])
  bestStep2 <- summary(new_Mod1)
  tidy_B2 <- tidy(bestStep2$coef)
  
  ##extra_2
  subset1_extra <- outreg(new_Mod1)
  len2 <- length(subset1_extra$`Model 1`)
  mod1_DF <- data.frame(row.names = 1)
  mod1_DF['N'] <- as.numeric(subset1_extra$`Model 1`[len2-3])
  mod1_DF['R2'] <- as.numeric(subset1_extra$`Model 1`[len2-2])
  mod1_DF['adj-R2'] <- as.numeric(subset1_extra$`Model 1`[len2-1])
  mod1_DF['AIC'] <- as.numeric(subset1_extra$`Model 1`[len2])
  
  #besttp3
 " new_Mod2 <- lm(Y~DF[,varNums3])
  bestStep3 <- summary(new_Mod2)
  tidy_B3 <- tidy(bestStep3$coef)
  
  ##extra 3
  
  subset2_extra <- outreg(new_Mod2)
  len3 <- length(subset2_extra$`Model 1`)
  mod2_DF <- data.frame(row.names = 1)
  mod2_DF['N'] <- as.numeric(subset2_extra$`Model 1`[len3-3])
  mod2_DF['R2'] <- as.numeric(subset2_extra$`Model 1`[len3-2])
  mod2_DF['adj-R2'] <- as.numeric(subset2_extra$`Model 1`[len3-1])
  mod2_DF['AIC'] <- as.numeric(subset2_extra$`Model 1`[len3])"
  #write to excel
  
  df_list <- list(stepwise = tidy_step, extra_step = newDF,
                  bestSubset1=tidy_Best,extra_best = mod_DF, bestsubset2 = tidy_B2,extra_best1 = mod1_DF)
  ##if there's more than 5 variables
  "df_list <- list(stepwise = tidy_step, extra_step = newDF,
                  bestSubset1=tidy_Best,extra_best = mod_DF,
                  bestsubset2 = tidy_B2,extra_best1 = mod1_DF,
                  bestSubset3=tidy_B3,extra_best2 = mod2_DF)"
  
  #wb <- createWorkbook()
  addWorksheet(wb, paste('ModelSelection', variable, sep = '_'))
  
  ##add an image
  addWorksheet(wb, paste('correllation_plot', variable, sep='_'))
  ggsave('corrMat.jpeg', gg_heatMap(scaleDF))
  insertImage(wb, paste('correllation_plot', variable, sep='_'),file = 'corrMat.jpeg')
  ##add the matrix sheet
  addWorksheet(wb, paste('corellation', variable, sep='_'))
  writeData(wb, paste('corellation', variable, sep='_'), corr_matrix(scaleDF))
  ##add the dissimilariyt sheet
  addWorksheet(wb, paste('dissimilarity', variable, sep = '_'))
  writeData(wb, paste('dissimilarity', variable, sep = '_'), diss_mat(scaleDF))
  
  ##The model plot
  curr_row <- 1
  
  for(n in seq_along(df_list)){
    writeData(wb, paste('ModelSelection', variable, sep = '_'), names(df_list)[n], startCol = 1, startRow = curr_row)
    writeData(wb, paste('ModelSelection', variable, sep = '_'), df_list[[n]], startCol = 1, startRow = curr_row+1)
    curr_row <- curr_row + nrow(df_list[[n]]) + 2
  }
  
  
  
  
  
  
  
  
  print(bestStep)
  print(selectMod)
  par(mar=c(1,1,1,1))
  par(mfrow=(c(2,2)))
  plot(selectMod)
  par(mfrow=(c(1,1)))
  
  checkresiduals(selectMod)
  print(dwtest(bestStep))
  
  
  plot(exhaust, scale='bic')
  #saveWorkbook(wb, fileToSave)
  return(wb)
}
#Tests for stationarity
augmented_dickey <- function(dataF){
  test_results <- c()
  for(i in 1:ncol(dataF)){
    test <- adf.test(dataF[,i])
    test_results <- c(test_results, test$p.value)
  }
  
  return(test_results)
}
##make data stationary
make_stationary <- function(dataF){
  
  newDF <- diff(dataF,differences = 2)
  
  return(newDF)
}
##cluster analysis dataframe
cor_clustering <- function(dataF){
  
  ##First transpose the data
  state <- t(as.matrix(dataF))
  
  ##calculate a dissimilarity matrix with corellation
  cor_rel <- diss(state, 'COR')
  
  ##Do the clustering
  corr_clust <- hclust(cor_rel, method = 'average')
  
  ##plot of dendo
  dendo <- plot(corr_clust)
  return(dendo)
}

###DTW clustering
dtw_clustering <- function(dataF){
  #First transpose the data
  state <- t(as.matrix(dataF))
  
  ##calculate a dissimilarity matrix with corellation
  dtw_diss <- diss(state, 'DTWARP')
  
  ##Do the clustering
  dtw_clust <- hclust(dtw_diss,method = 'centroid')
  
  ##plot of dendo
  dendo <- plot(dtw_clust)
  return(dendo)
}

#if(howManySheet == 'Yes')
if(howManySheet == 'Yes'){
  
  allSheets <- lapply(excel_sheets(excel_file), read_excel, path=excel_file)
  #xl_data <- loadWorkbook(excel_file)
  xl_data <- xlsx::loadWorkbook(excel_file)
  sheetNames <- names(getSheets(xl_data))
  
  #get all regions 
  regions <- colnames(allSheets[[1]])
  regions <- regions[2:length(regions)]
  ###convert strings to data
  quarters <- as.Date(as.matrix(allSheets[[1]][,1]))
  print('This a useless plot to test whether the data has been read well')
  print("It is also required to initialize plot() function")
  plot(allSheets[[1]][,2])
  
  
  ##dictionary of variables by their countries
  var_dict <- container::dict()
  for(i in 1:length(allSheets)){
    
    var_dict[sheetNames[i]] <- as.data.frame(read_xlsx(excel_file, sheet = sheetNames[i]))
  }
  
  ##the periodicy of the data
  print("Is it quarterly or monthnly or annually?")
  bool_val <- F
  while(bool_val != T){
    freq_var <- readline("Enter whether its a 'Quarter', a 'Month' or an 'Annual' data: ")
    freq_num <- 0
    if(freq_var == 'Quarter'){
      freq_num = 4
      bool_val = T
    } else if(freq_var == 'Month'){
      freq_num = 12
      bool_val = T
    } else if(freq_var == 'Annual'){
      freq_num = 1
      bool_val = T
    }
  }
  
  ##Create a dictionary for countries
  count_dict <- container::dict()
  for(i in 1:length(regions)){
    newDF <- data.frame(row.names = 1:nrow(var_dict[sheetNames[1]]))
    newDF[freq_var] <- quarters
    
    count_dict[regions[i]] <- newDF
  }
  
  ##add variables to the dictionary
  for( i in 1:length(regions)){
    
    country <- data.frame(row.names = 1:nrow(var_dict[sheetNames[1]]))
    country[freq_var] <- quarters
    region <- count_dict[regions[i]]
    for(j in 1:length(sheetNames)){
      datum <- var_dict[sheetNames[j]]
      count_names <- colnames(datum)
      country[sheetNames[j]]  <- datum[regions[i]]
      #var_dict$set(sheetNames[j], xts(var_dict[sheetNames[i]][-1], order.by = as.Date(var_dict[sheetNames[j]][,1], '%Y-%m-%d')))
    }
    
    #nuDF <- cbind(region, country)
    count_dict$set(regions[i], xts(country[-1], order.by = as.Date(country[,1], '%Y-%m-%d')))
    
  }
  
  
  print('which year does this data start on ?')
  starting_year <- as.numeric(readline('Enter the starting year: '))
  ##impute missing values:
  ##Standardize the data
  scaled_imp <- container::dict()
  for (i in 1:length(regions)){
    #convert to ts
    scaled_imp[regions[i]] <- ts(count_dict[regions[i]], start = starting_year, frequency = freq_num)
    ##impute data using seasdec
    scaled_imp$set(regions[i], na.seadec(scaled_imp[regions[i]],algorithm = 'interpolation'))
    
    state <- scaled_imp[regions[i]]
    
    scaleDF <- data.frame(row.names = 1:nrow(state))
    scaleDF[freq_var] <- as.Date(quarters)
    
    for (j in 1:length(sheetNames)){
      vari <- (state[,j] - min(state[,j]))/(max(state[,j]) - min(state[,j]))
      
      scaleDF[sheetNames[j]] <- vari
    }
    ##convert the dataframe to xts then to ts
    scaleDF<- xts(scaleDF[,-1], order.by=as.Date(scaleDF[,1], "%Y-%m-%d"))
    scaleDF <- ts(scaleDF, start = starting_year, frequency = freq_num)
    
    scaled_imp$set(regions[i], scaleDF)
    
  }
  
  ##convert variable dictionary to xts then standardize
  for(i in 1:length(sheetNames)){
    var_dict$set(sheetNames[i], xts(var_dict[sheetNames[i]][,-1], order.by = as.Date(var_dict[sheetNames[i]][,1], "%Y-%m-%d")))
    
    
    country <- var_dict[sheetNames[i]]
    
    ##new datafram
    nuDF <- data.frame(row.names = 1:nrow(country))
    nuDF[freq_var] <- quarters
    for(j in 1:length(regions)){
      vari <- (country[,j] - min(country[,j]))/(max(country[,j]) - min(country[,j]))
      
      nuDF[regions[j]] <- vari
    }
    nuDF <- xts(nuDF[,-1], order.by = as.Date(nuDF[,1], "%Y-%m-%d"))
    nuDF <- ts(nuDF, start = starting_year, frequency = freq_num)
  }
  
  
  
  ##compound plot
  for(i in 1:length(regions)){
    state <- scaled_imp[regions[i]]
    print(compound_ts(state) + ggtitle(regions[i]))
  }
  
  print("IS the data stationary?, Thw following tests whether the data has unit roots or not")
  print("If p-value is greatr than 0.05 or for augmented dickey, 0.01 - you need to difference to make stationary")
  ##Augmented Dickey Fuller.
  ##Test whether it is stationary - constant mean and variace, 
  ##The residuals move around mean of 0  
  for(i in 1:length(regions)){
    print(regions[i])
    state <- scaled_imp[regions[i]]
    print(augmented_dickey(state))
  }
  print("based on the outcome, decide whether to make stationary")
  print('difference the data')
  for(i in 1:length(regions)){
    scaled_imp$set(regions[i], diff(scaled_imp[regions[i]]))
  }
  
  print("Corellation matrix for a country variables")
  for(count in 1:length(regions)){
    ##Things for tomorrow, corellation heatmap
    state <- scaled_imp[regions[count]]
    print(gg_heatMap(state) + ggtitle(regions[count]))
  }
  #+ geom_text(aes(Var2, Var1, label=value))
  ##Stepwise and bestSUbsets for one sheet
  print('A function to do an ols and best subsets and print it out')
  print('This is time series data, some of the assumptiosn of MLR may be violated, so one should take that into account')
  print('Making data stationary is supposed to remove  autocorellation and spurrious regression, sometimes the autocorellation too strong')
  
  
  ##Stepwise 
  ##and ols
  check.packages('openxlsx')
  wb <- createWorkbook()
  
  print(colnames(scaled_imp[regions[1]]))
  y_variable <- readline('Type name of variable to use as Y: ')
  for(i in 1:length(regions)){
    datum <- scaled_imp[regions[i]]
    wb <- OLS_step_best(datum, wb, regions[i],y_variable )
  }
  saveWorkbook(wb, 'ModelSelection.xlsx')
  ##remove the package
  detach('package:openxlsx', unload = T)
  
  check.packages('TSclust')
  print("cluster analysis")
  print("cluster countries by variables")
  print("Decide how they move and cut the tree into how many clusters you require")
  ###Clustering clustering
  #cluster the regions
  #library(dendextend)
  for(i in 1:length(sheetNames)){
    varTS <- ts(var_dict[sheetNames[i]], start = starting_year, frequency = freq_num)
    
    varTS <- na.seadec(varTS)
    
    varTS <- t(as.matrix(varTS))
    
    varTDW <- diss(varTS, 'DTWARP')
    
    ##clustr regions
    dtw_clust <- hclust(varTDW,method = 'centroid')
    plot(dtw_clust,main=sheetNames[i])
  }
  
  
  print("The distance measure used here is Dynamic Time warping")
  print("corellation measures the similarity of variables using the corellation")
  print("DTW measures the movement over time, whether the occilations, the ridges and troughs are similar")
  print("the goal of clustering is to group items in terms of how similar they are, DTW groups them based on how similarly they move")
  print("corellation measures whether these are corellated or not.")
  
  ##DTW clustering 
  for (i in 1:length(regions)){
    state <- scaled_imp[regions[i]]
    ##fill missing values
    seasadj <- na.seadec(state)
    dtw_clustering(seasadj)
  }
  
  
  
  print("Forecasting")
  for_cast <- readline('would you like to do a forecast?: ')
  if(for_cast == 'Yes' || for_cast == 'yes'){
    print("this forecasts data using time series linear modelling an R function")
    ##Model with time series components - #trend and seasonality
    ##The CDS affected by Trend and seasonality
    starting_year <- as.numeric(readline("forecast prediction from which year?: "))
    periods <- as.numeric(readline("How many periods to forecast: "))
    for(i in 1:length(regions)){
      tsOb <- ts(count_dict[regions[i]], start = starting_year, frequency = freq_num)
      fillna <- na.seadec(tsOb, algorithm = "interpolation")
      fore_casting(fillna, starting_year, periods)
    }
    
    
    
    ##Selectinf the number of variables to ue for the code and stuff
    ##Test whether its's stationary, after differncing
    for (i in 1:length(regions)){
      datum <- scaled_imp[regions[i]]
      var_forecast(datum)
    }
  }
  
  
} else if(howManySheet == 'No'){
  
  
  ##convert to zoo file object
  xl_data <- read_xlsx(excel_file)
  quarters <- as.Date(as.matrix(xl_data[,1]))
  xl_data <- as.data.frame(xl_data)
  
  xl_data <- xts(xl_data[,-1], order.by = as.Date(xl_data[,1], "%Y-%m-%d"))
  
  print("Is it Quartely or monthly or annual data")
  bool_val <- F
  while(bool_val != T){
    freq_var <- readline("Enter whether its a 'Quarter', a 'Month' or an 'Annual' data: ")
    freq_num <- 0
    if(freq_var == 'Quarter'){
      freq_num = 4
      bool_val = T
    } else if(freq_var == 'Month'){
      freq_num = 12
      bool_val = T
    } else if(freq_var == 'Annual'){
      freq_num = 1
      bool_val = T
    }
  }
  starting_year <- as.numeric(readline("Enter the starting year: "))
  
  xl_data <- ts(xl_data, start = starting_year, frequency = freq_num)
  xl_data <- na.seadec(xl_data, algorithm = 'interpolation')
  
  ##standardize the data
  "Don't standardize the data, it is a bad idea"
  scaleDF <- data.frame(row.names = 1:nrow(xl_data))
  scaleDF[freq_var] <- as.Date(quarters)
  headings_ <- colnames(xl_data)
  for(i in 1:length(headings_)){
    vari <- (xl_data[,i] - min(xl_data[,i]))/(max(xl_data[,i]) - min(xl_data[,i]))
    
    scaleDF[headings_[i]] <- vari
  }
  ##convert to date and to ts
  scaleDF<- xts(scaleDF[,-1], order.by=as.Date(scaleDF[,1], "%Y-%m-%d"))
  scaleDF <- ts(scaleDF, start = starting_year, frequency = freq_num)
  
  print("decompose the data")
  decomposition(scaleDF)
  
  ##check whetehr there are unit roots within the data
  ##Decide whether you want to make the data stationary afterwards
  print(augmented_dickey(scaleDF))
  print("is the data stationary??")
  ##Time series data is rarely stationary and I will make it stationary
  print('Make data stationary')
  scaleDF <- make_stationary(scaleDF)
  
  ##Check if the data is now stationary
  print('is the data stationary')
  print(augmented_dickey(scaleDF))
  
  print("heatmap of the data")
  print(gg_heatMap(scaleDF))
  
  check.packages('openxlsx')
  print("OLS function")
  wb <- createWorkbook()
  print(colnames(scaleDF))
  y_variable <- readline('Type name of variable to use as Y: ')
  workBook <- OLS_step_best(scaleDF, wb, 'oneSheet', y_variable)
  file_out = T
  
  
  while(file_out == T){
    fileToSave <- readline('Save your file as: ')
    fileToSave <- paste(fileToSave, 'xlsx',sep = '.')
    if(file.exists(fileToSave) == T){
      print('Filename already exists')
    } else{
      file_out = F
    }
  }
  saveWorkbook(workBook, fileToSave)
  
  print("clustering the data")
  dtw_clustering(scaleDF)
  
  print("Forecasting")
  for_cast <- readline('would you like to do a forecast?: ')
  if(for_cast == 'Yes' || for_cast =='yes'){
    workB <- createWorkbook()
    ###Different kind of forecasting
    print("Forecasting with seasonality and trend")
    starting_year <- as.numeric(readline("forecast prediction from which year?: "))
    periods <- as.numeric(readline("How many periods to forecast: "))
    
    col_names <- colnames(xl_data)
    fore_dict <- fore_casting(xl_data, starting_year, periods)
    for(j in 1:length(col_names)){
      #addWorksheet(workB, paste(j, 'forecast', sep = '_'))
      addWorksheet(workB, col_names[j])
      writeData(workB, col_names[j], fore_dict[col_names[j]])
    }
    ###Different kind of forecasting
    addWorksheet(workB, 'forecast_regression')
    print("forecasting, CDS as the predicted variable")
    fore_predict(xl_data)
    writeData(workB, 'forecast_regression', fore_predict(scaleDF))
    file_out = T
    while(file_out == T){
      save_file <- readline('Save your file as: ')
      save_file <- paste(save_file, 'xlsx',sep = '.')
      if(file.exists(save_file) == T){
        print('Filename already exists')
      } else{
        file_out = F
      }
    }
    saveWorkbook(workB, save_file)
  }
  detach('package:openxlsx', unload = T)

  ##heatmap of alll all all all all alla all a
  ##heatmap(xx, Rowv = F, symm = T, distfun = function(c) as.dist(1 - c), hclustfun = function(d)hclust(d, method = 'single'), keep.dendro = F)
  
  
}
