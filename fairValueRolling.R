rm(list = ls())
graphics.off()

#package to load packages
check.packages <- function(pkg) {
  new.pkg <- pkg[! (pkg %in% installed.packages()[, 'Package'])]
  if(length(new.pkg))
    install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
}


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








#quarters <- as.Date(as.matrix(fairValue[,1]))
fairValue <- convertToXTS('fairValue_only.xlsx')
#dataTopredict <- convertToXTS('inputfrom1.xlsx')

##Create a dataframe to store the coefficients in
##create a dataframe tp add coefficients\
coefs <- data.frame(row.names = c('intercept', 'us_long_term', 'sa_gdp', 'sa_headline_cpi', 'sa_policy_rate'))
output <- data.frame(row.names = 1:8)
outputResidualsForGLS <- data.frame(row.names = c('R2', 'RMSE', 'MAE'))

check.packages(c('ggplot2', 'tseries', 'reshape2', 'modelr'))
##try roling again




"###A PROPER WAY of writing the solution

rollRegress <- function(FUN, startPos, startYear, window, tsDATA){
  tslength = nrow(tsDATA) - window
  years = (tslength+1)/4
  year = 0
  count = 0
  while(startPos < (tslength+1) && year< years+1 ){
    
    model_data = ts(ts)
  }
}"

i = 101
n = 0


##count to mod by 4
count = 0

while ( i <= 104 && n <12){
  startYear = 1990+n
  endYear = 2017+n
  window_size = i+112
  #the time series obejct
  ts_data1 <- ts(fairValue[i:window_size,], start= startYear, end = c(endYear, 4), frequency = 4)
  ##The variables
  us_long = ts_data1[,1]
  sa_headline_cpi = ts_data1[,2]
  sa_st_policy = ts_data1[,3]
  sa_dGDP = ts_data1[,4]
  sa_long = ts_data1[,5]
  
  modbind = cbind(us_long, sa_headline_cpi, sa_st_policy, sa_dGDP, sa_long)
  #the gls model
  GLSroll = gls(sa_long ~ us_long+sa_dGDP+sa_headline_cpi+sa_st_policy,
                correlation = corAR1())
  summ = summary(GLSroll)
  ##store coefficients to a dataframe
  coefs[paste(startYear,count,sep = '.')]  <- as.data.frame(summ$coefficients)
  #print(paste('startYear',paste(startYear, cunt) ,sep = ': '))
  
  
  ##output the predicted variables to a dataframe
  glsDATA = as.data.frame(summ$coefficients)
  
  francis <- data.frame(
    R2 = rsquare(GLSroll, data=ts_data1),
    RMSE = rmse(GLSroll, data=ts_data1),
    MAE = mae(GLSroll, data=ts_data1)
  )
  
  outputResidualsForGLS[paste(startYear,count,sep = '.')] <- t(francis)
  ##update the while loop[]
  i = i+1
  count = count +1
  if( count %% 4 == 0){
    n=n+1
  }
  
  
}

check.packages('xlsx')
write.xlsx(t(coefs), 'rollingCoefsFrom1990.xlsx',row.names)
