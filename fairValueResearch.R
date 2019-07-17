##Assumptions of multiple regression


packages <- c('readxl', 'xts', 'imputeTS', 'car', 'ggplot2', 'forecast', 'tseries')
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
check.packages(packages)


##compound plot, plots files 
compound_ts <- function(DF){
  
  
  return (autoplot(DF, facets = T)+
            xlab("Year") + ylab(""))
}

##make data stationary
make_stationary <- function(dataF){
  newDF <- data.frame(row.names = 1:nrow(dataF))
  newDF['quarters'] <- as.Date(quarters)
  namecol <- colnames(dataF)
  
  for(i in 1:ncol(dataF)){
    vari <- diff(dataF[,i])
    
    newDF[namecol[i]] <- vari
  }
  
  #newDF <- diff(dataF)
  newDF <- ts(newDF, start = 1965, frequency = 4)
  
  return(newDF)
}

##Decompose the time series data  into seasonally, trend
decomposition <- function(states){
  for(i in 1:ncol(states)){
    print(colnames(states)[i])
    decomposed <- decompose(states[,i])
    plot(decomposed)
  }
}


xl_data <- read_xlsx('fairValue_only.xlsx')



##convert to zoo
quarters <- as.Date(as.matrix(xl_data[,1]))
xl_data <- as.data.frame(xl_data)

#zoo (xts)
xl_data <- xts(xl_data[,-1], order.by = as.Date(xl_data[,1], "%Y-%m-%d"))





##convert to time series object
ts_data <- ts(xl_data, start = 1965, frequency = 4)
ts_data <- na.seadec(ts_data, algorithm = 'interpolation')



decomposition(ts_data)

ts_data <- ts(ts_data[99:210,1:5], start = 1989, frequency = 4)
#static_var <- make_stationary(ts_data)



##Regression fit
fit <- lm(ts_data[,5] ~ ts_data[,1]+ts_data[,2]+ts_data[,3]+ts_data[,4], data = ts_data)
#fit <- lm( y ~ x1+x2+x3+x4)
summary(fit)
summ <- summary(fit)

##stepwise
stepfit <- step(fit)
summary(stepfit)




##Fittd values vs orignal data 
ts_fit <- cbind((ts_data[,5]), (fit$fitted.values))
#ts_fit <- cbind(y, fit$fitted.values)

compound_ts(ts_fit)

ts.plot(ts_data[,5], fit$fitted.values, gpars=list(col=rainbow(2)) )
abline(h = c(0.08, 0.1, 0.12, 0.14, 0.16), v=c(1997,2015))






smooth_at <- holt(fit$fitted.values, h=10)
autoplot(smooth_at)

##Diagnostic of the regression
#layout(matrix(c(1,2,3,4),2,2))
par(mfrow=c(2,2))
plot(fit)

##fitted vs origina
par(mfrow=c(1,1))
##chgecking for outliers
leveragePlots(fit)

outlierTest(fit)


##multicollinearity
vif(fit)

##idependence of errors
##if p-value  is closer to 0, you need to make stationery
durbinWatsonTest(fit)


for(i in 1:ncol(ts_data)){
  print(adf.test(ts_data[,i]))
  
}



##Prediction
##xl_data <- xts(xl_data[,-1], order.by = as.Date(xl_data[,1], "%Y-%m-%d"))
predict_data <- read_xlsx('output.xlsx')
output <- as.data.frame(predict_data)
coefs <- summ$coefficients


intercept <- coefs

for(i in 1:nrow(output)){
  print(coefs[1] + (coefs[2] * output[i,1]) + (coefs[3]*output[i,2]) + (coefs[4]*output[i,3]) + (coefs[5]*output[i,4]))
}



###Arima modelling
#plot the ts data - to check for the trend and statio
check.packages('marima')
check.packages('vars')
plot(ts_data)
#pacf
pacf(ts_data[,5])

modFit <- VAR(ts_data, type='trend')
