rm(list = ls())
graphics.off()


check.packages <- function(pkg) {
  new.pkg <- pkg[! (pkg %in% installed.packages()[, 'Package'])]
  if(length(new.pkg))
    install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
}

convertToXTS <- function(excel_file){
  fileRead = read.excel.file(excel_file)
  fileRead = as.data.frame(fileRead)
  
  check.packages('xts')
  fileRead <- xts(fileRead[,-1], order.by = as.Date.POSIXct(fileRead[,1], "%Y-%m-%d"))
  
  return(fileRead)
}

read.excel.file <- function(nufile){
  check.packages('readxl')
  #data = read_excel(file)
  #if(length(which(is.na(fairValue) == TRUE)) == 0) {print('jesus')}
  
  return (read_excel(nufile))
}

check.packages(c('dlm','vars', 'mFilter', 'readxl', 'xts'))

#Tests for stationarity
augmented_dickey <- function(dataF){
  test_results <- c()
  for(i in 1:ncol(dataF)){
    test <- adf.test(dataF[,i])
    test_results <- c(test_results, test$p.value)
  }
  
  return(test_results)
}


fairValue <- read_excel('fairValue_only.xlsx')

quarters <- as.Date(as.matrix(fairValue[,1]))
fairValue <- as.data.frame(fairValue)


fairValue <- xts(fairValue[,-1], order.by = as.Date.POSIXct(fairValue[,1], "%Y-%m-%d"))





##convert to time series object
##112 items start in 1987
check.packages('imputeTS')
ts_data <- ts(fairValue, start = 1965, end = 2018, frequency = 4)
#ts_data <- na.seadec(ts_data, algorithm = 'interpolation')

##all variables
us_long_term = ts_data[,1]
sa_cpi = ts_data[,2]
sa_policy_rate = ts_data[,3]
sa_gdp = ts_data[,4]
sa_long_term = ts_data[,5]

##visualisa
plot.ts(cbind(us_long_term, sa_cpi, sa_policy_rate, sa_gdp, sa_long_term))


##check for serial correllation
#devtools::install_github("KevinKotze/tsm")
check.packages('tsm')

check.packages('tseries')
##unit roots
adf.test(us_long_term)
adf.test(sa_cpi)
adf.test(sa_policy_rate)
adf.test(sa_gdp)
adf.test(sa_long_term)



##model selection
#sa_long_term, 
modbind= cbind(sa_long_term,us_long_term, sa_cpi, sa_policy_rate, sa_gdp)


##modelCreation
if('MTS' %in% (.packages())){
  detach('package:MTS', unload = T)
}
check.packages('forecast')
##OLS without stationaru
model1 <- lm(sa_long_term~us_long_term+sa_cpi+sa_policy_rate+sa_gdp, data = modbind)
summary(model1)
dwtest(model1)
checkresiduals(model1)

par(mfrow=c(2,2))
plot(model1)
par(mfrow=c(1,1))

#fitted vs original data
ols_fit <- cbind(sa_long_term, model1$fitted.values)
ts.plot(ols_fit, col=1:2)
legend('topright', legend = colnames(ols_fit), col =1:2, lty=1, cex=.65)
abline(h = c(0.06, 0.08, .1, .12, .14, .16, .18), v=c(1997))

##make stationary 

us_long_diff = diff(us_long_term)
sa_cpi_diff = diff(sa_cpi)
sa_policy_diff = diff(sa_policy_rate)
sa_gdp_diff = diff(sa_gdp)
sa_long_diff = diff(sa_long_term)

model2diff <- lm(sa_long_diff~us_long_diff+sa_cpi_diff+sa_policy_diff+sa_gdp_diff)
dwtest(model2diff)
checkresiduals(model2diff)
plot(model2diff$fitted.values, model2diff$residuals)

summary(model2diff)

##ols fitted diff vs original
ols_diff <- cbind(diffinv(sa_long_diff), diffinv(model2diff$fitted.values))
ts.plot(ols_diff, col=1:2)
legend('topright', legend = colnames(ols_diff), col =1:2, lty=1, cex=.65)
abline(h = c(0.06, 0.08, .1, .12, .14, .16, .18), v=c(1997))

##compare stationary and non stationary fitted
ols_fitted = cbind(model1$fitted.values, model2diff$fitted.values)
ts.plot(ols_fitted, col=1:2)
###generalized last squares
check.packages('nlme')
gls1 = gls(sa_long_term ~ us_long_term+sa_gdp+sa_cpi+sa_policy_rate,
           correlation = corAR1())
summary(gls1)
#compute R2
check.packages('modelr')
data.frame(
  R2 = rsquare(gls1, data=modbind),
  RMSE = rmse(gls1, data=modbind),
  MAE = mae(gls1, data=modbind)
)
###fitted gls vs original values
gls_fitted = cbind(sa_long_term, gls1$fitted)
ts.plot(gls_fitted, col=1:2)
legend('topright', legend = colnames(gls_fitted), col =1:2, lty=1, cex=.65)
abline(h = c(0.06, 0.08, .1, .12, .14, .16, .18), v=c(1997))


##diagnostics
#residuals
plot(gls1$fitted, gls1$residuals)
checkresiduals(gls1)






##GLS
##create a dataframe tp add coefficients\
coefs <- data.frame(row.names = c('intercept', 'us_long_term', 'sa_gdp', 'sa_headline_cpi', 'sa_policy_rate'))
output <- data.frame(row.names = 1:164)
outputResidualsForGLS <- data.frame(row.names = c('R2', 'RMSE', 'MAE'))
##data to predict
dataTopredict = convertToXTS('inputfrom1.xlsx')
newRownames = as.data.frame(read_excel('rownames_1995.xlsx'))

par(mfrow=c(1,1))
###from 1965 - 112 quarters
#ts_data <- ts(fairValue, start = 1965, end = 2018, frequency = 4)
ts65 = ts(fairValue, start = 1965, end = c(1992,4), frequency = 4)
ts65 <- na.seadec(ts65, algorithm = 'interpolation')

us_long_term = ts65[,1]
sa_cpi = ts65[,2]
sa_policy_rate = ts65[,3]
sa_gdp = ts65[,4]
sa_long_term = ts65[,5]

gls65 = gls(sa_long_term ~ us_long_term+sa_gdp+sa_cpi+sa_policy_rate,
            correlation = corAR1())
summary(gls65)
plot(gls65$fitted, gls65$residuals)

#compute R2
judas <- data.frame(
  R2 = rsquare(gls65, data=ts65),
  RMSE = rmse(gls65, data=ts65),
  MAE = mae(gls65, data=ts65)
)

##add coefficients to the dataframe
coefs['coef65'] <- as.data.frame(gls65$coefficients)
outputResidualsForGLS['1965'] <- t(judas)
##plot the results
ts.plot(sa_long_term, gls65$fitted, col=1:2)
legend('topright', legend = c('normal_data', 'fitted_values'), col =1:2, lty=1, cex=.65)

gls65DATA = as.data.frame(gls65$coefficients)
list_ofY <- c()
##predict 2018 - now)
for(i in 1:nrow(dataTopredict)){
  us_long = dataTopredict[i,1]
  sa_head_cpi = dataTopredict[i,2]
  sa_st_policy = dataTopredict[i,3]
  saGDP = dataTopredict[i,4]
  
  y = gls65DATA[1,1] + gls65DATA[2,1]*us_long+ gls65DATA[3,1]*saGDP+
    gls65DATA[4,1]*sa_head_cpi + gls65DATA[5,1]*sa_st_policy
  list_ofY <- c(list_ofY, y)
}
##1965 output
output['1965output'] <- list_ofY

###from 1970 - 112 quarters
ts70 = ts(fairValue[21:216,], start = 1970, end = c(1997,4), frequency = 4)

us_long_term = ts70[,1]
sa_cpi = ts70[,2]
sa_policy_rate = ts70[,3]
sa_gdp = ts70[,4]
sa_long_term = ts70[,5]

gls70 = gls(sa_long_term ~ us_long_term+sa_gdp+sa_cpi+sa_policy_rate,
            correlation = corAR1())
summary(gls70)
##
#compute R2
judas <- data.frame(
  R2 = rsquare(gls70, data=ts70),
  RMSE = rmse(gls70, data=ts70),
  MAE = mae(gls70, data=ts70)
)

##add coefficients to the dataframe
coefs['coef70'] <- as.data.frame(gls70$coefficients)
outputResidualsForGLS['1970'] <- t(judas)
##plot the results

ts.plot(sa_long_term, gls70$fitted, col=1:2)
legend('topright', legend = c('normal_data', 'fitted_values'), col =1:2, lty=1, cex=.65)

gls70DATA = as.data.frame(gls70$coefficients)
list_ofY <- c()
##predict 2018 - now)
for(i in 1:nrow(dataTopredict)){
  us_long = dataTopredict[i,1]
  sa_head_cpi = dataTopredict[i,2]
  sa_st_policy = dataTopredict[i,3]
  saGDP = dataTopredict[i,4]
  
  y = gls70DATA[1,1] + gls70DATA[2,1]*us_long+ gls70DATA[3,1]*saGDP+
    gls70DATA[4,1]*sa_head_cpi + gls70DATA[5,1]*sa_st_policy
  list_ofY <- c(list_ofY, y)
}
##1970 output
output['1970output'] <- list_ofY

###from 1975 - 112 quarters
ts75 = ts(fairValue[41:216,], start = 1975, end = c(2002,4), frequency = 4)

us_long_term = ts75[,1]
sa_cpi = ts75[,2]
sa_policy_rate = ts75[,3]
sa_gdp = ts75[,4]
sa_long_term = ts75[,5]

gls75 = gls(sa_long_term ~ us_long_term+sa_gdp+sa_cpi+sa_policy_rate,
            correlation = corAR1())
summary(gls75)
#
#compute R2
judas <- data.frame(
  R2 = rsquare(gls75, data=ts75),
  RMSE = rmse(gls75, data=ts75),
  MAE = mae(gls75, data=ts75)
)

##add coefficients to the dataframe
coefs['coef75'] <- as.data.frame(gls75$coefficients)
outputResidualsForGLS['1975'] <- t(judas)
##plot the results
ts.plot(sa_long_term, gls75$fitted, col=1:2)
legend('topright', legend = c('normal_data', 'fitted_values'), col =1:2, lty=1, cex=.65)

gls75DATA = as.data.frame(gls75$coefficients)
list_ofY <- c()
##predict 2018 - now)
for(i in 1:nrow(dataTopredict)){
  us_long = dataTopredict[i,1]
  sa_head_cpi = dataTopredict[i,2]
  sa_st_policy = dataTopredict[i,3]
  saGDP = dataTopredict[i,4]
  
  y = gls75DATA[1,1] + gls75DATA[2,1]*us_long+ gls75DATA[3,1]*saGDP+
    gls75DATA[4,1]*sa_head_cpi + gls75DATA[5,1]*sa_st_policy
  list_ofY <- c(list_ofY, y)
}
##1975 output
output['1975output'] <- list_ofY

###from 1980 - 112 quarters
ts80 = ts(fairValue[61:174,], start = 1980, end = c(2007,4), frequency = 4)

us_long_term = ts80[,1]
sa_cpi = ts80[,2]
sa_policy_rate = ts80[,3]
sa_gdp = ts80[,4]
sa_long_term = ts80[,5]

gls80 = gls(sa_long_term ~ us_long_term+sa_gdp+sa_cpi+sa_policy_rate,
            correlation = corAR1())
summary(gls80)
#
#compute R2
judas <- data.frame(
  R2 = rsquare(gls80, data=ts80),
  RMSE = rmse(gls80, data=ts80),
  MAE = mae(gls80, data=ts80)
)
##add coefficients to the dataframe
coefs['coef80'] <- as.data.frame(gls80$coefficients)
outputResidualsForGLS['1980'] <- t(judas)

##plot the results
ts.plot(sa_long_term, gls80$fitted, col=1:2)
legend('topright', legend = c('normal_data', 'fitted_values'), col =1:2, lty=1, cex=.65)
abline(h = c(0.06, 0.08, .1, .12, .14, .16, .18), v=c(1997))


gls80DATA = as.data.frame(gls80$coefficients)
list_ofY <- c()
##predict 2018 - now)
for(i in 1:nrow(dataTopredict)){
  us_long = dataTopredict[i,1]
  sa_head_cpi = dataTopredict[i,2]
  sa_st_policy = dataTopredict[i,3]
  saGDP = dataTopredict[i,4]
  
  y = gls80DATA[1,1] + gls80DATA[2,1]*us_long+ gls80DATA[3,1]*saGDP+
    gls80DATA[4,1]*sa_head_cpi + gls80DATA[5,1]*sa_st_policy
  list_ofY <- c(list_ofY, y)
}
##1980 output
output['1980output'] <- list_ofY

###from 1985 - 112 quarters
ts85 = ts(fairValue[81:194,], start = 1985, end = c(2012,4), frequency = 4)

us_long_term = ts85[,1]
sa_cpi = ts85[,2]
sa_long_term = ts85[,5]

gls85 = gls(sa_long_term ~ us_long_term+sa_gdp+sa_cpi+sa_policy_rate,
            correlation = corAR1())
summary(gls85)
#
#compute R2
judas <- data.frame(
  R2 = rsquare(gls85, data=ts85),
  RMSE = rmse(gls85, data=ts85),
  MAE = mae(gls85, data=ts85)
)
##add coefficients to the dataframe
coefs['coef85'] <- as.data.frame(gls85$coefficients)
outputResidualsForGLS['1985'] <- t(judas)


##plot the results
ts.plot(sa_long_term, gls85$fitted, col=1:2)
legend('topright', legend = c('normal_data', 'fitted_values'), col =1:2, lty=1, cex=.65)
abline(h = c(0.06, 0.08, .1, .12, .14, .16, .18), v=c(1997))


gls85DATA = as.data.frame(gls85$coefficients)
list_ofY <- c()
##predict 2018 - now)
for(i in 1:nrow(dataTopredict)){
  us_long = dataTopredict[i,1]
  sa_head_cpi = dataTopredict[i,2]
  sa_st_policy = dataTopredict[i,3]
  saGDP = dataTopredict[i,4]
  
  y = gls85DATA[1,1] + gls85DATA[2,1]*us_long+ gls85DATA[3,1]*saGDP+
    gls85DATA[4,1]*sa_head_cpi + gls85DATA[5,1]*sa_st_policy

  list_ofY <- c(list_ofY, y)
}
##1985 output
output['1985output'] <- list_ofY


###from 1990 - 112 quarters
ts90 = ts(fairValue[101:216], start = 1990, end = c(2017,4), frequency = 4)

us_long_term = ts90[,1]
sa_cpi = ts90[,2]
sa_policy_rate = ts90[,3]
sa_gdp = ts90[,4]
sa_long_term = ts90[,5]

gls90 = gls(sa_long_term ~ us_long_term+sa_gdp+sa_cpi+sa_policy_rate,
            correlation = corAR1())
#ols90 = lm(sa_long_term ~ us_long_term+sa_gdp+sa_cpi+sa_policy_rate)
summary(gls90)
#
judas <- data.frame(
  R2 = rsquare(gls90, data=ts90),
  RMSE = rmse(gls90, data=ts90),
  MAE = mae(gls90, data=ts90)
)
##add coefficients to the dataframe
coefs['coef90'] <- as.data.frame(gls90$coefficients)
outputResidualsForGLS['1990'] <- t(judas)
##plot the results
ts.plot(sa_long_term, gls90$fitted, col=1:2)
legend('topright', legend = c('normal_data', 'fitted_values'), col =1:2, lty=1, cex=.65)
abline(h = c(0.06, 0.08, 0.09, .1, .12, .14), v=c(1997))

gls90DATA = as.data.frame(gls90$coefficients)
list_ofY = c()
##predict 2018 - now)
for(i in 1:nrow(dataTopredict)){
  us_long = dataTopredict[i,1]
  sa_head_cpi = dataTopredict[i,2]
  sa_st_policy = dataTopredict[i,3]
  saGDP = dataTopredict[i,4]
  
  y = gls90DATA[1,1] + gls90DATA[2,1]*us_long+ gls90DATA[3,1]*saGDP+
    gls90DATA[4,1]*sa_head_cpi + gls90DATA[5,1]*sa_st_policy
  list_ofY <- c(list_ofY, y)
}
##1990 outout
output['1990output'] <- list_ofY



###fitting  differnt lines (meaningless, but good for comparison) 
gls_fitted = cbind(gls65$fitted, gls70$fitted, gls75$fitted, gls80$fitted, gls85$fitted, gls90$fitted)
ts.plot(gls_fitted, col=1:6)
legend('topright', legend = c('gls65$fitted', 'gls70$fitted', 'gls75$fitted','gls80$fitted',
                              'gls85$fitted', 'gls90$fitted'), col =1:6, lty=1, cex=.65)

par(mfrow=c(1,1))
##plot of estimates
ts.plot(output, col = 1:6)
legend('topright', legend = colnames(output), col = 1:6, lty = 1, cex = .65)






par(mfrow=c(2,1))
##OLSDiff
coefsDiff <- data.frame(row.names = c('intercept', 'us_long_term', 'sa_gdp', 'sa_headline_cpi', 'sa_policy_rate'))
coefsNot <- data.frame(row.names = c('intercept', 'us_long_term', 'sa_gdp', 'sa_headline_cpi', 'sa_policy_rate'))
outputDiff <- data.frame(row.names = 1:164)
outputNot <- data.frame(row.names = 1:164)

##1965
ts65d = ts(fairValue, start = 1965, end = c(1992,4), frequency = 4)
ts65notD = ts(fairValue, start = 1965, end = c(1992,4), frequency = 4)
ts65d <- na.seadec(ts65d, algorithm = 'interpolation')
ts65notD <- na.seadec(ts65notD, algorithm = 'interpolation')

us_long_diff = diff(ts65d[,1])
sa_cpi_diff = diff(ts65d[,2])
sa_policy_diff = diff(ts65d[,3])
sa_gdp_diff = diff(ts65d[,4])
sa_long_diff = diff(ts65d[,5])
#OLSNOTDIFF
us_long = ts65notD[,1]
sa_cpi = ts65notD[,2]
sa_policy = ts65notD[,3]
sa_gdp = ts65notD[,4]
sa_long = ts65notD[,5]

olsdiff65 <- lm(sa_long_diff~us_long_diff+sa_cpi_diff+sa_policy_diff+sa_gdp_diff)
olsnotD65 <- lm(sa_long~us_long+sa_cpi+sa_policy+sa_gdp)


coefsDiff['coef65'] <- as.data.frame(olsdiff65$coefficients)
coefsNot['coefs65'] <- as.data.frame(olsnotD65$coefficients)


olsdiff65DATA = as.data.frame(diffinv(olsdiff65$coefficients)[2:6])
list_ofY <- c()
##predict 2018 - now!
for(i in 1:nrow(dataTopredict)){
  us_long = dataTopredict[i,1]
  sa_head_cpi = dataTopredict[i,2]
  sa_st_policy = dataTopredict[i,3]
  saGDP = dataTopredict[i,4]
  
  y = olsdiff65DATA[1,1] + olsdiff65DATA[2,1]*us_long + olsdiff65DATA[3,1]*sa_head_cpi + olsdiff65DATA[4,1]*+sa_st_policy
    olsdiff65DATA[5,1]*saGDP
    
  list_ofY <- c(list_ofY, y)
}
##1965 output
outputDiff['1965output'] <- (list_ofY)
ts.plot(diffinv(sa_long_diff), diffinv(olsdiff65$fitted), col=1:2)
ts.plot(sa_long, olsnotD65$fitted, col=1:2)
legend('topright', legend = c('normal_data', 'fitted_values'), col =1:2, lty=1, cex=.65)

summary(olsdiff65)
print('residuals of olsdiff')
checkresiduals(olsdiff65)

summary(olsnotD65)
print('residuas of olsNotD')
checkresiduals(olsnotD65)

##1970
ts70d = ts(fairValue[21:216,], start = 1970, end = c(1997,4), frequency = 4)
ts70notD = ts(fairValue[21:216,], start = 1970, end = c(1997,4), frequency = 4)


us_long_diff = diff(ts70d[,1])
sa_cpi_diff = diff(ts70d[,2])
sa_policy_diff = diff(ts70d[,3])
sa_gdp_diff = diff(ts70d[,4])
sa_long_diff = diff(ts70d[,5])

##ols
us_long = ts70notD[,1]
sa_cpi = ts70notD[,2]
sa_policy = ts70notD[,3]
sa_gdp = ts70notD[,4]
sa_long = ts70notD[,5]


olsdiff70 <- lm(sa_long_diff ~ us_long_diff+sa_cpi_diff+sa_policy_diff+sa_gdp_diff)
olsnotD70 <- lm(sa_long~us_long+sa_cpi+sa_policy+sa_gdp)


coefsDiff['coef70'] <- as.data.frame(olsdiff70$coefficients)
coefsNot['coef70'] <- as.data.frame(olsnotD70$coefficients)


olsdiff70DATA = as.data.frame(diffinv(olsdiff70$coefficients)[2:6])
list_ofY <- c()
##predict 2018 - now!
for(i in 1:nrow(dataTopredict)){
  us_long = dataTopredict[i,1]
  sa_head_cpi = dataTopredict[i,2]
  sa_st_policy = dataTopredict[i,3]
  saGDP = dataTopredict[i,4]
  
  y = olsdiff70DATA[1,1] + olsdiff70DATA[2,1]*us_long + olsdiff70DATA[3,1]*sa_head_cpi + olsdiff70DATA[4,1]*+sa_st_policy
  olsdiff70DATA[5,1]*saGDP
  
  list_ofY <- c(list_ofY, y)
}
##1970 output
outputDiff['1970output'] <- (list_ofY)


ts.plot(diffinv(sa_long_diff), diffinv(olsdiff70$fitted), col=1:2)
ts.plot(sa_long, olsnotD70$fitted, col=1:2)
legend('topright', legend = c('normal_data', 'fitted_values'), col =1:2, lty=1, cex=.65)

summary(olsdiff70)

##olsdiff70
print('residuals of olsdiff')
checkresiduals(olsdiff70)

summary(olsnotD70)
print('residuas of olsNotD')
checkresiduals(olsnotD70)

##1975
ts75d = ts(fairValue[41:216,], start = 1975, end = c(2002,4), frequency = 4)
ts75notD = ts(fairValue[41:216,], start = 1975, end = c(2002,4), frequency = 4)

us_long_diff = diff(ts75d[,1])
sa_cpi_diff = diff(ts75d[,2])
sa_policy_diff = diff(ts75d[,3])
sa_gdp_diff = diff(ts75d[,4])
sa_long_diff = diff(ts75d[,5])

##olsnot
us_long = ts75notD[,1]
sa_cpi = ts75notD[,2]
sa_policy = ts75notD[,3]
sa_gdp = ts75notD[,4]
sa_long = ts75notD[,5]

olsdiff75 <- lm(sa_long_diff ~ us_long_diff+sa_cpi_diff+sa_policy_diff+sa_gdp_diff)
olsnotD75 <- lm(sa_long~us_long+sa_cpi+sa_policy+sa_gdp)



coefsDiff['coef75'] <- as.data.frame(olsdiff75$coefficients)
coefsNot['coef75'] <- as.data.frame(olsnotD75$coefficients)

olsdiff75DATA = as.data.frame(diffinv(olsdiff75$coefficients)[2:6])
list_ofY <- c()
##predict 2018 - now!
for(i in 1:nrow(dataTopredict)){
  us_long = dataTopredict[i,1]
  sa_head_cpi = dataTopredict[i,2]
  sa_st_policy = dataTopredict[i,3]
  saGDP = dataTopredict[i,4]
  
  y = olsdiff75DATA[1,1] + olsdiff75DATA[2,1]*us_long + olsdiff75DATA[3,1]*sa_head_cpi + olsdiff75DATA[4,1]*+sa_st_policy
  olsdiff75DATA[5,1]*saGDP
  
  list_ofY <- c(list_ofY, y)
}
##1975 output
outputDiff['1975output'] <- (list_ofY)


ts.plot(diffinv(sa_long_diff), diffinv(olsdiff75$fitted), col=1:2)
ts.plot(sa_long, olsnotD75$fitted, col=1:2)
legend('topright', legend = c('normal_data', 'fitted_values'), col =1:2, lty=1, cex=.65)

summary(olsdiff75)

print('residuals of olsdiff')
checkresiduals(olsdiff75)

summary(olsnotD75)
print('residuas of olsNotD')
checkresiduals(olsnotD75)

##1980
ts80d = ts(fairValue[61:216,], start = 1980, end = c(2007,4), frequency = 4)
ts80notD = ts(fairValue[61:216,], start = 1980, end = c(2007,4), frequency = 4)


us_long_diff = diff(ts80d[,1])
sa_cpi_diff = diff(ts80d[,2])
sa_policy_diff = diff(ts80d[,3])
sa_gdp_diff = diff(ts80d[,4])
sa_long_diff = diff(ts80d[,5])

#olsnot
us_long = ts80notD[,1]
sa_cpi = ts80notD[,2]
sa_policy = ts80notD[,3]
sa_gdp = ts80notD[,4]
sa_long = ts80notD[,5]



olsdiff80 <- lm(sa_long_diff ~ us_long_diff+sa_cpi_diff+sa_policy_diff+sa_gdp_diff)
olsnotD80 <- lm(sa_long~us_long+sa_cpi+sa_policy+sa_gdp)



coefsDiff['coef80'] <- as.data.frame(olsdiff80$coefficients)
coefsNot['coef80'] <- as.data.frame(olsnotD80$coefficients)

olsdiff80DATA = as.data.frame(diffinv(olsdiff80$coefficients)[2:6])
list_ofY <- c()
##predict 2018 - now!
for(i in 1:nrow(dataTopredict)){
  us_long = dataTopredict[i,1]
  sa_head_cpi = dataTopredict[i,2]
  sa_st_policy = dataTopredict[i,3]
  saGDP = dataTopredict[i,4]
  
  y = olsdiff80DATA[1,1] + olsdiff80DATA[2,1]*us_long + olsdiff80DATA[3,1]*sa_head_cpi + olsdiff80DATA[4,1]*+sa_st_policy
  olsdiff80DATA[5,1]*saGDP
  
  list_ofY <- c(list_ofY, y)
}
##1980 output
outputDiff['1980output'] <- (list_ofY)


ts.plot(diffinv(sa_long_diff), diffinv(olsdiff80$fitted), col=1:2)
ts.plot(sa_long, olsnotD80$fitted, col=1:2)
legend('topright', legend = c('normal_data', 'fitted_values'), col =1:2, lty=1, cex=.65)

summary(olsdiff80)
print('residuals of olsdiff')
checkresiduals(olsdiff80)

summary(olsnotD80)
print('residuas of olsNotD')
checkresiduals(olsnotD80)


##1985
ts85d = ts(fairValue[81:216,], start = 1985, end = c(2012,4), frequency = 4)
ts85notD = ts(fairValue[81:216,], start = 1985, end = c(2012,4), frequency = 4)

us_long_diff = diff(ts85d[,1])
sa_cpi_diff = diff(ts85d[,2])
sa_policy_diff = diff(ts85d[,3])
sa_gdp_diff = diff(ts85d[,4])
sa_long_diff = diff(ts85d[,5])

##olsnot
us_long = ts85notD[,1]
sa_cpi = ts85notD[,2]
sa_policy = ts85notD[,3]
sa_gdp = ts85notD[,4]
sa_long = ts85notD[,5]

olsdiff85 <- lm(sa_long_diff ~ us_long_diff+sa_cpi_diff+sa_policy_diff+sa_gdp_diff)
olsnotD85 <- lm(sa_long~us_long+sa_cpi+sa_policy+sa_gdp)


coefsDiff['coef85'] <- as.data.frame(olsdiff85$coefficients)
coefsNot['coef85'] <- as.data.frame(olsnotD85$coefficients)

olsdiff85DATA = as.data.frame(diffinv(olsdiff85$coefficients)[2:6])
list_ofY <- c()
##predict 2018 - now!
for(i in 1:nrow(dataTopredict)){
  us_long = dataTopredict[i,1]
  sa_head_cpi = dataTopredict[i,2]
  sa_st_policy = dataTopredict[i,3]
  saGDP = dataTopredict[i,4]
  
  y = olsdiff85DATA[1,1] + olsdiff85DATA[2,1]*us_long + olsdiff85DATA[3,1]*sa_head_cpi + olsdiff85DATA[4,1]*+sa_st_policy
  olsdiff85DATA[5,1]*saGDP
  
  list_ofY <- c(list_ofY, y)
}
##1985 output
outputDiff['1985output'] <- (list_ofY)


ts.plot(diffinv(sa_long_diff), diffinv(olsdiff85$fitted), col=1:2)
ts.plot(sa_long, olsnotD85$fitted, col=1:2)
legend('topright', legend = c('normal_data', 'fitted_values'), col =1:2, lty=1, cex=.65)

summary(olsdiff85)

#olsNot
print('residuals of olsdiff')
checkresiduals(olsdiff70)

summary(olsnotD85)
print('residuas of olsNotD')
checkresiduals(olsnotD85)

##1990
ts90d = ts(fairValue[101:216,], start = 1975, end = c(2017,4), frequency = 4)
ts90notD = ts(fairValue[101:216,], start = 1975, end = c(2017,4), frequency = 4)


us_long_diff = diff(ts90d[,1])
sa_cpi_diff = diff(ts90d[,2])
sa_policy_diff = diff(ts90d[,3])
sa_gdp_diff = diff(ts90d[,4])
sa_long_diff = diff(ts90d[,5])

##olsnot
us_long = ts90notD[,1]
sa_cpi = ts90notD[,2]
sa_policy = ts90notD[,3]
sa_gdp = ts90notD[,4]
sa_long = ts90notD[,5]

olsdiff90 <- lm(sa_long_diff ~ us_long_diff+sa_cpi_diff+sa_policy_diff+sa_gdp_diff)
olsnotD90 <- lm(sa_long~us_long+sa_cpi+sa_policy+sa_gdp)



coefsDiff['coef90'] <- as.data.frame(olsdiff90$coefficients)
coefsNot['coef90'] <- as.data.frame(olsnotD90$coefficients)

olsdiff90DATA = as.data.frame(diffinv(olsdiff90$coefficients)[2:6])
list_ofY <- c()
##predict 2018 - now!
for(i in 1:nrow(dataTopredict)){
  us_long = dataTopredict[i,1]
  sa_head_cpi = dataTopredict[i,2]
  sa_st_policy = dataTopredict[i,3]
  saGDP = dataTopredict[i,4]
  
  y = olsdiff90DATA[1,1] + olsdiff90DATA[2,1]*us_long + olsdiff90DATA[3,1]*sa_head_cpi + olsdiff90DATA[4,1]*+sa_st_policy
  olsdiff90DATA[5,1]*saGDP
  
  list_ofY <- c(list_ofY, y)
}
##1990 output
outputDiff['1990output'] <- list_ofY
View(outputDiff)


ts.plot(diffinv(sa_long_diff), diffinv(olsdiff90$fitted), col=1:2)
ts.plot(sa_long, olsnotD90$fitted, col=1:2)
legend('topright', legend = c('normal_data', 'fitted_values'), col =1:2, lty=1, cex=.65)

summary(olsdiff90)
##olsnot
print('residuals of olsdiff')
checkresiduals(olsdiff80)

summary(olsnotD90)
print('residuas of olsNotD')
checkresiduals(olsnotD90)

check.packages( 'tibble')
check.packages('tibbletime')
row.names(output) <- newRownames$rownames
row.names(outputDiff) <- newRownames$rownames

nuOutput = as_tibble(output, rownames = NA)

#write to excel
#wb <- createWorkbook()
check.packages('openxlsx')
wb <- createWorkbook()
addWorksheet(wb,'Coefficients')

df_list <- list(coefficients_GLS = coefs, coefficientsOLSdifferenced=coefsDiff, undifferrencedCoefficients=coefsNot,
                outputGLS = output, outputDifferenced=outputDiff, GLSRMSEMAE = outputResidualsForGLS)
##The model plot
curr_row <- 1

for(n in seq_along(df_list)){
  writeData(wb,'Coefficients', names(df_list)[n], startCol = 1, startRow = curr_row)
  writeData(wb,'Coefficients', df_list[[n]], startCol = 1, startRow = curr_row+1, rowNames = T)
  curr_row <- curr_row + nrow(df_list[[n]]) + 2
}

saveWorkbook(wb, 'OutputAndCoeffs.xlsx')

##addroenames

##

par(mfrow=c(1,1))
##GLS ouput
ts.plot(output, col=1:6)
legend('topright', legend = colnames(output), col =1:6, lty=1, cex=.65)
##OLSDIFF OUTPUT
ts.plot(outputDiff, col=1:6)
legend('topright', legend = colnames(outputDiff), col =1:6, lty=1, cex=.65)