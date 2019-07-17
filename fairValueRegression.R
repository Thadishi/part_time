rm(list = ls())
graphics.off()

check.packages <- function(pkg) {
  new.pkg <- pkg[! (pkg %in% installed.packages()[, 'Package'])]
  if(length(new.pkg))
    install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
}

check.packages(c('dlm','vars', 'mFilter'))
check.packages('readxl')
fairValue <- read_excel('fairValue_only.xlsx')

quarters <- as.Date(as.matrix(fairValue[,1]))
fairValue <- as.data.frame(fairValue)

#zoo (xts)
check.packages('xts')
fairValue <- xts(fairValue[,-1], order.by = as.Date.POSIXct(fairValue[,1], "%Y-%m-%d"))





##convert to time series object
##112 items start in 1987
check.packages('imputeTS')
ts_data <- ts(fairValue, start = 1965, end = c(1991,4) , frequency = 4)
ts_data <- na.seadec(ts_data, algorithm = 'interpolation')

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

us = ac(us_long_term, main='US long term')
cpi = ac(sa_cpi, main='Sa CPI')
policy_rate = ac(sa_policy_rate, main='Sa policy rate')
gdp.acf = ac(sa_gdp, main='GDP')
sa_long = ac(sa_long_term, main='SA long term')


check.packages(c('tseries', 'lmtest', 'orcutt'))
##unit roots
adf.test(us_long_term)
adf.test(sa_cpi)
adf.test(sa_policy_rate)
adf.test(sa_gdp)
adf.test(sa_long_term)


##first difference
##differentiate
us_long_diff = diff(us_long_term,1)
sa_cpi_diff = diff(sa_cpi,1)
sa_policy_diff = diff(sa_policy_rate,1)
sa_gdp_diff = diff(sa_gdp,1)
sa_long_diff = diff(sa_long_term,1)

check.packages('forecast')
modbind= cbind(sa_long_term, us_long_term, sa_cpi, sa_policy_rate, sa_gdp)
modstat = cbind(sa_long_diff, us_long_diff, sa_cpi_diff, sa_policy_diff, sa_gdp_diff)

##plot next to each otehr
check.packages('ggplot2')
check.packages('caret')
check.packages('np')
#modfit
modfit <- lm(sa_long_term ~ sa_cpi+sa_gdp+sa_policy_rate+us_long_term)
modfitDiff <- lm(sa_long_diff ~ sa_cpi_diff+sa_gdp_diff+sa_policy_diff+us_long_diff, data = modstat)
summary(modfitDiff)
summary(modfit)



###fitted
autoplot(modfit[,'sa_long_term'], series="Data") +
  autolayer(fitted(modfitDiff), series="Fitted") +
  xlab("Year") + ylab("") +
  ggtitle("fair value mode") +
  guides(colour=guide_legend(title=" "))




##evaluate teh residuals
checkresiduals(modfit)
checkresiduals(modfitDiff)

check.packages('dynlm')


##linear model
mod_dyn <- dynlm(sa_long_term ~ sa_cpi+sa_gdp+sa_policy_rate+us_long_term)
summary(mod_dyn)

##model model 
mode = VAR(modbind, p=1, type = 'trend')
summary(mode)

