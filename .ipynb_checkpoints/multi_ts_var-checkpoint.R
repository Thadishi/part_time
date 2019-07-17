# WE ARE goign to create an varMax
library(XLConnect)
fiveYears = read.xlsx("EM_Risk_Monitor.xlsx", sheetIndex = 1)
fiveYears$quarter <- anytime::anydate(fiveYears$quarter)

plot.ts(fiveYears$Brazil, main="", xlab="")

#function to read all sheetrs into the workbook

library(readxl)    
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

mysheets <- read_excel_allsheets("EM_Risk_Monitor.xlsx")

library(vars)
library(astsa)

x = cbind(mysheets$fiveYearCDS$Brazil, mysheets$implied_volatility$Brazil, mysheets$EU_Banking_Risk$Brazil)
plot.ts(x, main="", xlab="")
fitvar1 = VAR(x,p=1,type="both")
summary(fitvar1)
