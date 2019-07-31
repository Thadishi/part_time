//This is a readme for the EmergingMarkets Rscritp
//I will try to be as clear as I can (I am not good at this, so please let me know where I fall)


1. Package 1 check.packages(pkg)
This is a package manager.
allows you to load many pakckages that you need simultaneously or when you need them.
If you do not have a package, it downloads them for you.

simply run # > check.packages('package') // to get the pakcage you require

2. ConvertToXTS(excel_file)
A function that takes in a string of where an excel file is located.
The function reads that file
if the excel is time series, converts the file to an XTS (extensible time series object)
An XTS is a ts object with extra functionality.

simply run # > ConvertToXTS('file.xlsx')

3. ConvertToTimeSeries(xtsObject, startPos, startYear, freq)
This is not counterintuitive I swear.
XTS has extra functionality, but there are legacy pakcages that work with ts only, so converting to XTS to make an excel file into  TS data fram is better.
But to do ts analysis, its best to convert to TS from XTS.

xtsObject is the xtensible time series object
startPos is the row in which you wihs to start your posisition (this is important because your are creating a min data frame and that makes for more accurate analysis.)
startYear - the starting year
freq - (1 - for annual data, 4 - quartely, 6 - halfyearly, 12- monthly....)

simply run # > ConvertToTimeSeries(ConvertToXTS('file.xlsx'), 20, 2020, 4)

4. ImputeData(tsObject)
most time series packages were written for ideal environments (even though it has been proven throughout the last 6000 years that humans aren't capable of producing ideal things.)
Most time series we work with has missing data, if the missing data is significant, probably note that down - but if it is <5 data points we use ImputeData to fill in the missing data using seasonal 

This takes in a time series object

simply run # > ImputeData(tsObject)

5. glsModel(data_frame)
//This function takes in a time series object/dataframe
// then using that creates a generalised least squares model with the first column as the Y variable.
//returns the model from which you can make claculations and inferences using.
This model is so similar to lm() that sometimes the results are the same. (lm being OLS)

simply run # > glsModel(data_frame)

6. modelfn(data_frame)
OLS model

7. modelfndiff(data_frame)
OLS model with differenced data
8. get_lower_try(matrice)

9. decomposition(states)

10. diss_mat(dataF)

11. corr_matrix(dataF)
