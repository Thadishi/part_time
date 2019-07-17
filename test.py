import pandas as pd


#read data from excel into countries.
import os
path  = os.getcwd()
files = os.listdir(path)

excel_files = [f for f in files if f[-4:] =='xlsx']
variable_names = [name.split('.')[0] for name in excel_files]
#print(excel_files)

import xlrd, xlwt
import xlsxwriter

##create variables fo rthe variables
import sys
file = sys.argv[1]
print(file)
#sheet_names = pd.read_excel('EM_Risk_Monitor.xlsx', sheet_name=None)
sheet_names = pd.read_excel(file, sheet_name=None)
variables = [var for var in sheet_names]
print(variables)



##Create countreis
fiveYearCDS = pd.read_excel(file, sheet_name='fiveYearCDS')
countries = []
for country_names in fiveYearCDS:
    countries.append(country_names)
#delete 'quarter' in countries
del countries[0]

##convert cds to CSV for R
fiveYearCDS.to_csv('fiveYearCDS.csv', index_label=False)

##
##convert all the country data frames to csv.
emerging = {state: pd.DataFrame(columns=variables) for state in countries}
for var in variables:
    for state in countries:
        emerging[state][var] = sheet_names[var][state]
#Add quarters to the dataframe
timeline = fiveYearCDS['quarter']
for g in countries:
    emerging[g].insert(0, 'quarter', timeline)

#print for R
for key in emerging:
    emerging[key].to_csv("Countries/"+key+".csv", index_label=False)
