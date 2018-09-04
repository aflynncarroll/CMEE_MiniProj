#!/usr/bin/env python3
#__author__ =  "Alexander Flynn-Carroll.af2017@imperial.ac.uk"
#__version__ = "0.0.1"
#__date__ = "05 Feb 2018"

import pandas as pd
from datetime import datetime
import sys

"""adj_matrix.py cleans the sparrow interaction data by removing incomplete data and comparing it with a list of known sparrow ID's."""

############################################################################################
# Take command line input to load in data
############################################################################################
print('Read in Data')

filename = sys.argv[-3]
ids = sys.argv[-2]
out_name = sys.argv[-1]
# Command line inputs to run and save file

# To run: example below
# run adj_matrix.py '../Data/MegaDataBase-v137-201311-201611-FY-Dominance_Lundy_20170512_AST.csv' '../Data/birdsex.1_validcolourcodes_AST.csv' '../Results/adj_data.csv'

############################################################################################
# Import Data
############################################################################################

dom_data = pd.read_csv(filename, sep = ',', header = 'infer',  usecols = [4,6,12,14,15,17,24,25])
# loads in population statistics csv with only the required columns

bird_id = pd.read_csv(ids, sep = ',', header = 'infer')
# known bird id's

############################################################################################
# Clean Data and keep only those on ID list
############################################################################################
print('Remove excess birds')

dom_data = dom_data[dom_data.individual1 != 'notidentified' ]
dom_data = dom_data[dom_data.individual2 != 'notidentified' ]
# removes nonidentified individuals from df


dom_data.individual1 = dom_data.individual1.str.replace('/','')
dom_data.individual2 = dom_data.individual2.str.replace('/','')
# strips the slashes out of the individual columns to match
# the bird_id file 

dom_data = dom_data[~dom_data.individual1.isin(bird_id.Code) == False]
dom_data = dom_data[~dom_data.individual2.isin(bird_id.Code) == False]
# Removes all columns where the individual is not in the list of 
# bird ID's

print('Fix date format')

dom_data['date-ELO2'] = dom_data['date-ELO2'].replace(['08/07/2014'], '07/08/2014')
dom_data['date-ELO2'] = dom_data['date-ELO2'].replace(['09/07/2014'], '07/09/2014')
dom_data['date-ELO2'] = dom_data['date-ELO2'].replace(['10/06/2015'], '06/10/2015')
dom_data['date-ELO2'] = dom_data['date-ELO2'].replace(['01/06/2016'], '06/01/2016')
dom_data['date-ELO2'] = dom_data['date-ELO2'].replace(['26/11/2016'], '11/26/2016')
# some dates were recorded in the opposite format, these dates are corrected


dom_data['date-ELO2'] = pd.to_datetime(dom_data['date-ELO2'], format="%m/%d/%Y")
# # make date date time


print('Add trip number')

def sample_period(row):
    """Assigns trip numbers to all interactions based on a date range."""
    if (row['date-ELO2'] > pd.to_datetime('2013-11-1')) & (row['date-ELO2'] < pd.to_datetime('2013-12-1')):
        return 1
    if (row['date-ELO2'] > pd.to_datetime('2014-3-1')) & (row['date-ELO2'] < pd.to_datetime('2014-9-1')):
        return 2
    if (row['date-ELO2'] > pd.to_datetime('2015-2-1')) & (row['date-ELO2'] < pd.to_datetime('2015-3-1')):
        return 3
    if (row['date-ELO2'] > pd.to_datetime('2015-5-1')) & (row['date-ELO2'] < pd.to_datetime('2015-7-1')):
        return 4
    if (row['date-ELO2'] > pd.to_datetime('2016-2-1')) & (row['date-ELO2'] < pd.to_datetime('2016-3-1')):
        return 5
    if (row['date-ELO2'] > pd.to_datetime('2016-5-1')) & (row['date-ELO2'] < pd.to_datetime('2016-7-1')):
        return 6
    if (row['date-ELO2'] > pd.to_datetime('2016-11-1')) & (row['date-ELO2'] < pd.to_datetime('2016-12-1')):
        return 7

dom_data['trip'] = dom_data.apply(lambda row:sample_period(row), axis=1)
# Adds trip number based on date to all of the rows - time period defined above


############################################################################################
# Save data
############################################################################################

dom_data.to_csv(out_name)
# save cleaned data based on line input

