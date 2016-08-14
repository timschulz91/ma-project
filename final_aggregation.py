# -*- coding: utf-8 -*-
"""
Created on Wed Jul 27 00:09:23 2016

@author: tim

Description:
Simple script to aggregate data to the required weekly ratios
"""

# === load data === #
import pandas as pd

dat = pd.read_csv('completeData.csv')

# fix the index
dat.index = pd.to_datetime(dat.hour)


# === aggregation === #
# aggregate within languages and across types
## non-mobile data
dat_nm = dat.query('type!="mw"').groupby(['language', 'hour'])
dat_nm = dat_nm.agg('sum')
## mobile data
dat_m = dat.query('type=="mw"')


# === process languages === #
# make languages a column in the data set
dat_nm['language'] = dat_nm.index.get_level_values(0)
# make hour the index of the data set
dat_nm.index = pd.to_datetime(dat_nm.index.get_level_values(1))
# get a list of all languages
langs1 = dat_nm.language.unique()
langs2 = dat_m.language.unique()
# can only work with languages that are included in both datasets
langs = list(set(langs1) & set(langs2))

# initiate a DataFrame to store all results
results = pd.DataFrame(columns=['language', 'week', 'visits', 'traffic'])

# for each language, calulate the ratios
for lang in langs:
    # find the corresponding subsets of mobile and non-mobile data
    temp_nm = dat_nm[dat_nm.language == lang]
    temp_m = dat_m[dat_m.language == lang]
    # aggregate to weekly data (by year)
    temp_nm_w = temp_nm.groupby([lambda x: x.year, lambda x: x.week]).sum()   
    temp_m_w = temp_m.groupby([lambda x: x.year, lambda x: x.week]).sum()
    
    # calculate the ratios
    visits_w = temp_m_w.visits / (temp_m_w.visits + temp_nm_w.visits)
    traffic_w = temp_m_w.traffic / (temp_m_w.traffic + temp_nm_w.traffic)
    # clear up memory
    del temp_m, temp_nm, temp_m_w, temp_nm_w

    #visits_w = visits.groupby([lambda x: x.year, lambda x: x.week]).mean()
    #traffic_w = traffic.groupby([lambda x: x.year, lambda x: x.week]).mean()
    # merge both series into a DataFrame that will be appended to the results
    # temp = pd.concat([visits, traffic], axis=1)
    temp = pd.concat([visits_w, traffic_w], axis=1)
    # make week a column, so there is no risk of messing up the index when
    # merging with other languages
    #temp['week'] = temp.index
    temp['year'] = temp.index.get_level_values(0)
    temp['week'] = temp.index.get_level_values(1)
    # add a coumn for the language
    temp['language'] = lang
    temp.reset_index(inplace=True, drop=True)
    # bump up the index so it does not conflict with other indeces
    temp.index = temp.index + 1000
    # append this temp to the overall results
    results = results.append(temp, verify_integrity=True, ignore_index=True)

# fix the week and year columns
results.year = [int(i) for i in results.year]
results.week = [int(i) for i in results.week]

# make separate columns for the year and week of the year
# results['year'] = results.apply(lambda x: x.week[0], 1)
# results['week'] = results.apply(lambda x: x.week[1], 1)

results.to_csv('WeeklyRatios.csv', index=False)

########################################
########## OLD #########################
# subset english
dat_nm_en = dat_nm.query('language=="en"')
dat_m_en = dat_m.query('language=="en"')

# fix index from multiindex to just hourly index
dat_nm_en.index = pd.to_datetime(dat_nm_en.index.get_level_values(1))

# calculate the ratio
dat_ratio_en = dat_m_en.visits / (dat_m_en.visits + dat_nm_en.visits)

# aggregate to weekly data
# dat_nm_w = dat_nm.groupby(['language', lambda x: x.week]).mean()
# dat_m_w = dat_m.groupby(['language', lambda x: x.week]).mean()
dat_ratio_en_w = dat_ratio_en.groupby([lambda x: x.year, lambda x: x.week])\
                .mean()