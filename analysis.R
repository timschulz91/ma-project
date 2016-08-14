# ======================
# MA Project
# Data analysis
# ======================

# === description === #
# This script is for cleaning and analysing the raw data obtained from
# main.py (or main_cli.py)


# === read data === #
# This whole section doesn't work in R, not efficient enough for my hardware
# Do this in Python (final_aggregation.py) instead
# require(data.table)
# require(zoo)
# require(xts)
# dat = data.table(read.csv('completeData.csv'))
# create the subset of mobile, english articles
# dat_en_mobile = zoo(dat[language=='en' & type=='mw' , visits],
#                     order.by=as.POSIXct(dat[language=='en' & type=='mw' , hour],
#                                         format='%Y-%m-%d %H:%M:%S', tz='GMT'))
# remove non-unique observations by averaging them out
# dat_en_mobile = aggregate(dat_en_mobile, identity, mean)
# create the subset of non-mobile, english articles
# sum up the different types for each hour
# dat_en_nmobile = dat[language!='en' & type=='mw' , sum(visits), by=hour]
# dat_en_nmobile = zoo(dat_en_nmobile$V1,
#                      order.by=as.POSIXct(dat_en_nmobile$hour,
#                                          format='%Y-%m-%d %H:%M:%S', tz='GMT'))
# remove any remaining non-unique observations by averaging them out
# dat_en_nmobile = aggregate(dat_en_nmobile, identity, mean)


# calculated the ratio
# dat_en_ratio = dat_en_mobile / (dat_en_mobile + dat_en_nmobile)
# aggregate to weekly data
# dat_en_ratio_w = apply.weekly(dat_en_ratio, mean)
# plot(dat_en_ratio_w)

require(data.table)
require(zoo)
require(nlstools)

# === load the data === #
dat = data.table(read.csv('WeeklyRatios.csv'))

# === data cleanup === #
# identify the languages
langs = unique(dat$language)
# for each language, ...
keep = c()
for(l in langs)
{
  # ... check if I have at least 60% non-NA
  condition1 = mean(!is.na(dat[language == l]$visits)) >= .6
  # ... check if I have at least 320 obsevations (40 weeks per year)
  condition2 = dim(dat[language == l])[1] >= 320
  # if both are satisfied, decide to keep the language
  if(condition1 & condition2) keep = c(keep, l)
}

dat = dat[dat$language %in% keep]
langs = sort(keep)

# create a year-week column, let day of the week be the last day of the week (7)
dat$yw = paste(dat$year, dat$week, '7', sep='-')
dat$yw = as.POSIXct(dat$yw, tz='GMT', '%Y-%U-%u')


# === model fitting === #
# ignore traffic in bytes for now; something weird is going on mid-August 2011
# for each language, fit the S-curve (without release dates of devices for now)

# issues with some languages, specify manually for now
skip = c('ace', 'als', 'ast', 'ba', 'bar', 'bh', 'bm', 'bo',
         'ceb', 'cv', 'dv', 'el', 'es', 'ext', 'fy', 'gan', 
         'hif', 'hu', 'ik', 'ilo', 'kk', 'km', 'lad', 'li',
         'lij', 'lmo', 'mo', 'mwl', 'mzn', 'na', 'nah', 'pag',
         'pam', 'pnt', 'qu', 'sc', 'scn', 'si', 'simple', 'sn',
         'tg', 'tn', 'ug', 'uk', 'vo', 'wo', 'yi', 'yo',
         'zh-yue')
langs = setdiff(langs, skip)

# a list to save the results in
results = vector(mode='list', length=length(langs))
names(results) = langs

for(l in langs)
{
  temp = dat[language == l]
  
  # NAs are the result of zero mobile traffic
  ## find the first week when it's not zero anymore
  ## replace all NAs up to that point with zero
  i = 1
  while(is.na(temp$visits[i])) i = i + 1
  temp$visits[1:(i-1)] = 0
  
  # remove NAs from the index
  temp = temp[!is.na(temp$yw)]
  # create a zoo object of the ratio
  visits = zoo(temp$visits, order.by = temp$yw)
  # remove NAs
  #visits = visits[!is.na(index(visits))]
  
  # model
  model = visits ~ K/(1+exp(-(a+b*1:length(visits))))
  
  logit_model = nls(model, algorithm = 'port',
                    start = list(K=.5, a=-15, b=.03),
                    lower = list(K=0, a=-Inf, b=-1),
                    upper = list(K=1, a=Inf, b=1))
  
  results[[l]] = logit_model
}

# remove results that ran into the boundary (b=1)
results = results[!lapply(results, function(x) coef(x)[3])==1]

# some specific results
# based on https://shishirshakya.blogspot.ca/2015/06/non-linear-regression-in-logistic.html
subresult = c('ar', 'cs', 'da', 'de', 'en', 'fr', 'he', 'hi', 'hr', 'nl', 'it', 
              'ja', 'ko', 'lt', 'pl', 'pt', 'ru', 'vi')
table1 = data.frame(lcode=1, K=1, a=1, b=1, Rsq=1)
for(l in subresult)
{
  # get the current result
  cr = results[[l]]
  RSS = sum(residuals(cr)^2)
  TSS = sum((dat[language == l]$visits -
               mean(dat[language == l]$visits, na.rm=T))^2, na.rm=T)
  R = 1 - RSS/TSS
  table1 = rbind(table, c(l, coef(cr), R))
}

table = table[-1,]
# stargazer(table1, summary=F, rownames=F, digits = 3)

# plot(dat[language=='en']$yw, dat[language=='en']$visits, type='l', xlab='Year', ylab='Ratio')
# lines(dat[language=='en']$yw[!is.na(dat[language=='en']$yw)], predict(results[['en']]))
# title(main='Ratio of mobile to all visits of English Wikimedia pages over time')

# === device release dates === #
releases = data.frame(date = c('2007-06-29',
                               '2008-07-11',
                               '2009-06-19',
                               '2010-06-24',
                               '2011-10-14', 
                               '2012-09-21',
                               '2013-09-20',
                               '2014-09-19',
                               '2015-09-25',
                               '2016-03-31',
                               '2010-06-04',
                               '2011-05-02',
                               '2012-05-29',
                               '2013-04-27',
                               '2014-04-11',
                               '2015-04-10',
                               '2016-03-11'),
                      device = c('iPhone',
                                 'iPhone3G',
                                 'iPhone3GS',
                                 'iPhone4',
                                 'iPhone4S',
                                 'iPhone5',
                                 'iPhone5S',
                                 'iPhone6',
                                 'iPhone6S',
                                 'iPhoneSE',
                                 'GalaxyS',
                                 'GalaxyS2',
                                 'GalaxyS3',
                                 'GalaxyS4',
                                 'GalaxyS5',
                                 'GalaxyS6',
                                 'GalaxyS7'),
                      brand = c(rep('Apple', 10),
                                rep('Samsung', 7)))
releases$date = as.POSIXct(releases$date)


# a list to save the results in
results2 = vector(mode='list', length=length(langs))
names(results2) = langs

# indicator function whether a date is inside a and b of a release date
days_0 = as.difftime(0, unit='days')
days_7 = as.difftime(7, unit='days')
days_14 = as.difftime(14, unit='days')
days_21 = as.difftime(21, unit='days')
days_28 = as.difftime(28, unit='days')
inrange = function(dates, releasedates, lower, upper)
{
  sapply(dates, function(x)
    {
      cond = sapply(releasedates, function(y) (y+lower <= x) & (y+upper > x))
      return(any(cond))
  })
}

# skip some more languages
skip2 = c('diq', 'fo', 'gv', 'ha', 'ii', 'kw', 'mhr', 'mi', 'mt', 'os', 'pnb', 
          'sco', 'vec', 'war')
langs2 = setdiff(langs, skip2)

# process languages
for(l in langs2)
{
  temp = dat[language == l]
  
  # NAs are the result of zero mobile traffic
  ## find the first week when it's not zero anymore
  ## replace all NAs up to that point with zero
  i = 1
  while(is.na(temp$visits[i])) i = i + 1
  temp$visits[1:(i-1)] = 0
  
  # remove NAs from the index
  temp = temp[!is.na(temp$yw)]
  # create a zoo object of the ratio
  visits = zoo(temp$visits, order.by = temp$yw)
  # remove NAs
  #visits = visits[!is.na(index(visits))]
  
  # create dummies (faster than doing it in estimation)
  i0 = inrange(index(visits), releases$date, days_0, days_7)
  i1 = inrange(index(visits), releases$date, days_7, days_14)
  i2 = inrange(index(visits), releases$date, days_14, days_21)
  i3 = inrange(index(visits), releases$date, days_21, days_28)
  
  # model
  model = visits ~ K/(1+exp(-(a+b*1:length(visits)))) +
    t0*i0 + t1*i1 + t2*i2 + t3*i3
  
  logit_model = nls(model, algorithm = 'port',
                    start = list(K=.5, a=-15, b=.03, t0=0, t1=0, t2=0, t3=0),
                    lower = list(K=0, a=-Inf, b=-1, t0=-Inf, t1=-Inf, t2=-Inf, t3=-Inf),
                    upper = list(K=1, a=Inf, b=1, t0=Inf, t1=Inf, t2=Inf, t3=Inf))
  
  results2[[l]] = logit_model
}



##################################################################


# === second type of dummies === #
# BOTH
inrange2 = function(dates, lower, upper) lower <= dates & dates < upper

# skip some languages that don't converge
skip3 = c('ab', 'af', 'arz', 'be', 'br', 'dk', 'eo', 'fo', 'frp', 'gu', 'haw',
          'he', 'hsb', 'ii', 'jv', 'kn', 'kr', 'mg', 'mhr', 'ml', 'mt', 'my',
          'nap', 'nn', 'nov', 'nrm', 'ny', 'oc', 'pi', 'pnb', 'su', 'tt', 'ur',
          'uz', 'wa', 'zh', 'zh-classical', # up to here for Apple
          'be-x-old', 'bn', 'ch', 'ckb', 'cu', 'cz', 'et', 'fa', 'fur', 'ga',
          'ha', 'jp', 'ku', 'kw', 'lb', 'mk', 'nv', 'pa', 'pap', 'pdc', 'rn',
          'roa-tara', 'sa', 'sco', 'sr', 'tk', 'tpi', 'vec')

langs3 = setdiff(langs, skip3)

# a list to save the results in
results3 = vector(mode='list', length=length(langs3))
names(results3) = langs3

custom = nls.control(maxiter = 1000)

# process languages
for(l in langs3)
{
  temp = dat[language == l]
  
  # NAs are the result of zero mobile traffic
  ## find the first week when it's not zero anymore
  ## replace all NAs up to that point with zero
  i = 1
  while(is.na(temp$visits[i])) i = i + 1
  temp$visits[1:(i-1)] = 0
  
  # remove NAs from the index
  temp = temp[!is.na(temp$yw)]
  # create a zoo object of the ratio
  visits = zoo(temp$visits, order.by = temp$yw)
  
  # create dummies for Apple
  dates = as.POSIXct(releases[releases$brand=='Apple', 'date'])
  for(i in 3:(length(dates)-1))
  {
    temp = inrange2(index(visits), dates[i], dates[i+1])
    assign(paste0('d', i), temp)
  }
  # create dummies for Samsung
  dates = as.POSIXct(releases[releases$brand=='Samsung', 'date'])
  for(i in 1:(length(dates)-1))
  {
    temp = inrange2(index(visits), dates[i], dates[i+1])
    assign(paste0('e', i), temp)
  }
  
  # describe the model
  model = visits ~ K/(1+exp(-(a+b*1:length(visits)))) +
    #t1*d1 + t2*d2 + 
    a3*d3 + a4*d4 + a5*d5 + a6*d6 + a7*d7 + a8*d8 + a9*d9 + s1*e1 + s2*e2 +
    s3*e3 + s4*e4 + s5*e5 + s6*e6
  
  # estimate the model
  logit_model = nls(model, algorithm = 'port',
                    start = list(K=.5, a=-15, b=.03, #t1=0, t2=0, 
                                 a3=0, a4=0, a5=0, a6=0, a7=0, a8=0, a9=0,
                                 s1=0, s2=0, s3=0, s4=0, s5=0, s6=0),
                    lower = list(K=0, a=-Inf, b=-1, #t1=-Inf, t2=-Inf,
                                 a3=-Inf, a4=-Inf, a5=-Inf, a6=-Inf, a7=-Inf,
                                 a8=-Inf, a9=-Inf, s1=-Inf, s2=-Inf, s3=-Inf,
                                 s4=-Inf, s5=-Inf, s6=-Inf),
                    upper = list(K=1, a=Inf, b=1, #t1=Inf, t2=Inf,
                                 a3=Inf, a4=Inf, a5=Inf, a6=Inf, a7=Inf, a8=Inf,
                                 a9=Inf, s1=Inf, s2=Inf, s3=Inf, s4=Inf, s5=Inf,
                                 s6=Inf),
                    control=custom)
  # save results
  results3[[l]] = logit_model
}

# APPLE only
# skip some languages that don't converge
skip4 = c('ab', 'af', 'arz', 'be', 'br', 'dk', 'eo', 'fo', 'frp', 'gu', 'haw',
          'he', 'hsb', 'ii', 'jv', 'kn', 'kr', 'mg', 'mhr', 'ml', 'mt', 'my',
          'nap', 'nn', 'nov', 'nrm', 'ny', 'oc', 'pi', 'pnb', 'su', 'tt', 'ur',
          'uz', 'wa', 'zh', 'zh-classical')

langs4 = setdiff(langs, skip4)

# a list to save the results in
results4 = vector(mode='list', length=length(langs4))
names(results4) = langs4

custom = nls.control(maxiter = 1000)

# process languages
for(l in langs4)
{
  temp = dat[language == l]
  
  # NAs are the result of zero mobile traffic
  ## find the first week when it's not zero anymore
  ## replace all NAs up to that point with zero
  i = 1
  while(is.na(temp$visits[i])) i = i + 1
  temp$visits[1:(i-1)] = 0
  
  # remove NAs from the index
  temp = temp[!is.na(temp$yw)]
  # create a zoo object of the ratio
  visits = zoo(temp$visits, order.by = temp$yw)
  
  # create dummies for Apple
  dates = as.POSIXct(releases[releases$brand=='Apple', 'date'])
  for(i in 3:(length(dates)-1))
  {
    temp = inrange2(index(visits), dates[i], dates[i+1])
    assign(paste0('d', i), temp)
  }
  
  # describe the model
  model = visits ~ K/(1+exp(-(a+b*1:length(visits)))) +
    #t1*d1 + t2*d2 + 
    a3*d3 + a4*d4 + a5*d5 + a6*d6 + a7*d7 + a8*d8 + a9*d9
  
  # estimate the model
  logit_model = nls(model, algorithm = 'port',
                    start = list(K=.5, a=-15, b=.03, #t1=0, t2=0, 
                                 a3=0, a4=0, a5=0, a6=0, a7=0, a8=0, a9=0),
                    lower = list(K=0, a=-Inf, b=-1, #t1=-Inf, t2=-Inf,
                                 a3=-Inf, a4=-Inf, a5=-Inf, a6=-Inf, a7=-Inf,
                                 a8=-Inf, a9=-Inf),
                    upper = list(K=1, a=Inf, b=1, #t1=Inf, t2=Inf,
                                 a3=Inf, a4=Inf, a5=Inf, a6=Inf, a7=Inf, a8=Inf,
                                 a9=Inf),
                    control=custom)
  # save results
  results4[[l]] = logit_model
}



# SAMSUNG only

# skip some languages that don't converge
skip5 = c('ab', 'af', 'arc', 'arz', 'be-x-old', 'bg', 'co', 'csb', 'cz', 'diq',
          'fa', 'fi', 'fr', 'fur', 'ga', 'gv', 'hr', 'ia', 'ii', 'mg', 'mhr',
          'mt', 'nds', 'ne', 'no', 'os', 'pa', 'pms', 'pnb', 'rm', 'rn', 'sa',
          'sk', 'sq', 'sr', 'sw', 'szl', 'tk', 'tt', 'vec', 'vls', 'xal', 'zh')

langs5 = setdiff(langs, skip5)

# a list to save the results in
results5 = vector(mode='list', length=length(langs5))
names(results5) = langs5

custom = nls.control(maxiter = 1000)

# process languages
for(l in langs5)
{
  temp = dat[language == l]
  
  # NAs are the result of zero mobile traffic
  ## find the first week when it's not zero anymore
  ## replace all NAs up to that point with zero
  i = 1
  while(is.na(temp$visits[i])) i = i + 1
  temp$visits[1:(i-1)] = 0
  
  # remove NAs from the index
  temp = temp[!is.na(temp$yw)]
  # create a zoo object of the ratio
  visits = zoo(temp$visits, order.by = temp$yw)
  
  # create dummies for Samsung
  dates = as.POSIXct(releases[releases$brand=='Samsung', 'date'])
  for(i in 1:(length(dates)-1))
  {
    temp = inrange2(index(visits), dates[i], dates[i+1])
    assign(paste0('e', i), temp)
  }
  
  # describe the model
  model = visits ~ K/(1+exp(-(a+b*1:length(visits)))) + s1*e1 + s2*e2 +
    s3*e3 + s4*e4 + s5*e5 + s6*e6
  
  # estimate the model
  logit_model = nls(model, algorithm = 'port',
                    start = list(K=.5, a=-15, b=.03, s1=0, s2=0, s3=0, s4=0,
                                 s5=0, s6=0),
                    lower = list(K=0, a=-Inf, b=-1, s1=-Inf, s2=-Inf, s3=-Inf,
                                 s4=-Inf, s5=-Inf, s6=-Inf),
                    upper = list(K=1, a=Inf, b=1, s1=Inf, s2=Inf, s3=Inf,
                                 s4=Inf, s5=Inf, s6=Inf),
                    control=custom)
  # save results
  results5[[l]] = logit_model
}
l

##################################################################
##################################################################

# === get language names === #
# This script creates a data frame (ltable) with all language codes and some
# summary statistics for Wikimedia projects in that language
# It also loads the findLanguage('language_code') function
source('language_decoder.R')

# from the languages excluded, identify the top 5 in terms of articles
excluded = data.frame(id = skip3)
excluded$articles = sapply(excluded$id, function(x) ltable[ltable$id==x,
                                                           'articles'])



# === tables for paper === #
source('toLaTex.R')
# general case (no device effects)
toLaTex(data=dat, r_object=results, ltable=ltable, n=10)
# both Apple and Samsung
toLaTex(data=dat, r_object=results3, ltable=ltable, n=10)
# Apple only
toLaTex(data=dat, r_object=results4, ltable=ltable, n=10)
# Samsung only
toLaTex(data=dat, r_object=results5, ltable=ltable, n=10)







require(stargazer)
# limit these tables to the top 10 languages (measured by number of articles)
toplangs = ltable[ltable$articles >= sort(ltable$articles, decreasing=T)[10],]

# 0. some info about the top languages
tl_info = toplangs[,c('language', 'id', 'articles', 'pages', 'edits')]
# stargazer(tl_info, summary=F)

# 1. table of general results
ids = toplangs$id
tl_tab1 = matrix(ncol = length(toplangs$id),
                 nrow = 2*length(summary(results[[ids[1]]])$coefficients[,1])+1)
# make the individual languages the columns
colnames(tl_tab1) = toplangs$language
# make every second row the name of the parameter (every other row is its SE)
rownames = names(summary(results[[ids[1]]])$coefficients[,1])
rownames = rep(rownames, each=2)
rownames = paste0('$', rownames, '$')
rownames[2 * seq(length(rownames)/2)] = ''
# add one element for R^2
rownames = c(rownames, '$R^2$')
rownames(tl_tab1) = rownames

for(l in ids)
{
  # get results for the current language
  cr = results[[l]]
  # check that these results actually exist
  if(is.null(cr)) next
  # calculate r_squared
  RSS = sum(residuals(cr)^2)
  TSS = sum((dat[language == l]$visits -
               mean(dat[language == l]$visits, na.rm=T))^2, na.rm=T)
  R = 1 - RSS/TSS
  # get the coefficients and standard errors
  c = sprintf('%.4f', summary(cr)$coefficients[,1])
  se = sprintf('%.4f', summary(cr)$coefficients[,2])
  # find out if an estimate is significant
  star = sapply(summary(cr)$coefficients[,4], function(p) {
    if(p < .1) s = '*'
    else s = ''
    if(p < .05) s = '**'
    if(p<.01) s = '***'
  })
  c = paste0('$', c, '^{', star, '}$')
  se = paste0('(', se, ')')
  coefs = c(t(cbind(c, se)))
  # add R^2 to the vector
  coefs = c(coefs, round(R, 4))
  # find the correct column for the current vector
  lang = tl_info[tl_info$id==l, 'language']
  tl_tab1[,lang] = coefs
}

# exclude NA columns
keep = apply(tl_tab1, 2, function(col) !all(is.na(col)))
tl_tab1 = tl_tab1[,keep]

# convert this table to something that's latex readable
## make the rownames a separate column
tl_tab1 = cbind(rownames(tl_tab1), tl_tab1)
colnames(tl_tab1)[1] = 'Parameter'
## make the colnames the first row
tl_tab1 = rbind(colnames(tl_tab1), tl_tab1)
# add latex formatting
latex_lines = apply(tl_tab1, 1, function(line) paste(line, collapse=' & '))
latex_table = paste(latex_lines, collapse='\\\\ \n')
cat(latex_table)