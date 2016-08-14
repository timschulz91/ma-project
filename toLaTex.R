# ============================== #
#                                #
# Results to LaTex code function #
#                                #
# ============================== #

# a function that summarizes several tables of results and outputs the top n
# languages (in terms of Wikipedia articles) formatted as the body of a LaTex
# table (\tabular) if an estimate for the language is available

require(stargazer)

toLaTex = function(data, r_object, ltable, n)
{
  # - data is the object that contains the observed data (has to be a datatable)
  # - r_object is the list object that stores the results for various languages
  # - ltable is the df of languages and their number of articles etc.
  # - n is the number of languages to be summarized in the table (if available)
  
  
  # === figure out languages === #
  # find the languages of interest
  toplangs = ltable[ltable$articles >= sort(ltable$articles, decreasing=T)[n],]
  
  # get some basic info about the languages
  tl_info = toplangs[,c('language', 'id', 'articles', 'pages', 'edits')]
  
  # find out how many parameter estimates will be in the table
  npars = length(summary(r_object[[ids[1]]])$coefficients[,1])
  
  
  # === create table === #
  # initialize the table of results
  ids = toplangs$id
  tab = matrix(ncol = n, nrow = 2*npars + 1)
  
  # make the individual languages the columns
  colnames(tab) = toplangs$language
  
  # make every second row the name of the parameter (every other row is its SE)
  rownames = names(summary(r_object[[ids[1]]])$coefficients[,1])
  rownames = rep(rownames, each=2)
  rownames = paste0('$', rownames, '$')
  rownames[2 * seq(length(rownames)/2)] = ''
  # add one element for R^2
  rownames = c(rownames, '$R^2$')
  rownames(tab) = rownames
  
  
  # === fill in table === #
  for(l in ids)
  {
    # get results for the current language
    cr = r_object[[l]]
    # check that these results actually exist
    if(is.null(cr)) next
    # for some reason l='war' always throws an error, so skip it
    if(l=='war') next
    # calculate r_squared
    RSS = sum(residuals(cr)^2)
    TSS = sum((data[language == l]$visits -
                 mean(data[language == l]$visits, na.rm=T))^2, na.rm=T)
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
      return(s)
    })
    c = paste0('$', c, '^{', star, '}$')
    se = paste0('(', se, ')')
    coefs = c(t(cbind(c, se)))
    # add R^2 to the vector
    coefs = c(coefs, round(R, 4))
    # find the correct column for the current vector
    lang = tl_info[tl_info$id==l, 'language']
    tab[,lang] = coefs
  }
  
  # exclude NA columns
  keep = apply(tab, 2, function(col) !all(is.na(col)))
  tab = tab[,keep]
  
  
  # === to LaTex === #
  # convert this table to something that's latex readable
  ## make the rownames a separate column
  tab = cbind(rownames(tab), tab)
  colnames(tab)[1] = 'Parameter'
  ## make the colnames the first row
  tab = rbind(colnames(tab), tab)
  # add latex formatting
  latex_lines = apply(tab, 1, function(line) paste(line, collapse=' & '))
  latex_table = paste(latex_lines, collapse='\\\\ \n')
  cat(latex_table)
  # DONE
}