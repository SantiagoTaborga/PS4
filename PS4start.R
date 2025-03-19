# COVID REPORT
# Evil data team
# 03.23.2022

# Here's what we could manage. Sorry. You do the rest.

# packages
  library(tidyverse)
  library(scales)
  library(lfe)
  library(modelsummary)
  library(gt)
  library(data.table)


# create the dataset ------------------
## 2021-2022 deaths (BIG FILES)
  covid <-
    data.table::fread('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2022.csv') %>%
    filter(!is.na(fips), state != 'Puerto Rico') %>%
    select(fips, county, state, date, deaths) %>%
    group_by(fips, county, state) %>%
    summarise(deaths = max(deaths, na.rm = T) - min(deaths, na.rm = T))

## estimated mask usage from July 2020 survey
  mask <-
    read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/mask-use/mask-use-by-county.csv') %>%
    mutate(
      fips = as.integer(COUNTYFP),
      always.mask = ALWAYS, #always masking
      .keep = 'none'
    ) # for merging   

## prep CDC data from directory
  vax <-
    read_csv('cdc vax mar1.csv') %>%
    filter( # drop unknown/incomplete/questionable reports
      FIPS != 'UNK', 
      Recip_State != 'VI', 
      Completeness_pct > 0, 
      !is.na(Administered_Dose1_Recip)
    ) %>% 
    mutate(
      fips = as.integer(FIPS), 
      population = Census2019,
      vax.complete = Series_Complete_Pop_Pct, # percent vaxd
      svi.index = SVI_CTGY, # social vulnerability index
      .keep = 'none'
    )  

## merge  
  covid <-
    left_join(covid, mask) %>%
    left_join(vax) %>%
    mutate(deaths.scaled = deaths / population * 100000) %>%
    ungroup() # scale by population
  
  rm(mask, vax)
  
  summary(covid)

  
# COVID deaths nationally ----------
## VIZ  
  covid %>%
    ggplot(aes(x = (1 + deaths))) +
    geom_histogram(color = 'white') +
    scale_x_log10() # to mitigate skew

## stat summary and more
  summary(covid$deaths)
  # find some examples?


# Mask usage -----------------------
## VIZ: "Always wears a mask"
  covid %>%
    ggplot(aes(x = always.mask)) +
    geom_histogram(color = 'white')

## helpers
  summary(covid$always.mask)
  # find hi/lo counties?


# Rates of vaccination -------------

## VIZ: overall vax rates
  covid %>%
    ggplot(aes(x = vax.complete)) +
    geom_histogram(color = 'white')

## VIZ: vax rates by Social Vulnerability Index category
  covid %>%
    ggplot(aes(y = vax.complete, x = svi.index, color = svi.index)) +
    geom_boxplot() # drop the NA category; it's awful

## find high/low counties
  covid %>%
    select(vax.complete, state, county) %>%
    filter(vax.complete %in% c(min(vax.complete, na.rm = T), 
                               max(vax.complete, na.rm = T)))


# Impact on 2022 COVID deaths ------
## regression estimates
  mods <- 
    list(
      m1 = felm(deaths.scaled ~ always.mask + population + svi.index | state, data = covid),
      m2 = felm(deaths.scaled ~ vax.complete + population + svi.index | state, data = covid),
      m3 = felm(deaths.scaled ~ always.mask + vax.complete + svi.index | state, data = covid)
    )

## regression table
  modelsummary(
    mods, 
    gof_map = c('nobs'), 
    stars = TRUE, 
    output = 'gt'
  )


