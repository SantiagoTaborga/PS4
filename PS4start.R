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
  library(kableExtra)
  


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

# graph theme ----------
  theme_personalized = theme_classic() +
    theme(
      axis.title.x = element_text(size = 9),
      axis.title.y = element_text(size = 9),
      legend.text = element_text(size = 9),
      text = element_text(family = "Times")
    )
  
# COVID deaths nationally ----------
## VIZ  
  covid %>%
    ggplot(aes(x = (1 + deaths))) +
    geom_histogram(color = 'white', fill = '#900603') +
    scale_x_log10() + # to mitigate skew
    scale_y_continuous(
      expand = expansion(mult = c(0,0.05))
    ) +
    labs(x = "# of deaths",
         y = "# of counties"
         ) +
    theme_personalized

## stat summary and more
  summary(covid$deaths)
  # find some examples?
  
  # percent of counties with less than 1000 deaths
  covid %>%
    filter(deaths < 1000) %>%
    nrow() / nrow(covid) * 100 
  
  # table with summary stats for mean and median
  median_counties <- covid %>%
    filter(deaths == 29)
  
  mean_countires <- covid %>%
    filter(deaths <= 85,
           deaths >= 84)
# Mask usage -----------------------
## VIZ: "Always wears a mask"
  covid %>%
    ggplot(aes(x = always.mask)) +
    geom_histogram(color = 'white', fill = '#900603') +
    scale_x_continuous(labels = scales::percent_format(scale = 100)) +
    scale_y_continuous(
      expand = expansion(mult = c(0,0.05))
      ) +
    labs(x = "% of people who 'always wear a mask'",
         y = "# of counties"
    ) +
    theme_personalized
  
## helpers
  summary(covid$always.mask)
  
  covid %>%
    filter(always.mask < 0.613,
           always.mask > 0.393) %>%
    nrow() / nrow(covid) * 100
  # find hi/lo counties?
  hi_lo_mask <- covid %>%
    filter(always.mask %in% c(
      min(always.mask, na.rm = TRUE), 
      max(always.mask, na.rm = TRUE)))

# Rates of vaccination -------------

## VIZ: overall vax rates
  covid %>%
    ggplot(aes(x = vax.complete)) +
    geom_histogram(color = 'white', fill = '#900603') +
    scale_x_continuous(
      labels = scales::percent_format(scale = 1),
      breaks = seq(0, 75, by = 25)) +
    scale_y_continuous(
      expand = expansion(mult = c(0,0.05))
    ) +
    labs(x = "% of people who are completely vaccinated",
         y = "# of counties"
    ) +
    theme_personalized

  summary(covid$vax.complete)
  
  covid %>%
    filter(vax.complete > 50) %>%
    nrow() / nrow(covid) * 100
## VIZ: vax rates by Social Vulnerability Index category
  covid %>%
    filter(!is.na(svi.index)) %>%
    ggplot(aes(x = vax.complete, y = svi.index)) +
    geom_boxplot(outlier.shape = 16, outlier.size = 3, outlier.color = "#900603", alpha = 0.4) + 
    scale_x_continuous(
      labels = scales::percent_format(scale = 1)) +
    labs(y = "Social Vulnerability Index", 
         x = "% Vaccination Completion"
         ) +
    theme_personalized

  covid %>%
    filter(svi.index == "D") %>%
    summary(vax.complete)
  

## find high/low counties
  covid %>%
    select(vax.complete, state, county) %>%
    filter(vax.complete %in% c(min(vax.complete, na.rm = T), 
                               max(vax.complete, na.rm = T))) %>%
    arrange(desc(vax.complete)) %>%
    knitr::kable(
      col.names = c("% fully vaccinated", "State", "County"),
      booktabs = TRUE, align = "c"
    ) %>%
    kableExtra::kable_styling(latex_options = "striped")


# Impact on 2022 COVID deaths ------
## regression estimates
  mods <- 
    list(
      "Masks" = felm(deaths.scaled ~ always.mask + population + svi.index | state, data = covid),
      "Vaccination" = felm(deaths.scaled ~ vax.complete + population + svi.index | state, data = covid),
      "Masks and Vaccination" = felm(deaths.scaled ~ always.mask + vax.complete + svi.index | state, data = covid)
    )

## regression table
  table <- modelsummary(
    mods, 
    coef_order = c("always.mask", "vax.complete", "population", "svi.indexB", "svi.indexC", "svi.indexD"),
    coef_rename = c(
      "always.mask" = "Mask Usage", 
      "vax.complete" = "Vaccine Completion", 
      "population" = "Population",
      "svi.indexB" = "SVI - B",
      "svi.indexC" = "SVI - C",
      "svi.indexD" = "SVI - D"
    ),
    gof_map = c('nobs'),
    output = 'gt',
    star = TRUE
  )
  
### Add header above the table
  table %>%
    tab_spanner(
      label = "Number of deaths",
      columns = c(`Masks`, `Vaccination`, `Masks and Vaccination`)
    ) 
