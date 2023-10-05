## Tidyverse HW 10/3/2023
## Jared Adam
raw_wcrdata = read.csv("https://raw.githubusercontent.com/saraherm/Applied_Biostatistics/main/20230214_WCRdata.csv")
library(tidyverse)

test_wcr <- raw_wcrdata

?difftime
?gsub

as_tibble(test_wcr)

unique(test_wcr$Variety)

date_fxn <- function(x, na.rm = FALSE) (as.Date(x, format = "%m/%d/%Y")) #fxn I will use for dates
## FORMAT = this follows the current format of the column, not the format of what you want the new column to read as 

cleaning_wcr <- test_wcr %>% 
  dplyr::mutate(root_g = gsub("g","", RootMass.g., fixed = T),
                shoot_g= gsub("g","", ShootMass.g., fixed = T),
                WCR_added = X.WCRAdded,
                WCR_recovered = X.WCRRecovered,
                WCR_avg_mg = WCRWeightAvg.mg.) %>% 
  dplyr::select(-RootMass.g., -ShootMass.g., -X.WCRRecovered, -WCRWeightAvg.mg., -X.WCRAdded) %>% 
  dplyr::mutate_at(c("DateWCR","DateGerm"), date_fxn) %>%  # mutate_at to implement a fxn on specific columns
  dplyr::mutate(wcr_age = difftime(DateGerm, DateWCR, units = "days")) %>% 
  dplyr::mutate_at(c("root_g", "shoot_g", "WCR_added", "WCR_recovered"), as.numeric) %>% 
  dplyr::mutate(WCR_survival = WCR_recovered / WCR_added) %>% 
  mutate(test = gsub("'", "", Variety)) %>%
  mutate(variety = gsub(" ", "_", test)) %>% 
  dplyr::select(variety, root_g, shoot_g, wcr_age, WCR_survival)

as_tibble(cleaning_wcr) 
unique(cleaning_wcr$variety)

blue_river <-
  subset(cleaning_wcr, variety == "Blue_River_Organic")
as_tibble(blue_river)
