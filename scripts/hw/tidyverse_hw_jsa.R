## Tidyverse HW 10/3/2023
## Jared Adam

# I am show casing here several ways of accomplishing this and resolving issues, like a negative difftime
# I encourage you to try these changes indiviudally to see what is happening


raw_wcrdata = read.csv("https://raw.githubusercontent.com/saraherm/Applied_Biostatistics/main/20230214_WCRdata.csv")
library(tidyverse)

test_wcr <- raw_wcrdata

?difftime
?gsub

as_tibble(test_wcr)

date_fxn <- function(x, na.rm = FALSE) (as.Date(x, format = "%m/%d/%Y")) #fxn I will use for dates
## FORMAT = this follows the current format of the column, not the format of what you want the new column to read as 


cleaning_wcr <- test_wcr %>% 
  mutate(root_g = gsub("g","", RootMass.g., fixed = T),
                shoot_g= gsub("g","", ShootMass.g., fixed = T),
                WCR_added = X.WCRAdded, #by doing this, I am always keeping the original column. This is not needed if you add a new object with each iteration
                WCR_recovered = X.WCRRecovered,
                WCR_avg_mg = WCRWeightAvg.mg.) %>% 
  select(-RootMass.g., -ShootMass.g., -X.WCRRecovered, -WCRWeightAvg.mg., -X.WCRAdded) %>% 
  mutate_at(c("DateWCR","DateGerm"), date_fxn) %>%  # mutate_at to implement a fxn on specific columns
  mutate(wcr_age = difftime(DateGerm, DateWCR, units = "days")) %>%
  mutate(wcr_age = gsub("days", "", wcr_age), 
         wcr_age = gsub("-", "", wcr_age)) %>% 
  mutate_at(c("root_g", "shoot_g", "WCR_added", "WCR_recovered", "wcr_age"), as.numeric) %>% 
  mutate(WCR_survival = WCR_recovered / WCR_added) %>% 
  mutate(variety = gsub("'", "", Variety)) %>%
  mutate(variety = gsub(" ", "_", variety)) %>% 
  select(variety, root_g, shoot_g, wcr_age, WCR_survival)

as_tibble(cleaning_wcr) 
unique(cleaning_wcr$variety)

blue_river <-
  subset(cleaning_wcr, variety == "Blue_River_Organic")
as_tibble(blue_river)
