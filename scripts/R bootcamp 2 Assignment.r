##### Start #####
FieldData = read.csv("FieldData.csv")
str(FieldData)
print(FieldData)

is.factor(FieldData) 
is.character(FieldData)
is.integer(FieldData)
is.numeric(FieldData)

library(tidyverse)
library(dplyr)
##### C #####
weight_g = c(FieldData$weight_g)
is.integer(weight_g)

weight_g = as.character(weight_g)
is.character(weight_g)

##### Real Data #####
#I do not know how to do this in R, but would like to 

milkweeddata = read.csv("https://raw.githubusercontent.com/saraherm/Applied_Biostatistics/main/milkweed_defense.csv")
str(milkweeddata)
View(milkweeddata)
##### plots for MW #####

# mass vs trichomes #

# plot #
plot(milkweeddata$Caterpillar_mass, milkweeddata$Trichomes)
# ggplot #
library(ggplot2)
ggplot(data = milkweeddata, aes(x=Caterpillar_mass, y=Trichomes)) +
         geom_point(size = 2 , aes(color = Milkweed_species))

#mass vs latex 

# plot # 
plot(milkweeddata$Caterpillar_mass, milkweeddata$Latex)
# ggplot #
library(ggplot2)
ggplot(data = milkweeddata, aes(x=Caterpillar_mass, y=Latex)) +
  geom_point(size = 2 , aes(color = Milkweed_species))    

#mass vs toxins 

# plot # 
plot(milkweeddata$Caterpillar_mass, milkweeddata$Toxin_amt)
# ggplot #
library(ggplot2)
ggplot(data = milkweeddata, aes(x=Caterpillar_mass, y=Toxin_amt)) +
  geom_point(size = 2 , aes(color = Milkweed_species))    

# C # 
milkweeddata2 = filter(milkweeddata, milkweeddata$Trichomes < 20)
View(milkweeddata2)

# plot # 
plot(milkweeddata2$Caterpillar_mass, milkweeddata2$Trichomes)
# ggplot #
library(ggplot2)
ggplot(data = milkweeddata2, aes(x=Caterpillar_mass, y=Trichomes)) +
  geom_point(size = 2 , aes(color = Milkweed_species))  




##### Part 3 ####
raw_wcrdata = read.csv("https://raw.githubusercontent.com/saraherm/Applied_Biostatistics/main/20230214_WCRdata.csv")
library(tidyverse)

?gsub
test <- raw_wcrdata %>% 
  mutate(root_g = gsub("g","", RootMass.g., fixed = T),
         shoot_g= gsub("g","",ShootMass.g., fixed = T)) %>% 
  select(Variety, shoot_g, root_g, DateGerm, DateWCR) %>% 
  mutate(across(2:3, as.numeric)) 

test_na <- test %>% 
  na.omit(root_g) %>% 
  na.omit(shoot_g)

which(is.na(test_na$root_g))

new <- dmy(test$DateGerm)
as_tibble(new)

date_test <- test

as.Date(test$DateWCR, '%d/%m/%Y')

as_tibble(date_test)
test$DateGerm = as.Date(x = test$DateGerm, format = "%m/%d/%Y")
as_tibble(test)
test$DateWCR <- as.Date(x = test$DateWCR, format = "%m/%d/%Y")
as_tibble(test)

test$DateWCR = as.Date(test$DateWCR, format = "%m/%d/%y")
as_tibble(test)

?as.Date



is.na(test_na$shoot_g)
as_tibble(test_na)

is.na(test$root_g)

is.na(test$root_g)
test_na <- na.omit(test$shoot_g)
as_tibble(test_na)
is.na(test_na)
where(is.na(test))

?na.omit

?summarise
test$root_g <- as.numeric(test$root_g)

as_tibble(test)




# %>% 
#   select(root_g, shoot_g)
as_tibble(test)



View(raw_wcrdata)
# raw_wcrdata = read.csv("20230214_WCRdata.csv")

#NEED this to run to get cleaned data object AFTER fxn is run, but after fxns run? IDK anymore.... 3/2/2023
cleaned_wcrdata = clean_wcrdata(input_data)


# Jared on 9.13.2023
test_wcr <- raw_wcrdata

as_tibble(test_wcr)

unique(test_wcr$Variety)

date_fxn <- function(x, na.rm = FALSE) (as.Date(x, format = "%m/%d/%Y")) #fxn I will use for dates
## FORMAT = this follows the current format of the column, not the format of what you want the new column to read as 

cleaning_wcr <- test_wcr %>% 
  dplyr::mutate(root_g = gsub("g","", RootMass.g., fixed = T),
         shoot_g= gsub("g","",ShootMass.g., fixed = T),
         WCR_added = X.WCRAdded,
         WCR_recovered = X.WCRRecovered,
         WCR_avg_mg = WCRWeightAvg.mg.) %>% 
  dplyr::select(-RootMass.g., -ShootMass.g., -X.WCRRecovered, -WCRWeightAvg.mg., -X.WCRAdded) %>% 
  dplyr::mutate_at(c("DateWCR","DateGerm"), date_fxn) %>%  # mutate_at to implement a fxn on specific columns
  dplyr::mutate(age = difftime(DateGerm, DateWCR, units = "days")) %>% 
  dplyr::mutate_at(c("root_g", "shoot_g", "WCR_added", "WCR_recovered"), as.numeric) %>% 
  dplyr::mutate(WCR_survival = WCR_recovered / WCR_added) %>% 
  mutate(test = gsub("'", "", Variety)) %>%
  mutate(variety = gsub(" ", "_", test)) %>% 
  dplyr::select(Variety, root_g, shoot_g, age, WCR_survival, variety)

as_tibble(cleaning_wcr) 
unique(cleaning_wcr$variety)




# seeking date clarity 

date_test <- test_wcr %>%
  select(DateGerm, DateWCR)

as_tibble(date_test)

?strptime

date_test$DateGerm <- strptime(as.character(date_test$DateGerm), "%d/%m/%Y")
as_tibble(date_test)

##











#THE OG function that has been split up below. NO roxygen2 here
#keeping this here for reference, but not running 
#Will be changing the parameters as I continue, so be mindful of order!!!
clean_wcrdata <- function(input_data) {
  #remove the 'g' from the data set
  
  input_data$RootMass.g. = gsub("g","",input_data$RootMass.g., fixed=T)
  input_data$ShootMass.g. = gsub("g","",input_data$ShootMass.g., fixed=T)
  
  #changing data type to as.numeric 
  
  input_data$RootMass.g. = as.numeric(input_data$RootMass.g.)
  input_data$ShootMass.g. = as.numeric(input_data$ShootMass.g.)          
  
  #adding new column to the data set for time est age of plants when catperillar added
  #using lubridate rather than base R
  #converting to date first 
  
  input_data$DateGerm = as.Date(input_data$DateGerm, format = "%m/%d/%Y")
  input_data$DateWCR = as.Date(input_data$DateWCR, format = "%m/%d/%Y")
  
  # mathing the difference in time 
  
  clean_time_diff_age = (input_data$DateWCR - cleaned_wcrdata$DateGerm)
  
  #adding the the column, did not work
  
  add_column(clean_time_diff_age)
  
  print(clean_time_diff_age)
  
  # survival rate, #still not showing up 
  
  WCR_survival= (cleaned_wcrdata$X.WCRAdded / cleaned_wcrdata$X.WCRRecovered)
  
  add_column(WCR_survival) 
  

  
  library(dplyr)
  
  #Filtering Blue river 
  
  BlueRiver = filter(cleaned_wcrdata, Variety == "BlueRiverOrganic")
 
  
  return(input_data)
  }



#' Function to remove 'g' character
#'
#' @param input_data A raw dataframe
#'
#' @return dataframe that has remove g characters
#' @export
#'
#' @examplessee 
clean_wcrdata_1 <- function(input_data) {
  #remove the 'g' from the data set
  
  input_data$RootMass.g. = gsub("g","",input_data$RootMass.g., fixed=T)
  input_data$ShootMass.g. = gsub("g","",input_data$ShootMass.g., fixed=T)
  
  return(input_data)
  
}

#' Not running as of 2/28/2023!!!
#' EVERYTHING is bReAkInG... meh.
#' I think it is not working becuase of some naming nueance somewhere that I am doing wrong.. 
#' @param gsub_data 
#'
#' @return
#' @export
#'
#' @examples
clean_wcrdata_2 <- function(input_data) {
  #changing data type to as.numeric 
  
  input_data$RootMass.g. = as.numeric(input_data$RootMass.g.)
  input_data$ShootMass.g. = as.numeric(input_data$ShootMass.g.)
  
  return(input_data)

}


#' Date format stuff
#' works in the higher single fxn
#'
#' @param numeric_data 
#'
#' @return date columns to as.Date and formatting to my liking
#' @export
#'
#' @examples#' 
clean_wcrdata_3 <- function(input_data) {
  #adding new column to the data set for time est age of plants when caterpillar added
  #using lubridate rather than base R
  #converting to date first 
  library(lubridate)
  
  #trying to get this back to numeric because coladd wont work with as.Date??
  
  #numeric_data$DateGerm = as.numeric(numeric_data$DateGerm)
  #numeric_data$DateWCR = as.numeric(numeric_data$DateWCR)
  
  #the OG code for changing these to as.Date
  input_data$DateGerm = as.Date(input_data$DateGerm, format = "%m/%d/%Y")
  input_data$DateWCR = as.Date(input_data$DateWCR, format = "%m/%d/%Y")
  
 
 return(input_data)
}

#' Attempting to subset/ filter Blue River Organic 
#'
#' @param clean_dates 
#'
#' @return
#' @export
#'
#' @examples new column adding in there, but not in the data set itself
clean_variety_BR_wcrdata <- function(input_data) {
#filter just Blue River variety here 

BlueRiver = filter(input_data, variety) == "BlueRiverOrganic"

return(input_data)

}

#' Adding a column, but getting it wrong 
#'
#' @param clean_dates 
#'
#' @return
#' @export
#'
#' @examples
clean_wcrdata_4 <- function(input_data) {
  
#using lubridate rather than base R  
library(lubridate)
  library(dplyr)
  library(ggpubr)
  #BUT how do I make this a new column???? AHH 
#time_diff_age = mutate(duration(clean_dates$DateGerm, clean_dates$DateWCR, units = "days"))
mutate()
# weeks
#clean_time_diff_age = difftime(strptime("2021.06.07", format = "%m.%d.%Y"),
         #strptime("2021.07.14", format = "%m.%d.%Y"),units="weeks")
#add_column(clean_time_diff_age)

##new stokes attempt, will try then 
#cleaned_wcrdata$clean_time_test = 

##### I could not get the above to work 
  
# mathing the difference in time, in days

clean_time_diff_age_days = (cleaned_wcrdata$DateWCR - cleaned_wcrdata$DateGerm)

#adding the the column, did not work

add_column(clean_time_diff_age)  

return(input_data)
}
#' WCR Survival 
#'
#' @param input_data 
#'
#' @return
#' @export
#'
#' @examples
clean_wcrdata_5 <- function(input_data) {
  # survival rate, #still not showing up 
  
  WCR_survival= (cleaned_wcrdata$X.WCRAdded / cleaned_wcrdata$X.WCRRecovered)
  
  add_column(WCR_survival)
  
  print(WCR_survival)
  
  
  
  
  return(input_data)
}

# PIPING #
library(tidyverse)
#piping all of these functions together
raw_wcrdata %>%
  clean_wcrdata_1() %>%
  clean_wcrdata_2() %>%
  clean_wcrdata_3() %>%
  clean_wcrdata_4() %>%
  clean_wcrdata_5()

#Issue here, I think 
#this piping will not output beauce my next line of code is not working. SOS 

#NEED this to run to get cleaned data object
cleaned_wcrdata = clean_wcrdata(raw_wcrdata)


#show me the money 
clean_wcrdata(raw_wcrdata)

#NEED this to run to get cleaned data object
cleaned_wcrdata = clean_wcrdata(raw_wcrdata)


#for loops 

##### Trying to resolve code outside of the fxns #####



raw_wcrdata = read.csv("https://raw.githubusercontent.com/saraherm/Applied_Biostatistics/main/20230214_WCRdata.csv")

test = c(2,3,4,3,4,5)


install.packages("tibble")
library(tibble)

add_column(test)




library(lubridate)
duration(wcrdata$DateGerm, wcrdata$DateWCR, units = "days")

#this is wrong, but trying to subset blue river 
View(wcrdata)
wcr_data1 = filter(wcrdata, wcrdata$Variety  == "Blue River Organic")
print(wcr_data1)