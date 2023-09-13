library(tidyverse)
library(palmerpenguins)
data(package = 'palmerpenguins')

palmerpenguins <- penguins
penguins_raw <- penguins_raw
as_tibble(penguins)
head(penguins)

#what I want to add: mutate vs mutate_at (do a class change), try and summarise with characters (and then have them mutate_at to change and correctly), 


which(is.na(penguins_raw), arr.ind = TRUE) #checking for NA locations in penguins_raw

penguins_raw <-
  na.omit(penguins_raw) #I must add this because there are NAs within this data set and these functions will not work otherwise 

# select()
penguins %>% #From the penguins data set
  select(species, bill_length_mm, year) # selecting columns species, bill_length, and      year 

# select()
penguins %>% #From the penguins data set
  select(-sex)# selecting all columns except for sex

# select()
penguins %>% #From the penguins data set
  select(starts_with('b')) # selecting columns that start with 'b' and using starts_with

# rename()
penguins %>%
  select(species, bill_length_mm, year) %>% #selecting the columns I want to look at
  rename( #rename function. notice here the two methods of changing names
    Species = species, #changing species to Species without quotes
    "Year" = year #changing year to Year with quotes
  )


# select() and arrange()
penguins %>%
  select(species, bill_length_mm, year) %>% 
  arrange(bill_length_mm) #I want to look at bill length in an increasing order from      smallest to largest 

# select(), arrange(), and desc()
penguins %>%
  select(species, bill_length_mm, year) %>% 
  arrange(desc(bill_length_mm)) #using the desc() function to command the order from      largest to smallest 

# select() and arrange()
penguins %>%
  select(species, bill_length_mm, year) %>% 
  arrange(year, desc(bill_length_mm)) #year and bill separated by a comma

# select() and filter()
penguins %>%
  select(species, bill_length_mm, bill_depth_mm, year) %>% 
  filter(year == 2007) #using the '==' operator to show everything with the year 2007

# select(), filter(), and mean()

penguins %>%
  select(species, bill_length_mm, year) %>% 
  filter(
    year == 2007,#using the '==' operator to show everything with the year 2007
    bill_length_mm > mean(bill_length_mm), # using '>' to view every row where the bill length is greater than the avg bill length 
  ) 

# count()
penguins %>% 
  count(species)

# select() and filter()
penguins %>%
  select(species, bill_length_mm, year) %>% 
  filter(
    year == 2007,#using the '==' operator to show everything with the year 2007
    bill_length_mm > mean(bill_length_mm),# using '>' to view every row where the bill length is greater than the avg bill length
    species == "Chinstrap" | species == "Gentoo" # look in species and pull out chinstrap and gentoo. | allows to command two species, similar to 'or'
  ) 

# select () and filter()
penguins %>%
  select(species, bill_length_mm, year) %>% 
  filter(
    year == 2007,#using the '==' operator to show everything with the year 2007
    bill_length_mm > mean(bill_length_mm),# using '>' to view every row where the bill length is greater than the avg bill length 
    species != "Adelie" #does not equal operator
  ) 

# select(), mutate(), select()
penguins %>%
  select(species, bill_length_mm, year) %>% 
  mutate( #mutate()
    sp_year = paste(species, "-", year), #adding a new column named 'sp_year' and pasting the species column and year column with a dash between them. 
    rn_bill_length_mm = round(bill_length_mm) #creating a column of rounded bill lengths 
  ) %>% 
  select(species, year, sp_year, bill_length_mm, rn_bill_length_mm) #placing these new columns an order I would like 

# summarize ()
penguins %>% 
  summarize( #summarize to run summary stats 
  bill_length_mean = mean(bill_length_mm), #new column with mean value of bill length
  bill_length_sd = sd(bill_length_mm), #new column with standard deviation value of bill length
  bill_depth_mean = mean(bill_depth_mm), #new column with mean value of bill depth
  bill_depth_sd = sd(bill_depth_mm) #new column with standard deviation value of bill depth
  )

# group_by() and summarize()
penguins %>% 
  group_by(species) %>% #grouping by one column, species 
  summarize( #summarize to run summary stats 
    bill_length_mean = mean(bill_length_mm), #new column with mean value of bill length
    bill_length_sd = sd(bill_length_mm), #new column with standard deviation value of bill length
    bill_depth_mean = mean(bill_depth_mm), #new column with mean value of bill depth
    bill_depth_sd = sd(bill_depth_mm) #new column with standard deviation value of bill depth
  )

# group_by() and summarize()
penguins %>% 
  group_by(species, year) %>% #grouping by two columns, species and year 
  summarize( #summarize to run summary stats 
    bill_length_mean = mean(bill_length_mm), #new column with mean value of bill length
    bill_length_sd = sd(bill_length_mm), #new column with standard deviation value of bill length
    bill_depth_mean = mean(bill_depth_mm), #new column with mean value of bill depth
    bill_depth_sd = sd(bill_depth_mm) #new column with standard deviation value of bill depth
  )

