install.packages("tidyverse")
install.packages("palmerpenguins")

library(tidyverse)
library(palmerpenguins)

penguins_raw <- penguins_raw

as_tibble(penguins_raw)

# na check 
which(is.na(penguins_raw))

penguins_na <- na.omit(penguins_raw)

which(is.na(penguins_na))

# column names 

colnames(penguins_na)

# ctrl shift m = %>% 

df_select <- penguins_na %>%
  select(Species,'Culmen Length (mm)', 'Date Egg', 'Individual ID')

as_tibble(df_select) #viewing the new df

df_deselect <- df_select %>% 
  select(-'Individual ID')
df_deselect

penguins_almost_clean <- df_deselect %>% 
  rename(species = Species,
         bill_length_mm = "Culmen Length (mm)", 
         year = "Date Egg")

unique(penguins_almost_clean$species)

penguins_case_when <- penguins_almost_clean %>% 
  mutate(species = case_when(species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
                             species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo",
                             species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap", 
                             .default = as.character(species)))


?mean

year_function <- function(x) (as.factor(format(x, "%Y")))

clean_penguins <- penguins_case_when %>% 
  mutate_at(c("year"), year_function)

#clean_penguins$year <- as.factor(clean_penguins$year)


clean_penguins %>% 
  arrange(bill_length_mm)

?arrange
clean_penguins %>% 
  arrange(year, desc(bill_length_mm))
# (-bill_length_mm)

clean_penguins %>% 
  filter(year == 2007)

clean_penguins %>% 
  filter(year == 2007,
         bill_length_mm > mean(bill_length_mm))
clean_penguins %>% 
  filter(year == 2007, 
         bill_length_mm > mean(bill_length_mm), 
         species == "Chinstrap" | species == "Gentoo")

#species != "Adelie"

clean_penguins %>% 
  mutate(
    log_bill_length = log(bill_length_mm)) %>% 
  select(species, year, log_bill_length)


clean_penguins %>%
  group_by(species, year) %>% 
  summarize(
    bill_length_mean = mean(bill_length_mm), 
    bill_length_sd = sd(bill_length_mm)
  )
