library(dplyr)
library(tibble)
library(tidyr)

surveys <- read.csv("https://raw.githubusercontent.com/saraherm/Applied_Biostatistics/main/combined.csv")
test <- surveys

?tibble
test
as_tibble(test)


?tribble


test %>% .$month
test %>% .[["month"]]

new <- test %>% 
  .$month 
test[1,]
test[c(1,2), c(4)]
test[1:5, 6]


log_test <- test %>% 
  mutate(ht_log = log(hindfoot_length))
as_tibble(log_test)

trib


new <- log(test$hindfoot_length)



tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE)
)

# making a new df 
new_df <- tibble(
  a = 1:30 * 54321,
  b = lubridate::now(),
  c = round(runif(n=30, min = 25, max = 100000),0),
  d = LETTERS[1:30]
)
new_df 

# adding a column name of id if needed 
newer_df <- new_df %>% 
  mutate(ID = LETTERS[1:30])

# for (i in LETTERS[1:20]) {
#   print("this",i)
# }

transformations_df <- new_df %>% 
  mutate(log_a = log(a)) %>% 
  tidyr::separate(b, into = c("date","time"), sep = " ", remove = FALSE) %>% 
  tidyr::separate(time, into = c("hour", "minute", "second"), sep = ":", remove = FALSE) %>% 
  select(d,a,log_a,date,hour,minute)
transformations_df

?arrange

