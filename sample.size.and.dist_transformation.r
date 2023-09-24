# starting with the ames data set, not life science, but makes for good data vis

install.packages("modeldata")
library(modeldata)

#loading in by name 
ames <- ames
#base R plot to show data dist 
plot(density(ames$Sale_Price))

# this data set is big with n=2930 samples
# what might happen to this distribution if we did not have enough samples? 
test_low <- sample(ames$Sale_Price, 10, replace = TRUE)
plot(density(test_low))

test_mid <- sample(ames$Sale_Price, 100, replace = TRUE)
plot(density(test_mid))

test_normal <- ames$Sale_Price
plot(density(test_normal))

test_high <- sample(ames$Sale_Price, 10000, replace = TRUE)
plot(density(test_high))


test_log <- ames
test_log$Sale_Price <- log(test_log$Sale_Price) 
plot(density(test_log$Sale_Price))

?qqnorm
qqnorm(ames$Sale_Price)
qqnorm(test_log$Sale_Price)


# boxcox test to exclude in the live coding 

# test_model <- glm(Sale_Price ~ Neighborhood, data = ames)
# bc <- boxcox(ames$Sale_Price ~ ames$Neighborhood)

# lambda <- bc$x[which.max(bc$y)]
# 
# bcmodel <- glm((Sale_Price^lambda-1)/lambda ~ Neighborhood, data = ames)
# 
# qqnorm(test_model$residuals)
# qqnorm(bcmodel$residuals)


# Lily and monarchs: slides in r boot camp 3 

monarch <- read.csv("https://raw.githubusercontent.com/saraherm/Applied_Biostatistics/main/2.18.22.csv")
library(tidyverse)
#view the data
as_tibble(monarch)

#column names?
colnames(monarch)

data <- monarch

names(data) <- tolower(names(data))
as_tibble(data)


#how many species? 
data %>% 
  count(species)

#clean this up a bit with select
data_select <- data %>% 
  select(species, location, egg_tot, week)
as_tibble(data_select)

qqnorm(data_select$egg_tot)
#optional additional arguments , pch = 1, frame = FALSE
?qqline
qqline(data_select$egg_tot)
hist(data_select$egg_tot)

?shapiro.test
#normality test 
shapiro.test(data_select$egg_tot)

#ks.test
?ks.test
ks_data <- ks.test(data_select$egg_tot, 'dnorm')
ks_data


#lettuce get rid of tuberosa 
data2 <- data_select %>% 
  filter(species != "TUBEROSA") 

#did it work? 
data2 %>% 
  count(species)
as_tibble(data2)

#hist time: notice the y axis scale 
hist(data2$egg_tot)

#quick visual
plot(data2$week, data2$egg_tot)


# DO NOT change anything to a function
# class change with function 
# as_factor <- function(x) {as.factor(format((x)))}
# 
# data_factor <- data2 %>% 
#   mutate_at(c("species", "location", "week"),as_factor) # can change more than one 
# as_tibble(data_factor) # factor here put "" around week
# 
# #quick visual 
# plot(data_factor$week, data_factor$egg_tot) #why did this happen? 
# 
# ##
# #ALTERNATIVE class change
# # with no fxn, dplyr option 
# dplyr_monarch <- data_select
# dplyr_monarch <- dplyr_monarch %>% 
#   as_tibble() %>% 
#   mutate(across(1:2, as.factor), 
#          across(4, as.factor))
# dplyr_monarch
# plot(dplyr_monarch$week, dplyr_monarch$egg_tot) # and these values are different than the above... 
# ##


#Let’s test the homogeneity of variance when all weeks are included
# p-value </= 0.05 means there is a difference in populations 
# H0 : all populations and variances are equal 
# Ha : at least two populations differ 
# formula : values ~ groups
# Two options 
# interaction first 
fligner.test(egg_tot ~ interaction(week, species), data2)
#now, just looking at egg ~ sp (only need to show one of these)
fligner.test(egg_tot ~ species, data2)
fligner.test(data2$egg_tot~data2$week)
# Reminder
#A test for homogeneity of variance is performed to determine whether the 
#variances of two or more groups or populations are equal. The assumption of 
#equal variances is often made in many statistical tests, such as the t-test, 
#ANOVA, and regression analysis. Violation of this assumption can lead to 
#incorrect conclusions, particularly in cases where the sample sizes are unequal.

# this information is in the ppt 

# POST SLIDES 
#Remove all weeks <4 and >10
unique(data2$week)
data3 <- data2 %>% 
   filter(between(week, 4, 10))
unique(data3$week)

#Let’s see if excluding the weeks where monarchs were not yet active helps with
#the homogeneity of variance
fligner.test(data3$egg_tot~data3$week)
#NOPE

#let's take a peek at a plot
boxplot(egg_tot ~ species, data = data3, col = "purple")
