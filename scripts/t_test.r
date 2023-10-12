##### Lecture ####
#t tests
#differnece between two dependent means 
#not cross species 
  #BUT measuring the same individual over time and space
    #juvenile vs adult jumping length of same individual 
      # se gives variance from the mean 
# NOT can a grasshopper jump further than a cricket, 
  # BUT can the young grasshopper jump further than an adult grasshopper (same grasshopper)

# Assumptions 
#mean test is normal and continuous 
  #variances are homogeneous 

#bar graph, box plot, one more
#WHAT if data not normal?
  #shapiro test for stat 
  #bell or histogram for figure 
#can use these tests for change in spaces as well 

#Willcoxon signed-rank test when you'd use the paired t-test
  #two nominal and one measured variable 
    #one of the nominal has two values; before and after 
  #look at median rather than mean in non-normal 
    #bc if normal, mean and median in the middle 
  #compares sample median vs hypothetical median 
    #good sample size is about 30 individuals

##### Demo ####
library(tidyverse)
crabs <- read.csv("https://raw.githubusercontent.com/saraherm/Applied_Biostatistics/main/horseshoe_crab_PairedT.csv")
#measurment v number of crabs 
#nominal 1 = 2011 vs 2012
#nominal 2 = name of beach
#did horseshoe crabs go up ? 
as_tibble(crabs)

# check assumptions
  # normal and homo variance 
# visualize distribution, the analysis will look at diff between years
hist(crabs$diff_btwn_yrs)
# skewed right 
# stat test 
shapiro.test(crabs$diff_btwn_yrs)
# p-value = super small = not normal 


#t test of years with conf level (automatic 95% conf level)
t.test(crabs$X2011, crabs$X2012,
       paired = TRUE, # need this if data are paired 
       conf.level = 0.95)
# reject null: there is a diff between means of 2011 and 2012
# testing the mean 

library(ggplot2)

ggplot(crabs, aes(x = X2011, y = X2012)) +
  geom_point(size = 2, shape = 8) +
  geom_abline()+
  labs(x = "2011", y = "2012")
  


plot(crabs$X2011, crabs$X2012, xlab = "2011", ylab = "2012")
abline(0,1, col="blue")
#abline (slope, intercept)

#bc not normal dist, non-parametric 
  # typically the safer option
?wilcox.test
# this is testing for the median 
#NEED to specify whether paired or not 
# we are doing this because the data are not normal and now must choose an alt test 
wilcox.test(crabs$X2011, crabs$X2012, paired = TRUE)
#sig p-value 


install.packages("ggpubr")
library(ggplot2)
library(ggpubr)


#for paired data, specifically 
ggpaired(crabs, 
         cond1 = "X2011", 
         cond2 = "X2012",
         color = "condition",
         line.color = "gray",
         line.size = 0.5,
         palette = "jco")
# show individual points and around means and how they change from year to year
  # Showing the PAIRS


# another example
install.packages("MASS")
library(MASS)
?immer

#pops of barley acorss two time points
immer <- immer
immer <- as_tibble(immer)

wilcox.test(immer$Y1, immer$Y2, 
            paired = TRUE) # there is a difference in medians 

# plot 
ggplot(immer, aes(x = Y1, y = Y2)) +
  geom_point(size = 2, shape = 20) +
  geom_abline() +
  labs( x = "Year One", y = "Year Two")
         