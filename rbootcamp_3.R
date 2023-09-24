#r bootcamp day 3 code
# thi sis niceieh
#surveys <- read_csv("https://raw.githubusercontent.com/saraherm/Applied_Biostatistics/master/combined.csv")

surveys <- read.csv("https://raw.githubusercontent.com/saraherm/Applied_Biostatistics/main/combined.csv")

test <- surveys

#Jared
library(tibble)
library(dplyr)

#Extract first few lines of data to check it
surveys
#or
print(surveys, n=50)
#or
head(surveys)

#Jared
#eliminate the need for str and head/ print 
as_tibble(surveys)

#open data set in rstudio data viewer
view(surveys)

#whats the structure of the data?!
str(surveys)

# We can extract specific values by specifying row and column indices in the format: 
# data_frame[row_index, column_index]

# For instance, to extract the first row and column from surveys:
surveys[1, 1]

# First row, sixth column:
surveys[1, 6] 
#NL is species ID



# We can also use shortcuts to select a number of rows or columns at once
# To select all columns, leave the column index blank
# For instance, to select all columns for the first row:
surveys[1, ]

# The same shortcut works for rows --
# To select the first column across all rows:
surveys[, 1]

# An even shorter way to select first column across all rows:
surveys[1] # No comma! 

# To select multiple rows or columns, use vectors!
# To select the first three rows of the 5th and 6th column
surveys[c(1, 2, 3), c(5, 6)] 

# We can use the : operator to create those vectors for us:
surveys[1:3, 5:6] 

# This is equivalent to head_surveys <- head(surveys)
head_surveys <- surveys[1:6, ]
head_surveys

# subsetting with single square brackets ("[]") always returns a data frame.
# If you want a vector, use double square brackets ("[[]]")

# For instance, to get the first column as a vector:
first_col <- surveys[[1]]

# To get the first value in our data frame:
surveys[[1, 1]]

#You can ask what data types are! 
is.factor(surveys$sex)

#see how it doesnt think its a factor? we can convert it.
surveys$sex <- as.factor(surveys$sex)
is.factor(surveys$sex)
#all bettter!

####next is the milkweed example###
monarch <- read.csv("https://raw.githubusercontent.com/saraherm/Applied_Biostatistics/main/2.18.22.csv")
qqnorm(monarch$EGG_TOT)
#optional additional arguments , pch = 1, frame = FALSE
qqline(monarch$EGG_TOT)
#optional arguments , col = "steelblue", lwd = 2

#another option, useful with data that is not as zero inflated
#qqPlot function also avilable from car package, Let’s see how to plot the same.
install.packages(car)
library(car)
qqPlot(monarch$EGG_TOT)

#qqPlot function provides better visualization compared to previous one. If almost 
#all points fall approximately along this straight line, so we can assume 
#normality.

#but, let's check normality in a statistical way, not visually
shapiro.test(monarch$EGG_TOT)
#here, we see a very small p-value. which means these data are not normal 
#because the data is significantly different from a normal distribution

#another formal statistical test to check normaily is Kolmogorov-Smirnov Test
#Use ks.test() function to do this!! (hint – it requires more info than shapiro 
#test, check the help page to read more) 

ks_test <- ks.test(monarch$EGG_TOT, 'dnorm')
?ks.test  #to learn how to use other distributions

#Let’s remove tuberosa from the data set!!
#First – we need to install a package!!!
install.packages('tidyverse')
library(tidyverse)

#We create a new object that contains the data we want to explore and 
#excludes the data we don’t need using filter()

data2 <- filter(monarch, monarch$SPECIES != "TUBEROSA")

#Can we parse these data down further?
#Were monarchs present the entire summer? 
#Can we remove some weeks to gain clarity?
#Let’s make a plot() to see

plot(data2$WEEK, data2$EGG_TOT)

#Let’s test the homogeneity of variance when all weeks are included
fligner.test(data2$EGG_TOT~data2$WEEK)

#why test for homogeneity of variance? a remninder...
#A test for homogeneity of variance is performed to determine whether the 
#variances of two or more groups or populations are equal. The assumption of 
#equal variances is often made in many statistical tests, such as the t-test, 
#ANOVA, and regression analysis. Violation of this assumption can lead to 
#incorrect conclusions, particularly in cases where the sample sizes are unequal.

#If the variances of the groups or populations are significantly different, 
#the standard errors of the estimated means will be biased, which can lead to 
#incorrect conclusions about the statistical significance of group differences. 
#For example, if the variances of the groups are unequal, the t-test may 
#indicate a significant difference between the means of the groups even if there
#is no true difference.

#In order to work with this dataset we have to make sure “WEEK” is treated 
#like the appropriate data type!
  
is.character(data2$WEEK)

monarch$WEEK <- as.character(data2$WEEK)
is.character(data2$WEEK)

install.packages("dplyr")
library(dplyr)
#Remove all weeks <4 and >10
#Could remove one at a time….
data3 <- filter(data2, data2$WEEK != "1")

#OR! keep the ones you want all at once
data4<- filter(data2, data2$WEEK == c("4", "5", "6", "7", "8", "9", "10"))

#Let’s see if excluding the weeks where monarchs were not yet active helps with
#the homogeneity of variance
fligner.test(data4$EGG_TOT~data4$WEEK)

#let's take a peek at a plot
boxplot(EGG_TOT ~ SPECIES, data = data4, col = "purple")

#tada!
