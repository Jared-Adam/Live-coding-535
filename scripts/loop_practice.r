# Example data
data <- data.frame(x1 = as.numeric(1:5), x2 = as.numeric(7:3), x3 = as.character(c("this", "jawn", "is", "about", "right")))
as_tibble(data)

# will not work without fucking up the character column 
data_f <- data
for(i in c(1,3:ncol(data_f))) {
  
  # # Save original class.
  # class_col_i <- class(data_f[ , i])
  # 
  # Convert column as character.
  data_f[ , i] <- as.factor(as.numeric(data_f[ , i]))
 
  
     # if(i > 2)
     #  break 
}
as_tibble(data_f)

class(data_f$x1) # This should be a numeric
class(data_f$x2) # This should be a factor
class(data_f$x3) # This should be a factor

data_dplyr <- data

#dplyr solution to this 
data_d_as_f <- data_dplyr %>% 
  as_tibble() %>% 
  mutate(across(1:2, as.factor),
         across(3, as.character))













## loop practice: ncol adding 10

loop_test_data <- data.frame(x1 = 1:5,
                            x2 = 6:10, 
                            x3 = 11:15)
loop_1 <- loop_test_data

for(i in 1:ncol(loop_1)) {
  loop_1[,i] <- loop_1[,i] + 10
}
loop_1


# loop over every row 

loop_2 <- loop_test_data

for(i in 1:nrow(loop_2)) {
  loop_2[i,] <- loop_2[i,] - 100
}
loop_2


loop_3 <- loop_test_data
i <- 1 # I want to start iterating at the value 1
while (i <= 2) { # while loop should continue as long as i is smaller or equal to 2
  loop_3[,i] <- loop_3[,i] + 100 # add value 100 to df columns as long as condition is met
  i <- i + 1 # changing condition so the value of i should be increased by 1. i = our column value [,1] 
}
loop_3


## repeat loop 

loop_4 <- loop_test_data

i <- 1
repeat{
  loop_4[,i] <- loop_4[,i] + 100 
  i <- i + 1
  
  if(i>2) { # if >3, all columns change
    break
  }
  
}
loop_4



# subsetting loop in R

data_ss <- data.frame(x1 = 1:8,            # Create example data frame
                   x2 = letters[1:8],
                   x3 = 18:11,
                   group = c("gr1", "gr1", "gr1",
                             "gr2", "gr2",
                             "gr3", "gr3", "gr3"))
data_ss       

my_splits <- split(data_ss, data_ss$group) # split by group. list object creating three dfs 
my_splits

my_splits$gr1

split_names <- c("data_new1", # housing for splits 
                 "data_new2", 
                 "data_new3")
for (i in 1:length(my_splits)) {
  
  assign(split_names[i], my_splits[[i]])
}
data_new1
data_new2
data_new3
