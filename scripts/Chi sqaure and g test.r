# 10/4/2023: Chi squared and G test 
# frequency distributions  
# both are non parametric 
  # two discrete variables = chi squared or g test 
# assumptions 
  # large sample size (50/60-1000 samples)
  # if sample size is small, we use an 'exact test'
  # assumed independence 

# chi square
  # goodness of fit 
  # one nominal variable with two or more values 
  # H0 : number of obs in each category is equal to that predicted by biological theory 
  # measure how far null data are from observed data

# G test 
  # hypothesis test for categorical data 
  # observed vs expected 


#### bills and birds 
# bill cross on either right or left side 
# are there equal numbers of right and left crossed crossbills?
#frequencies 
expected = c(.5,.5) #expected ratio/ proportion 
observed = c(1752,1895) # right vs left observed 

#p instead of y 
chisq.test(x = observed, p = expected)
#reject the null that they are equal 
#more left than right billed 
#significant 

# while not good practice to do both, let's look at the g-test of this as well
Expected.count <- sum(observed)*expected
g <- 2*sum(observed*log(observed/Expected.count))
g

degrees <- 1

?pchisq
pchisq(g, df = degrees, lower.tail = FALSE)


install.packages("RVAideMemoire")
library(RVAideMemoire)
G.test(x=observed, p=expected)



#aphids
#aptera
#does aphid choice differ depending on natural host? 
aptera_observed = c(33,17)
aptera_expected = c(.5,.5)
chisq.test(x = aptera_observed, p = aptera_expected)
#significant 

# sophia ytube data
ytube = read.csv("https://raw.githubusercontent.com/smucciolo/A.nerii_choice_2022/main/ytubesFINAL.csv")
ch_sum <- sum(ytube$CHOSESYR)
ch_inc <- sum(ytube$CHOSEINC)

# make all syriaca choices negative
negsyr <- ytube
negsyr$CHOSESYR <- negsyr$CHOSESYR*(-1)

#create column for mirror plot

clean_data <- negsyr

# adding all values to the new variable
clean_data$choice_sum <- clean_data$CHOSESYR+clean_data$CHOSEINC
 # 0 = no choice 
# for plotting, we cannot use 0 
library(dplyr)
# get rid of NAs for plot (would report in a manuscript)

clean_data <- 
  filter(clean_data, clean_data$CHOICE != "NA")

clean_data[clean_data == "INC"] <- "A.incarnata"
clean_data[clean_data == "SYR"] <- "A.syriaca"

library(ggplot2)
# mirror plot (review why this is needed/ used)

ggplot(clean_data, aes(y = INITIAL, x = choice_sum, fill = CHOICE)) +
  geom_col(position = "stack") +
  facet_grid(rows = vars(MORPH)) +
  labs(x = "Number of Aphids per Trt Arm",
       y = "Initial host plant")+
  theme(text = element_text(family = "Avenir", size = 12), legend.position = "top",
        legend.title = element_blank(), 
        axis.title = element_text(size = 12), 
        plot.title = element_text(size = 14), axis.text.x = element_blank(),
        axis.ticks.x  = element_blank())+
  scale_fill_manual(values = c("darkolivegreen4", "cyan4"))











#mirror chart from last year 
ytube_2022 = read.csv("https://raw.githubusercontent.com/saraherm/Fear_in_Aphids/master/ytube.csv")
ytube$choice = as.numeric(ytube$choice)
is.numeric(ytube$choice)
library(ggplot2)

ggplot(ytube, aes(y = treatment, x = choice, fill = treatment))+
  geom_col(position = "stack")+
    labs(x="Number of aphids per treatment arm")+
  scale_fill_manual(values = c("darkblue","lightblue"))
#scale fill manual for manual colors 
#fill? 


#g-test with crossbill 


crossbill = read.csv("https://raw.githubusercontent.com/saraherm/Applied_Biostatistics/main/crossbill.csv")

ggplot(crossbill, aes(x=bill_type, y=observed, fill=bill_type))+
  geom_bar(stat = "identity", width = 0.5)
#fill?