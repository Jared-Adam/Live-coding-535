#beginning of the assignment 
#reading in the .csv I made with the data from r-markdown 


##### Dolphins, part 1 #####
dolphin = ttest_and_chi_homework

#visualize the data
#data looks good = normal 
hist(dolphin$length_cm)
shapiro.test(dolphin$length_cm)

#question 1
#sig. difference between body length of pop a and b?
#must be two-sample 
t.test(dolphin$length_cm ~ dolphin$population, conf.level = 0.95)
#p-value = 1.282e-07
#this means sig 

#question 2
#sig dif between the proportion of male and female dolphins in pop a and b? 
#of the 18 individuals/ pop., we must define expected and observed frequencies 
#defined by m,f

pop_a_observed = c(10,8)
pop_a_expected = c(.5,.5)

chisq.test(x = pop_a_observed, p = pop_a_expected)

pop_b_observed = c(8,10)
pop_b_expected = c(.5,.5)

chisq.test(x = pop_b_observed, p = pop_b_expected)

#the results for both of these tests yielded the same p-value 

chisq.test(dolphin$sex, dolphin$population)

#this version of the chisq.test, p-value = 0.7389
#but is this information what we want? 

#question 3 
#sig diff between the body length male and female dolphins in pop a?
#need to subset the populations of the dolphins 
dolphin_a = subset(dolphin, population == "a")

t.test(dolphin_a$length_cm ~ dolphin_a$sex, conf.level = 0.95)
#p-value = 0.0002319

#question 4 
#sig diff between the body length male and female dolphins in pop b?
#need to subset the populations of the dolphins 

dolphin_b = subset(dolphin, population == "b")

t.test(dolphin_b$length_cm ~ dolphin_b$sex, conf.level = 0.95)
#p-value = 0.6508
  

##### CPB, part 2 #####
cpb = read.csv("https://raw.githubusercontent.com/saraherm/Applied_Biostatistics/main/CPB_ovipos_ent535.csv")

#check the data 
hist(cpb$no_eggs)
shapiro.test(cpb$no_eggs)
shapiro.test(control$no_eggs)
shapiro.test(risk$no_eggs)
#this is not normally distributed 
#this means to compare the influence of trt on egg_no, the wilcox.test will be used 

?wilcox.test

#need to subset here to create new vectors for my x and y 
control = subset(cpb, treatment == 'cont')
risk = subset(cpb, treatment == 'risk')
wilcox.test(control$no_eggs, risk$no_eggs)
#saying look into these new vectors and compare no_eggs
#these results say that there is a sig difference between treatments and egg_no

#let's visualize!
library(ggplot2)
install.packages("wesanderson")
library(wesanderson)

labels = c("Control", "Risk")  #vector for my x axis names 

ggplot(cpb, aes(y = no_eggs, x = treatment, fill = treatment))+
  stat_boxplot(geom = "errorbar", #adding the perpendicular error bar 
               width = 0.25) + #bar width
  geom_boxplot()+
  labs(x = "Treatment", y = "Number of eggs")+ #axis labels 
  theme(
    legend.position = "none", #legend be gone
    axis.line = element_line(color = "black", linewidth = .7), #I want axis lines only where information is present, no need for a box around the whole figure 
    axis.ticks.length = unit(0.15, "cm"))+ #changing the size of the axis ticks 
    scale_x_discrete(label = labels)+ #adding x labels with a vector I made of two 
  scale_fill_manual(values = wes_palette(2, name = "Royal1")) #gimme that Wes Adny color
  
  
  

       