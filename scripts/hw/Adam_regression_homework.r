#Regression homework

library(tidyverse)
library(dplyr)
library(ggplot2)

##### Part 1 ####
lagos = read.csv("https://raw.githubusercontent.com/saraherm/Applied_Biostatistics/main/lagos.csv")

qqnorm(lagos$tn) # not straight 
shapiro.test(lagos$tn) # super low p value: p-value < 2.2e-16
qqnorm(lagos$secchi) # not straight 
shapiro.test(lagos$secchi) # super low p value: p-value < 2.2e-16 
#this data is not normal, time to transform 



#log transform these jawns
lagos_new = mutate(lagos, log_tn = log(tn))
lagos_clean = mutate(lagos_new, log_secchi = log(secchi))

qqnorm(lagos_new$log_tn) # better now


#### C basic linear regression 
model1 = lm(log_secchi ~ log_tn, data = lagos_clean)
summary(model1)
#residuals look good
#p = < 2.2e-16 ... that is pretty low 
#r2 = 0.5133

#let's check model assumptions with a histogram 
hist(residuals(model1))

#### D multiple regression with cat variables 
unique(lagos_clean$SurLand)
model2 = lm(log_secchi ~ log_tn + SurLand, data = lagos_clean)
summary(model2)

#residuals look good 
hist(residuals(model2))

ggplot(lagos_clean, aes(x=log_tn, y=log_secchi, color = SurLand, shape = SurLand))+
  geom_point()+
  stat_smooth(method = "lm", se = FALSE, fullrange = TRUE)
#all of these have a negative slope, so with an increase in N and decrease in disc depth, it is predicted the water quality is lower 

##### E multiple regression with measurement variables 
install.packages("psych")
library(psych)

#building my models to run for the stepwise in both directions 
model.null = lm(log_tn ~ 1, data=lagos_clean)

model_full = lm(log_tn ~ lat + lon + res.time + log_secchi + tp + state + SurLand, data = lagos_clean)

step(model.null,
     scope = list(upper=model_full),
     direction = "both",
     data= lagos_clean)


#this model had the lowest AIC value 
model.final = lm(log_tn ~ log_secchi +state + SurLand + tp +res.time +lat, data = lagos_clean)

summary(model.final)
hist(residuals(model.final))
plot(fitted(model.final),
     residuals(model.final))
#these data look great visually 



##### Part 2 #####
#BEANS

beans = read.csv("https://raw.githubusercontent.com/saraherm/Applied_Biostatistics/main/sbr-brazil-2006.csv")

#lettuce see what is going on 
qqnorm(beans$severity)
qqnorm(beans$TMAX30 + beans$severity + beans$TMIN30)

#building my models to run for the stepwise in both directions 
model.null = lm(severity ~ 1, data=beans)
#null model is to debunk normal model? 
model_full = lm(severity ~  RAIN30 + RD30_0 + RD30_1 + RD30_5 + TMAX30 + TMIN30 + TMED30 + DTMIN15 + DTMAX32, data = beans)

step(model.null,
     scope = list(upper=model_full),
     direction = "both",
     data= beans)


model_final = lm(formula = severity ~ RAIN30 + RD30_0, data = beans)
#these variables had the lowest AIC

summary(model_final)
hist(residuals(model_final)) #dist here is werid, but bell curved..... 
plot(fitted(model_final),
     residuals(model_final))

#LETTUCE BUILD A GGPLOT
library(ggplot2)
library(ggpmisc) 

# ggplot

ggplot(beans, aes(x=severity, y=RAIN30, color = severity))+
  geom_point()+
  stat_poly_eq()+ # r2 value from ggpmisc
  stat_smooth(method = "lm", se = FALSE, fullrange =TRUE, color = "black")+
  labs(x = "Disease Severity", #getting some labels on there
       y="30 days of rain (cm)",
       title = paste("Rain accumulation x Disease severity"),
       subtitle = paste("y-axis label refers to total rain accumulation 30 days following disease detection"))+
  theme(axis.text = element_text(size=10), #changing the font sizes of axis
        axis.title.x = element_text(size=11),#changing the font sizes of axis titles
        axis.title.y = element_text(size=11),
        axis.line.x = element_line(color = 'black', size=0.75, linetype='solid'),#adding the x and y axis lines 
        axis.line.y = element_line(color = 'black', size=0.75, linetype='solid'))
