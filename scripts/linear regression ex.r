#linear regression 
#is there an effect relationship between variuablers? 
  #cause and effect 
#goals-3 
#1 two measurment variables are associated with eachother? 
  #summarized with p-value 
  #ex. getting on tredmill and measuring pulse at differnet speeds
    #tabled of pulse (y) and speed (x) 
    #y changes based on x (pulse based on speed)
#1a are two variables associated without looking for cause and effect 
  #ex. amphipod pops eggs and weight of mothers
    #weight and egg count
    #does number of eggs have any relation to dry weight of mother? 
    #looking for inference on the system 
    #multivariate?
#2 strength of variable relationship 
    #r squared value : closer to 1 = stroner relationship 
#3 find the equation of the line to find the line that fits the cloud of points 

#regression vs correlation 
  #correlation: sample both measurement variables randomly from pop
  #regression: choose values of independent variable 
  #ex. interested in diff between foot length and body height 
    #predictive power 
    #measure height and foot length of random people 
      #this is correlation becuase measured values of both variables 
      #r squared tells you strength of that relationship 
  #ex. lizard speed at diff temps 
    #changin temp to see change in speed
    #r squared doesnt tell us much, but we want a p-value here 

#correlation and causation 
  #confounding variables influence the relationship between the two variables we are sampling
    #sig assocation between a and b does not mean a caused variation in b


#hn is that slope of best fit line is equal to 0 
#ind/dep variables: cause = indpendent on the x axis (casuing relationship) dependent on y-axis 
#assumption of normality: largley non-normal data is ok as long as it is linear 
  # homoscedasticity is more important than normality 
#assumption: data must be linear!!!
  # can transform, but linear is important 
  # plot with ggplot to test for linearity 
#assumption: independence 


##### Class example #####
#christmas bird count 
#birders go out and count the birds within a 15 mile diameter area during one day over the winter 
#total number of spcies in one year acorss three states (Delmarva Peninsula)

bird = read.csv("https://raw.githubusercontent.com/saraherm/Applied_Biostatistics/main/linear_regression.csv")

# step one : check for normality of dependent variable 
hist(bird$Species)
#double check with Shapiro test 
shapiro.test(bird$Species)
#p-value = 0.4809 is high (not sig)

#is this linear? 
plot(Latitude ~ Species, data=bird, xlab= "latitude", ylab= "Species")
#linear enough 

#procede with analysis 
#species as a function of latitude 
model = lm(Species ~ Latitude, data = bird)
summary(model)
#output = get model equation 
#residuals: vertical distance of each point from the fitted line 
      #we want median close to 0 
#coefficients: shows estimate for model parameters
    #value of y int ...
#model diagnostics is last: p = 0.06134 and r sqaured = 0.2143
#no sig rln between latitude and species 
#failing to reject the null 
#"marginally sig rln between latitude and the number of species present"

#NOW we check model assumptions 
#hist of the residuals of the model 
hist(residuals(model))
#is this normal? 
#approximately! but can test with shapiro test again 


#plot time with trend line and bigger dots 

plot(Species~Latitude, 
     data=bird, 
     pch=16, 
     xlab= "latitude",
     ylab = "species")
   abline(585.15, -12.04) #intercept and slope from earlier 
   #no plus sign here but NEED to run these lines of code together 
   
   
#ggplot 
library(ggplot2)
ggplot(bird, aes(x=Latitude, y=Species))+
       geom_point()+
         stat_smooth(method = "lm", se=FALSE, fullrange=TRUE)+
         theme_light()

#correlation: 
?cor.test
cor.test(~Species + Latitude, 
         data=bird, 
         method = "pearson",
         conf.level = 0.95)
#p-value = pretty much identical to our lm    