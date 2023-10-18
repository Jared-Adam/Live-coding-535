#Jared Adam 
##### ggplot homework ####
library(ggplot2)
data("mtcars")
View(mtcars)
str(mtcars)


##### ggplot ####
#my line is not as smooth? maybe my computer 
ggplot(data=mtcars, aes(x=wt, y=mpg))+
  geom_point(color="blue", shape=15) + #changing data to blue squares with shape=15
  stat_smooth(method=lm, se=TRUE, color="black")+ #changing line to black 
  labs(x = "Weight (1000 lbs)",#axis titles 
  y = "Miles/(US) gallon")+
  theme(axis.text = element_text(size=8), #changing the font sizes of axis
        axis.title.x = element_text(size=10),#changing the font sizes of axis titles
        axis.title.y = element_text(size=10))

##### box plot #####
# am = Transmission (0=auto,, 1= manual) : binary
#am needs to be a factor for the two levels: 0 and 1; auto and manual 
mtcars$am = as.factor(mtcars$am)
is.factor = mtcars$am
levels(mtcars$am)
am_labels = factor(mtcars$am, labels=c("Automatic", "Manual"))
print(am_labels)
print(mtcars$am)
#am_labels = two level cat information

#this graph shows manual vs automatic by forward gear number 
#x = continuous 
#y = discrete 
ggplot(data=mtcars, aes(x=disp, y=am_labels, fill=factor(gear)))+
  geom_boxplot(fatten=2.5)+ #making the median line wider (fatter) because the one in the picture looks bigger
  theme( 
  panel.grid.major = element_line(size = 0.5, linetype = "solid", color = "grey"),#major lines = grey 
  panel.grid.minor = element_line(size = 0.5, linetype = "solid", color = "grey"),#minor lines = grey 
  panel.background = element_rect(fill = "white", color = "black", size = 0.5, linetype = 'solid')) + #backgroun =white, border = black 
  scale_x_continuous(name="Displacement (cu.in.)",limits = c(50,max(mtcars$disp)))+ #axis names and legend label
  scale_y_discrete(name="Transmission")+
  labs(fill="Number of forward gears")
?difftime 
                
##### histogram #####
# histogram of Gross Horsepower #

## FIRST ## 
#otherwise ggplot will be cOnFuSeD
#vs as factor
mtcars$vs = as.factor(mtcars$vs)
levels(mtcars$vs)
am_labels = factor(mtcars$am, labels=c("V-Shaped", "Round"))

#bins() = number of 'bins' aka bars on the histogram 
#binwidth() changes the width of the bins 
#

ggplot(mtcars, aes(x =hp, fill = vs))+
  geom_histogram(col=('black'), stat = "bin", binwidth= 25, position = "identity", alpha=.5)+ #stat removes default, I set to bin to change sclae? idk 
  scale_x_continuous( breaks=seq(0,300,by=100))+ #changing the x axis scale 
  theme( # remove the grid lines
    legend.position = "none",#booting the legend, buhhhh-bye 
    panel.background = element_blank(),
    axis.text.x=element_text(color="red"),#changing text x axis colors
    axis.text.y=element_text(color="blue"),#changing text y axis colors
  axis.line.x = element_line(color = 'black', size=0.75, linetype='solid'),#adding the x and y axis lines 
  axis.line.y = element_line(color = 'black', size=0.75, linetype='solid'))

  