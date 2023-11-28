
library(vegan)
library(ggplot2)


#Now, grab data from my github - the file is called "Insect_metadata2". 
 #I have a few slides to show what these data represent in terms of a real experiment. 

data<- read.csv("https://raw.githubusercontent.com/saraherm/Applied_Biostatistics/main/Insect_metadata2.csv")


#Let's look at the dataframe and make sure things look good. You can see we have 
 # multiple continuous data columns that are our dependent variables. 

#View(data)
data$Treatment <- as.factor(data$Treatment) ##makes trial categorical


# create a species data matrix, so you can tell the model how to calculate the community composition.

species <- data [,3:29]
library(vegan)
#calculate the distance
dist <- vegdist(species, "bray")
#View(species)



permanova <- adonis2(dist ~ Treatment, permutations=999, method='bray', data=data)
permanova

beta_disp <- betadisper(d = dist, group = data$Treatment, type = 'centroid')
boxplot(beta_disp)
plot(beta_disp)

#if interaction, use *
#default permutation number 999 <- this is what I have seen in all example code I have found
#binary = T if presence absence. can do this with qualitative or quantitative data.
#method - can use other ones, like euclidean distance etc, check package for other ones



Treatment <- factor(data[,2]) #treatment column into an object
Treatment

ord<- metaMDS(species, k =2) #Function metaMDS performs Nonmetric Multidimensional Scaling (NMDS), and tries to find a stable solution using several random starts. In addition, it standardizes the scaling in the result, so that the configurations are easier to interpret, and adds species scores to the site ordination. 
ord$stress
stressplot(ord)



scrs<- scores(ord, display = 'sites') #Function to access either species or site scores for specified axes in some ordination methods

scrs <- cbind(as.data.frame(scrs), Treatment = data$Treatment) #This turns the scores into a dataframe with treatment included 

cent <- aggregate(cbind(NMDS1, NMDS2) ~ Treatment, data = scrs, FUN = mean) #this provides the values for the centroid

segs <- merge(scrs, setNames(cent, c('Treatment','oNMDS1','oNMDS2')),
              by = 'Treatment', sort = FALSE) #this creates a matrix with the NMDS numbers


ggplot(scrs, aes(x = NMDS1, y = NMDS2, colour = Treatment)) +
  geom_segment(data = segs,
               mapping = aes(xend = oNMDS1, yend = oNMDS2)) + # spiders
  geom_point(data = cent, size = 8) +                         # centroids
  geom_point() +                                              # sample scores
  coord_fixed()
