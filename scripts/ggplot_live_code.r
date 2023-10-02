# Jared 10/2/2023

library(palmerpenguins)
library(ggplot2)

ggplot2::theme_set(ggplot2::theme_minimal()) #setting my theme to minimal from the beginning 

#this package adds the 'penguins' data set, but you can name it still

penguins <- penguins


# Bill length vs. depth
# start simple 
ggplot(data = penguins,
       aes(x = bill_length_mm,
           y = bill_depth_mm,
           group = species)) + # group these points by species (as seen in the legend)
  geom_point(aes(color = species, # changing the color based on species
                 shape = species)) # making each species it's own shape 

# making the points bigger
ggplot(data = penguins,
       aes(x = bill_length_mm,
           y = bill_depth_mm,
           group = species)) +
  geom_point(aes(color = species, 
                 shape = species),
             size = 3, # changing the size of the points 
             alpha = 0.8) # changing the transparency of the points (I like the softer look)

# add a line to the point spread with AND without SE and change color
ggplot(data = penguins, 
       aes(x = bill_length_mm, 
           y = bill_depth_mm,
           group = species)) +
  geom_point(aes(color = species, 
                 shape = species),
             size = 3,
             alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, aes(color = species)) + # Change se = FALSE #adding the predicted values line with the standard error spread
  scale_color_manual(values = c("darkorange","purple","cyan4")) # changing the colors of the species

# full figure with labels and legend moving
bill_len_dep <- ggplot(data = penguins,
                       aes(x = bill_length_mm,
                           y = bill_depth_mm,
                           group = species)) +
  geom_point(aes(color = species, 
                 shape = species),
             size = 3,
             alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, aes(color = species)) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(title = "Penguin bill dimensions",
       subtitle = "Bill length and depth for Adelie, Chinstrap and Gentoo Penguins at Palmer Station LTER",
       x = "Bill length (mm)",
       y = "Bill depth (mm)",
       color = "Penguin species",
       shape = "Penguin species") +
  theme(legend.position = c(0.85, 0.15), # changing the legend position 
        plot.title.position = "plot", #changing the position of the titles 
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot")

bill_len_dep


# histogram of flipper length 

# simple hist
ggplot(data = penguins, aes(x = flipper_length_mm)) + #histogram for flipper length
  geom_histogram(aes(fill = species)) # filling the histogram by species (3)

# change the transparency with the alpha value 1 = solid 
ggplot(data = penguins, aes(x = flipper_length_mm)) +
  geom_histogram(aes(fill = species), 
                 alpha = 0.5, # transparency of 0.5
                 position = "identity") # stacks the facets on top of each other 

# colors and labels
flipper_hist <- ggplot(data = penguins, aes(x = flipper_length_mm)) +
  geom_histogram(aes(fill = species), 
                 alpha = 0.5, 
                 position = "identity") +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) + # color change 
  labs(x = "Flipper length (mm)",
       y = "Frequency",
       title = "Penguin flipper lengths") #titles 

flipper_hist


#boxplot 
# flipper length 
ggplot(data = penguins, 
       aes(x = species, 
           y = flipper_length_mm)) + 
  geom_boxplot(aes(color = species)) # color the box plots by species 

ggplot(data = penguins, 
       aes(x = species, 
           y = flipper_length_mm)) +
  geom_boxplot(aes(color = species), 
               width = 0.3, show.legend = FALSE) # width of each 'box' and i do not want a legend

# geom_jitter adds points around the boxes
# this is a shortcut for geom_point(identity = "jitter)
# jitter allows for overplotting and is particulary good with smaller data sets with one discrete position

ggplot(data = penguins, aes(x = species, y = flipper_length_mm)) +
  geom_boxplot(aes(color = species), width = 0.3, show.legend = FALSE) +
  geom_jitter(aes(color = species), alpha = 0.5, show.legend = FALSE, 
              position = position_jitter(width = 0.2, seed = 0)) # seeds allow for reproducibility. I encourage you to google " jitter seed "

# rounding it out with color and labels
flipper_box <- ggplot(data = penguins, aes(x = species, y = flipper_length_mm)) +
  geom_boxplot(aes(color = species), width = 0.3, show.legend = FALSE) +
  geom_jitter(aes(color = species), alpha = 0.5, show.legend = FALSE, 
              position = position_jitter(width = 0.2, seed = 0)) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(x = "Species",
       y = "Flipper length (mm)") # axis labbels 

flipper_box


# Facets flipper length x body mass including sex

# simple to start and coloring by sex
ggplot(penguins, aes(x = flipper_length_mm, # specifying variables 
                     y = body_mass_g)) +
  geom_point(aes(color = sex)) # color these by sex 

# big addition of labels and and colors and all that jazz
ggplot(penguins, aes(x = flipper_length_mm,
                     y = body_mass_g)) +
  geom_point(aes(color = sex)) +
  scale_color_manual(values = c("darkorange","cyan4"), na.translate = FALSE) + # I have na's here, I do not want them
  labs(title = "Penguin flipper and body mass",
       subtitle = "Dimensions for male and female Adelie, Chinstrap and Gentoo Penguins at Palmer Station LTER",
       x = "Flipper length (mm)",
       y = "Body mass (g)",
       color = "Penguin sex") +
  theme(legend.position = "bottom", # moving legend 
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"), # changing font style 
        plot.caption.position = "plot") +
  facet_wrap(~species) # splitting all species into their own respective facets within the same plot 

# adding facets 
facet_species <- ggplot(penguins, aes(x = flipper_length_mm,
                     y = body_mass_g)) +
  geom_point(aes(color = sex)) +
  scale_color_manual(values = c("darkorange","cyan4"), na.translate = FALSE) +
  labs(title = "Penguin flipper and body mass",
       subtitle = "Dimensions for male and female Adelie, Chinstrap and Gentoo Penguins at Palmer Station LTER",
       x = "Flipper length (mm)",
       y = "Body mass (g)",
       color = "Penguin sex") +
  theme(legend.position = "bottom",
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot") +
  facet_wrap(~species)
facet_species
