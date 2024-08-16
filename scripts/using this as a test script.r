library(tidyverse)
df<-read_csv("https://reneshbedre.github.io/assets/posts/ancova/manova_data.csv")

# summary stats 
df %>% 
  group_by(plant_var) %>% 
  summarise(n=n(),
            mean = mean(height),
            sd = sd(height))
df %>% 
  group_by(plant_var) %>% 
  summarise(n = n(),
            mean = mean(canopy_vol),
            sd = sd(canopy_vol))

library(gridExtra)
p1 <- ggplot(df, aes(x = plant_var, y = height, fill - plant_var))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = 0.2)+
  theme(legend.position = "top")

p2 <- ggplot(df, aes(x = plant_var, y = canopy_vol, fill - plant_var))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = 0.2)+
  theme(legend.position = "top")

grid.arrange(p1, p2, ncol =2)

dep_vars <- cbind(df$height, df$canopy_vol)
fit <- manova(dep_vars ~ plant_var, data = df)
summary(fit)

library(effectsize)
eta_squared(fit)

library(MASS)
post_hoc <- lda(df$plant_var ~ dep_vars, CV=F)
post_hoc

plot_lda <- data.frame(df[,"plant_var"], lda = predict(post_hoc)$x)
ggplot(plot_lda)+ 
  geom_point(aes(x = lda.LD1, y = lda.LD2, colour = plant_var), size = 4)







