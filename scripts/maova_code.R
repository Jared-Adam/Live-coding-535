# data 
library(tidyverse)
df<-read_csv("https://reneshbedre.github.io/assets/posts/ancova/manova_data.csv")

# Do i need to do this? 
# Next, we can calculate the summary statistics for dependent variable height and dependent variable canopy volume:
df %>% group_by(plant_var) %>%  summarise(n = n(), mean = mean(height), sd = sd(height))

df %>% group_by(plant_var) %>%  summarise(n = n(), mean = mean(canopy_vol), sd = sd(canopy_vol))
# Here we can see the n, mean, and standard deviation of each variable.

# Now, we can use a new package to help us here in making a plot of both dependent variables side by side 
# so we can visualize how the plant varieties may influence our dependent variables.
library(gridExtra)
p1 <- ggplot(df, aes(x = plant_var, y = height, fill = plant_var)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + theme(legend.position="top")
p2 <- ggplot(df, aes(x = plant_var, y = canopy_vol, fill = plant_var)) + geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) + theme(legend.position="top")
grid.arrange(p1, p2, ncol=2)
# It looks like, from this plot, that both dependent variables seem to be affected 
# by the independent variable in a similar manner, though on different scales.

# In order to assess this statistically, we can now run the MANOVA.
dep_vars <- cbind(df$height, df$canopy_vol)
fit <- manova(dep_vars ~ plant_var, data = df)
summary(fit)

# Next, we can calculate the effect size
library(effectsize)
effectsize::eta_squared(fit)
# The Pillai’s Trace test statistics is statistically significant [Pillai’s Trace = 1.03, F(6, 72) = 12.90, p < 0.001] and 
# indicates that plant varieties has a statistically significant association with both combined plant height and canopy volume.
# 
# The measure of effect size (Partial Eta Squared; ηp2) is 0.52 and suggests that there is a large effect of
# plant varieties on both plant height and canopy volume.


# The MANOVA results suggest that there are statistically significant (p < 0.001) differences between plant varieties, 
# but it does not tell which groups are different from each other. To know which groups are significantly different, the post-hoc test needs to carry out.


# To test the between-group differences, the univariate ANOVA can be done on each dependent variable, 
# but this will be not appropriate and lose information that can be obtained from multiple variables together.
# 
# Here we will perform the linear discriminant analysis (LDA) to see the differences between each group. 
# LDA will discriminate the groups using information from both the dependent variables.
library(MASS)
post_hoc <- lda(df$plant_var ~ dep_vars, CV=F)
post_hoc


plot_lda <- data.frame(df[, "plant_var"], lda = predict(post_hoc)$x)
ggplot(plot_lda) + geom_point(aes(x = lda.LD1, y = lda.LD2, colour = plant_var), size = 4)
#The LDA scatter plot discriminates against multiple plant varieties based on the two dependent variables. 
# The C and D plant variety has a significant difference (well separated) as compared to A and B. 
# A and B plant varieties are more similar to each other. Overall, LDA discriminated between multiple plant varieties.







# Assumptions 
####
###
##
#
library(rstatix)
df %>% group_by(plant_var) %>%  shapiro_test(height, canopy_vol)

library(mvnormalTest)
mardia(df[, c("height", "canopy_vol")])$mv.test

library(heplots)
boxM(Y = df[, c("height", "canopy_vol")], group = df$plant_var)

library(rstatix)
# get distance
mahalanobis_distance(data = df[, c("height", "canopy_vol")])$is.outlier

library(gridExtra)
p1 <- df  %>% group_by(plant_var) %>% filter(plant_var == "A") %>% ggplot(aes(x = height, y = canopy_vol)) + geom_point() + ggtitle("Variety: A")
p2 <- df  %>% group_by(plant_var) %>% filter(plant_var == "B") %>% ggplot(aes(x = height, y = canopy_vol)) + geom_point() + ggtitle("Variety: B") 
p3 <- df  %>% group_by(plant_var) %>% filter(plant_var == "C") %>% ggplot(aes(x = height, y = canopy_vol)) + geom_point() + ggtitle("Variety: C") 
p4 <- df  %>% group_by(plant_var) %>% filter(plant_var == "D") %>% ggplot(aes(x = height, y = canopy_vol)) + geom_point() + ggtitle("Variety: D") 
grid.arrange(p1, p2, p3, p4, ncol=2)


cor.test(x = df$height, y = df$canopy_vol, method = "pearson")$estimate
