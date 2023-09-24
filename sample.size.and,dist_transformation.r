install.packages("modeldata")
library(modeldata)
ames <- ames

test <- sample(ames$Sale_Price, 10000, replace = TRUE)
plot(density(test))
test2 <- ames
test2$Sale_Price <- log(test2$Sale_Price) 
plot(density(test2$Sale_Price))

?qqnorm
qqnorm(ames$Sale_Price)

qqnorm(test2$Sale_Price)

test_model <- glm(Sale_Price ~ Neighborhood, data = ames)
bc <- boxcox(ames$Sale_Price ~ ames$Neighborhood)


lambda <- bc$x[which.max(bc$y)]

bcmodel <- glm((Sale_Price^lambda-1)/lambda ~ Neighborhood, data = ames)

qqnorm(test_model$residuals)
qqnorm(bcmodel$residuals)


# Lily and monarchs 

monarch <- read.csv("https://raw.githubusercontent.com/saraherm/Applied_Biostatistics/main/2.18.22.csv")
qqnorm(monarch$EGG_TOT)
#optional additional arguments , pch = 1, frame = FALSE
qqline(monarch$EGG_TOT)