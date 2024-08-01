# Class Lab: Artificial Neural Networks in R

## Overview
This project demonstrates the implementation of Artificial Neural Networks (ANN) in R using the Iris dataset. The steps include data preparation, training multiple ANN models with different configurations, and evaluating the models' performance.

## Prerequisites
Make sure you have R and RStudio installed on your machine. You also need the following R packages:
- `neuralnet`
- `keras`
- `tensorflow`
- `tidyverse`

## Installation

Install the required packages by running the following commands in your R console:

```R
install.packages(c("neuralnet", "keras", "tensorflow"), dependencies = TRUE)
install.packages("tidyverse")

library(neuralnet)
library(tidyverse)

data(iris)
iris <- iris %>% mutate_if(is.character, as.factor)
summary(iris)

set.seed(254)
data_rows <- floor(0.80 * nrow(iris))
train_indices <- sample(1:nrow(iris), data_rows)
train_data <- iris[train_indices,]
test_data <- iris[-train_indices,]
model <- neuralnet(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                   data = train_data, hidden = c(4,2), linear.output = FALSE)
plot(model, rep = 'best')
pred <- predict(model, test_data)
model2 <- neuralnet(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                    data = train_data, hidden = c(3,6,7), linear.output = FALSE)
plot(model2, rep = 'best')
pred2 <- predict(model2, test_data)
model3 <- neuralnet(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                    data = train_data, hidden = c(4,5,1,10), linear.output = FALSE)
plot(model3, rep = 'best')
pred3 <- predict(model3, test_data)

model4 <- neuralnet(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                    data = train_data, hidden = c(15, 25, 35, 45, 45, 25, 15, 45, 55, 12, 22, 32, 42, 42, 22, 12, 40), 
                    linear.output = FALSE)
plot(model4, rep = 'best')
pred4 <- predict(model4, test_data)

evaluate_model <- function(pred, test_data) {
  labels <- c("setosa", "versicolor", "virginica")
  prediction_label <- data.frame(pred) %>% 
    mutate(pred = labels[max.col(pred)]) %>% 
    select(pred) %>% 
    unlist()
  confusion_matrix <- table(test_data$Species, prediction_label)
  accuracy <- mean(test_data$Species == prediction_label) * 100
  list(confusion_matrix = confusion_matrix, accuracy = accuracy)
}

evaluation1 <- evaluate_model(pred, test_data)
print("Evaluation of Model 1:")
print(evaluation1$confusion_matrix)
print(paste("Accuracy:", evaluation1$accuracy))

evaluation2 <- evaluate_model(pred2, test_data)
print("Evaluation of Model 2:")
print(evaluation2$confusion_matrix)
print(paste("Accuracy:", evaluation2$accuracy))

evaluation3 <- evaluate_model(pred3, test_data)
print("Evaluation of Model 3:")
print(evaluation3$confusion_matrix)
print(paste("Accuracy:", evaluation3$accuracy))

evaluation4 <- evaluate_model(pred4, test_data)
print("Evaluation of Model 4:")
print(evaluation4$confusion_matrix)
print(paste("Accuracy:", evaluation4$accuracy))
