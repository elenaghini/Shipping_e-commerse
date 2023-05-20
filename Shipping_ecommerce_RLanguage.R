# Load the required libraries
install.packages("Hmisc")
library(readr)
library(Hmisc)

#library untuk menjawab soal no 3 Random Forest

# Read the dataset
data <- read_csv("shipping_ecommerce.csv")

#mencari kolersi
cor(data$Customer_rating, data$Customer_care_calls, method = "pearson")

cor(data$Customer_rating, data$Discount_offered, method = "pearson")

cor(data$Customer_rating, data$Prior_purchases, method = "pearson")

cor(data$Customer_rating, data$Weight_in_gms, method = "pearson")

cor(data$Customer_rating, data$Class, method = "pearson")

summary(data)

# Linear regression model
lm_model <- lm(Customer_rating ~ Prior_purchases + Discount_offered, data = data)
summary(lm_model)

#CHISQUARE
chi_square <- chisq.test(table(data$Warehouse_block, data$Mode_of_Shipment))
chi_square

chi_square1 <- chisq.test(table(data$Mode_of_Shipment, data$Product_importance))
chi_square1

chi_square2 <- chisq.test(table(data$Gender, data$Product_importance))
chi_square2

chi_square3 <- chisq.test(table(data$Warehouse_block, data$Product_importance))
chi_square3

#anova
anova_model <- aov(Customer_rating ~ Warehouse_block, data = data)
summary(anova_model)

#t-test
male_ratings <- data$Customer_rating[data$Gender == "M"]
female_ratings <- data$Customer_rating[data$Gender == "F"]
t_test <- t.test(male_ratings, female_ratings)
t_test

class1<- data$Customer_rating[data$Class==0]
class2<- data$Customer_rating[data$Class==1]
t_test1<-t.test(class1, class2)
t_test1
