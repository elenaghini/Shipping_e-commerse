# Install and load required packages
install.packages("caret")
install.packages("randomForest")
library(caret)
library(randomForest)

# Load the data
data <- read.csv("shipping_ecommerce.csv")

# Preprocess the data
data$Customer_rating <- as.factor(data$Customer_rating)
data$Customer_care_calls <- as.numeric(data$Customer_care_calls)
data$Prior_purchases <- as.numeric(data$Prior_purchases)
data$Discount_offered <- as.numeric(data$Discount_offered)
data$Weight_in_gms <- as.numeric(data$Weight_in_gms)
data$Warehouse_block <- as.factor(data$Warehouse_block)
data$Mode_of_Shipment <- as.factor(data$Mode_of_Shipment)
data$Product_importance <- as.factor(data$Product_importance)
data$Gender <- as.factor(data$Gender)
data$Class <- as.factor(data$Class)

# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data$Customer_rating, p = 0.1, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Train the random forest model
model <- randomForest(Customer_rating ~ Customer_care_calls + Prior_purchases + Discount_offered + Weight_in_gms + Warehouse_block + Mode_of_Shipment + Product_importance + Gender + Class, data = trainData)

# Make predictions on the test data
predictions <- predict(model, newdata = testData)

# Evaluate the model
confusionMatrix(predictions, testData$Customer_rating)
