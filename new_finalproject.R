install.packages("tidyverse")  # For data manipulation and visualization
install.packages("factoextra") # For K-Means visualization
install.packages("corrplot") # For correlation matrix visualization
install.packages("ggplot2")      # General visualizations
install.packages("ggcorrplot")   # Heat maps for correlations
install.packages("Matrix")
install.packages("GGally")
install.packages("glmnet")

library(tidyverse)
library(factoextra)
library(corrplot)
library(ggplot2)
library(ggcorrplot)
library(GGally)
library(glmnet)


getwd()

#change this back
data <- read.csv("/Users/shravanpejavar/Desktop/ECON460_Dataset_Cleaned.csv", header = TRUE, sep = ',')
head(data)
str(data)

# drops all n/a values
data <- data %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) 

head(data)

colnames(data) # shows column names in the dataset

# Compute and view correlation matrix
cor_matrix <- cor(data)
print(cor_matrix)

# Create a heatmap using ggcorrplot

heatmap_plot <- ggcorrplot(cor_matrix,
                           method = "circle",    # Use circle or square tiles
                           type = "lower",       # Show only lower triangle
                           lab = TRUE,           # Add correlation values as text
                           lab_size = 1.5,       # Adjust label size
                           tl.cex = 2,          # Adjust text label size
                           tl.srt = 45,          # Rotate axis text for better readability
                           title = "Correlation Heatmap with Abbreviated Labels",
                           colors = c("blue", "white", "red"))  # Define color gradient

print(heatmap_plot)

# Perform OLS regression THIS NEEDS FIXING
model <- lm(Your.2023.academic.year.average.GPA.in....Ignore.if.you.are.2024.1st.year.student. ~ ., 
            data = data)


summary(model)

features_for_clustering <- c(
  "Your.2023.academic.year.average.GPA.in....Ignore.if.you.are.2024.1st.year.student.",
  "X.Adjusted..Additional.amount.of.studying..in.hrs..per.week",
  "X.Adjusted..How.often.do.you.go.out.partying.socialising.during.the.week.",
  "X.Adjusted..On.a.night.out..how.many.alcoholic.drinks.do.you.consume.",
  "X.Adjusted..Monthly.Allowance.in.2023",
  "X.Adjusted..Were.you.on.scholarship.bursary.in.2023.",
  "X.Adjusted..How.many.classes.do.you.miss.per.week.due.to.alcohol.reasons...i.e..being.hungover.or.too.tired.."
)

clustering_data <- data %>%
  select(all_of(features_for_clustering)) 

clustering_data_scaled <- scale(clustering_data)

# determining optimal number of clusters using elbow method 
fviz_nbclust(clustering_data_scaled, kmeans, method = "wss") +
  labs(title = "Optimal Number of Clusters (Elbow Method)")

# determining optimal number of clusters using silhouette method
fviz_nbclust(clustering_data_scaled, kmeans, method = "silhouette") +
  labs(title = "Optimal Number of Clusters (Silhouette Method)")

# applying k-means clustering
k <- 7 # decided to use k = 7 instead of k = 4 due to insignificance when tested
kmeans_result <- kmeans(clustering_data_scaled, centers = k, nstart = 25)

# view K-Means results
print(kmeans_result)

fviz_cluster(kmeans_result, data = clustering_data_scaled) +
  labs(title = "K-Means Clustering Visualization") # visualizing clusters

# adding cluster labels to the original dataset
data$Cluster <- as.factor(kmeans_result$cluster)

# checking for significance
anova_result <- aov(
  Your.2023.academic.year.average.GPA.in....Ignore.if.you.are.2024.1st.year.student. ~ as.factor(Cluster), 
  data = data
)
summary(anova_result) # seeing if clusters are significant

colnames(data) # checking that cluster variable are added to the dataset



# now splitting the data into train and test sets

set.seed(123) # setting a seed for reproducibility 
n <- nrow(data) # determine number of rows in the dataset
train_indices <- sample(1:n, size = 0.8 * n) # generate random sample of indices for split

# splitting the data
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# checking the sizes
cat("Training data size:", nrow(train_data), "\n")
cat("Test data size:", nrow(test_data), "\n")



# NOW BUILDING RANDOM FOREST MODEL

# Loading necessary libraries

install.packages("randomForest")
install.packages("caret")
library("randomForest")
library("caret")

# Define the formula
model_formula <- Your.2023.academic.year.average.GPA.in....Ignore.if.you.are.2024.1st.year.student. ~ .

colnames(data)
head(data)
str(data)

# Ensure categorical variables are treated as factors in both datasets
categorical_vars <- c("Cluster",
                      "X.Adjusted..What.year.were.you.in.last.year..2023...",
                      "X.Adjusted..Monthly.Allowance.in.2023",
                      "X.Adjusted..How.strong.is.your.relationship.with.your.parent.s.",
                      "Female..Dummy.")  
train_data[categorical_vars] <- lapply(train_data[categorical_vars], factor)
test_data[categorical_vars] <- lapply(test_data[categorical_vars], factor)

# removing irrelevant variables, train
# remove your sex from here
reduced_train_data <- train_data %>%
  select(-c(Your.Sex., X.Adjusted..Your.Accommodation.Status.Last.Year..2023., Arts...Social.Sciences, Economic...Management.Sciences, AgriSciences, Medicine.and.Health.Services, Engineering, Science, Law, Education, X.Adjusted..Are.you.currently.in.a.romantic.relationship., X.Adjusted..Do.your.parents.approve.alcohol.consumption., X.Adjusted..Were.you.on.scholarship.bursary.in.2023.))
# removing irrelevant variables, test
reduced_test_data <- test_data %>%
  select(-c(Your.Sex., X.Adjusted..Your.Accommodation.Status.Last.Year..2023., Arts...Social.Sciences, Economic...Management.Sciences, AgriSciences, Medicine.and.Health.Services, Engineering, Science, Law, Education, X.Adjusted..Are.you.currently.in.a.romantic.relationship., X.Adjusted..Do.your.parents.approve.alcohol.consumption., X.Adjusted..Were.you.on.scholarship.bursary.in.2023.))

# training the model
rf_model <- randomForest(
  formula = model_formula,
  data = reduced_train_data,
  ntree = 500,                  # Number of trees
  mtry = floor(sqrt(ncol(reduced_train_data) - 1)),  # Number of variables tried at each split
  importance = TRUE             # Enable variable importance
)

# View the model summary
print(rf_model)

# seeing what is considered important variables
importance(rf_model)
varImpPlot(rf_model, type = 1)


# Align factor levels in the test dataset with the training dataset
factor_cols <- c("X.Adjusted..What.year.were.you.in.last.year..2023...",
                 "X.Adjusted..Monthly.Allowance.in.2023",
                 "X.Adjusted..How.strong.is.your.relationship.with.your.parent.s.",
                 "Female..Dummy.",
                 "Cluster")
for (col in factor_cols) {
  reduced_test_data[[col]] <- factor(reduced_test_data[[col]], levels = levels(reduced_train_data[[col]]))
}


# predicting gpa's on test data
test_predictions_reduced <- predict(rf_model, newdata = reduced_test_data)
# calculating test performance metrics
mse_test <- mean((reduced_test_data$Your.2023.academic.year.average.GPA.in....Ignore.if.you.are.2024.1st.year.student. - test_predictions_reduced)^2)
rmse_test <- sqrt(mse_test)
r_squared_test <- 1 - (sum((reduced_test_data$Your.2023.academic.year.average.GPA.in....Ignore.if.you.are.2024.1st.year.student. - test_predictions_reduced)^2) /
                         sum((reduced_test_data$Your.2023.academic.year.average.GPA.in....Ignore.if.you.are.2024.1st.year.student. - mean(reduced_train_data$Your.2023.academic.year.average.GPA.in....Ignore.if.you.are.2024.1st.year.student.))^2))

cat("Test Set Performance Metrics:\n")
cat("MSE:", mse_test, "\n")
cat("RMSE:", rmse_test, "\n")
cat("R-squared:", r_squared_test, "\n")

# reducing overfitting, cross-validation 
control <- trainControl(method = "cv", number = 5)
rf_cv <- train(
  model_formula,
  data = reduced_train_data,
  method = "rf",
  trControl = control,
  ntree = 500
)
print(rf_cv)

library(ggplot2)
ggplot(data.frame(Actual = reduced_test_data$Your.2023.academic.year.average.GPA.in....Ignore.if.you.are.2024.1st.year.student.,
                  Predicted = test_predictions_reduced), aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted GPA", x = "Actual GPA", y = "Predicted GPA") +
  theme_minimal()

# NOW BUILDING LASSO MODEL

response <- data$"Your.2023.academic.year.average.GPA.in....Ignore.if.you.are.2024.1st.year.student."  # Replace with your dependent variable name
predictors <- data %>% select(-"Your.2023.academic.year.average.GPA.in....Ignore.if.you.are.2024.1st.year.student.")  # Exclude the dependent variable

# Ensure only numeric predictors are included
predictors <- predictors %>% select_if(is.numeric)

# Split into response (Y) and predictors (X)
# data or complete data?
Y <- data[, 3]  # First column is the response
X <- as.matrix(data[, -3])  # Remaining columns are predictors

lasso_categorical_vars <- c("Cluster",
                            "X.Adjusted..What.year.were.you.in.last.year..2023...", 
                            "X.Adjusted..Your.Accommodation.Status.Last.Year..2023.",
                            "X.Adjusted..Monthly.Allowance.in.2023",
                            "X.Adjusted..Were.you.on.scholarship.bursary.in.2023.",
                            "X.Adjusted..Are.you.currently.in.a.romantic.relationship.", 
                            "X.Adjusted..Do.your.parents.approve.alcohol.consumption.",   
                            "X.Adjusted..How.strong.is.your.relationship.with.your.parent.s.",
                            "Female..Dummy.")  
cat_indices <- which(lasso_categorical_vars %in% colnames(X))
if (length(cat_indices) > 0) {
  for (i in cat_indices) {
    X[, i] <- as.factor(X[, i])  # Convert column to factor
    X[, i] <- as.factor(X[, i])    # Convert column to factor
  }
}
print(X)

# X <- apply(X, 2, function(col) ifelse(is.na(col), mean(col, na.rm = TRUE), col)) # remove n/a values from predictor columns

# identifies n/a cells and removes those rows from both Y and X sets
# complete_cases <- !is.na(Y) & complete.cases(X)  # Ensures no NA in Y or X
# Y <- Y[complete_cases]
# X <- X[complete_cases, ]

# Fit Lasso Regression using glmnet
# lasso_model <- glmnet(X, Y, alpha = 1)  # alpha = 1 for Lasso

# Perform cross-validation to find the optimal lambda
set.seed(123)  # For reproducibility
cv_model <- cv.glmnet(X, Y, alpha = 1)

# Optimal lambda
optimal_lambda <- cv_model$lambda.min
cat("Optimal Lambda:", optimal_lambda, "\n")

# Plot the cross-validation results
plot(cv_model)

# Coefficients at optimal lambda
lasso_coefficients <- coef(cv_model, s = "lambda.min")
print(lasso_coefficients)

# Extract significant variables (non-zero coefficients)
significant_vars <- rownames(lasso_coefficients)[lasso_coefficients[, 1] != 0]
significant_vars <- significant_vars[-1]  # Remove the intercept
cat("Significant Variables:", significant_vars, "\n")

# Convert coefficients to a data frame for visualization
coeff_df <- as.data.frame(as.matrix(lasso_coefficients))
coeff_df <- coeff_df %>% mutate(Variable = rownames(.)) %>% filter(s1 != 0)

# Bar plot of significant variables
ggplot(coeff_df, aes(x = reorder(Variable, s1), y = s1)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Significant Variables in Lasso Regression",
       x = "Variable",
       y = "Coefficient") +
  theme_minimal()

# Split into training and testing sets (80-20 split)
set.seed(123)
train_indices <- sample(1:nrow(X), size = 0.8 * nrow(X))
print(colnames(data))

X_train <- X[train_indices, ]
X_test <- X[-train_indices, ]
Y_train <- Y[train_indices]
Y_test <- Y[-train_indices]

# Refit Lasso on training data
lasso_train <- glmnet(X_train, Y_train, alpha = 1)

# Predict on test data using the optimal lambda
Y_pred <- predict(lasso_train, s = optimal_lambda, newx = X_test)

# Evaluate performance
lasso_mse <- mean((Y_test - Y_pred)^2)
lasso_rmse <- sqrt(lasso_mse)
r_squared_lasso <- 1 - (sum((Y_test - Y_pred)^2) /
                         sum((Y_test - mean(Y_train))^2))

cat("Mean Squared Error on Test Data:", mse, "\n")
cat("RMSE:", lasso_rmse, "\n")
cat("R-squared:", r_squared_lasso, "\n")

# Context for MSE
variance_y <- var(Y)
cat("Variance of Y:", variance_y, "\n")

baseline_mse <- mean((Y_test - mean(Y_train))^2)
cat("Baseline MSE (predicting mean):", baseline_mse, "\n")
