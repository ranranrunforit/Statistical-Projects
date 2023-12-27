# Predict the Housing Prices in Ames

# Step 0: Load necessary R libraries or Python packages
library(caret)
library(xgboost)
library(randomForest)
library(glmnet)

set.seed(9)

#####################################################################

# Process train data
# Step 1: Preprocess the training data, then fit the two models.
# Note: At this step, you are strictly not allowed to access the test data.
train_data = read.csv("train.csv", stringsAsFactors = FALSE)

# Replace the missing values with zero
train_data[is.na(train_data)] = 0

# Variables to Remove
train_data = subset(
  train_data,
  select = -c(
    Street,
    Utilities,
    Condition_2,
    Roof_Matl,
    Heating,
    Pool_QC,
    Misc_Feature,
    Low_Qual_Fin_SF,
    Pool_Area,
    Longitude,
    Latitude
  )
)

# Winsorization variable
winsor_vars = c(
  "Lot_Frontage",
  "Lot_Area",
  "Mas_Vnr_Area",
  "BsmtFin_SF_2",
  "Bsmt_Unf_SF",
  "Total_Bsmt_SF",
  "Second_Flr_SF",
  'First_Flr_SF',
  "Gr_Liv_Area",
  "Garage_Area",
  "Wood_Deck_SF",
  "Open_Porch_SF",
  "Enclosed_Porch",
  "Three_season_porch",
  "Screen_Porch",
  "Misc_Val"
)

# Winsorization function
winsorize = function(data, fraction = 0.95)
{
  lim = quantile(data, probs = fraction)
  data[data > lim] = lim
  data
}

train_data[winsor_vars] = apply(train_data[winsor_vars], 2, winsorize)

# Process train data
train.x  = subset(train_data, select = -c(PID, Sale_Price)) # train data without "PID" and "Sale_Price"
train.y = log(train_data$Sale_Price)# log transformed "Sale_Price"

# replace missing by zero already did it above
# train.x$Garage_Yr_Blt[is.na(train.x$Garage_Yr_Blt)] = 0

categorical.vars = colnames(train.x)[which(sapply(train.x,
                                                  function(x)
                                                    mode(x) == "character"))]
train.matrix = train.x[, !colnames(train.x) %in% categorical.vars,
                       drop = FALSE]
n.train = nrow(train.matrix)


for (var in categorical.vars) {
  mylevels = sort(unique(train.x[, var]))
  m = length(mylevels)
  m = ifelse(m > 2, m, 1)
  tmp.train = matrix(0, n.train, m)
  col.names = NULL
  for (j in 1:m) {
    tmp.train[train.x[, var] == mylevels[j], j] = 1
    col.names = c(col.names, paste(var, '_', mylevels[j], sep = ''))
  }
  colnames(tmp.train) = col.names
  train.matrix = cbind(train.matrix, tmp.train)
}


#####################################################################

# Process test data
# Step 2: Preprocess test data, then save predictions into two files: mysubmission1.txt and mysubmission2.txt. (The specific format for these files is detailed below.)
# Note: At this step, you are strictly not allowed to access the training data.

test_data <- read.csv("test.csv", stringsAsFactors = FALSE)
PIDs <- test_data[, 1]

# Replace the missing values with zero
test_data[is.na(test_data)] = 0

# Variables to Remove
test_data = subset(
  test_data,
  select = -c(
    Street,
    Utilities,
    Condition_2,
    Roof_Matl,
    Heating,
    Pool_QC,
    Misc_Feature,
    Low_Qual_Fin_SF,
    Pool_Area,
    Longitude,
    Latitude
  )
)

test_data[winsor_vars] = apply(test_data[winsor_vars], 2, winsorize)


test.x = subset(test_data, select = -c(PID)) # Test data without "PID"

test.categorical.vars = colnames(test.x)[which(sapply(test.x,
                                                      function(x)
                                                        mode(x) == "character"))]
test.matrix = test.x[, !colnames(test.x) %in% test.categorical.vars,
                     drop = FALSE]
n.test = nrow(test.matrix)

for (var in test.categorical.vars) {
  mylevels = sort(unique(test.x[, var]))
  m = length(mylevels)
  m = ifelse(m > 2, m, 1)
  tmp.test = matrix(0, n.test, m)
  col.names = NULL
  for (j in 1:m) {
    tmp.test[test.x[, var] == mylevels[j], j] = 1
    col.names = c(col.names, paste(var, '_', mylevels[j], sep = ''))
  }
  colnames(tmp.test) = col.names
  test.matrix = cbind(test.matrix, tmp.test)
}

# Solution 1: Ensure that the column names of test.matrix are identical to those in train.matrix, both in name and order. This might involve dropping columns (representing new levels) and appending columns with zeroes (for levels present in the training set but absent in the test set). Maintaining the same order is crucial since XGBoost uses a numerical matrix as opposed to a data frame.
# Find columns to drop from test.matrix
columns_to_drop_from_test_matrix = setdiff(colnames(test.matrix), colnames(train.matrix))

# Drop columns from test.matrix
test.matrix = test.matrix[, !(names(test.matrix) %in% columns_to_drop_from_test_matrix)]

# Add missing columns in test.matrix and fill with zeroes
missing_cols = setdiff(colnames(train.matrix), colnames(test.matrix))
test.matrix[, missing_cols] = 0

# Sort columns in both data frames
test.matrix = test.matrix[, sort(names(test.matrix))]
train.matrix = train.matrix[, sort(names(train.matrix))]


#####################################################################

# Model 1 XGBoost

# XGBoost nrounds could be 1000, or 5000
set.seed(9)
xgb.model = xgboost(
  data = as.matrix(train.matrix),
  label = as.matrix(train.y),
  max_depth = 6,
  eta = 0.05,
  nrounds = 5000,
  subsample = 0.5,
  verbose = FALSE
)

df = data.frame(PID = PIDs, Sale_Price = round(exp(predict(
  xgb.model, as.matrix(test.matrix)
)), digits = 1))

write.table(
  df,
  "mysubmission1.txt",
  row.names = FALSE,
  quote = FALSE,
  sep = ", "
)


#####################################################################


# Model 2 Elastic net

# Elastic net regression
# with alpha = 0.2 (I also tried alpha = 0.5, which worked too)
mylasso.lambda.seq = exp(seq(-10, 1, length.out = 100))
set.seed(9)
cv.out = cv.glmnet(as.matrix(train.matrix),
                   as.matrix(train.y),
                   alpha = 0.2,
                   lambda = mylasso.lambda.seq)

best.lam = cv.out$lambda.min

Ytest.pred = exp(predict(cv.out, s = best.lam, newx = as.matrix(test.matrix)))

colnames(Ytest.pred) = "Sale_Price"

Elastic_df = data.frame(PID = PIDs, Sale_Price = round(Ytest.pred[, "Sale_Price"], digits = 1))
write.table(
  Elastic_df,
  "mysubmission2.txt",
  row.names = FALSE,
  quote = FALSE,
  sep = ", "
)



