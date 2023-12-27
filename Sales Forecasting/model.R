# Step 0: Load necessary R libraries or Python packages
library(tidyverse)
library(lubridate)
library(glmnet)
library(readr)
library(dplyr)

################################################################################
# Project 2: What We Have Tried (II) #364
# Singular Value Decomposition (SVD)
# svd function
mysvd <- function(train) {
  train_dep <- train %>%
    select(Dept, Store, Date, Weekly_Sales) %>%
    spread(Store, Weekly_Sales, fill = 0) %>% arrange(Dept)
  
  n_comp <- 8
  new_train <-
    do.call(rbind, lapply(unique(train_dep$Dept), function(i) {
      dept <- train_dep %>% filter(Dept == i)
      
      X <- dept[,-c(1:2)]
      store.mean <- colMeans(X)
      X_centered <- sweep(X, 2, store.mean, "-")
      svd <- svd(X_centered)
      
      if (nrow(dept) > n_comp) {
        dept[,-c(1:2)] =
          sweep(svd$u[, 1:n_comp] %*% diag(svd$d[1:n_comp]) %*% t(svd$v[, 1:n_comp]), 2, store.mean, "+")
      }
      data_mod <- as.data.frame(dept)
      data_mod$Dept <- i
      data_mod$Date <- dept$Date
      
      data_mod
    }))
  
  new_train %>% gather(Store, Weekly_Sales, -Dept, -Date) %>% mutate(Store = as.integer(Store)) %>% select(Dept, Store, Date, Weekly_Sales)
}


################################################################################
# Project 2: What We Have Tried (III) #366
# A post-prediction adjustment
# shift function
myshift = function(test_pred) {
  test_pred_dep <- test_pred %>%
    select(Dept, Store, Date, Weekly_Pred) %>%
    spread(Store, Weekly_Pred, fill = 0) %>% arrange(Dept)
  
  shift = 1
  threshold = 1.1
  
  new_test_pred <-
    do.call(rbind, lapply(unique(test_pred_dep$Dept), function(i) {
      dept <- test_pred_dep %>% filter(Dept == i)
      
      idx = week(dept$Date) %in% 48:52
      holiday = dept[idx, 3:47]
      baseline = mean(rowMeans(holiday[c(1, 5),], na.rm = TRUE))
      surge = mean(rowMeans(holiday[2:4,], na.rm = TRUE))
      holiday[is.na(holiday)] = 0
      if (is.finite(surge / baseline) & surge / baseline > threshold) {
        shifted.sales = ((7 - shift) / 7) * holiday
        shifted.sales[2:5,] = shifted.sales[2:5,] + (shift / 7) * holiday[1:4,]
        shifted.sales[1,] = holiday[1,]
        dept[idx, 3:47] = shifted.sales
      }
      data_mod <- as.data.frame(dept)
      data_mod$Dept <- i
      data_mod$Store <- dept$Store
      
      data_mod
    }))
  
  temp = new_test_pred %>% gather(Store, Weekly_Pred,-Date,-Dept) %>% mutate(Store = as.integer(Store))
  
  #newtest <-
  left_join(test_pred, temp,  by = c('Dept', 'Store', 'Date')) %>%
    mutate(Weekly_Pred = ifelse(!is.na(Weekly_Pred.y), Weekly_Pred.y, Weekly_Pred.x)) %>% select(Dept, Store, Date, Weekly_Pred)
}


# file_path = paste0('Proj2_Data/fold_', index, '/train.csv')
# train = read.csv(file_path)

# file_path = paste0('Proj2_Data/fold_', index, '/test.csv')
# test = read.csv(file_path)

train = read.csv('train.csv')
test = read.csv('test.csv')

# Project 2: What We Have Tried (II) #364
# Singular Value Decomposition (SVD)
my_train <- mysvd(train)

# find the unique pairs of (Store, Dept) combo that appeared in both training and test sets
train_pairs <-
  my_train[, 1:2] %>% count(Store, Dept) %>% filter(n != 0)
test_pairs <-
  test[, 1:2] %>% count(Store, Dept) %>% filter(n != 0)
unique_pairs <- intersect(train_pairs[, 1:2], test_pairs[, 1:2])

################################################################################
# Project 2: What We Have Tried (III) #366
# Variable Year

# pick out the needed training samples, convert to dummy coding, then put them into a list
train_split <- unique_pairs %>%
  left_join(my_train, by = c('Store', 'Dept')) %>%
  mutate(Wk = factor(ifelse(
    year(Date) == 2010, week(Date) - 1, week(Date)
  ), levels = 1:52)) %>%
  mutate(Yr = year(Date))
train_split = as_tibble(model.matrix( ~ Weekly_Sales + Store + Dept + Yr + Wk + I(Yr ^ 2), train_split)) %>% group_split(Store, Dept)

# do the same for the test set
test_split <- unique_pairs %>%
  left_join(test, by = c('Store', 'Dept')) %>%
  mutate(Wk = factor(ifelse(
    year(Date) == 2010, week(Date) - 1, week(Date)
  ), levels = 1:52)) %>%
  mutate(Yr = year(Date))
test_split = as_tibble(model.matrix( ~ Store + Dept + Yr + Wk + I(Yr ^ 2), test_split)) %>% mutate(Date = test_split$Date) %>% group_split(Store, Dept)

# pre-allocate a list to store the predictions
test_pred <- vector(mode = "list", length = nrow(unique_pairs))

# perform regression for each split, note we used lm.fit instead of lm
for (i in 1:nrow(unique_pairs)) {
  tmp_train <- train_split[[i]]
  tmp_test <- test_split[[i]]
  
  # Project 2: What We Have Tried (III) #366
  # Variable Year
  mycoef <-
    lm.fit(as.matrix(tmp_train[,-(2:4)]), tmp_train$Weekly_Sales)$coefficients
  mycoef[is.na(mycoef)] <- 0
  tmp_pred <-
    mycoef[1] + as.matrix(tmp_test[, 4:56]) %*% mycoef[-1]
  
  test_pred[[i]] <-
    cbind(tmp_test[, 2:3], Date = tmp_test$Date, Weekly_Pred = tmp_pred)
}

test_pred <- bind_rows(test_pred)

# Project 2: What We Have Tried (III) #366
# A post-prediction adjustment 

# shift for fold 5
if (basename(getwd()) == "fold_5") {
  test_pred = myshift(test_pred)
}

test_pred <- test  %>%
  left_join(test_pred, by = c('Store', 'Dept', 'Date'))

id = which(is.na(test_pred$Weekly_Pred))
test_pred$Weekly_Pred[id] = 0

test_pred$Weekly_Pred = round(test_pred$Weekly_Pred, digits = 2)

# file_path = paste0('Proj2_Data/fold_', index, '/mypred.csv')
# write_csv(test_pred, file_path)
write_csv(test_pred, 'mypred.csv')
