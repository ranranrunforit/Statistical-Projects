# Load libraries
library(text2vec)
library(glmnet)
library(slam)
library(pROC)
library(dplyr)

set.seed(954)

# Load the vocabulary

myvocab <- scan(file = "myvocab.txt", what = character())

# Read all data(both train and test)

train <- read.table("train.tsv", stringsAsFactors = FALSE,
                    header = TRUE)

train$review <- gsub('<.*?>', ' ', train$review)

test <- read.table("test.tsv", stringsAsFactors = FALSE,
                   header = TRUE)
test$review <- gsub('<.*?>', ' ', test$review)

# Preprocess data(both train and test)

it_train = itoken(train$review,
                  preprocessor = tolower,
                  tokenizer = word_tokenizer)

it_test = itoken(test$review,
                 preprocessor = tolower,
                 tokenizer = word_tokenizer)

vectorizer = vocab_vectorizer(create_vocabulary(myvocab,
                                                ngram = c(1L, 2L)))

# create matrices

dtm_train = create_dtm(it_train, vectorizer)

dtm_test = create_dtm(it_test, vectorizer)


# train ridge regression model
# alpha = 1 is lasso regression (default) and alpha = 0 is ridge regression.

model <- cv.glmnet(
  x = dtm_train,
  y = train$sentiment,
  family = 'binomial',
  alpha = 0,
  type.measure = "auc"
)


# predict on test data

prob_preds <- predict(model,
                      dtm_test,
                      type = 'response',
                      s = model$lambda.1se)


# store predictions in mysubmission.csv file
result <- data.frame(test$id, round(prob_preds, digits = 15))
colnames(result) <- c('id', 'prob')
write.table(
  result,
  "mysubmission.csv",
  row.names = FALSE,
  quote = FALSE,
  sep = ", "
)

# test_y <- read.table('test_y.tsv', stringsAsFactors = FALSE,
# header = TRUE)
# 
# (split_auc = auc(test_y$sentiment, prob_preds))
