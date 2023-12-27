setwd(dirname(rstudioapi::getSourceEditorContext()$path))

options(digits.secs = 6)

current.folder <- getwd()

# below works too but need gtools package
# library(gtools)
# sub.folders <- mixedsort(list.dirs(current.folder, recursive=TRUE)[-1])

sub.folders <- list.dirs(current.folder, recursive = TRUE)[-1]
sub.folders <-
  sub.folders[order(as.numeric(gsub(".*?([0-9]+)", "\\1", sub.folders)))]
script.paths <- file.path(current.folder, "models.R")

# helper function for calculating RMSE
calc_rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

# global variable
# time_fold <<- data.frame()

time_fold <- data.frame()
saveRDS(time_fold, file = "time_fold.rds")

# Define a function to run the script in each sub-folder
run_script <- function(folder) {
  setwd(folder)
  time <- system.time(source(script.paths))[["elapsed"]]
  
  test.y <- read.csv("test_y.csv")
  # print(folder)
  pred_xgb <- read.csv("mysubmission1.txt")
  names(test.y)[2] <- "True_Sale_Price"
  pred_xgb <- merge(pred_xgb, test.y, by = "PID")
  xgb_rmse <-
    calc_rmse(
      actual = log(pred_xgb$Sale_Price),
      predicted = log(pred_xgb$True_Sale_Price)
    )
  
  pred_ela <- read.csv("mysubmission2.txt")
  names(test.y)[2] <- "True_Sale_Price"
  pred_ela <- merge(pred_ela, test.y, by = "PID")
  ela_rmse <-
    calc_rmse(
      actual = log(pred_ela$Sale_Price),
      predicted = log(pred_ela$True_Sale_Price)
    )
  
  cbind(xgb_rmse, ela_rmse, time)
}

# Apply the function to each sub-folder using lapply
result = do.call(rbind, lapply(sub.folders, run_script))

# setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rownames(result) = basename(sub.folders)

result <- cbind("Train/Test split" = rownames(result), result)
rownames(result) <- NULL
# gsub("\\./", "", list.dirs()[- 1]) not in right order 1, 10, 2

# below works too but need gtools package
# rownames(result) = mixedsort(gsub("\\./", "", list.dirs()[- 1]))
colnames(result) <-
  c("Train/Test split", "XGBoost", "ElasticNet", "ExecutionTime")

result = transform(result, XGBoost = as.numeric(XGBoost), 
                   ElasticNet = as.numeric(ElasticNet),
                   ExecutionTime = as.numeric(ExecutionTime)
)

# names(dimnames(result)) <- c("Train/Test split", "")
#getwd()
saveRDS(result, file = file.path(dirname(getwd()), "rmse_result.rds"))

library(knitr)
kable(result,
      align = 'c',
      digits = 4,
      caption = "XGBoost average RMSE, ElasticNet average RMSE, and Execution Time for the 10 folds.")

# getwd() found current at fold10

# could set directory back to where the driver.R at
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# or add a file path to it
time_fold <- readRDS(file.path(dirname(getwd()), "time_fold.rds"))
rownames(time_fold) <- basename(sub.folders)
time_fold <-
  cbind("Train/Test split" = rownames(time_fold), time_fold)
rownames(time_fold) <- NULL
colnames(time_fold) <-
  c(
    "Train/Test split",
    "Pre-processing train data",
    "Pre-processing test data",
    "XGBoost model Training & Prediction",
    "ElasticNet model Training & Prediction"
  )

time_fold[, 2:5] <- sapply(time_fold[, 2:5],as.numeric)

saveRDS(time_fold, file = file.path(dirname(getwd()), "time_fold.rds"))
# names(dimnames(time_fold)) <- c("Train/Test split", "")

kable(time_fold,
      align = 'c',
      digits = 4,
      caption = "Running time for different processes (in seconds)")

