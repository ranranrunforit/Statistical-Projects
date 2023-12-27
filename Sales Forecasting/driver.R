setwd(dirname(rstudioapi::getSourceEditorContext()$path))

options(digits.secs = 6)

current.folder <- getwd()

sub.folders <- list.dirs(current.folder, recursive = TRUE)[-1]
sub.folders <-
  sub.folders[order(as.numeric(gsub(".*?([0-9]+)", "\\1", sub.folders)))]
script.paths <- file.path(current.folder, "model.R")


# time_fold <- data.frame()
# saveRDS(time_fold, file = "time_fold.rds")

# Define a function to run the script in each sub-folder
run_script <- function(folder) {
  setwd(folder)
  
  print(paste0("working on ", folder))
  
  time <- system.time(source(script.paths))[["elapsed"]]
  
  print("mypredict finished")
  
  time
}

result = do.call(rbind, lapply(sub.folders, run_script))

rownames(result) = basename(sub.folders)

result <- cbind("Train/Test split" = rownames(result), result)
rownames(result) <- NULL
# gsub("\\./", "", list.dirs()[- 1]) not in right order 1, 10, 2

# below works too but need gtools package
# rownames(result) = mixedsort(gsub("\\./", "", list.dirs()[- 1]))
colnames(result) <-
  c("Train/Test split",  "ExecutionTime")

result = transform(result,
                   ExecutionTime = as.numeric(ExecutionTime)
)

saveRDS(result, file = file.path(getwd(), "time_result.rds"))

library(knitr)
kable(result,
      align = 'c',
      digits = 4,
      caption = "Execution Time for the 10 folds.")


####################################################################
myeval = function() {
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  file_path = paste0('test_with_label.csv')
  test = read.csv(file_path)
  num_folds = 10
  wae = rep(0, num_folds)
  
  for (i in 1:num_folds) {
    file_path = paste0('fold_', i, '/mypred.csv')
    test_pred = read.csv(file_path)
    
    new_test <- test_pred %>%
      left_join(test, by = c('Date', 'Store', 'Dept'))
    
    actuals = new_test$Weekly_Sales
    preds = new_test$Weekly_Pred
    weights = if_else(new_test$IsHoliday.x, 5, 1)
    wae[i] = sum(weights * abs(actuals - preds)) / sum(weights)
  }
  return(wae)
}

wae = myeval()

print(wae)
# 2078.726 2589.338 2253.936 2823.098 5156.012 4218.348 2269.904 2143.839 2221.145 2372.425

mean(wae)
# 2812.677

saveRDS(wae, file = file.path(getwd(), "wae_result.rds"))
