## Statistical Projects


**[Simulation Study](http://htmlpreview.github.io/?https://github.com/ranranrunforit/Statistical-Projects/blob/main/Simulation%20Project/sim-proj.html)** 

- Conduct a simulation study to illustrate the factors influencing statistical power in linear regression.
- Investigate the impact of sample size, signal strength, and noise level on statistical power.
- Visualize and explain the effects of these factors on statistical power.
- Examine the significance of regression test and its implications.
- Evaluate the effectiveness of using Test RMSE for selecting the "best" model.

**Keywords**: statistical power analysis, linear regression  
**Packages**: R scales

**[Predict the housing prices in Ames](http://htmlpreview.github.io/?https://github.com/ranranrunforit/Statistical-Projects/blob/main/Housing%20Prediction/Housing-Data-Report.html)**
- Develop linear regression and tree-based models for housing price prediction.
- Compare the effectiveness of L1-norm penalty (Lasso), L2-norm penalty (Ridge), and Elastic Net regularization methods for linear regression.
- Apply data pre-processing techniques such as one-hot encoding and Winsorization.

**Keywords**: regularization, Elastic Net, Gradient Boosting Machine (GBM)  
**Packages**: R glmnet, xgboost, caret, randomForest

**[Walmart Store Sales Forecasting](http://htmlpreview.github.io/?https://github.com/ranranrunforit/Statistical-Projects/blob/main/Sales%20Forecasting/Sales%20Forecasting%20Report.html)**
- Utilize singular value decomposition (SVD) for feature engineering.
- Train a linear regression model using historical data for sales prediction.
- Implement post-prediction adjustment to mitigate the impact of holidays on sales predictions.

**Keywords**: sales forecasting, SVD  
**Packages**: R tidyverse, lubridate, glmnet, readr, dplyr

**[Movie Review Sentiment Analysis](http://htmlpreview.github.io/?https://github.com/ranranrunforit/Statistical-Projects/blob/main/Sentiment%20Analysis/Sentiment%20Analysis.html)**
- Construct vocabulary using "bag-of-words" and "n-gram" strategies.
- Build document-term matrix (DTM) and employ logistic regression with Lasso regularization for vocabulary trimming.
- Apply two-sample t-test to rank words based on t-statistics and categorize them into positive and negative lists.
- Process movie review text for text embedding into matrix.
- Train cross-validation logistic regression model with Ridge regularization for sentiment prediction.

**Keywords**: NLP, word embedding, sentiment analysis  
**Packages**: R text2vec, glmnet, slam, pROC

**[Movie Recommender System App](https://ranranrunforit.shinyapps.io/movie_recommender_system_app/)/[Movie Recommender System](http://htmlpreview.github.io/?https://github.com/ranranrunforit/Statistical-Projects/blob/main/Recommender%20System/Recommender%20System.html)**
- System I: Recommendation Based on Genres
  - Develop a function to recommend top highly-rated movies based on average ratings and user-selected genre.
  - Provide 10 movie recommendations based on user's input genre.

- System II: Recommendation Based on IBCF
  - Create cosine similarity matrix among movies and implement IBCF function for movie recommendations based on user input and similarity matrix.
  - Present sample movies for user ratings to inform IBCF recommendations.

**Keywords**: IBCF, cosine similarity  
**Packages**: R shiny, recommenderlab
