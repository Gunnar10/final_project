# final_project  

I created a fish prediction app that has some EDA on certain fish caught on the Atlantic and Gulf coasts. There is also a modeling and prediction tab that will allow th user to fit a Linear Regression model predicting the fish weight, and a Random Forest model predicting the fish type.  On the Model Fitting tab the user can choose the variables to fit both models and run them to produce some model fit statistics. On the prediction tab the user can specify the values of the variables to use the Linear Model to predict a fish's weight and the Random Forest model to predict the fish type.

Please paste this code into the console of RStudio to install all the necessary packages used in the app:  

install.packages(c("shiny", "shinydashboard", "tidyverse", "caret"))

Please paste this code into the console of RStudio to run the app:  

shiny::runGitHub(repo = "Gunnar10/final_project", subdir = "./finalProjApp")