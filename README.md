<h1>Data Science and Analysis Application</h1>

<h2>Summary</h2>
An attempt at creating an easy-to-use application for modelling, evaluation and tuning of machine learning models for classification without the need for any coding knowledge.<br><br>
Datasets limited to one-hot encoded files and analysis limited to binary classification of cross-sectional data.<br><br>

Try it at: https://austinlim.shinyapps.io/DataScienceAnalyticsApp/

<h2>Background</h2>
During an internship, I had the privilege of learning to use an automated machine learning platform called DataRobot and I was inspired by its easy-to-use point-and-click user interface yet it was able to build incredibly powerful and sophisticated models for prediction. Hence, I wanted to make an application that could help simplify the process of doing data analysis in the same vein where all users need to do is to upload a dataset, afterwhich all they need to do is choose the model settings they want to build and with the press of a button, the model evaluation is done for the user.

<h2>Tools and Frameworks:</h2>

1. Shiny
2. DT
3. DataExplorer
4. 14 different classification models:<br>
  &emsp;logistic regression<br>
  &emsp;xgboost<br>
  &emsp;adaboost<br>
  &emsp;randomforest<br>
  &emsp;isolationforests<br>
  &emsp;svm<br>
  &emsp;knn<br>
  &emsp;assocation rule mining and more<br>

<h2>Features</h2>
<h3>1.Interactive Data Table</h3>
Users will be able to specify the type of separators and headings and even inspect the data row by row.

![image](https://user-images.githubusercontent.com/88301287/169785680-70bfcf09-ba2e-4ed0-b3b0-955abed8405e.png)

<h3>2.Automated EDA Reporting</h3>
Users will receive an automated report on the basic statistics, missingess, histograms, correlation matrix of their data almost instantly.

![image](https://user-images.githubusercontent.com/88301287/169785833-f95066fb-8bd1-4fba-b35e-121927c6f1e4.png)

<h3>3.Easy-to-use Model Building and Hyperparameter Tuning</h3>
Users will be able to specify the model and target variable they want to build, as well as tweak the model's hyperparameters with the click of a button.

![image](https://user-images.githubusercontent.com/88301287/169785989-fb538ef1-598a-4760-97df-37345d660e95.png)

<h3>4.Model Leaderboards</h3>
Built models will immediately be updated in the model leaderboards and the scores of the models can be sorted by accuracy, auc, f1, tpr, fpr.

![image](https://user-images.githubusercontent.com/88301287/169786049-e2abb2a2-fcb5-43b5-8354-89b8a8552a90.png)

<h3>5.Prediction Making</h3>
Users can upload their own csv files that they want to make predictions for and download the predictions once they are done.

![image](https://user-images.githubusercontent.com/88301287/169786127-a786cc3b-e654-4808-8ad5-ba7017ee0530.png)

<h2>Discussion</h2>
This project has room for improvement such as:

1. This application is built for classification tasks, an extention of this application could be built to handle other types of tasks such as regression and anomaly detection.
2. This application can only take in data that has already been pre-processed, a complementary project could deal with creating the application for pre-processing the data in a point-and-click fashion similar to DataRobot's sister platform Paxata.
3. This application is limited to only cross-sectional analysis. Studies have shown that using cross-sectional methods on time-series may result in inaccurate results and hence an extention could seek to implement time-series models.
4. An extention of this application can look into implementation of other features such as feature selection using Boruta package and resampling methods like SMOTE to deal with class imbalances.
5. Another feature that would be helpful in simplifying the process of data analysis is an analysis of the feature importance of each variable as this would help users understand their model results as opposed to treating it like a blackbox.<br><br>

<footer>There may be other areas for improvement that I may not have considered. I would love to get into a discussion on what they are and how I can implement them.</footer>
