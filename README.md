**# Future-prediction**
This is the code repository for the paper  "Predicting Future MRI-based Brain Atrophy Based on Longitudinal Data"

This repository contains scripts and models for analyzing brain atrophy/enlargement using Elastic Net Linear Regression (ENLR).

**Workflow**
1.	Run Models: Start by defining the outcome using annualized change function then  running the models using ENLR function to generate predictions.
   
2.	Correlation Analysis:
   
    o	To visualize the correlation between the true values and predicted values, run the scatter plot file.

4.	Model Comparison:
   
    o	To compare each model using the Cocor package, run the Pvalue file.
    o	To visualize the model differences, execute the Bar plot file.

6.	Validity Check:
   
    o	Check the validity of the models by running the External Evaluation (CN/MCI) file.

8.	Progression Prediction:
   
    o	For progression prediction, run the separate files for CN and MCI models.

10.	Performance Evaluation:
    
    o	To evaluate the performance of predicted results against the true values, run the ROC_Values and then AUC files.

12.	Additional Analysis:
    
    o	To verify the ENLR function and Annualized Percentage Change, run the corresponding files.

**Requirements**

 R and required packages (readr, glmnet, caret, Metrics, ggplot2, extrafont, cocor, pROC)

**Acknowledgements**

Special thanks to Professor Jussi Tohka (jussi.tohka@uef.fi), and Elaheh Moradi (elaheh.moradi@uef.fi) for their support.

