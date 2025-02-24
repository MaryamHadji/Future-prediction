**# Future-prediction**
This is the code repository for the paper  "Predicting Future MRI-based Brain Atrophy Based on Longitudinal Data"

This repository contains scripts and models for analyzing brain atrophy/enlargement using Elastic Net Linear Regression (ENLR).

**Workflow**
1.	Run Models: Start by running the models to generate predictions.
   
2.	Correlation Analysis:
  o	To visualize the correlation between the true values and predicted values, run the scatter plot file.

3.	Model Comparison:
  o	To compare each model using the Cocor package, run the Pvalue file.
  o	To visualize the model differences, execute the Bar plot file.

4.	Validity Check:
  o	Check the validity of the models by running the External Evaluation (CN/MCI) file.

5.	Progression Prediction:
  o	For progression prediction, run the separate files for CN and MCI models.

6.	Performance Evaluation:
  o	To evaluate the performance of predicted results against the true values, run the ROC_Values and then AUC files.

7.	Additional Analysis:
  o	To verify the ENLR function and Annualized Percentage Change, run the corresponding files.

**Requirements**

•	R and required packages (readr, glmnet, caret, Metrics, ggplot2, extrafont, cocor, pROC)

**Acknowledgements**

Special thanks to Professor Jussi Tohka (jussi.tohka@uef.fi), and Elaheh Moradi (elaheh.moradi@uef.fi) for their support.

