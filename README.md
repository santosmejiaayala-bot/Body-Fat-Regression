# Body Fat Regression
Final project for a statistical modeling course exploring variable selection and model diagnostics.
Predicts body fat percentage using multiple linear regression in R.

Skills:
R
Multiple Linear Regression
ggplot2
Model Selection
Data Visualization

Final predictors: Abdomen, Weight, Wrist

<img width="562" height="133" alt="image" src="https://github.com/user-attachments/assets/108bb6b0-e176-4ba0-9937-5d9f1f351e64" />

As you can see, this ANOVA table shows the only significant variables out of all variables in the dataset were Abdomen, Weight, and Wrist.  Abdomen had a positive coefficient, while Weight and Wrist had negative coefficients.  

For interpretation, Abdomen refers intuitively to the size of your belly, so as your belly grows, holding all other variables constant, your body fat percentage increases.  Weight is better interpreted as the density of the body.  Holding all other variables constant, an increase in weight corresponds to an increase in density, which means a lower body fat percentage, as fat is the least dense tissue in the body.  Wrist refers to the frame of a person, a reliable measure considering the bony nature of the wrist.  Holding all other variables constant, a larger frame corresponds to actually being more thick-boned, which corresponds to a lower body fat percentage.

<img width="441" height="215" alt="image" src="https://github.com/user-attachments/assets/966f2d14-3d08-4f5e-bb6d-3dd99bf7dd56" />

As you can see in the residuals and QQ plots, the data follows all assumptions of a linear regression (homoskedascity, normality, no residual trends, etc.)

<img width="363" height="208" alt="image" src="https://github.com/user-attachments/assets/146c33de-55e7-40fb-a3ff-16c6acbad4ae" />

There was one outlier, but it was found to negligibly affect the overall model when included, so I decided to leave it in.
