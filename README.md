To run model, run scripts 01 and 03 in that order. Script 02 is not necessary.

To get report, run scripts 01, 03, and 05 in that order. Note that there is no script 04.

The file 01_preprocess.R preprocesses the original MARP data and saves it into a new csv file. The script centers continuous variables by subtracting the age and dividing by the standard deviation. It also filters rows with NA, and subjects that didn’t pass the attention check. It also cleans the denomination_names, sample_type_names, and compensation_names variables. This leaves a processed dataset with 10,170 observations.

The file 02_prior_checks.R creates a fake dataset with the assumptions from the model and then fits the model using Stan. This step helps find possible errors in the model code and see if the model recovers the parameters from the simulated data.

File 05_model_report.Rmd creates a report with plots. 

