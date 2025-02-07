# CLABSI_prediction_models

The project shares the code used to build models for 7-days CLABSI prediction as presented in the following paper: https://www.medrxiv.org/content/10.1101/2024.11.04.24316689v1

The **pipeline** directory contains numbered R scripts used to:

- prepare data (1_xx.R)

- impute data (2_xx.R)

- build models (3_xx.R)

- evaluate models (4_xx.R)

- predict a new observation using model objects (5_predict_with_model_objects.R). Model objects are not shared in this project. Contact the corresponding author of the paper in case of interest in the model objects. 

The **R** directory contains functions used to prepare the data, build models and evaluate the models.

The **config** directory contains configuration files used in data preparation and CLABSI outcome calculation.

The **docu** directory contains the documentation of the prepared data.
