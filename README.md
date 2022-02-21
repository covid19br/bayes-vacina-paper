# bayes-vacina

Bayesian Modeling of vaccination effect in Brazilian COVID-19 Epidemics

We construct an statistical model with explicit vaccination coverage covariates. The model imputes the temporal changes in relative risk of hospitalization and deaths between the age group studied and an age group of reference to vaccination.

The important function is "modelo_inla_shift.R" that prepares the data and runs the model for each state. The function "roda_inla.sh" is a wrapper in bash to parallelize outside R (only tested in Linux environments).

Code tested in R 4.1.2, INLA 21.2.23 and tidyverse 1.3.1. Other packages might have been used.
