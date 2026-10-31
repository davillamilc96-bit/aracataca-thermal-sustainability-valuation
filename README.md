# aracataca-thermal-sustainability-valuation
Reproducible code for hedonic econometric and Random Forest analyses of thermal sustainability attributes and cadastral property values in Aracataca, Colombia.

# Thermal Sustainability Attributes and Cadastral Property Valuation in Aracataca, Colombia

## Overview

This repository contains the R scripts developed for the study **"Are Thermal Sustainability Attributes Reflected in Cadastral Property Values? Evidence from a Climate-Vulnerable Municipality Using Hedonic Econometric Models and Machine Learning."**

The research examines whether housing characteristics associated with constructive and environmental thermal sustainability are reflected in cadastral property values in Aracataca, Colombia, a municipality characterized by high temperatures and socioeconomic vulnerability.

The study combines hedonic econometric models and Random Forest regression to investigate the relationships between housing attributes and three cadastral valuation components:

- **Land unit value:** cadastral land value per square meter of land.
- **Building unit value:** cadastral construction value per square meter of built area.
- **Integrated property value:** combined land and construction value divided by land area.

## Research Methodology

The empirical analysis uses cadastral records, geographic information, environmental indicators, and housing characteristics identified through street-level imagery.

The initial sample includes 198 residential properties with available imagery. After model-specific exclusions, the analytical samples comprise 196–197 observations for the econometric models and 197 observations for the Random Forest models.

Three complementary analytical procedures were implemented:

**1. Selection Bias Assessment**

Logistic regression was used to estimate the probability of inclusion in the analytical sample based on observed property characteristics. Inverse probability weighting (IPW) was applied to mitigate potential selection bias associated with uneven street-level imagery availability.

**2. Hedonic Econometric Models**

Weighted ordinary least squares (OLS) regression was used to examine conditional associations between cadastral property values and environmental, structural, locational, and infrastructure characteristics. Diagnostic procedures assessed model specification, multicollinearity, and heteroscedasticity.

**3. Random Forest Regression**

Random Forest models were estimated to capture potentially nonlinear relationships and interactions. Hyperparameters were selected using repeated five-fold cross-validation, with five repetitions. Model performance was evaluated using cross-validated and out-of-bag (OOB) squared correlations between observed and predicted values. Permutation importance was used to assess predictor relevance.

## Repository Contents

The repository includes three R scripts:

| Script | Description |
|---|---|
| `Alistamiento_Sesgo.R` | Data preparation and assessment of potential sample-selection bias. |
| `Modelos_Lineales.R` | Estimation and diagnostic evaluation of hedonic econometric models. |
| `RANDOM_FOREST_FINAL_REPRODUCIBLE_CORREGIDO.R` | Random Forest estimation, hyperparameter tuning, validation, and variable importance analysis. |

The scripts are provided to document the analytical procedures used in the research.

## Main Findings

The results indicate that cadastral valuation patterns differ across land, building, and integrated property values.

Accessibility is consistently associated with land and integrated cadastral values, while vegetation exhibits negative conditional associations. Favorable building height is positively associated with construction and integrated values.

Most observed housing attributes associated with thermal sustainability do not exhibit systematic associations with higher cadastral property values.

These findings highlight the importance of distinguishing property valuation from thermal performance when designing urban planning and housing adaptation policies.

## Data Availability and Reproducibility

The repository provides the analytical R scripts developed for this study.

The underlying cadastral and property-level datasets are not publicly distributed through this repository.

Consequently, the scripts document the analytical workflow, but full numerical reproduction requires access to the original datasets and appropriate configuration of input files and software dependencies.

## Software

The statistical analyses were implemented in **R**.

Users seeking to execute the scripts should review the package dependencies and input-file paths specified in each script.

## Citation

If you use or adapt the analytical code, please acknowledge the associated research:

*Are Thermal Sustainability Attributes Reflected in Cadastral Property Values? Evidence from a Climate-Vulnerable Municipality Using Hedonic Econometric Models and Machine Learning.*

Full bibliographic details will be added following publication.

## License

The code is distributed under the MIT License, as specified in the repository.

## Repository Link

https://github.com/davillamilc96-bit/aracataca-thermal-sustainability-valuation
