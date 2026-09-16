# Claims Development and Reserving

## Overview

Group project for the "Data Science for Non-Life Insurance" course (KU Leuven, MAFE), aimed at projecting outstanding claims reserves for accident years 2009–2023.

The analysis compares a traditional actuarial approach (Chain-Ladder) with a machine learning approach (Random Forest), and combines the two into a single credibility-weighted estimate.

## Data

Individual, claim-level data (accident year, development year, calendar year, incremental/cumulative payments, inflation index, open/closed status, reporting lag), aggregated into a development triangle for the Chain-Ladder analysis.

## Methodology

- **Exploratory analysis**: reporting-delay distribution, incremental triangle heatmap, claim-size distribution, severity by accident year, closure dynamics
- **Chain-Ladder reserving**: nominal and inflation-adjusted versions, with Mack model diagnostics (residual analysis, rank-correlation test) and bootstrap for predictive uncertainty
- **Random Forest model**: separate models for frequency, severity and claim closure, used to project RBNS reserves and estimate pure IBNR via the reporting-lag distribution
- **Cox proportional hazards** correction for right-censoring in claim closure probabilities
- **Back-testing** framework comparing Chain-Ladder and Random Forest projections against actual results
- **Credibility-based blend** of the two reserve estimates, weighted by their bootstrap variances

## Repository structure

- `code/` – R Markdown with the full analysis
- `report/` – project report and presentation slides

## Tools

- R, R Markdown (dplyr, tidyr, ggplot2, randomForest, survival)
