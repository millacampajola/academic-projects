# Motor Insurance Tariff Adequacy — A Monte Carlo Case Study

## Overview

Group project for the "Simulation Methods in Finance and Insurance" course (HEC Lausanne), assessing whether a motor insurance portfolio's tariff can be safely reduced while remaining profitable and solvent.

A frequency-severity model is fitted to a portfolio of 1,002 policies, used to simulate the aggregate loss distribution via Monte Carlo, and translated into risk premium, Solvency II capital, and tariff sensitivity results.

## Data

Portfolio-level data: number of claims and individual claim amounts per policy, premium, fees, and policy characteristics (age, gender, vehicle type and use, geographic area).

## Methodology

- **Data cleaning**: standardizing claim amount fields and removing anomalous records
- **Frequency and severity modelling**: MLE fitting and AIC/goodness-of-fit comparison across candidate distributions (Poisson selected for frequency, Gamma for severity)
- **Monte Carlo estimation**: simulation-based estimates of expected frequency and severity, validated against theoretical and empirical values
- **Variance reduction techniques**: antithetic variates and control variates, with resulting variance reduction quantified
- **Aggregate loss distribution**: Compound Poisson-Gamma simulation of the collective risk model, used to estimate the risk premium
- **Solvency II capital requirement**: 99.5% Value-at-Risk and Solvency Capital Requirement (SCR) estimation
- **Combined ratio and tariff sensitivity**: profitability assessment and reinsurance considerations, including a sensitivity analysis of tariff reductions overall and by vehicle segment (SUV vs. non-SUV)

## Repository structure

- `code/` – R script with the full analysis
- `report/` – project report and presentation slides

## Tools

- R (fitdistrplus, MASS, actuar, ggplot2, dplyr, gridExtra)
