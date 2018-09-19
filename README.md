# Data Science


# R:

# Dual Time Dynamics (DTD):
Applying Dual Time Dynamics to Credit Risk Models

Dual time Dynamics attempts to model the loss behaviour of a portfolio of retail loans as a function of calendar time and portfolio maturation time, as well as portfolio risk profile and the impact of the macro-economy as an exogenous factor.

DTD decomposes vintage level data to months-on-books (maturation), calendar date (exogenous) and vintage (quality). It follows the same concept in Age-Cohort-Period model which is commonly used in sociology, epidemiology and other population and demography studies.
In credit risk stress testing models, we would like to model delinquency or default rates to forecast them in different macroeconomic scenarios. In this context, estimating time effect is our main interest but it is contaminated by vintage quality and loan ages. More precisely, since observation date (t) minus date of loan issue (v) is equal to loan age (a) ( a = t — v), it is not possible to estimate these factors based on a unique solution.

I worked on adapting the framework for application to stress testing models across a range of retail products.


# Monte Carlo Tree Search (MC Tree Search):

The tree search algorithm attempts to mimic the intuition of a portfolio manager in the selection of a 'best' stress testing model for a portfolio of retail loans.  The trajectory the portfolio default probability will follow in a hypothetical stress situation is the basis of selection and is evaluated by managers using several heuristics. The algorithm attempts to capture these heuristics, significantly increasing the efficiency of the modelling process.

# Dynamic Time Warping (DTW):

An algorithm to match predicted model trajectory and yearly economic scenario trajectory

# Random Forest & Gradient Boosting Algorithm for Load Prediction: (RF_GBM_for_load_prediction)

Used RF & GBM for predicting load (electricity) in a Day Ahead Power Market.
