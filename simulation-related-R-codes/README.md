### Simulation and Hazard Ratio Estimation in R

This folder contains critical R functions and plotting scripts for simulating data using the Gompertz distribution and estimating hazard ratios (HR) using bootstrap and asymptotic methods. It also includes tools for evaluating false positive rates and comparing scenarios under null and alternative hypotheses.

### Functions

1. simulation_func.R

Description: Simulates data under both null and alternative scenarios using the Gompertz distribution.

Usage: Utilize this function to generate synthetic datasets for testing and validating hazard ratio estimation methods.

2. bootstrap_func.R

Description: Estimates the 95% confidence interval (CI) for hazard ratios using bootstrap methods.

Usage: Apply this function for non-parametric CI estimation in simulated data.

3. sim_asymptotic_func.R

Description: Calculates the 95% confidence interval (CI) for hazard ratios using asymptotic methods.

Usage: Use this function for parametric CI estimation in large simulated datasets.

4. sim_fpr_func.R

Description: Calculates the false positive rate (FPR) for the simulated null data or discovery rate for the alternative.

Usage: Evaluate the performance of hazard ratio estimation methods by assessing the FPR over multiple simulations.

### Simulation and Plotting Scripts

5. sim_null.R

Description: Simulates data under the null scenario and estimates the hazard ratio and its CI using both bootstrap and asymptotic methods. This script also facilitates the comparison of these methods.

Usage: Run this script to test the behavior of hazard ratio estimation methods when there is no effect (null scenario).

6. sim_alter.R

Description: Simulates data under the alternative scenario and estimates the hazard ratio and its CI using both bootstrap and asymptotic methods. It also compares the results across methods.

Usage: Use this script to evaluate the methods under an alternative hypothesis where an effect is present.

7. sim_fpr_plot.R

Description: Plots the false positive rate (or discovery rate) over time (days) for both bootstrap and asymptotic methods.

Usage: Visualize the FPR (or discovery rate) to compare the accuracy and reliability of the different hazard ratio estimation techniques.

8. sim_plot_null_alter.R

Description: Plots the hazard ratios for both the null and alternative scenarios.

Usage: Use this script to compare the hazard ratio behavior under different scenarios, providing insights into the methods' performance.
