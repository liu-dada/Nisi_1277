## Hazard Ratio Estimation and Plotting in R

This folder contains essential R functions and plotting scripts for estimating hazard ratios (HR) and visualizing their confidence intervals (CI) using both bootstrap and asymptotic methods. These tools are designed for comparative analysis across different methods and gender-specific groups.

### Functions

1. hr_Bootstrap.R

Description: Estimates the 95% confidence interval (CI) for hazard ratios using bootstrap methods.

Usage: Apply this function when a non-parametric estimation of the hazard ratio CI is desired.

2. hr_Asymptotic.R

Description: Calculates the 95% confidence interval (CI) for hazard ratios using asymptotic methods.

Usage: Use this function for parametric CI estimation based on large-sample properties.

### Plotting Scripts

3. plot_Bootstrap.R

Description: Generates hazard ratio plots stratified by gender (female and male) using the output data from hr_Bootstrap().

Usage: Ideal for visualizing bootstrap-based hazard ratio estimates and their confidence intervals.

4. plot_Asymptotic.R

Description: Creates hazard ratio plots stratified by gender (female and male) using the output data from hr_Asymptotic().

Usage: Useful for visualizing asymptotic-based hazard ratio estimates and their confidence intervals.

5. plot_2methods.R

Description: Compares hazard ratios and their corresponding confidence intervals obtained from both bootstrap and asymptotic methods.

Usage: Use this script to assess and compare the results from different estimation methods.

6. band_Bootstrap.R

Description: Produces a band plot from the bootstrap method's hazard ratio estimates.

Usage: Visualize the range of bootstrap-based hazard ratio estimates over time or other dimensions.

7. band_Asymptotic.R

Description: Generates a band plot using the asymptotic method's hazard ratio estimates.

Usage: Visualize the range of asymptotic-based hazard ratio estimates over time or other dimensions.
