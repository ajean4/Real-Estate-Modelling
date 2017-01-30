# Real-Estate-Modelling

This project tries to estimate housing prices by postal code in Ontario using an illustrative data set in the absence of real data. Large data sets were removed from the folder, but can be downloaded from the sites provided.
 
As not every area has sales observations in each period, it is sometimes neccessary to impute the value. The project uses Multilevel Bayesian Models to impute prices in the region. 

A list of shape files by postal codes was downloaded from:
	
https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm, 

and combined with the population and dwelling counts table from: 
	
https://www12.statcan.gc.ca/census-recensement/2011/dp-pd/hlt-fst/pd-pl/Table-Tableau.cfm?LANG=Eng&T=1201&S=22&O=A.

An average housing price was simulated per region, as well as the number of property transactions. 

Some notes:

1. The population and dwelling count was provided by the first 3 letters of the Postal code. I assumed that the population was uniformly spread across all postal codes sharing the same first 3 letters.

2. The area for each postal code was calucated based on the area of each polygon in the shape file. Some of the shape files represented a centroid of the postal code, so they had 0 area. These were imputed by median postal code. Some FSA groups had 0 area, so it was not possible to use for imputation. 

3. The density of each postal code was calculated as Population (population in FSA [first 3 letters of postal code] divided by number of postal codes in FSA) divided by the calculated area of the postal code. If the calculated density was Inf or NA, it was set to the median.

4. Property prices were generated randomly, using the estimated population and density. One property was simulated for each time and postal code. It is possible to simulate multiple property sales for each postal code. 

5. I sampled the generated data to take a subset. A model was fit to the subset, and used to make predictions on the entire dataset. 

Plots were generated to compare the model with observed data. 

Areas of improvement:
	
-Better imputation strategy, possibly from external data

-Better model http://arxiv.org/abs/1505.01164

-Due to time constraints the model was tested to make sure it ran correctly, but was not given enough iterations to converge to a solution. The model should be run for more iterations

-The postal code shape file contained centroids, but not the actual postal code boudaries. This caused the area estimate to be inconsitent. 

-The population was spread evenly across postal codes in each FSA. The actual population for each postal code shoule be used. 
