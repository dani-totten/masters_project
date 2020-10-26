# masters_project
masters project on EVT

- Generalized extreme value (GEV) distribution for modeling behavior in model tails
- Extremal types theorem is like the central limit theory, but for maxima in a given period
- block sizes impact GEV distribution
- simulated a known distribution to model maximum based on distribution of order statistics to compare GEVs from different block sizes to the the true maxima using goodness of fit tests

abstract
Extreme value theory (EVT) is a branch of statistics that concerns the probability of unlikely events. There are numerous practical applications including hydrology and financial modeling; as well as any other event in which a high magnitude event needs to be predicted. The Generalized extreme value (GEV) distribution is a commonly used model that uses the largest value within an interval of observations, or block maxima, to generate a distribution of events that occur in the tails of typical distributions. The selection of block size has a large impact on the model because small blocks sizes yield a large sample of less extreme events, and conversely, larger blocks yield a smaller sample of more rare events. When the underlying distribution of the population has the form of a known distribution, the empirical distribution of maxima over the block can be calculated using the distribution of the largest order statistic. The objective of this paper is to use a simulation study to demonstrate that GEV models based on larger block sizes are more similar to the empirical distribution of maximum order statistic than GEV models based on smaller block sizes, but larger block sizes suffer from less stable parameter estimation.
