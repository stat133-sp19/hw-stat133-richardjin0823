# workout03-richard-jin

###Overview
"Binomial" is a package that gives its users access to a variety of binomial related functions to calculate relevant statistics, create binomial probability tables, and plot histograms and line graphs for binomial distribution and it contains the following functions: 

bin_choose(n,k): the number of combinations in which k successes can occur in n trials

bin_cumulative(trials, prob): Calculates the binomial probability distribution and cumulative binomial probability distribution of a binomial distribution with specified probability and number of trials and displays the result in data frame

bin_distribution(trials, prob): Calculates the binomial probability distribution of a binomial distribution with specified probability and number of trials and displays result in data frame

bin_variable(trials, prob): Creates a list that details the trials and probability of a binomial distribution based on the trials and probability inputs after checking the validity of the inputs

bin_probability(success, trials, prob): Calculates the probability of a specified number of successes, with a specified number of trials, with a specified success probability, under binomial assumptions

bin_mean(trials, prob): Checks whether inputted trials and probability are valid and uses an auxiliary function to calculate the mean

bin_variance(trials,prob): Checks whether inputted trials and probability are valid and uses an auxiliary function to calculate the variance

bin_mode(trials, prob): Checks whether inputted trials and probability are valid and uses an auxiliary function to calculate the mode

bin_skewness(trials, prob): Checks whether inputted trials and probability are valid and uses an auxiliary function to calculate the skewness

bin_kurtosis(trials, prob): Checks whether inputted trials and probability are valid and uses an auxiliary function to calculate the kurtosis

###Motivation 
This gives user easier access to essential binomial functions that are not inherently available in R. I am also very motivated to do well in STAT133 by performing well on workout assignments.

###Usage
####Mean
bin_mean(10, 0.3)
#> [1] 3

####Variance
bin_variance(10, 0.3)
#> [1] 2.1

####Mode
bin_mode(10, 0.3)
#> [1] 3

####Skewness
bin_skewness(10, 0.3)
#> [1] 0.2760262

####Kurtosis
bin_kurtosis(10, 0.3)
#> [1] -0.1238095

####Binomial Choose
bin_choose(n = 5, k = 2)
#> [1] 10

####Binomial Probability
bin_probability(success = 2, trials = 5, prob = 0.5)
#> [1] 0.3125

####Binomial Distribution
bin_distribution(trials = 5, prob = 0.5)
#>   success probability
#> 1       0     0.03125
#> 2       1     0.15625
#> 3       2     0.31250
#> 4       3     0.31250
#> 5       4     0.15625
#> 6       5     0.03125

####Binomial Cumulative
bin_cumulative(trials = 5, prob = 0.5)
#>   success probability cumulative
#> 1       0     0.03125    0.03125
#> 2       1     0.15625    0.18750
#> 3       2     0.31250    0.50000
#> 4       3     0.31250    0.81250
#> 5       4     0.15625    0.96875
#> 6       5     0.03125    1.00000

####Binomial Variable
bin_variable(trials = 10, p = 0.3)
#> "Binomial Variable"
#> 
#>  Parameters              
#>  - number of trials: 10.0
#>  - prob of success :  0.3

####Binomial Variable Summary and Print
bin1 <- bin_variable(trials = 10, p = 0.3)
binsum1 <- summary(bin1)
binsum1
#> "Summary Binomial"
#> 
#>  Parameters              
#>  - number of trials: 10.0
#>  - prob of success :  0.3
#> 
#>  Measures              
#>  - mean    :  3.0000000
#>  - variance:  2.1000000
#>  - mode    :  3.0000000
#>  - skewness:  0.2760262
#>  - kurtosis: -0.1238095


