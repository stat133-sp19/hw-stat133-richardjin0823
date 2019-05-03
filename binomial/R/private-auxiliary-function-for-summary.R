#Title: Auxiliary Mean
#Description: Takes in the probability parameter and number of trials parameter for the binomial distribution and finds the mean
#Parameter: trials is the inputted number of trials for a binomial distribution (numeric)
#Parameter: prob is the the inputted probability for a binomial distribution (numeric)
#Return: Returns the mean of a binomial distribution with given probability and number of trials
aux_mean <- function (trials, prob) {
  trials * prob
}

#Title: Auxiliary Variance
#Description: Takes in the probability parameter and number of trials parameter for the binomial distribution and finds the variance
#Parameter: trials is the inputted number of trials for a binomial distribution (numeric)
#Parameter: prob is the the inputted probability for a binomial distribution (numeric)
#Return: Returns the variance of a binomial distribution with given probability and number of trials
aux_variance <- function (trials, prob) {
  trials * prob * (1-prob)
}

#Title: Auxiliary Mode
#Description: Takes in the probability parameter and number of trials parameter for the binomial distribution and finds the mode
#Parameter: trials is the inputted number of trials for a binomial distribution (numeric)
#Parameter: prob is the the inputted probability for a binomial distribution (numeric)
#Return: Returns the mode of a binomial distribution with given probability and number of trials
aux_mode <- function (trials, prob) {
  floor(trials * prob + prob)
}
#Title: Auxiliary Skewness
#Description: Takes in the probability parameter and number of trials parameter for the binomial distribution and finds the skewness
#Parameter: trials is the inputted number of trials for a binomial distribution (numeric)
#Parameter: prob is the the inputted probability for a binomial distribution (numeric)
#Return: Returns the skewness of a binomial distribution with given probability and number of trials
aux_skewness <- function (trials, prob) {
  (1 - 2 * prob) / (trials * prob * (1-prob)) ^ (1/2)
}

#Title: Auxiliary Kurtosis
#Description: Takes in the probability parameter and number of trials parameter for the binomial distribution and finds the kurtosis
#Parameter: trials is the inputted number of trials for a binomial distribution (numeric)
#Parameter: prob is the the inputted probability for a binomial distribution (numeric)
#Return: Returns the kurtosis of a binomial distribution with given probability and number of trials
aux_kurtosis <- function(trials, prob) {
  (1 - 6 * prob * (1 - prob)) / (trials * prob * (1 - prob))
}

