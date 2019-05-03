#' @title Binomial Choose
#' @description Calculates the number of combinations in which k successes can occur in n trials
#' @param n number of trials (numeric)
#' @param k number of successes (numeric)
#' @return the number of combinations in which k successes can occur in n trials
#' @export
#' @examples
#' bin_choose (n = 5, k = 2)
#' bin_choose (n = 5 , k = 0)
#' bin_choose (n = 5, k = 1:3)
bin_choose <- function (n, k) {
  if (!all(k <= n)) stop ("k cannot be greater than n")
  else {factorial(n) / (factorial(k) * factorial (n-k))}
}

#' @title Binomial Probability
#' @description Calculates the probability of a specified number of successes, with a specified number of trials, with a specified success probability, under binomial assumptions
#' @param success is the specified number of success for this binomial probability (numeric)
#' @param trials is the specified number of trials for this binomial probability (numeric)
#' @param prob is the specified probability of success for this binomial probability (numeric)
#' @return the binomial probability for specified values of success, trials, and probability
#' @export
#' @examples
#' bin_probability (success = 2, trials = 5, prob = 0.5)
#' bin_probability (success = 0:2, trials = 5, prob = 0.5)
#' bin_probability (success = 55, trials = 100, prob = 0.45)
bin_probability <- function (success, trials, prob) {
  if (check_prob(prob)) {
    if(check_trials(trials)) {
      if (check_success(success, trials)) {
        bin_choose(trials, success) * ((prob) ^ (success)) * ((1-prob) ^ (trials - success))
      }
    }
  }
}

#' @title Binomial Distribution
#' @description Calculates the binomial probability distribution of a binomial distribution with specified probability and number of trials and displays result in data frame
#' @param trials the specified number of trials for this binomial distribution (numeric)
#' @param prob the specificed probability of success for this binomial distribution (numeric)
#' @return a data frame that displays the binomial distribution with number of successes in the left column and the probability in the right column, with classes "bindis" and "data.frame"
#' @export
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)
bin_distribution <- function (trials, prob) {
  df <- data.frame()
  for (i in 1:(trials+1)) {
    df[i,1] <- i-1
    df[i,2] <- bin_probability(i-1, trials, prob)
  }
  colnames(df) <- c("success", "probability")
  class(df) <- c("bindis","data.frame")
  df
}

#plot.bindis()
#' @export
#' @examples
#' dis1 <- bin_distribution(trials = 5, prob = 0.5)
#' plot(dis1)
plot.bindis <- function (x) {
  barplot(x$probability, names.arg = x$success, xlab = "successes", ylab = "probability")
}

#' @title Binomial Cumulative
#' @description Calculates the binomial probability distribution and cumulative binomial probability distribution of a binomial distribution with specified probability and number of trials and displays the result in data frame
#' @param trials the specified number of trials for this binomial distribution (numeric)
#' @param prob the specificed probability of success for this binomial distribution (numeric)
#' @return a data frame that displays the binomial distribution with number of successes in the left column, the probability in the middle column, and the cumulative probability in the right column, with classes "bindcum" and "data.frame"
#' @export
#' @examples
#' bin_cumulative (5, 0.5)
bin_cumulative <- function (trials, prob) {
  df <- bin_distribution (trials, prob)
  for (i in 1:(trials+1)) {
    df$cumulative[i] <- sum(df$probability[1:i])
  }
  class(df) <- c("bincum", "data.frame")
  df
}

#plot.bincum()
#' @export
#' @examples
#' dis2 <- bin_cumulative(trials = 5, prob = 0.5)
#' plot(dis2)
plot.bincum <- function(x) {
  plot(y = x$cumulative, x = x$success, xlab = "successes", ylab = "probability")
  lines(y = x$cumulative, x = x$success)
}

#' @title Binomial List Function
#' @description Creates a list that details the trials and probability of a binomial distribution based on the trials and probability inputs after checking the validity of the inputs
#' @param trials the specified number of trials for this binomial distribution list (numeric)
#' @param prob the specificed probability of success for this binomial distribution list (numeric)
#' @return A named list that details the number of trials and probability parameters of a binomial distribution
#' @export
#' @examples
#' bin_variable(5, 0.5)
bin_variable <- function(trials, prob) {
  if (check_trials(trials)) {
    if (check_prob(prob)){
      lst <- list(trials = trials, prob = prob)
      class(lst) <- "binvar"
      lst
    }
  }
}

#print.binvar()
#' @export
#' @examples
#' bin1 <- bin_variable(10, 0.3)
#' bin1
print.binvar <- function (x) {
  cat('"Binomial Variable"\n\n')
  cd <- data.frame(c("- number of trials:","- prob of success :"), c(x$trials, x$prob))
  colnames(cd) <- c("Parameters         ","")
  print(cd, row.names = FALSE, col.names = FALSE)
}


#summary.binvar()
#' @export
#' @examples
#' bin1 <- summary(bin_variable(10, 0.3))
#' bin1
summary.binvar <- function (x) {
  lst <- list(trials = x$trials ,prob = x$prob,mean = aux_mean(x$trials, x$prob),variance = aux_variance(x$trials, x$prob), mode = aux_mode(x$trials, x$prob),skewness = aux_skewness(x$trials, x$prob),kurtosis = aux_kurtosis(x$trials, x$prob))
  class(lst) <- "summary.binvar"
  lst
}

#print.summary.binvar()
#' @export
#' @examples
#' bin1 <- bin_variable(10, 0.3)
#' binsum1 <- summary(bin1)
#' binsum1
print.summary.binvar <- function (x) {
  cat('"Summary Binomial"\n\n')
  cd <- data.frame(c("- number of trials:","- prob of success :"), c(x$trials, x$prob))
  colnames(cd) <- c("Parameters         ","")
  print(cd, row.names = FALSE, col.names = FALSE)
  cat('\n')
  cd2 <- data.frame(c("- mean    :","- variance:","- mode    :","- skewness:","- kurtosis:"),
                    c(x$mean, x$variance, x$mode, x$skewness, x$kurtosis ))
  colnames(cd2) <- c("Measures   ","")
  print(cd2, row.names = FALSE, col.names = FALSE)
}

#' @title Binomial Mean
#' @description Chekcks whether inputted trials and probability are valid and uses an auxiliary function to calculate the mean
#' @param trials the inputted number of trials for a binomial distribution (numeric)
#' @param prob the inputted probability for a binomial distribution (numeric)
#' @return returns the mean of a binomial distribution with given probability and number of trials
#' @export
#' @examples
#' bin_mean(10, 0.3)
bin_mean <- function (trials, prob) {
  if (check_prob(prob)) {
    if(check_trials(trials)) {
      aux_mean(trials, prob)
    }
  }
}

#' @title Binomial Variance
#' @description Chekcks whether inputted trials and probability are valid and uses an auxiliary function to calculate the variance
#' @param trials the inputted number of trials for a binomial distribution (numeric)
#' @param prob the inputted probability for a binomial distribution (numeric)
#' @return returns the variance of a binomial distribution with given probability and number of trials
#' @export
#' @examples
#' bin_variance(10, 0.3)
bin_variance <- function (trials, prob) {
  if (check_prob(prob)) {
    if(check_trials(trials)) {
      aux_variance(trials, prob)
    }
  }
}

#' @title Binomial Mode
#' @description Chekcks whether inputted trials and probability are valid and uses an auxiliary function to calculate the mode
#' @param trials the inputted number of trials for a binomial distribution (numeric)
#' @param prob the inputted probability for a binomial distribution (numeric)
#' @return returns the mode of a binomial distribution with given probability and number of trials
#' @export
#' @examples
#' bin_mode(10, 0.3)
bin_mode <- function (trials, prob) {
  if (check_prob(prob)) {
    if(check_trials(trials)) {
      aux_mode(trials, prob)
    }
  }
}

#' @title Binomial Skewness
#' @description Chekcks whether inputted trials and probability are valid and uses an auxiliary function to calculate the skewness
#' @param trials the inputted number of trials for a binomial distribution (numeric)
#' @param prob the inputted probability for a binomial distribution (numeric)
#' @return returns the skewness of a binomial distribution with given probability and number of trials
#' @export
#' @examples
#' bin_skewness(10, 0.3)
bin_skewness <- function (trials, prob) {
  if (check_prob(prob)) {
    if(check_trials(trials)) {
      aux_skewness(trials, prob)
    }
  }
}

#' @title Binomial Kurtosis
#' @description Chekcks whether inputted trials and probability are valid and uses an auxiliary function to calculate the kurtosis
#' @param trials the inputted number of trials for a binomial distribution (numeric)
#' @param prob the inputted probability for a binomial distribution (numeric)
#' @return returns the kurtosis of a binomial distribution with given probability and number of trials
#' @export
#' @examples
#' bin_kurtosis(10, 0.3)
bin_kurtosis <- function (trials, prob) {
  if (check_prob(prob)) {
    if(check_trials(trials)) {
      aux_kurtosis(trials, prob)
    }
  }
}
