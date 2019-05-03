#Title: Probability Checker
#Description: Checks whether the inputted probability is valid (length must be 1, must be a number, must be between 0 and 1 inclusive)
#Parameter: prob is the inputted probability that will be checked for a binomial distribution
#Return: Returns TRUE if the inputted probability is valid. Otherwise, the function will generate an error that comments on what is wrong with the input.
check_prob <- function (prob) {
  if (length(prob) != 1) stop ("probability must have length of 1")
  if (is.numeric(prob) == FALSE) stop("pobability has to be a number")
  if ((prob < 0) | (prob > 1)) stop("probability has to be a number between 0 and 1")
  else {TRUE}
}

#Title: Trials Checker
#Description: Checks whether the inputted trials is valid (length must be 1, must be a number, must be a non-negative integer)
#Parameter: trials is the inputted trial number that will be checked for a binomial distribution
#Return: Returns TRUE if the inputted trials is valid. Otherwise, the function will generate an error that comments on what is wrong with the input.
check_trials <- function (trials) {
  if (length(trials) != 1) stop("trials must has length of 1")
  if (is.numeric(trials) == FALSE) stop ("trials has to be a number")
  if (trials %% 1 != 0) stop ("trials has to be an integer")
  if (trials < 0) stop("trials has to be non-negative")
  else {TRUE}
}

#Title: Success Checker
#Description: Checks whether the inputted successes is valid for a given number of trials (successes can be a vector, all elements must be numbers, all elements must be integers, all elements must be between 0 and the number of trials) and whether the inputted trials are valid (based on Trials checker)
#Parameter: success is the inputted number of successes (can be vector or just one number) that will be checked based on a given number of trials for a binomial distribution
#Parameter: trials is the inputted number of trials that success will be checked against for a binomial distribution
#Return: Returns TRUE if trials and success are both valid. Otherwise, the function will generate an error that comments on what is wrong with the input.
check_success <- function (success, trials) {
  if (check_trials(trials)) {
    if (!all(is.numeric(success))) stop ("successes have to be numbers")
    if (!all(success %% 1 == 0)) stop ("successes have to be integers")
    if (!all((success <= trials) & (success >= 0))) stop("successes have to be a number between 0 and trials")
    else {TRUE}
  }
}
