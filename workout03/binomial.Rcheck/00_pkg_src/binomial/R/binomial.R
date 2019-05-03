# check functions

check_prob <- function(prob=0.5) {
  if(is.numeric(prob)==FALSE) {
    stop("invalid prob value")
  } else if(length(prob)>1) {
    stop("invalid prob value")
  } else if(prob<0) {
    stop("invalid prob value")
  } else if(prob>1) {
    stop("invalid prob value")
  } else {
    return(TRUE)
  }
}

check_trials <- function(trials = 1) {
  if(is.numeric(trials)==FALSE) {
    stop("invalid trials value")
  } else if(length(trials)>1) {
    stop("invalid trials value")
  } else if(trials<0) {
    stop("invalid trials value")
  } else if((trials%%1)!=0) {
    stop("invalid trials value")
  } else {
    return(TRUE)
  }
}

check_success <- function(success=1, trials=1) {
  if(check_trials(trials)==TRUE) {
    if(is.numeric(success)==FALSE) {
      stop("invalid success value")
    } else if(any(success>trials)) {
      stop("success cannot be greater than trials")
    } else if(any(success<0)) {
      stop("invalid success value")
    } else if((any(success%%1!=0))) {
      stop("invalid success value")
    } else {
      return(TRUE)
    }
  } else {
    stop("invalid trials value")
  }
}

# auxilliary functions

aux_mean <- function(trials=1,prob=0.5) {
  n <- trials
  p <- prob
  binmean <- n*p
  return(binmean)
}

aux_variance <- function(trials=1, prob=0.5) {
  n <- trials
  p <- prob
  binvar <- n*p*(1-p)
  return(binvar)
}

aux_mode <- function(trials=1, prob=0.5) {
  n <- trials
  p <- prob
  m <- n*p + p
  if(m%%1 == 0) {
    binmode <- c(m, m-1)
  } else {
    binmode <- floor(m)
  }
  return(binmode)
}

aux_skewness <- function(trials=1, prob=0.5) {
  n <- trials
  p <- prob
  skew <- (1-2*p)/sqrt(n*p*(1-p))
  return(skew)
}

aux_kurtosis <- function(trials=1, prob=0.3) {
  n <- trials
  p <- prob
  kurt <- (1-6*p*(1-p))/(n*p*(1-p))
  return(kurt)
}

# main functions

bin_choose <- function(n=1,k=1) {
  if(any(!(n%%1==0)) | any(!(k%%1==0))) {
    stop("Values must be whole numbers")
  } else if(any(k>n)) {
    stop("k cannot be greater than n")
  } else if(any(n<0) | any(k<0)) {
    stop("Values must be positive")
  } else if(length(n)<length(k)) {
    n <- rep(n, length(k))
  } else if(length(n)>length(k)) {
    k <- rep(k, length(n))
  }
  nchoosek <- factorial(n)/(factorial(k)*factorial(n-k))
  return(nchoosek)
}

bin_probability <- function(success=1, trials=2, prob=0.5) {
  check_trials(trials)
  check_success(success, trials)
  check_prob(prob)
  k <- success
  n <- trials
  p <- prob
  nchoosek <- bin_choose(n,k)
  prob <- nchoosek*(p^k)*((1-p)^(n-k))
  return(prob)
}

#' @title binomial distribution
#' @description calculates the distribution of different success in a certain amount of trials
#' @param trials number of trials
#' @param prob probability of success
#' @return dataframe of class "bindis"
#' @export
#' @examples
bin_distribution <- function(trials=1, prob=0.5) {
  check_trials(trials)
  check_prob(prob)
  bindis <- matrix(rep(0, 2*(trials+1)), ncol=2)
  colnames(bindis) <- c("success", "probability")
  for(i in 1:(trials+1)){
    bindis[i,1] <- i-1
    bindis[i,2] <- bin_probability(trials, prob, success=(i-1))
  }
  bindis <- as.data.frame(bindis)
  class(bindis) <- c("bindis", "data.frame")
  return(bindis)
}

#' @export
plot.bindis <- function(bindis) {
  barplot(bindis$probability, names.arg=bindis$success, xlab="successes", ylab="probability")
}

