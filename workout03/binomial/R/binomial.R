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
  check_trials(trials)
  if(is.numeric(success)==FALSE) {
    stop("invalid success value")
  } else if(any(success>trials)) {
    stop("invalid success value")
  } else if(any(success<0)) {
    stop("invalid success value")
  } else if((any(success%%1!=0))) {
    stop("invalid success value")
  } else {
    return(TRUE)
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

#' @title binomial choose
#' @description calculates the number of ways to choose k objects from n
#' @param n number of objects to choose from
#' @param k number to choose
#' @return numeric vector of choose values
#' @export
#' @examples
bin_choose <- function(n=1,k=1) {
  if(any(!(n%%1==0)) | any(!(k%%1==0))) {
    stop("Values must be whole numbers")
  } else if(any(k>n)) {
    stop("k cannot be greater than n")
  } else if(any(n<0) | any(k<0)) {
    stop("Values must be positive")
  } else if(1<length(k)) {
    n <- rep(n, length(k))
  } else if(length(n)>1) {
    stop("n must be of length 1")
  }
  nchoosek <- factorial(n)/(factorial(k)*factorial(n-k))
  return(nchoosek)
}

#' @title binomial probability
#' @description calculates the probability of success in a certain amount of trials
#' @param success number of successes
#' @param trials number of trials
#' @param prob probability of success
#' @return numeric vector of probabilities
#' @export
#' @examples
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

#' @title binomial cumulative distribution
#' @description calculates the cumulative distribution of success in a certain amount of trials
#' @param trials number of trials
#' @param prob probability of success
#' @return dataframe of class "bincum"
#' @export
#' @examples
bin_cumulative <- function(trials=1, prob=0.5) {
  check_trials(trials)
  check_prob(prob)
  bincum <- matrix(rep(0, 3*(trials+1)), ncol=3)
  colnames(bincum) <- c("success", "probability", "cumulative")
  for(i in 1:(trials+1)){
    bincum[i,1] <- i-1
    bincum[i,2] <- bin_probability(trials, prob, success=(i-1))
    for(j in 1:i) {
      bincum[i,3] <- bincum[i,3] + bincum[j,2]
    }
  }
  bincum <- as.data.frame(bincum)
  class(bincum) <- c("bincum", "data.frame")
  return(bincum)
}

#' @export
plot.bincum <- function(bincum) {
  plot(x=bincum$success, y=bincum$cumulative, xlab="successes", ylab="probability")
  lines(x=bincum$success, y=bincum$cumulative)
}

bin_variable <- function(trials=1, prob=0.5) {
  check_trials(trials)
  check_prob(prob)
  binvar <- list(trials=trials, prob=prob)
  class(binvar) <- "binvar"
  return(binvar)
}

#' @export
print.binvar <- function(binvar) {
  cat('"Binomial variable"')
  cat("\n\n")
  cat("Parameters")
  cat("\n")
  cat("- number of trials: ")
  cat(binvar$trials)
  cat("\n")
  cat("- probability of success : ")
  cat(binvar$prob)
}

#' @export
summary.binvar <- function(binvar) {
  binsum <- list(trials=binvar$trials,
                 prob=binvar$prob,
                 mean=aux_mean(binvar$trials, binvar$prob),
                 variance=aux_variance(binvar$trials, binvar$prob),
                 mode=aux_mode(binvar$trials, binvar$prob),
                 skewness=aux_skewness(binvar$trials, binvar$prob),
                 kurtosis=aux_kurtosis(binvar$trials, binvar$prob))
  class(binsum) <- "summary.binvar"
  return(binsum)
}

#' @export
print.summary.binvar <- function(binsum) {
  cat('"Summary Binomial"')
  cat("\n\n")
  cat("Parameters")
  cat("\n")
  cat("- number of trials: ")
  cat(binsum$trials)
  cat("\n")
  cat("- prob of success : ")
  cat(binsum$prob)
  cat("\n\n")
  cat("Measures")
  cat("\n")
  cat("- mean    : ")
  cat(binsum$mean)
  cat("\n")
  cat("- variance: ")
  cat(binsum$variance)
  cat("\n")
  cat("- mode    : ")
  cat(binsum$mode)
  cat("\n")
  cat("- skewness: ")
  cat(binsum$skewness)
  cat("\n")
  cat("- kurtosis: ")
  cat(binsum$kurtosis)
}

#' @title binomial mean
#' @description calculates the mean success in a binomial distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return computed mean
#' @export
#' @examples
bin_mean <- function(trials=1, prob=0.5) {
  check_trials(trials)
  check_prob(prob)
  binmean <- aux_mean(trials, prob)
  return(binmean)
}

#' @title binomial variance
#' @description calculates the variance in a binomial distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return computed variance
#' @export
#' @examples
bin_variance <- function(trials=1, prob=0.5) {
  check_trials(trials)
  check_prob(prob)
  binvar <- aux_variance(trials, prob)
  return(binvar)
}

#' @title binomial mode
#' @description calculates the mode in a binomial distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return computed mode
#' @export
#' @examples
bin_mode <- function(trials=1, prob=0.5) {
  check_trials(trials)
  check_prob(prob)
  binmode <- aux_mode(trials, prob)
  return(binmode)
}

#' @title binomial skewness
#' @description calculates the skewness of a binomial distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return computed skewness
#' @export
#' @examples
bin_skewness <- function(trials=1, prob=0.5) {
  check_trials(trials)
  check_prob(prob)
  binskew <- aux_skewness(trials, prob)
  return(binskew)
}

#' @title binomial kurtosis
#' @description calculates the kurtosis of a binomial distribution
#' @param trials number of trials
#' @param prob probability of success
#' @return computed kurtosis
#' @export
#' @examples
bin_kurtosis <- function(trials=1, prob=0.5) {
  check_trials(trials)
  check_prob(prob)
  binkurt <- aux_kurtosis(trials, prob)
  return(binkurt)
}
