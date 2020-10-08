#' Wilcox Location Parameter
#'
#' Modified function to calculate Wilcox' Location paramenter
wilcox.loc <- function(vec, na.rm = FALSE){
  n <- length(vec)
  # If number of observations is less than 2 then we just return mean as location estimate
  if(n <= 2){
    return(mean(vec, na.rm = na.rm))
  }
  
  # Calculating the paired avagerages
  pairAvg <- sort(c(vec, combn(vec, 2, function(x)mean(x, na.rm = na.rm))))
  return(median(pairAvg, na.rm = na.rm))
}

#' Cohen's d
#'
#' Function to calculate Cohen's D value when testing effect size
cohens_d <- function(x, y, na.rm = TRUE) {
  if(na.rm){
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
  }
  n.x <- length(x)- 1
  n.y <- length(y)- 1
  mean.diff  <- abs(mean(x) - mean(y))
  if(n.x == 0 & n.y > 0) {
    common.sd <- sqrt(n.y * var(y)/n.y)
  } else if (n.x > 0 & n.y == 0){
    common.sd <- sqrt(n.x * var(x)/n.x)
  } else if (n.x > 0 & n.y  > 0) {
    common.sd <- sqrt((n.x * var(x) + n.y * var(y))/(n.x + n.y))
  } else {
    common.sd <- sd(c(x, y)) / 2
  }
  
  return(mean.diff/common.sd)
}



# Helper functions to calculate the quantiles and standard deviations for the
# source points. Used in the points_sd and confidence interval functions
quant_funcs <- list(average = quantile, robust = quantile,
                    weighted =  purrr::possibly(Hmisc::wtd.quantile, c(`5%` = NaN, `95%` = NaN)))
quant_args <- list(list(probs = c(0.05, 0.95)),  list(probs = c(0.05, 0.95)),
                   list(probs = c(0.05, 0.95), type = "i/n"))

get_quant <- function(pts, wt)invoke_map(quant_funcs, quant_args, x = pts, na.rm = TRUE, weights = wt)

sd_funcs <- list(average = function(x, w, na.rm)sd(x, na.rm = na.rm),
                 robust = function(x, w, na.rm)mad(x, na.rm = na.rm),
                 weighted = weighted.sd)
sd_args <- list(list(na.rm = TRUE), list(na.rm = TRUE), list(na.rm = TRUE))
get_sd <- function(pts, wt)invoke_map(sd_funcs, sd_args, x = pts, w = wt)

