

knowledge_averager <- function(x1, x2, x3, x4, x5, x6) {
  x_mat <- cbind(x1, x2, x3, x4, x5, x6)
  knowledge <- rowMeans(x_mat, na.rm = TRUE)
  return(knowledge)
}


add_parens <- function(x, digits=3){
  x <- as.numeric(x)
  return(paste0("(", sprintf(paste0("%.", digits, "f"), x), ")"))
}

format_num <- function(x, digits=3){
  x <- as.numeric(x)
  return(paste0(sprintf(paste0("%.", digits, "f"), x)))
}

se_mean <- function(x){
  x_nona <- x[!is.na(x)]
  n <- length(x_nona)
  return(sd(x_nona)/(sqrt(n)))
}


weighted_var <- function(x, w, na.rm = FALSE){
  if (na.rm) { w <- w[i <- !is.na(x)]; x <- x[i] }
  
  # https://en.wikipedia.org/wiki/Weighted_arithmetic_mean#Weighted_sample_variance
  mu_star = weighted.mean(x, w, na.rm = na.rm)
  
  v_1 <- sum(w)
  v_2 <- sum(w^2)
  
  numerator <- sum(w*((x - mu_star)^2))
  denominator <- v_1 - (v_2/v_1)
  return(numerator/denominator)
  
}

weighted_sd <- function(x, w, na.rm = FALSE){
  return(sqrt(weighted_var(x, w, na.rm = na.rm)))
}

weighted.se <- function(x, w, na.rm=FALSE){
  return(sqrt(weighted.var.mean(x = x, w = w, na.rm=na.rm)))
}

weighted.var.mean <- function(x, w, na.rm=FALSE)
  #  Computes the variance of a weighted mean following Cochran 1977 definition
{
  if (na.rm) { w <- w[i <- !is.na(x)]; x <- x[i] }
  n = length(w)
  xWbar = weighted.mean(x,w,na.rm=na.rm)
  wbar = mean(w)
  out = n/((n-1)*sum(w)^2)*(sum((w*x-wbar*xWbar)^2)-2*xWbar*sum((w-wbar)*(w*x-wbar*xWbar))+xWbar^2*sum((w-wbar)^2))
  return(out)
}
