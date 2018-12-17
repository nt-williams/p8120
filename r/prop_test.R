
prop_test <- function(x, n, p = NULL, method = c("wald", "wilson"), 
                      alternative = c("two.sided", "less", "greather"), 
                      conf.level = 0.95, correct = FALSE) {
  
  if (method == "wald") {
    if (correct) {
      stop("Nick hasn't gotten this far yet")
    } else {
      p_mle <- x / n
      se <- sqrt(p_mle * (1 - p_mle) / n)
      z <- qnorm(0.5 * (1 + conf.level))
      ci <- p_mle + c(-1, 1) * z * se
    }
    return(ci)
  } else {
    if (missing(alternative)) {
      prop.test(x = x, n = n, p = p, alternative = "two.sided", 
                conf.level = conf.level, correct = correct)
    } else {
      prop.test(x = x, n = n, p = p, alternative = alternative, 
                conf.level = conf.level, correct = correct)
    }
  }
}

prop_test(14, 100, method = "wald")
prop_test(14, 100, method = "wald", correct = TRUE)
prop_test(14, 100, method = "wilson")
