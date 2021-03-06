
prop_test <- function(x, n, p = NULL, method = c("wald", "wilson"), 
                      alternative = c("two.sided", "less", "greater"), 
                      conf.level = 0.95, correct = FALSE) {
  
  method <- match.arg(method)
  alternative <- match.arg(alternative)
  parameter <- 1
  names(parameter) <- "df"
  
  if (method == "wald") {
    if (correct) {
      stop("Nick hasn't gotten this far yet")
    } else {
      method <- "1 sample test of proportions without continuity correction"
      
      p_mle <- x / n
      names(p_mle) <- "MLE proportion"
      
      se_ci <- sqrt(p_mle * (1 - p_mle) / n)
      z_cv <- qnorm(0.5 * (1 + conf.level))
      ci <- round(p_mle + c(-1, 1) * z_cv * se_ci, 4)
      attr(ci, "conf.level") <- conf.level
      
      if (!is.null(p)) {
        se <- sqrt(p * (1 - p) / n)
        statistic <- (p_mle - p) / se
      } else {
        p <- 0.5
        se <- sqrt(p * (1 - p) / n)
        statistic <- (p_mle - p) / se
        
      }
      
      if (any(n*p < 5, n*p*(1 - p))) {
        warning("Normal approximation may be incorrect. Consider using exact method")
      } 
      
      names(statistic) <- "Z"
      names(p) <- "proportion"
      
      if (alternative == "two.sided") {
        if (statistic < 0) {
          p.value <- pnorm(statistic) * 2
        } else {
          p.value <- pnorm(statistic, lower.tail = FALSE) * 2
        }
      } else if (alternative == "less") {
        p.value <- pnorm(statistic)
      } else if (alternative == "greater") {
        p.value <- pnorm(statistic, lower.tail = FALSE)
      }
    }
    vals <- list(null.value = p, alternative = alternative, method = method, 
                 estimate = p_mle, data.name = "Test", statistic = statistic, 
                 p.value = p.value, conf.int = ci)
    class(vals) <- "htest"
    print(vals)
    
  } else if (method == "wilson") {
    prop.test(x = x, n = n, p = p, alternative = alternative, 
              conf.level = conf.level, correct = correct)
  }
}

prop_test(2, 6)
prop_test(14, 100, method = "wald")
prop_test(14, 100, method = "wald", correct = TRUE)
prop_test(14, 100, method = "wilson")
prop_test(14, 100, 0.2, alternative = "two.sided")
test <- prop_test(14, 100, .15)
prop_test(14, 100, method = "wilson", correct = TRUE)
