
riskdiff <- function(x, y = NULL, weight = NULL, conf.level = 0.95, 
                     rev = c("neither", "rows", "columns", "both")) {
  
  if (!is.null(y))
    dnn <- c(deparse(substitute(x)), deparse(substitute(y)))
  else if (is.null(y)) 
    dnn <- names(dimnames(x))
  
  if (is.matrix(x) && !is.null(y)) {
    stop("If x is a matrix, y should be NULL")
  }
  if (is.matrix(x)) {
    x <- epitable(x, rev = rev)
  }
  if (!is.null(y) && is.null(weight)) {
    x <- table(x, y) %>% 
      epitable(rev = rev)
  } else if (!is.null(y) && !is.null(weight)) {
    x <- xtabs(weight ~ x + y) %>% 
      epitable(rev = rev)
  }
  
  names(dimnames(x)) <- dnn
  
  return(x)
}

x <- xtabs(~ azt + aids, data = azt)

riskdiff(x = x)

riskdiff(x = azt$aids, y = azt$azt)

riskdiff(x = esoph$alcgp, y = esoph$agegp, weight = esoph$ncontrols)

