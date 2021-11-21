#' Kullback-Leibler Divergence
#'
#' @description Computes the Kullback-Leibler divergence between two probability distributions.
#'
#' @param p numeric. probability distributions.
#' @param q numeric. probability distributions.
#' @param base character. log bases. "log", "log2", "log10". default is "log"
#' @param margin logical. Choose whether to return individual values or totals. 
#' The default value is FALSE, which returns individual values.
#' 
#' @return numeric. Kullback-Leibler divergence of probability distributions p and q.
#' 
#' @seealso \code{\link{jsd}}.
#' @export
#' @examples
#' # Sample data for probability distributions p.
#' event <- c(115, 76, 61, 39, 55, 10, 1)
#' no_event <- c(3, 3, 7, 10, 28, 44, 117)
#' 
#' p <- event / sum(event)
#' q <- no_event / sum(no_event)
#' 
#' kld(p, q)
#' kld(p, q, base = "log2")
#' kld(p, q, margin = TRUE)
#' 
#' @export
kld <- function(p, q, base = c("log", "log2", "log10"), margin = FALSE) {
  base <- match.arg(base)
  
  klds <- p * do.call(base, list(p / q))
  if (margin) {
    return(sum(klds, na.rm = TRUE))
  } else {
    klds
  }
}


#' Jensen-Shannon Divergence
#'
#' @description Computes the Jensen-Shannon divergence between two probability distributions.
#'
#' @param p numeric. probability distributions.
#' @param q numeric. probability distributions.
#' @param base character. log bases. "log", "log2", "log10". default is "log"
#' @param margin logical. Choose whether to return individual values or totals. 
#' The default value is FALSE, which returns individual values.
#' 
#' @return numeric. Jensen-Shannon divergence of probability distributions p and q.
#' 
#' @seealso \code{\link{kld}}.
#' @export
#' @examples
#' # Sample data for probability distributions p.
#' event <- c(115, 76, 61, 39, 55, 10, 1)
#' no_event <- c(3, 3, 7, 10, 28, 44, 117)
#' 
#' p <- event / sum(event)
#' q <- no_event / sum(no_event)
#' 
#' jsd(p, q)
#' jsd(p, q, base = "log2")
#' jsd(p, q, margin = TRUE)
#' 
#' @export
jsd <- function(p, q, base = c("log", "log2", "log10"), margin = FALSE) {
  base <- match.arg(base)
  
  m <- (p + q) / 2
  
  jsds <- 0.5 * (kld(p , m, base) + kld(q , m, base))
  
  if (margin) {
    return(sum(jsds, na.rm = TRUE))
  } else {
    jsds
  }
}

# Modified so that an error does not occur even when the independent variable x has only one unique value.
# statistic, p.value, df
get_tab_lm <- function(x) {
  info_lm <- summary(x)
  
  df <- data.frame(r.squared = info_lm$r.squared, 
                   adj.r.squared = info_lm$adj.r.squared,
                   sigma = info_lm$sigma,
                   statistic = ifelse(is.null(info_lm$fstatistic), NA, 
                                      info_lm$fstatistic[1]),
                   p.value = ifelse(is.na(anova(x)$'Pr(>F)'[1]), NA, 
                                    anova(x)$'Pr(>F)'[1]),
                   df = ifelse(is.null(info_lm$fstatistic), NA, 
                               round(info_lm$fstatistic[2])),
                   logLik = as.numeric(logLik(x)),
                   AIC = AIC(x),
                   BIC = BIC(x),
                   deviance = deviance(x),
                   df.residual = x$df.residual,
                   nobs = length(x$residuals))
  
  row.names(df) <- NULL
  
  df
}


get_tab_chisq <- function(x) {
  df <- data.frame(statistic = as.numeric(x$statistic),
             p.value = x$p.value,
             parameter = x$parameter,
             method = x$method)
  
  row.names(df) <- NULL
  
  df
}

