#' KewNewPkb
#' @param x a n p matrix of n observation
#' @param y is another vector
#' @return a vercot
#' @author mutecyphetr
#' @details
#' This is an example function named 'hello'
#' which prints 'Hello, world!'.
#' @seealso \code{lm}
#' @export
#'

KewlNewPkg <- function(x,y) {
  p <- ncol(x)
  if(p < 10)
    stop("there are fewer than 10 predictors")
  pvalues <- numeric(p)
  for(i in seq_len(p)){
    fit <- lm (y ~x[,i])
    summ <- summary(fit)
    pvalues[i] <- summ$coefficients[2,4]
  }
  ord <- order(pvalues)
  ord <- ord[1:10]
  x10 <- x[,ord]
  fit <- lm(y ~x10)
  coef(fit)
}

#' Prediction with love
#' takes code from \code{KewlNewPkg}
#'
#' Takes coefficients and has love for them
#'
#' @param X an x by 10 matrix in the window
#' @param b a small dog with a waggy tail that no one wants
#' @return a numberic vector with love
#' @export
#'
predict10 <- function(X,b){
  X <- cbind(1,X)
   drop(X%*%b)
}
