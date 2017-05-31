#' Tidying methods for bicreg objects
#' 
#' Tidying methods for "bicreg" object from the BMA package, which contains a 
#' Bayesian Model Averaging (BMA) object. Only a \code{tidy} method is provided,
#' not an \code{augment} or \code{glance} method.
#' 
#' @param x An object of class "bicreg"
#' @param conditional Whether to return conditional posteril means and
#' standard deviations.
#' @param ... Extra arguments (not used)

#' @return A \code{data.frame} with columns
#'   \item{term}{Term witin the model}
#'   \item{PIP}{the posterior inclusion probability that each variable 
#'   is non-zero}
#'   \item{post.mean}{the posterior mean of each coefficient (from model 
#'   averaging)}
#'   \item{post.sd}{the posterior standard deviation of each coefficient 
#'   (from model averaging)}
#'   \item{cond.post.mean}{the posterior mean of each coefficient conditional 
#'   on the variable being included in the model}
#'   \item{cond.post.sd}{the posterior standard deviation of each coefficient 
#'   conditional on the variable being included in the model}
#'   
#' @name bma_tidiers
#' @examples
#' 
#' @export
tidy.bicreg <- function(x, conditional = FALSE, ...) {
    co <- data.frame(c(1, x$probne0 / 100), x$postmean, x$postsd, 
                     row.names = names(x$postmean),
                     stringsAsFactors = FALSE)
    nn <- c("PIP", "post.mean", "post.sd")
    
    if (conditional) {
        co$cond.post.mean <- x$condpostmean
        co$cond.post.sd   <- x$condpostsd
        nn <- c(nn, 'cond.post.mean', 'cond.post.sd')
    }
    
    ret <- fix_data_frame(co, newnames = nn, newcol = 'term')
    ret
}
