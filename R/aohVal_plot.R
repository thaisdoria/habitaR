#' Plot 'aohVal' objects
#'
#' Provide a \code{\link[graphisc]{plot}} of the graphical representation of
#' validation results from the species AOH mapping.
#'
#' @usage
#' plot(aohVal)
#' @param aohVal A object from a class 'aohVal', which can be obtained with
#' \code{\link[habitaR]{aohVal}} function.
#' @import graphics
#' @return \code{plot.aohVal} returns a graphical representation of 'validation
#' analysis performed with \code{\link[habitaR]{aohVal}} following Rondinini
#' et al. (2011).
#' @encoding UTF-8
#' @author Thaís Dória & Daniel Gonçalves-Souza
#' @export plot.aohVal

plot.aohVal<-function(aohVal){
  plot(as.numeric(aohVal$MP), as.numeric(aohVal$PP), type="p", ylim=c(0,1),
     xlim=c(0,1), xlab="Model Prevalence (MP)", ylab="Point Prevalence (PP)",
     cex=sqrt(aohVal$MATCH.EOO)/4)
abline(c(0,1), lwd=0.8, lty=5, col="dark grey")
}
