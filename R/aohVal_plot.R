plot.aohVal<-function(aohVal){
  plot(as.numeric(aohVal$MP), as.numeric(aohVal$PP), type="p", ylim=c(0,1),
     xlim=c(0,1), xlab="Model Prevalence (MP)", ylab="Point Prevalence (PP)",
     cex=sqrt(aohVal$MATCH.EOO)/4)
abline(c(0,1), lwd=0.8, lty=5, col="dark grey")
}
