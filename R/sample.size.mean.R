#for estimating population mean
sample.size.mean<-function(alpha, sigma, ME){
  #alpha is desired confidence level of the estimate, for 95% confidence, it is 0.05
  # Cochran formula 4.5.
  zstar<-qnorm(1-alpha/2)
  #sigma is assumed standard deviation of the population mean
  #ME  is allowed margin of error,
  n<-zstar^2*sigma^2/ME^2 
  return(n)
}
#example
#sample.size.mean(0.05, 9.48, 1.2)
