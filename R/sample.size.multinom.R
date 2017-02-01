sample.size.multinom<-function(alpha, nclass, proportion, precision){
  #alpha is desired confidence level of the estimate, for 95% confidence, it is 0.05
  #nclass is number of classes
  #proportion is a proportion that is closest to 0.5 of the area or class area
  # Congalton formula 5.10
  chi<-qchisq(1-(alpha/nclass), df=1)
  n<-chi*proportion*(1-proportion)/precision^2 
  return(n)
}

#example
#sample.size.multinom(0.05, 8, 0.3, 0.05)
