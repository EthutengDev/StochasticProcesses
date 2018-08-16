yuleProcessSim <- function(i, l, t){
  x = numeric(); #initialise X
  x[1] = i; #initial state of the process
  tl = numeric(); #initialise arrival times
  
  
  tl = c(0, cumsum(rexp(l-1,rep(i, l-1) + 1:(l-1))))
  
  for (j in 1:length(t)) {
    for (k in 2:length(tl)) {
      if(t[j]>=tl[k-1] & t[j]<=tl[k]){
        x[j+1] = x[1] + (k-1);
      }
    }
  }
  
  #return simulated Yule process
  return(x);
  
}

#example
arrivalTimes = seq(from=0, to=300, by=1);
yuleSim = yuleProcessSim(i = 2, l = 25, t = arrivalTimes, lambda = 0.3);

plot(yuleSim, type = 'l', ylab = 'X(t)', xlab = 't', main = 'Simulated Yule Process')
