Base.TD <- function(trialN, feat, rew, weight, param){
  
  lr = param[1]
  lr = as.numeric(lr)
  
  N <- length(feat) # rows based on trials
  D <- max(feat) # columns based on cues/features presented
  w <- matrix(weight, 1, D) # row of 0 for associative weights for each cue/feature
  
  # create output dataframe
  x = c('LoopN', 'trialInClust', 'trialNum', 'w1','w2','w3','w4','w5','RPE','Value')
  TD.Out <- matrix(ncol = length(x), nrow = N) # loop number, cue/feature number, total trial number, weights, prediction error, and value estimate
  colnames(TD.Out) <- x
  
  floorCeiling <- function(estimate){
    if(estimate <= 1){
      estimate <- 1
    }else if(estimate >= 7){
      estimate <- 7
    }
    return(estimate)
  }
  
  for (n in 1:N) { # for each trial in all of the trials
    S = feat[n] # current feature/cue
    V = w[S] # value estimate is the associative weight of the current feature
    V = floorCeiling(V)
    RPE = rew[n] - V # reward prediction error is the reward of current feature minus the value estimate of current feature
    w[S] = w[S] + lr * RPE # the updated associative weight is the current feature's associative weight plus the prediction error weighted by learning rate
    
    trialClust = sum(feat[1:n] == S) # get trial number for specific feature by summing all of it's appearances thus far
    trial = trialN[n]; # total trial number over all features
    
    # append row containing trial data to output dataframe
    TD.Out[n,] <- c(n, trialClust, trial, w[1], w[2], w[3], w[4], w[5], RPE, V)
  }
  TD.Out <- as.data.frame(TD.Out)
  colnames(TD.Out) <- x
  return(TD.Out)
}
