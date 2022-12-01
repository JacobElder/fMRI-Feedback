TD.AsymLRDecaySA_paramRecov <- function(param){
  
  pos_lr = param[1]
  neg_lr = param[2]
  dec = param[3]
  pos_lr = as.numeric(pos_lr)
  neg_lr = as.numeric(neg_lr)
  dec = as.numeric(dec)
  #if(sigP[1]==1){
    slo = param[4]
    slo = as.numeric(slo)
    slo = 1 / slo
  #}else if(sigP[1]==0){
  #  slo = 1
  #}
  #if(sigP[2]==1){
  #  if(sigP[1]==1){
  #    shi = param[5]
  #    shi = as.numeric(shi)
  #  }else if(sigP[1]==0){
  #    shi = param[4]
  #    shi = as.numeric(shi)
  #  }
  #}else if(sigP[2]==0){
    shi = 0
  #}
  
  N <- 148 # rows based on trials
  D <- 5 # columns based on cues/features presented
  w <- matrix(4, 1, D) # row of 0 for associative weights for each cue/feature
  
  # create output dataframe
  x = c('trialNum', 'w1','w2','w3','w4','w5','RPE','Value', 'cluster', 'prob', 'clustType', 'valence', 'feedback')
  TD.Out <- matrix(ncol = length(x), nrow = N) # loop number, cue/feature number, total trial number, weights, prediction error, and value estimate
  colnames(TD.Out) <- x
  
  feat <- c(rep(1,16), rep(2,47), rep(3,41), rep(4,23), rep(5,21))
  feat <- sample(feat, length(feat), replace = F)
  #clustAssign <- sample(c(1,2,3,4,5), 5, replace = F)
  probs <- c(.9, .7, .5, .3, .1)
  probsAssign <- sample(probs, 5, replace = F)
  feedAssign <- function(curClust){
    curProbs <- probsAssign[curClust]
    valence <- sample(c(1,2), 1, prob = c(curProbs, (1-curProbs)))
    return(c(curProbs, valence))
  }
  responseCondition <- function(response, valenceInput){
    if(valenceInput == 1){
      if(response < 4){
        feedback <- sample((floor(response)+2):7, size = 1)
      }else if(response == 4){
        feedback <- sample(5:7, size = 1)
      }else if(response > 4){
        feedback <- sample((floor(response)):7, size = 1)
      }
    }else if(valenceInput == 2){
      if(response > 4){
        feedback <- sample(1:(ceiling(response)-2), size = 1)
      }else if(response == 4){
        feedback <- sample(1:3, size = 1)
      }else if(response < 4){
        feedback <- sample(1:(ceiling(response)), size = 1)
      }
    }
  }
  floorCeiling <- function(estimate){
    if(estimate <= 1){
      estimate <- 1
    }else if(estimate >= 7){
      estimate <- 7
    }
    return(estimate)
  }
  sigTrans <- function(feed, slope=1, shift=0){
    feed = feed - 4
    output = 1 / (1 + exp(slope * -(feed) - shift ) )
    output = (output * 6) + 1
    return(output)
  }
  
  
  for (n in 1:N) { # for each trial in all of the trials
    
    cluster = feat[n] # current feature/cue
    trialReward = feedAssign(cluster)
    S = which(probs==trialReward[1])
    w <- w*(dec) + 4*(1-dec)
    V = w[S] # value estimate is the associative weight of the current feature
    trialProb = trialReward[1]
    trialValence = trialReward[2]
    rew <- responseCondition(V, trialValence)
    feed = sigTrans(rew,slope=slo,shift=shi)
    
    RPE = feed - V # reward prediction error is the reward of current feature minus the value estimate of current feature
    if (RPE <= 0) { # if the reward prediction error is negative, use negative learning rate
      w[S] = w[S] + neg_lr * RPE # the updated associative weight is the current feature's associative weight plus the prediction error weighted by learning rate
    } else { # if the reward prediction error is positive, use the positive learning rate
      w[S] = w[S] + pos_lr * RPE # the updated associative weight is the current feature's associative weight plus the prediction error weighted by learning rate
    }
    
    V<-round(V)
    #V<-floorCeiling(V)
    
    # append row containing trial data to output dataframe
    TD.Out[n,] <- c(n, w[1], w[2], w[3], w[4], w[5], RPE, V, cluster, trialProb, S, trialValence, rew)
  }
  TD.Out <- as.data.frame(TD.Out)
  colnames(TD.Out) <- x
  return(TD.Out)
}
