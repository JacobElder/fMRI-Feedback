graphMixParamRecov <- function(param, type = "in"){
  library(igraph)
  library(compiler)
  
  pos_lr = param[1]
  neg_lr = param[2]
  mix = param[3]
  pos_lr = as.numeric(pos_lr)
  neg_lr = as.numeric(neg_lr)
  mix = as.numeric(mix)
  
  N <- 148 # rows based on trials
  D <- 148 # columns based on cues/features presented
  wpe <- matrix(4, 1, D) # row of 4 for associative weights for each cue/feature
  ws <- matrix(4, 1, D) 

  s <- matrix(NA, N, D)
  
  x = c('trialNum', paste0('wpe',1:148) , paste0('ws',1:148), 'RPE','V_PE', 'S_PE', 'V_S', 'Value', 'SR', 'cluster', 'prob', 'clustType', 'Idx', 'valence', 'feedback')
  # create output dataframe
  TD.Out <- matrix(ncol = length(x), nrow = N) # loop number, cue/feature number, total trial number, weights, prediction error, and value estimate
  colnames(TD.Out) <- x
  
  learn <- function(weights, dist, lr, RPE){

      weights = weights + (lr/ ( 1 + as.numeric(dist) ) ) * RPE

    return(weights)
  }
  learn <- cmpfun(learn)
  
  floorCeiling <- function(estimate){
    if(estimate <= 1){
      estimate <- 1
    }else if(estimate >= 7){
      estimate <- 7
    }
    return(estimate)
  }
  floorCeiling <- cmpfun(floorCeiling)
  
  featOrdered <- c(rep(1,47), rep(2,21), rep(3,16), rep(4,41), rep(5,23))
  Idx <- c(1L, 4L, 10L, 16L, 20L, 21L, 25L, 26L, 27L, 32L, 33L, 34L, 38L, 
           44L, 64L, 66L, 67L, 68L, 74L, 76L, 84L, 90L, 91L, 92L, 93L, 96L, 
           97L, 99L, 100L, 101L, 102L, 105L, 106L, 112L, 114L, 115L, 119L, 
           120L, 124L, 125L, 126L, 130L, 131L, 139L, 144L, 145L, 147L, 8L, 
           9L, 15L, 18L, 28L, 29L, 30L, 31L, 35L, 57L, 70L, 75L, 79L, 94L, 
           104L, 107L, 108L, 113L, 122L, 123L, 127L, 7L, 13L, 36L, 39L, 
           47L, 48L, 54L, 63L, 71L, 86L, 87L, 103L, 121L, 128L, 140L, 148L, 
           2L, 5L, 6L, 12L, 14L, 19L, 22L, 23L, 37L, 40L, 41L, 43L, 46L, 
           49L, 50L, 51L, 52L, 53L, 55L, 56L, 59L, 60L, 61L, 62L, 65L, 69L, 
           72L, 77L, 80L, 89L, 95L, 110L, 111L, 116L, 117L, 118L, 134L, 
           135L, 138L, 142L, 143L, 3L, 11L, 17L, 24L, 42L, 45L, 58L, 73L, 
           78L, 81L, 82L, 83L, 85L, 88L, 98L, 109L, 129L, 132L, 133L, 136L, 
           137L, 141L, 146L)
  IdxCluster1<-sample(Idx[1:47], replace = F)
  IdxCluster2<-sample(Idx[48:68], replace = F)
  IdxCluster3<-sample(Idx[69:84], replace = F)
  IdxCluster4<-sample(Idx[85:125], replace = F)
  IdxCluster5<-sample(Idx[126:148], replace = F)
  counter <- c(1, 1, 1, 1, 1)
  feat <- sample(feat, length(feat), replace = F)
  #clustAssign <- sample(c(1,2,3,4,5), 5, replace = F)
  probs <- c(.9, .7, .5, .3, .1)
  probsAssign <- sample(probs, 5, replace = F)
  traitAssign <- function(curClust){
    curIdxList <- get(paste0("IdxCluster",curClust))
    index <- counter[curClust]
    curTrait <- curIdxList[index]
    return(curTrait)
  }
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
  
  V_S = 4
  
  rewList <- matrix(ncol=1, nrow=148)
  
  for (n in 1:N) { # for each trial in all of the trials
    
    cluster = feat[n] # current feature/cue
    trait = traitAssign(feat[n]) # which trait
    counter[cluster] <- counter[cluster] + 1
    trialReward = feedAssign(cluster) # assign feedback
    S = which(probs==trialReward[1])
    
    trait = V(graph)[trait] # trait select
    
    V_PE = floorCeiling(wpe[S]) # value estimate is the associative weight of the current trait
    
    
    trialProb = trialReward[1]
    trialValence = trialReward[2]
    
    s[n, ] <- similarity(graph, method="dice", mode= "all" )[S, 1:148] # similarity of all traits to current trait
    
    #curSim <- s[1:N, S] # similarity vector of current trait
    if(n>1){
      #s[s==0] <- .00000000001
    curSim <- s[1:(n-1), S] # similarity vector of current trait
    #curSim <- s[feat[1:n]] # similarity vector of current trait
    #if(n>1){
      ws <- apply(s, MARGIN = 2, function(x) sum(x[1:(n-1)] * rewList[1:(n-1)]) / sum(x[1:(n-1)]) ) # apply weighted sum to each column
    #}
      if(sum(curSim)!=0){
        V_S = sum(curSim * rewList[1:(n-1)]) / sum(curSim) # value estimate is weighted sum of current trait
      }else if(sum(curSim)==0){
        V_S = mean(rewList[1:(n-1)])
      }
    }
    
    VE = V_PE*(mix) + V_S*(1-mix)
    
    SR = round(VE, digits=0)
    
    rew <- responseCondition(VE, trialValence)
    rewList[n] <- rew
    
    RPE = rew - V_PE
    S_PE = rew - V_S
    
    if (RPE <= 0) { # if the reward prediction error is negative, use negative learning rate
      lr = neg_lr # use neg learning rate
    } else { # if the reward prediction error is positive, use the positive learning rate
      lr = pos_lr # use pos learning rate
    }
    
    dist<-distances(posGraph, v = S, mode= as.character(type) )
    wpe <- learn(wpe, dist, lr, RPE )
    
    # append row containing trial data to output dataframe
    TD.Out[n, ] <- c(n, wpe, ws, RPE, V_PE, S_PE, V_S, VE, SR, cluster, trialProb, S, trait, trialValence, rew)
  }
  TD.Out <- as.data.frame(TD.Out)
  colnames(TD.Out) <- x
  return(TD.Out)
}
