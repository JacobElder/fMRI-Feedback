graphMix <- function(trialN, feat, rew, weight, graph, type, cue, param){
  library(igraph)
  library(purrr)
  library(compiler)
  
  pos_lr = param[1]
  neg_lr = param[2]
  mix = param[3]
  pos_lr = as.numeric(pos_lr)
  neg_lr = as.numeric(neg_lr)
  mix = as.numeric(mix)
  
  N <- length(feat) # rows based on trials
  D <- 148 # columns based on cues/features presented
  stopifnot("ERROR: Weight is not suitable size. Please choose a constant weight or input a vector of 148 elements." = length(weight) == 1 | length(weight) == 148)
  if(length(weight) == 1){
    wpe <- matrix(weight, 1, D) # row of 0 for associative weights for each cue/feature
  }else if(length(weight) == 148 ) {
    wpe <- weight
  }
  if(length(weight) == 1){
    ws <- matrix(weight, 1, D) # row of 0 for associative weights for each cue/feature
  }else if(length(weight) == 148 ) {
    ws <- weight
  }
  s <- matrix(NA, N, D)
  
  x = c('LoopN', 'trialNum', paste0('wpe',1:148) , paste0('ws',1:148), 'RPE','V_PE', 'S_PE', 'V_S', 'Value')
  # create output dataframe
  TD.Out <- matrix(ncol = length(x), nrow = N) # loop number, cue/feature number, total trial number, weights, prediction error, and value estimate
  colnames(TD.Out) <- x
  
  # create a function in which learning exponential decreases at each distance from the trait
  #learn <- function(indices, weights, lr, lambda, RPE, nodeDist, degP){
    #weights[indices]= weights[indices] + lr * exp(-lambda * (nodeDist-1) )*RPE
    #weights[indices] = weights[indices] + lr * ( lambda/nodeDist )*RPE
    #weights[indices] = weights[indices] + (lr/(1 + lambda * nodeDist)) * RPE
    
    # if(as.character(degP)=="no"){
    #   weights[indices]= weights[indices] + lr * exp(-lambda * (nodeDist-1) )*RPE  
    # }else if(as.character(degP)=="out"){
    #   weights[indices]= weights[indices] + ( lr * exp(-lambda * (as.numeric(degree(graph, v = indices, mode = "out"))/69) * (nodeDist-1) )*RPE )
    #   #weights[indices]= weights[indices] + ( lr * exp(-lambda * nodeDist )*RPE ) * (1 - (as.numeric(degree(graph, v = indices, mode = "out"))/69) )
    # }else if(as.character(degP)=="in"){
    #   weights[indices]= weights[indices] + ( lr * exp(-lambda * ( 1 - (as.numeric(degree(graph, v = indices, mode = "in"))/61) ) * (nodeDist-1) )*RPE  )
    # }else if(as.character(degP)=="inout" || as.character(degP)=="outin"){
    #   weights[indices]= weights[indices] + ( lr * exp(-lambda * (as.numeric(degree(graph, v = indices, mode = "out"))/69) * ( 1 - (as.numeric(degree(graph, v = indices, mode = "in"))/61) ) *  (nodeDist-1) )*RPE  )
    # }
    
  
  learn <- function(weights, dist, lr, RPE){
    
    # if(as.character(degP)=="no"){

      weights = weights + (lr/ ( 1 + as.numeric(dist) ) ) * RPE
    #   
    # }else if(as.character(degP)=="out"){
    # 
    #   weights = weights + (lr/ ( 1 + as.numeric(dist) ) ) * RPE * (1-(as.numeric(degree(graph, mode = "out"))/69))
    # 
    # }else if(as.character(degP)=="in"){
    #   
    #   #weights = weights + (lr/ ( 1 + as.numeric(dist) ) ) * RPE * (as.numeric(degree(graph, mode = "in"))/61)
    #   weights = weights + (lr/ ( 1 + as.numeric(dist) ) ) * RPE * as.numeric(degree(graph, mode = "in"))
    #   
    # }else if(as.character(degP)=="inout" || as.character(degP)=="outin"){
    #   
    #   weights = weights + (lr/ ( 1 + as.numeric(dist) ) ) * RPE * (1-(as.numeric(degree(graph, mode = "out"))/69)) * as.numeric(degree(graph, mode = "in"))
    # 
    # }
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
  
  V_S = 4
  
  for (n in 1:N) { # for each trial in all of the trials
    
    S = V(graph)[feat[n]] # current trait
    # # extract nodes at each order/distance from current trait
    # nodes <- 1:5 %>%
    #   map(~ ego(graph, order = .x, nodes = S, mode = as.character(type), mindist = .x))
    # # extract node indices (nodes is character string/igraph object)
    # nodInds <- 1:5 %>%
    #   map(~ as.vector(nodes[[.x]][[1]]))
    # # extract number of elements at each order/distance
    # nodeDists <- 1:5 %>%
    #   map(~ length(nodInds[[.x]]))
    # # if node has no outdegree connections, set to maximum distance as 0
    # if(all(nodeDists==0)){
    #   maxDist=0
    # # else maximum distance is the last order/distance with values
    # }else{
    #   maxDist <- max(which(nodeDists!=0))
    # }
    
    if(as.character(cue)=="single"){
      traits <- S
    }else if(as.character(cue)=="out"){
      traits <- as.vector(ego(graph, order = 1, nodes = S, mode = "out", mindist = 0)[1][[1]])
    }else if(as.character(cue)=="in"){
      traits <- as.vector(ego(graph, order = 1, nodes = S, mode = "in", mindist = 0)[1][[1]])
    }else if(as.character(cue)=="all"){
      traits <- as.vector(ego(graph, order = 1, nodes = S, mode = "all", mindist = 0)[1][[1]])
    }
    V_PE = floorCeiling(mean(wpe[traits])) # value estimate is the associative weight of the current trait
    RPE = rew[n] - V_PE
    
    s[n, ] <- similarity(graph, method="dice", mode= "all" )[S, 1:148] # similarity of all traits to current trait
    
    #curSim <- s[1:N, S] # similarity vector of current trait
    if(n>1){
      #s[s==0] <- .00000000001
    curSim <- s[1:(n-1), S] # similarity vector of current trait
    #curSim <- s[feat[1:n]] # similarity vector of current trait
    #if(n>1){
      ws <- apply(s, MARGIN = 2, function(x) sum(x[1:(n-1)] * rew[1:(n-1)]) / sum(x[1:(n-1)]) ) # apply weighted sum to each column
    #}
      if(sum(curSim)!=0){
        V_S = sum(curSim * rew[1:(n-1)]) / sum(curSim) # value estimate is weighted sum of current trait
      }else if(sum(curSim)==0){
        V_S = mean(rew[1:(n-1)])
      }
    }
    S_PE = rew[n] - V_S
    
    VE = V_PE*(mix) + V_S*(1-mix)
    
    # if(modulate[1]=="PE"){
    #   RPE = rew[n] - V # reward prediction error is the reward of current trait minus the value estimate of current trait
    #   if(modulate[2]=="out"){
    #     RPE = ( (RPE)*(1-(as.numeric(degree(graph, v = S, mode = "out"))/69)) )
    #   }else if(modulate[2]=="in"){
    #     RPE = ( (RPE)*((as.numeric(degree(graph, v = S, mode = "in"))/61)) )
    #   }else if(modulate[2]=="outin"){
    #     RPE = ( (RPE)*((as.numeric(degree(graph, v = S, mode = "in"))/61))*(1-(as.numeric(degree(graph, mode = "out"))/69)) )
    #   }else{
    #     RPE <- RPE
    #   }
    # }else if(modulate[1]=="feed"){
    #   feed = rew[n]
    #   if(modulate[2]=="out"){
    #     feed = ( (feed-4)*(1-(as.numeric(degree(graph, v = S, mode = "out"))/69)) ) + 4
    #   }else if(modulate[2]=="in"){
    #     feed = ( (feed-4)*((as.numeric(degree(graph, v = S, mode = "in"))/61)) ) + 4
    #   }else if(modulate[2]=="outin"){
    #     feed = ( (feed-4)*((as.numeric(degree(graph, v = S, mode = "in"))/61))*(1-(as.numeric(degree(graph, mode = "out"))/69)) ) + 4
    #   }else{
    #     feed <- feed
    #   }
    #   RPE = feed - V # reward prediction error is the reward of current trait minus the value estimate of current trait
    # }
    
    if (RPE <= 0) { # if the reward prediction error is negative, use negative learning rate
      lr = neg_lr # use neg learning rate
    } else { # if the reward prediction error is positive, use the positive learning rate
      lr = pos_lr # use pos learning rate
    }
    # # update value for current trait
    # w[S] <- w[S] + lr * RPE
    # # if there are outdegree connections
    # if(maxDist!=0){ # apply graph learning function
    #   learnOutput <- 1:maxDist %>%
    #     map(~ learn(nodInds[[.x]], w, lr, lambda, RPE, .x, degP = as.character(degP) ))
    #   # for(k in 1:maxDist){
    #   #   w[nodInds[[k]]] <- learnOutput[[k]]
    #   # }
    #   w<-lapply(1:maxDist, function(i) replace(w, nodInds[[i]], learnOutput[[i]] ) )[[maxDist]] # update associative weights using output of graph learning function
    # }
    
    # if(length(type)==1){
    #   dist<-distances(posGraph, v = S, mode= as.character(type) )
    #   w <- learn(w, dist, lr, RPE, degP = as.character(degP) )
    # }else if(length(type)>1){
    #   dist<-distances(posGraph, v = S, mode= as.character(type[1]) )
    #   w <- learn(w, dist, lr, RPE, degP = as.character(degP) )
    #   dist<-distances(posGraph, v = S, mode= as.character(type[2]) )
    #   w <- learn(w, dist, lr, RPE, degP = as.character(degP) )
    # }
    
    dist<-distances(posGraph, v = S, mode= as.character(type) )
    wpe <- learn(wpe, dist, lr, RPE )
    
    trial = trialN[n]; # total trial number over all features
    
    # append row containing trial data to output dataframe
    TD.Out[n, ] <- c(n, trial, wpe, ws, RPE, V_PE, S_PE, V_S, VE)
  }
  TD.Out <- as.data.frame(TD.Out)
  colnames(TD.Out) <- x
  colnames(s) <- paste0('s',1:148)
  TD.Out <- cbind(TD.Out, s)
  return(TD.Out)
}
