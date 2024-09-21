##' Construct matrix with the response to a press perturbation
##'
##' From system.simulate output, return a large matrix containing
##'    the perturbation response, with variables in columns and each
##'    iteration as a row. For example, a dia object with 20 nodes 
##'    and a simulation that generated 10,000 stable matrices would
##'    have 20 columns and 10,000 rows.
##'
##'
##' @title Report System Simulate Impact
##' @param sim the result from \code{system.simulate}
##' @param perturb a named vector that indicates which nodes were perturbed and the relative magnitude of the perturbation.
##' @param monitor a named vector of signs (-1,0,1) or NA that indicates the outcome of the perturbation.
##' @export
impact.ss <- function(sim,perturb=0,monitor=NA,epsilon=1.0E-5, track_sims=5000) {
  
  
  ## Utility function
  extend.vector <- function(named,nodes,default) {
    if(is.null(names(named)))
      v <- rep(named,length.out=length(nodes))
    else {
      k <- match(names(named),nodes)
      if(any(is.na(k)))
        warning("Unknown nodes:",paste(names(named)[is.na(k)],collapse=" "))
      v <- rep(default,length(nodes))
      v[k] <- named
    }
    v
  }
  ##
  
  s <- Sys.time()
  # Summarise simulation outcomes -------------------------------------------
  As <- sim$A                                         # list of community matrices
  nodes <- node.labels(sim$edges)                     # node names
  
  Wlog <- matrix(nrow=1,ncol=length(sim$edges$From))
  Wlog <- as.data.frame(Wlog)
  exA <- QPress::adjacency.matrix(sim$edges,required.groups = c(0,4))
  whichW <- which(exA != 0)   # which pairwise matches of nodes have an interaction in the qnm
  if(!(length(whichW)==dim(Wlog)[2])){stop("ERROR: you have uncertain edges in your model. make sure these edges are required when generating the sample community matrix, exA.")}
  tempedgenames <- matrix("",nrow=dim(exA)[1],ncol=dim(exA)[2])
  for (i in 1:dim(sim$edges)[1]) {
    tempedgenames[sim$edges$To[i],sim$edges$From[i]] <- paste(sim$edges$From[i],sim$edges$To[i],sep='_')
  }
  colnames(Wlog) <- tempedgenames[whichW]             # to get edge responses. column names of Wlog are the edges
  
  allimp <- matrix(0,length(As),length(nodes))        # to get node responses
  colnames(allimp) <- nodes
  
  perturb <- extend.vector(perturb,nodes,0)
  monitor <- extend.vector(monitor,nodes,NA)
  
  message("Extracting simulation info...")
  # create matrix with number of simulations showing increase/decrease/no response (columns) for each variable (rows)
  for(i in seq_along(As)) {
    ## pull the node responses
    impact <- signum(drop(As[[i]]%*%perturb),epsilon=epsilon)
    allimp[i,] <- impact
    
    ## pull the edge values
    Wlog[i,] <- As[[i]][whichW]
    
    if(i%%track_sims==0){message(i, " sims logged.")}
  }
  message(round(Sys.time()-s))
  
  return(list(allimp,Wlog))
}
