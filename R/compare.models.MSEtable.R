##' Generate a table of simulation output for comparison to quant MSEs
##'
##' This function constructs a data frame designed to match
##' the tabular output from quantitative MSEs in Punt et al.
##' It can provide BOTH qualitatively coded simulation output 
##' as ("S neg","W neg","Equiv,"W pos","S pos"), 
##' and the percent of total simulations with a 
##' negative / positive response (whichever has majority); or
##' just the qualitatively coded simulation output.
##' Models and perturbations can vary. 
##'
##' The user may then use the dataframe 
##' to construct tables or graphs outside of this function.
##'
##'
##' @title Qual Sim Table
##' @param sim.list the result from \code{system.simulate} as a **named** list
##' @param perturb.list a list of named vectors that indicates, for each simulation, which nodes were perturbed and the relative magnitude of the perturbation.
##' @param monitor.list a list of named vectors of signs (-1,0,1) or NA that indicates, for each simulation, the required outcome of the perturbation.
##' @param strong_lower (proportion of total) outcomes >= are treated as a strong response
##' @param weak_lower (proportion of total) outcomes >=, and < `strong_lower`, are treated as a weak response. outcomes < are equivocal
##' @param epsilon outcomes below this *in absolute magnitude* are treated as zero.
##' @param variable.key if NA, all variables written out. a dataframe with the variables to be included in the output dataframe. should have at least one column with the heading **variable**
##' @param qual if TRUE, only the qualitatively coded simulation output is written out. if FALSE, simulation output includes % of total simulations with majority response type
##' @param no.response for quantitative output. Specify a string or number to write over the 'p_sim' values of all 'no response' variables. Suggested either 'NA' or '0'. 
##' @export
compare.models.MSEtable <- function(sim.list,perturb.list,monitor.list=NA,
                         strong_lower=0.8, weak_lower=0.6, epsilon=1.0E-5,
                         variable.key=NULL, qual=FALSE, no.response=NULL) {

  
  ## Utility functions
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
  
  get.node.names <- function(sim){
    node.names <- unique(c(as.character(sim$edges$From),as.character(sim$edges$To)))
    return(node.names)
  }
  ##
  

  # Get variables in each model ---------------------------------------------
  variables_by_model    <- lapply(sim.list, get.node.names)
  all_sim_variables     <- unique(unlist(variables_by_model))
  common_sim_variables  <- Reduce(intersect,variables_by_model)
  all_perturbed_nodes <- unique(names(unlist(perturb.list)))

  
  
  # Summarise simulation outcomes -------------------------------------------
  
  for(s in seq(1,length(sim.list))){
    
    sim       <- sim.list[[s]]
    perturb   <- perturb.list[[s]]
    monitor   <- monitor.list[[s]]
    sim.title <- names(sim.list)[s]
    
    As <- sim$A
    nodes <- node.labels(sim$edges)
    results <- matrix(0,length(nodes),3)
    perturbed_nodes <- names(perturb)
    
    perturb <- extend.vector(perturb,nodes,0)
    monitor <- extend.vector(monitor,nodes,NA)
    
    # create matrix with number of simulations showing increase/decrease/no response (columns) for each variable (rows)
    for(i in seq_along(As)) {
      impact <- signum(drop(As[[i]]%*%perturb),epsilon=epsilon)
      if(all(monitor==impact,na.rm=T)) {
        results <- results + outer(impact,-1:1,'==')
      }
    }
    rownames(results) <- nodes
    colnames(results) <- c("neg","no response", "pos")
    
    # create dataframe from matrix
    results.df <- results %>% as.data.frame() %>%
      rownames_to_column(var="variable") %>%
      pivot_longer(cols=c(2,3,4), names_to="change",values_to="n_sim") %>%
      # get the proportion of total simulations that fall under increase / decrease / no response
      group_by(variable) %>%
      mutate(total_sim=sum(n_sim)) %>%
      ungroup() %>%
      mutate(p_sim=n_sim/total_sim) %>%
      # grab the direction with the greatest proportion of simulations
      group_by(variable) %>%
      arrange(desc(p_sim)) %>%
      # top_n(n=1,wt=p_sim) %>%  # top_n allows duplicates when wt is tied (50/50 outcomes)
      slice(which.max(p_sim)) %>%
      # classify the strength and name the response
      mutate(strength=ifelse(change=="no response",NA,
                             ifelse(p_sim >= strong_lower, "S",
                                    ifelse(p_sim >= weak_lower, "W","Equiv")))) %>%
      mutate(change2=ifelse(!is.na(strength) & strength=="Equiv",NA,change)) %>%
      unite(strength,change2,col="response",sep=" ", remove=TRUE, na.rm=TRUE) %>%
      mutate(node_to_perturb=ifelse(variable %in% perturbed_nodes,"y","n")) %>%
      mutate(model=sim.title) %>%
      dplyr::select(-n_sim, -total_sim) %>% mutate(p_sim=100*p_sim)
      
      if(qual){
        results.df <- dplyr::select(results.df, variable, response, node_to_perturb, model)
      } else if(!is.null(no.response)){
        results.df <- results.df %>% mutate(p_sim=ifelse(change=="no response", no.response, p_sim))
      }
    
    if(s==1){
      all.results.df <- results.df
    } else{
      all.results.df <- bind_rows(all.results.df, results.df)
    }
  }
  
  # Return table ---------------------------------------------------------
  
  # if there is a variable key, the variable type can be used to filter variables in the output table.
  if(!is.null(variable.key)){
    all.results.df <- all.results.df %>% filter(variable %in% variable.key$variable)
  } 
  
  
  return(all.results.df)

}

