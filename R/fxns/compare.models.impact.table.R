##' Generate the dataframe and table plot used to display the impact of a perturbation
##'
##' This function constructs a dataframe that can be used to construct
##' a table, which shows the semi-qualitative outcome of
##' simulations in which a positive or negative
##' outcome occurs with a certain degree of certainty (strong/weak).
##' Models variables and perturbations can vary. 
##'
##'
##' \code{compare.models.impact.tbl} is an interactive fx based on
##' impact.barplot0 from SWotherspoon.
##'
##' @title Impact Table
##' @param sim.list the result from \code{system.simulate} as a **named** list
##' @param perturb.list a list of named vectors that indicates, for each simulation, which nodes were perturbed and the relative magnitude of the perturbation.
##' @param monitor.list a list of named vectors of signs (-1,0,1) or NA that indicates, for each simulation, the required outcome of the perturbation.
##' @param strong_lower (proportion of total) outcomes >= are treated as a strong response
##' @param weak_lower (proportion of total) outcomes >=, and < `strong_lower`, are treated as a weak response. outcomes < are equivocal
##' @param epsilon outcomes below this *in absolute magnitude* are treated as zero.
##' @param common.variables in the output data frame, include only variables that are present in both models
##' @param variable.key a dataframe with the variables to be included in the output dataframe. should have at least one column with the heading "variable"
##' @param variable.order the column name in the variable key to sort the variables by in the output dataframe (character)
##' @param plot Output a ggplot of the impact table as the third object in the list? If FALSE, writes out only dataframes. 
##' @param plot.perturbed In the plot, include the perturbed variables? Default is TRUE
##' @param common.perturbed In the plot, include only the variables perturbed in all models? Default is FALSE
##' @param main.axis In the plot, should the simulation names be on the X axis / top of table (default) or on the Y axis / left of table?
##' @export 
##' 
compare.models.impact.tbl <- function(sim.list,perturb.list=0,monitor.list=NA,
                                      strong_lower=0.8, weak_lower=0.6, epsilon=1.0E-5,
                                      common.variables=FALSE,variable.key=NA,variable.order=NA,
                                      plot=FALSE, main.axis="X",
                                      plot.perturbed=TRUE, common.perturbed=FALSE,
                                      table.palette=c("#04BC09", "#ABF1AD","#BBBBBB", "#dfc27d","#a6611a","black")) {
  
  
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
  variables_by_model     <- lapply(sim.list, get.node.names)
  all_sim_variables      <- unique(unlist(variables_by_model))
  common_sim_variables   <- Reduce(intersect,variables_by_model)
  all_perturbed_nodes    <- unique(names(unlist(perturb.list)))
  xclude_perturbed_nodes <- all_perturbed_nodes[!(all_perturbed_nodes %in% common_sim_variables)]
  
  
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
    colnames(results) <- c("decrease","no response", "increase")
    
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
      mutate(strength=ifelse(p_sim >= strong_lower, "strong",
                             ifelse(p_sim >= weak_lower, "weak","equivocal"))) %>%
      mutate(response=ifelse(change=="no response" & strength=="strong","None",
                             ifelse(strength=="equivocal","Equivocal",
                                    ifelse(strength=="strong" & change=="increase","Positive-Strong",
                                           ifelse(strength=="strong" & change=="decrease","Negative-Strong",
                                                  ifelse(strength=="weak" & change=="increase","Positive-Weak",
                                                         ifelse(strength=="weak" & change=="decrease","Negative-Weak",NA))))))) %>%
      # tell us which of the variables in the data frame were perturbed in the simulation
      mutate(node_to_perturb=ifelse(variable %in% perturbed_nodes,"y","n")) %>%
      mutate(model=sim.title)
    
    if(s==1){
      all.results.df <- results.df
    } else{
      all.results.df <- bind_rows(all.results.df, results.df)
    }
  }
  
  # Generate tables ---------------------------------------------------------
  
  # if there is a variable key, the variable type can be used to organize all variables listed in the key. perturbed nodes go at the end of the table
  if(any(!is.na(variable.key)) & common.variables==FALSE){
    variable.key <- variable.key %>% filter(variable %in% all_sim_variables) %>% # make sure there are no orphan variables in the key
      filter(!(variable %in% all_perturbed_nodes)) # take perturbed nodes out of the key
    if(!is.na(variable.order)){
      variable.key <- variable.key %>% arrange(across(starts_with(variable.order)))
    }
    ordered_variables <- c(all_perturbed_nodes, rev(variable.key$variable))
  }
  
  # if there is a key, common variables can be organized by variable type. perturbed nodes go at the end of the table
  if(any(!is.na(variable.key)) & common.variables==TRUE){
    variable.key <- variable.key %>% filter(variable %in% common_sim_variables) %>% # make sure there are only common variables in the key
      filter(!(variable %in% all_perturbed_nodes)) # take perturbed nodes out of the key
    if(!is.na(variable.order)){
      variable.key <- variable.key %>% arrange(across(starts_with(variable.order)))
    }
    ordered_variables <- c(all_perturbed_nodes, rev(variable.key$variable))
  }
  
  # if there is not a key, the variable order will be alphabetical. perturbed nodes go at the end of the table
  if(all(is.na(variable.key))){
    if(common.variables){
      message("only including common variables in output table.")
      ordered_variables <- c(all_perturbed_nodes, 
                             rev(sort(common_sim_variables[which(!(common_sim_variables %in% all_perturbed_nodes))])))
    } else{
      ordered_variables <- c(all_perturbed_nodes,
                             rev(sort(all_sim_variables[which(!(all_sim_variables %in% all_perturbed_nodes))])))
    }}
  
  # filter results for variables that should be included in the table (the data frame written out will still include all results)
  #    - only include shared variables (if specified, or if no variable key is provided)
  if(common.variables | any(!is.na(variable.key))){
    #  - include the perturbed variables?
    if(plot.perturbed){
      response_levels <- c("Positive-Strong","Positive-Weak","Equivocal","Negative-Weak","Negative-Strong","Perturbed")
      #- include all perturbed variables, or only those perturbed in all models?
      if(common.perturbed){
        table.results.df <- filter(all.results.df, variable %in% ordered_variables & !(variable %in% xclude_perturbed_nodes))
      }else{
        table.results.df <- filter(all.results.df, variable %in% ordered_variables)
      }
    }else{
      table.results.df <- filter(all.results.df, (variable %in% ordered_variables & !(variable %in% all_perturbed_nodes)))
      response_levels <- c("Positive-Strong","Positive-Weak","Equivocal","Negative-Weak","Negative-Strong")
      table.palette <- table.palette[1:5]
    }
  } else{
    #  - include the perturbed variables?
    if(plot.perturbed){
      response_levels <- c("Positive-Strong","Positive-Weak","Equivocal","Negative-Weak","Negative-Strong","Perturbed")
      #- include all perturbed variables, or only those perturbed in all models?
      if(common.perturbed){
        table.results.df <- filter(all.results.df, !(variable %in% xclude_perturbed_nodes))
      } else{
        table.results.df <- all.results.df
      }
    } else{
      table.results.df <- filter(all.results.df, !(variable %in% all_perturbed_nodes))
      response_levels <- c("Positive-Strong","Positive-Weak","Equivocal","Negative-Weak","Negative-Strong")
      table.palette <- table.palette[1:5]
    }
  }
  
  
  if(plot==FALSE){
    
    return(list(table.results.df, all.results.df))
    
  } else{
    
    plotdat <- table.results.df %>% mutate(response=ifelse(node_to_perturb=="y","Perturbed",response))
    plotdat %<>% mutate(no_input=ifelse(response=="None","None",NA)) %>%
      mutate(response=ifelse(response=="None",NA,response))
    
    plotdat$response <- factor(plotdat$response, levels=response_levels)
    if(length(names(sim.list)) != length(unique(names(sim.list)))){stop("ERROR: All models must have unique names!")}
    
    
    if(main.axis=="X"){
      plotdat$variable <- factor(plotdat$variable, levels=unique(ordered_variables))
      plotdat$model <- factor(plotdat$model,levels=names(sim.list))
      
      sq.hab.plot <- ggplot(plotdat) + 
        # show legend makes sure all colors are in key, even if that level isn't in the graph
        geom_point(aes(y=variable,x=model,fill=response),pch=22,size=8,color="grey60", show.legend=TRUE) +
        geom_point(aes(y=variable,x=model,pch=as.factor(no_input)),size=7,color="grey60") +
        labs(x="",y="") + 
        scale_fill_manual(name="Response",values=table.palette, drop=FALSE, na.translate=FALSE) +
        scale_shape_manual(name="",values=c(0),na.translate=FALSE) +
        scale_x_discrete(position = "top",expand = c(0.1, 0.1)) +
        theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=0))
    } else if(main.axis=="Y"){
      plotdat$variable <- factor(plotdat$variable, levels=rev(unique(ordered_variables)))
      plotdat$model <- factor(plotdat$model,levels=rev(names(sim.list))) 
      
      sq.hab.plot <- ggplot(plotdat) + 
        # show legend makes sure all colors are in key, even if that level isn't in the graph
        geom_point(aes(x=variable,y=model,fill=response),pch=22,size=8,color="grey60", show.legend=TRUE) +
        geom_point(aes(x=variable,y=model,pch=as.factor(no_input)),size=7,color="grey60") +
        labs(x="",y="") + 
        scale_fill_manual(name="Response",values=table.palette, drop=FALSE, na.translate=FALSE) +
        scale_shape_manual(name="",values=c(0),na.translate=FALSE) +
        scale_x_discrete(position = "top",expand = c(0.05, 0.05)) +
        theme_bw() + theme(axis.text.x=element_text(angle=45,hjust=0))
    }
    
    return(list(table.results.df, all.results.df, sq.hab.plot))
    
  }
  
}
