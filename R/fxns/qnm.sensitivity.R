qnm_sensitivity <- function(dia.mod, my.sim, perturb=c("HAB")){
  ##### From QPRESS #####
  # percent matrices accepted
  p.accept <- my.sim$accepted/my.sim$total
  
  # link weights: mean, min, max, sdev
  weight <- as.data.frame(my.sim$w) 
  weight <- weight %>%
    pivot_longer(cols=all_of(colnames(weight)),names_to="Edge",values_to="Value")
  
  edge_summary <- weight %>%
    group_by(Edge) %>%
    summarise(Mean=mean(Value),
              Min=min(Value),
              Max=max(Value),
              SDev=sd(Value)) %>%
    mutate(lowerSD=Mean-SDev,
           upperSD=Mean+SDev)
  
  ##### iGRAPH #####
  # Write function to create an igraph-ready adjacency matrix of the dia models
  create_adj = function (diamod) 
  {
    A <- QPress::adjacency.matrix(diamod)
    nodes <- QPress::node.labels(diamod)#grab the node names to assign to columns/rows
    rownames(A) <- nodes[c(1:length(nodes))] #assign row names
    colnames(A) <- nodes[c(1:length(nodes))] #assign column names
    A_trans <- t(A) #transpose so that igraph reads it correctly
    A_pos <- abs(A_trans) #absolute value because igraph doesn't do negatives (I think)
    igraph <- igraph::graph_from_adjacency_matrix(A_pos, mode = "directed", add.rownames = TRUE)
  }
  
  ig <- create_adj(diamod = dia.mod)
  # edge density
  ed <- edge_density(ig)
  # distance from each perturbed node to all other nodes in the graph
  for(i in seq(1,length(perturb))){
    # perturbed node from list
    tmp_perturb <- perturb[i]
    # calculate distances for all other nodes. distances calculates the length of all the shortest paths 
    tmpdist <- unlist(lapply(V(ig), function(x){
      distances(ig,v=x, to=tmp_perturb)
    }))
    tmpdat <- data.frame(perturbed=rep(tmp_perturb, length(tmpdist)),
                         shortest.path=tmpdist) %>%
      rownames_to_column("node")
    if(i==1){dist_dat<-tmpdat} else{
      dist_dat <- dist_dat %>% bind_rows(tmpdat)
    }
  }
  
  out <- list(p.accept,edge_summary,ed, dist_dat)
  names(out) <- c("percent.accepted","link.weight.df", "edge.density", "shortest.path.df")
  return(out)
}