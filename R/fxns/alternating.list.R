alternating.list <- function(list1,list2,list3=NULL){
  new_list <- list()
  j <- 1
  ## only two lists
  if(is.null(list3)){
    for(i in seq(1,length(list1))){
      new_list[[j]] <- list1[[i]]
      new_list[[j+1]] <- list2[[i]]
      j <- j+2
    }
  } else{
    ## three lists
    for(i in seq(1,length(list1))){
      new_list[[j]] <- list1[[i]]
      new_list[[j+1]] <- list2[[i]]
      new_list[[j+2]] <- list3[[i]]
      j <- j+3
    }
  } # end 3 lists
  return(new_list)
}