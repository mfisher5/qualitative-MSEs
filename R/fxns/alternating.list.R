#' Merge lists into single alternating list.
#'
#' Merge two lists into a single list, which alternates
#' objects. Support function for re-formatting QNM output
#' for graphing.
#'
#' @param list1 list of QNM output
#' @param list2 list of QNM output
#' @param list3 optional third list.
#' @return list
#' 
#' @export

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