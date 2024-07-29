run_hcr <- function(model_list, p, name="Custom HCR"){
  
  # is the input model list a nested list of models, or a 1D list of models?
  type <- ifelse(class(model_list[[1]])=="list","nl",
                 ifelse(class(model_list[[1]])=="data.frame", "l", NA))
  
  # 1D list of models
  if(type=="l"){
    
    
    
  } else if(type=="nl"){
    
    
    
    
  } else{
    
    stop("ERROR: cannot detect class of model_list. Input should be a list of Dia models, or a nested list of Dia models.")
    
  }
  
  
}