prepare.dia <- function(dia_name="MSE-QNM_2023-Jun_00-constantF", add_constraints=c(), dia_path='data'){
  
  # Load the model
  m <- model.dia(paste0(dia_path,'/',dia_name,'.dia'))
  
  # Add negative self-feedbacks on all variables
  m <- enforce.limitation(m)
  
  # Edge restrictions (if applicable)
  if(length(add_constraints)==0){
    m.sampler=NULL
  } else{
    # Parse constraints
    mc <- parse.constraints(add_constraints,m)
    # Create sampler for community matrices
    m.sampler <- community.ordering.sampler(mc)
  }
  
  return(list(m,m.sampler))
  
}