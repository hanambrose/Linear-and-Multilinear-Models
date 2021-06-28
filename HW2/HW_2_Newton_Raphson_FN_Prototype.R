# Function to run Newton Raphson Algorithm adapted for a general GLM problem
## Read data into function with responses & predictors

Newton_Raphson_FN <- function(raw_data){
  require(psych)
y <-  raw_data$y

X <-  raw_data$X_design
#clus <-  as.factor(raw_data$clus)

#Z   <- model.matrix(~clus-1,data=clus)    

#m_i_clus <- as.vector(colSums(Z))
#m_i_clus_string <- as.vector(x=rep(x=(m_i_clus),times=m_i_clus))

#### Starting values for algorithm.   The following should be initiated:

p_hat_0 <-  mean(y)

Nu_hat_0 <-  log(p_hat_0/(1-p_hat_0))

y_star_0 <- ## as per Taylot Series Approx  ##
  
W_0 <- ## Weight Matrix  

  
  beta_hat_new <- ## Compute from above
    
 
repeat{
 
  p_hat_new <- ## Compute from model using beta_hat_new
  Nu_hat_new <-  log(p_hat_new/(1-p_hat_new))
  y_star_new <- # Use above
  W_new <-     # Use p_hat_new etc.
    
  beta_hat_new1 <-   beta_hat_new + ## term that uses all new components above
    
    if(
       abs(beta_hat_new1 - beta_hat_new) < ### Convergence condition
       )
      {
      break
      }
else
{
  beta_hat_new <- c(beta_hat_new1)

}

  }
conv_values <- (beta_hat_new1)


return(conv_values)

}