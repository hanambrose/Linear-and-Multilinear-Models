# Function to run Newton Raphson Algorithm and estimate mu, sigma_mu and sigma_ep

Newton_Raphson_FN <- function(raw_data){
  require(psych)
y <-  raw_data$y_norm
unweighted_X <-  raw_data$X_design
clus <-  as.factor(raw_data$clus)

Z   <- model.matrix(~clus-1,data=clus)    

m_i_clus <- as.vector(colSums(Z))
m_i_clus_string <- as.vector(x=rep(x=(m_i_clus),times=m_i_clus))

# Starting values for algorithm.   
mu_old <- c( raw_data$mu_init[1])
sigsq_mu_old <- c( raw_data$sigsq_mu_init[1])
sigsq_ep_old <- c( raw_data$sigsq_ep_init[1])

repeat{
  P <- ( Z %*% sqrt(solve(diag(colSums(Z))))) %*% t(Z %*% (sqrt(solve(diag(colSums(Z))))))
  Q <- diag(nrow(raw_data)) -  P
  ai <-  diag(( sigsq_ep_old +( m_i_clus_string * sigsq_mu_old))^(-1))
  b <- sigsq_ep_old^(-1)
  V_iv <- ai %*% P + b * Q
  ai_rt <- diag(( sigsq_ep_old +( m_i_clus_string * sigsq_mu_old))^(-0.5))
  b_rt <- sigsq_ep_old^(-0.5)
  V_iv_half <-  ai_rt %*% P + b_rt * Q
  
  
unweighted_err <- (y - unweighted_X * as.vector(mu_old)) 

####  Estimating Equation 1

mu_new <- mu_old + 
              solve(
                      t(unweighted_X) %*% V_iv %*% unweighted_X 
                    ) %*% (
                            t(unweighted_X) %*% V_iv %*% unweighted_err
                           )

unweighted_err <- (y - unweighted_X * as.vector(mu_new)) 

num_sigsq_mu <- t(unweighted_err) %*% V_iv %*% (Z %*% t(Z) )  %*% V_iv %*% unweighted_err
den_sigsq_mu <- sum(diag( V_iv %*% (Z %*% t(Z) )))

num_sigsq_ep <- t(unweighted_err) %*% V_iv %*%  V_iv %*% unweighted_err
den_sigsq_ep <- sum(diag( V_iv))

####  Estimating Equation 2

sigsq_mu_new <- sigsq_mu_old * (num_sigsq_mu/den_sigsq_mu)

####  Estimating Equation 3

sigsq_ep_new <- sigsq_ep_old * (num_sigsq_ep/den_sigsq_ep)

#print(mu_new)
#print(sigsq_mu_new)
#print(sigsq_ep_new)   

    if(
       abs(mu_new - mu_old) < 0.000001  
       &&
       abs(sigsq_mu_new - sigsq_mu_old) < 0.000001
       &&
       abs(sigsq_ep_new - sigsq_ep_old) < 0.000001
       )
      {
      break
      }
else
{
  mu_old <- c(mu_new)
  sigsq_mu_old <- c(sigsq_mu_new)
  sigsq_ep_old <- c(sigsq_ep_new)
  
}

  }
conv_values <- (rbind(mu_new,sigsq_mu_new,sigsq_ep_new ))
rownames(conv_values) <- c("mu_new","sigsq_mu_new","sigsq_ep_new")

return(conv_values)

}