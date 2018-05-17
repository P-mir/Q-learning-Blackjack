#deep Q-learning, approximating Q-table with ANN



# install.packages("keras")
library(keras)
# install_keras()

alpha=0.1
gamma=1
epsion=0.9

model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 2, input_shape = c(784)) %>% 
  layer_activation('relu') %>% 
  layer_dense(units = 10) %>% 
  layer_activation('relu') %>% 
  layer_dense(units = 4) %>% 
  layer_activation('linear') %>% 
    summary(model)


  model %>% compile(
    loss = 'mse',
    optimizer = optimizer_rmsprop(),
  )

# # Use the fit() function to train the model for 30 epochs using batches of 100 what:
#     
# history <- model %>% fit(
#   state, reward, 
#   epochs = 30, batch_size = 100, 
#   validation_split = 0.2,
    #verbose = 0
#   )
#     
# # The history object returned by fit() includes loss and accuracy metrics which we can plot:
# plot(history)
  
  # Generate predictions on new data:
  #   
  #   model %>% predict_classes(x_test)
 
  
   
  get_action = function(state){
    reward = model %>% predict_classes(#array of state)
      
    if (runif(1)<epsilon){
      return(sample(A,1))
    }
    else{
      if (max(Q[state,])==min(Q[state,])){
        return(sample(A,1))
      }
      else{
        return(colnames(Q)[which.max(Q[state,])])
      }
    }
  }
  
deep_Qlearning = function(){
  
}
  

# input are states
#predict reward AKA predict Q-table
#at each step the error is (predicted reward-actual reward)^2+
# 






# 
# library(foreach)
# library(doParallel)
# no_cores <- detectCores()
# cl=makeCluster(no_cores)
# clusterExport(cl, c("party", "draw_card", "draw_hand", "Qlearning","reset","reset_Qmatrix","random_action","cautious_strategy",
#                     "choose_action","table","table_C",'deck',"end","grid","S","A","draw"))
# registerDoParallel(cl)
# 
# foreach(i=1:n_episodes,.export = c("party", "draw_card", "draw_hand", "Qlearning","reset","reset_Qmatrix","random_action","cautious_strategy",
#                                    "choose_action","table","table_C",'deck',"end","grid","S","A","draw")) %dopar% {
#                                      