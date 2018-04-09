#deep Q-learning, approximate Q-table with ANN



# install.packages("keras")
library(keras)
install_keras()

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


rms = RMSprop()
model.compile(loss='mse', optimizer=rms)