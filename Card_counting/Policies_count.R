# install.packages("ggplot2")
# install.packages("knitr")
library(ggplot2)
library(knitr)

#Decaying epsilon-greedy Q-learning implementation

alpha=0.1  
gamma=1  #discount 


#  approximately 49 counts that need to be accounted for
tot_count=49
cnt=seq(-24,24)
player_score = seq(2,20)
dealer_score = seq(1,10)


#generate all possible state 
grid=expand.grid(cnt,player_score,dealer_score)
S=paste(grid[,1],grid[,2],grid[,3],sep="-")   #construct rownames of Q table
# Set of actions, draw another cards or stick with the cards
A=c("D","S")

# reinitialize the Action-State matrix
reset_Qmatrix = function(){
Q <<- matrix(0,nrow=nrow(grid),ncol=2)
colnames(Q) <<- A
rownames(Q) <<- S
}


reset_Qmatrix()

# random strategy, benchmark
random_action = function(){
  return(sample(A,1))
}

# Cautious strategy, draw until sum of card is 11 then stop

draw = c()
for (i in seq(1,172,19)){
  
  draw=c(draw,seq(i,i+9))
}

cautious_strategy= function(state){

  if (state %in% draw){
    return("D")
    }
  else{
    return("S")
  }
}

table_C = function(){
  Table = matrix(nrow = 19,ncol = 10)
  colnames(Table) = seq(1,10)
  rownames(Table) = seq(2,20)
  for (i in 1:10){
    for (j in 1:10){
      Table[i,j] = "D"
    }
  }
  for (i in 11:19){
    for (j in 1:10){
      Table[i,j] = "S"
    }
  }
  return(kable(Table))
}





# epsilon-greedy strategy of the Q-learning method

#return the action that maximize the action-value function for a given state
#if both actions output the same value the agent choose randomly
# in epsilon% of the cases the agent choose randomly anyway (exploration)


choose_action = function(state){
  epsilon = 1/n_episodes   #Exploration factor
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

# Q-learning update 
Qlearning = function(){
  # cat("action is: ",action,"\n")
  Q[state,action] <<- Q[state,action] + alpha*(reward+gamma*max(Q[state1,])-Q[state,action])
}


#give the index of the Q table row corresponding to current state
row_Qmatrix = function(){
  if( score(p_hand) == 0){
    
  }
  else if (score(p_hand) == 21){
    
  }
  else{

    state <<- (counter+25)+tot_count*(score(p_hand)-1)-tot_count + 19*tot_count * d_hand[1] - 19*tot_count
  }
  return(state)
}

# Variance depending of the number of iteration
# var_plot= function(){
#   x=c(seq(1,99),seq(100,990,10),seq(1000,9500,500),seq(10000,12000,2000))
#   y=c()
#   k=1
#   for (i in x){
#     game(i,res = FALSE)
#     y[k]=n_win/n_game
#     k = k+1
#   }
#   dt = data.frame(x = x, y = y)
#   ggplot(dt, aes(x , y))+
#     geom_point(color="blue")+
#     ggtitle("")
# }
# var_plot()




# Learning curve, percentage of win according to number of iterations
# average performance on 30 games by default

average_win= function(sample = 30){
  x=c(seq(20,30),seq(100,990,10))#,seq(1000,9500,500),seq(10000,15000,1000))
  y=c()
  perf= c()
  k=1
  for (i in x){
    for(j in 1:sample){
    game(i,res = FALSE)
    perf[j] = n_win/n_game
    }
    y[k] = mean(perf)
    k = k+1
    print(i)
    }
    # plot(x,y)  #ggplot curve
    dt = data.frame(x = x, y = y)
    ggplot(dt, aes(x , y))+
      geom_line(color="darkblue")+
      geom_point(color="darkblue")+
      ggtitle("Learning curve")
  }









