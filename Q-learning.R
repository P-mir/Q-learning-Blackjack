#Decaying epsilon-greedy Q-learning implementation

alpha=0.1  
gamma=1  #discount 
epsilon = 0   #Exploration factor, reduce it after a while
#epsilon=1/num_episode # test to decrease linearly based to num_episode


player_states= seq(2,20)
dealer_states = seq(1,10)



#generate all possible state (state space is 190)
grid=expand.grid(player_states,dealer_states)
S=paste(grid$Var1,grid$Var2,sep="-")
# Set of actions, draw another cards or stick with the cards
A=c("D","S")

# reinitialize the Action-State matrix
reset_Qmatrix = function(){
Q <<- matrix(0,nrow=190,ncol=2)
colnames(Q) <<- A
rownames(Q) <<- S
}

#epsilon-greedy strategy
#return the action that maximize the action-value function for a given state
#if both actions output the same value the agent choose randomly
# in epsilon% of the cases the agent choose randomly anyway (exploration)

# random strategy, benchmark
random_action = function(){
  return(sample(A,1))
  }

# epsilon-greedy strategy of the Q-learning method
choose_action = function(state){
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



