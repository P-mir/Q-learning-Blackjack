#Decaying epsilon-greedy Q-learning implementation


# install.packages("foreach")
# library(foreach)


# 
# last_state = c()
# last_action = c()
alpha=0.6  #learning rate, to optimize later
gamma=0.9  #discount 
epsilon = 0.1   #Exploration factor, reduce it after a while
#epsilon=1/num_episode # test to decrease linearly based to num_episode

action = c(1,0)
player_states= seq(2,21)
dealer_states = seq(1,10)
n_episodes=100


#generate all possible state (state space is 200)
grid=expand.grid(player_states,dealer_states)
S=paste(grid$Var1,grid$Var2,sep="-")
# Set of actions, draw another cards or stick with the cards
A=c("D","S")

# Action-State matrix
reset_Qmatrix = function(){
Q <<- matrix(0,nrow=200,ncol=2)
colnames(Q) <<- A
rownames(Q) <<- S
}


#epsilon-greedy strategy
#return the action that maximize the action-value function for a given state
#if both actions output the same value the agent choose randomly
# in epsilon% of the cases the agent choose randomly anyway (exploration)

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

# # Q = Q*(1-alpha) + alpha(reward + discount * utility of next state-current utility of the state)
Qlearning = function(){
  action = choose_action(state)
  Q[state,action] <<- Q[state,action] + alpha*(reward+gamma*max(Q[state1,])-Q[state,action])
}




choose_action(state)

