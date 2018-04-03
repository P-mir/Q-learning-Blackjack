#Decaying epsilon-greedy Q-learning implementation

install.packages("tidyverse")
install.packages("purrr")

library(purrr)

Q = []
last_state = c()
last_action = c()
alpha=0.1  #learning rate, to optimize later
gamma=0.9  #discount 
epsilon = 0.9   #Exploration factor, reduce it after a while
#epsilon=1/num_episode # test to decrease linearly based to num_episode

action = c(1,0)
player_states= seq(2,21)
dealer_states = seq(1,10)


#generate all possible state (state space is 200)
grid=expand.grid(player_states,dealer_states)
S=paste(grid$Var1,grid$Var2,sep="-")
# Set of actions, draw another cards of stop
A=c("D","S")

# Action-State matrix
Q=matrix(0,nrow=200,ncol=2)
colnames(Q)=A
rownames(Q)=S



