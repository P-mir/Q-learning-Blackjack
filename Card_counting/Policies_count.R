# install.packages("ggplot2")
# install.packages("knitr")
library(ggplot2)
library(knitr)

#Decaying epsilon-greedy Q-learning implementation

alpha=0.1  
gamma=1  #discount 


#  approximately 13 counts that need to be accounted for




# reinitialize the Action-State matrix
reset_Qmatrix = function(decks){
  
  if (decks==1){limit=10}
  if (decks==2){limit=10}
  if (decks==3){limit=20}
  if (decks==4 || decks==5){limit=20}
  if (decks==6){limit=22}
  if (decks==7){limit=24}
  if (decks==8){limit=26}
  if (decks==9){limit=28}
  if (decks==10){limit=30}
  
cnt=seq(-limit,limit)
player_score = seq(2,20)
dealer_score = seq(1,10)
 
max_cnt<<-max(cnt)
tot_count<<-length(cnt)
  
#generate all possible state 
grid<-expand.grid(cnt,player_score,dealer_score)
S=paste(grid[,1],grid[,2],grid[,3],sep="-")   #construct rownames of Q table

# Set of actions, draw another cards or stick with the cards
A<<-c("D","S")
Q <<- matrix(0,nrow=nrow(grid),ncol=2)
colnames(Q) <<- A
rownames(Q) <<- S
}

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
  Q[state,action] <<- Q[state,action] + alpha*(reward+gamma*max(Q[state1,])-Q[state,action])
  # cat("Q[",state,",",action,"]: ",Q[state,action] + alpha*(reward+gamma*max(Q[state1,])-Q[state,action]),"\n")
  }


#give the index of the Q-table row corresponding to current state
row_Qmatrix = function(){
  if( score(p_hand) == 0){
    
  }
  else if (score(p_hand) == 21){
    
  }
  else{

    state <<- counter+max_cnt+1+tot_count*(score(p_hand)-1)-tot_count + 19*tot_count * d_hand[1] - 19*tot_count
  }
  return(state)
}

# Variance depending of the number of iteration
var_plot= function(){
  x=c(seq(10000,12000,1000),seq(10000,100000,5000),seq(100000,1000000,100000),seq(1000000,10000000,1000000))
  y=c()
  k=1
  for (i in x){
    game(n_episodes=i,shuffle_every=0,decks=1,res=FALSE)
    y[k]=n_win/n_game
    k = k+1
  }
  dt = data.frame(x = x, y = y)
  ggplot(dt, aes(x , y))+
    geom_point(color="blue")+
    ggtitle("")+
    xlab("Games played")+
    ylab("Percentage of winnings")
}
var_plot()




# Learning curve, percentage of win according to number of iterations
# average performance on 30 games by default

learning_curve= function(sample = 20){
  x=c(seq(1000,3000,1000),10000,15000,25000,50000,100000,250000,500000)
  y=data.frame()
  perf= data.frame()
  k=1
  s=1
  for (d in c(1,3,8)){
  for (i in x){
    for(j in 1:sample){
    game(n_episodes=i,shuffle_every=0,decks=d,res=FALSE)
    perf[k,j] = n_win/n_game
    }
    y[s,k] = rowMeans(perf[k,])
    k = k+1
    print(i)
  }
    k=1
    s=s+1
  }
    # plot(x,y)  #ggplot curve
    dt = data.frame(x = x, y = y[1,])
    ggplot(dt, aes(x , y[1,]))+
      ggtitle("Learning curve")+
      xlab("Games played")+
      ylab("Percentage of winnings")+
      geom_line(aes(y = unlist(y[1,]),colour="1 Deck"))+
      geom_point(aes(y = unlist(y[1,]),colour="1 Deck"))+
      
      geom_line(aes(y = unlist(y[2,]), colour = "3 Decks"))+
      geom_point(aes(y = unlist(y[2,]), colour = "3 Decks"))+

      geom_line(aes(y = unlist(y[3,]), colour = "8 Decks"))+
      geom_point(aes(y = unlist(y[3,]), colour = "8 Decks"))
    

    
  }



 learning_curve()
 
 learning_curves= function(sample = 15){
   x=c(seq(1000,3000,1000),10000,15000,25000,50000,100000,500000,1000000,5000000)
   y=data.frame()
   perf= data.frame()
   k=1
   s=1
   for (S in c(1,3,5,0)){
     for (i in x){
       for(j in 1:sample){
         game(n_episodes=i,shuffle_every=S,decks=1,res=FALSE)
         perf[k,j] = n_win/n_game
       }
       y[s,k] = rowMeans(perf[k,])
       k = k+1
       print(i)
     }
     k=1
     s=s+1
   }
  #ggplot curve
   dt = data.frame(x = x, y = y[1,])
   ggplot(dt, aes(x , y[1,]))+
     ggtitle("Learning curve")+
     xlab("Games played")+
     ylab("Percentage of winnings")+
     geom_line(aes(y = unlist(y[1,]),"Shuffle every game"))+
     geom_point(aes(y = unlist(y[1,]),"Shuffle every game"))+
     
     geom_line(aes(y = unlist(y[2,]), "Shuffle every 3 games"))+
     geom_point(aes(y = unlist(y[2,]), "Shuffle every 3 games"))+
     
     geom_line(aes(y = unlist(y[3,]),"Shuffle every 5 games"))+
     geom_point(aes(y = unlist(y[3,]),"Shuffle every 5 games"))+
     
     geom_line(aes(y = unlist(y[4,]), "No deck shuffling"))+
     geom_point(aes(y = unlist(y[4,]),"No deck shuffling"))
 }

 
 learning_curves()





