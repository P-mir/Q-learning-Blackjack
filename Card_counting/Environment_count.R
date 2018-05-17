source("Policies.r")

d=1 #number of deck

# 1 = Ace, 2-10 = Number cards, Jack/Queen/King = 10
end = TRUE

#prendre une carte et la remetre a la fin ok 
#mélanger tous les x game ok
# counter at each moment the player discover a new card

counter=0
Count = function(infos="quiet"){
  
  for (i in 1:length(p_hand)){  
    if(p_hand[i] %in% c(10)){
      counter<<-counter-1
  
      if (infos == "loud"){
        cat("modify phand bcose of initial -1 \n")
      }
    }
    
    if(p_hand[i] %in% c(1,2,3,4)){
      counter<<-counter+1
      if (infos == "loud"){
        cat("update counter (player hand) +1 \n")
      }
    }
  }
    
    if(d_hand[1] %in% c(10)){counter<<-counter-1
  cat("modify dhand bcose of initial -1 \n")
    }
    if(d_hand[1] %in% c(1,2,3,4)){counter<<-counter+1
    cat("modify dhand bcose of initial +1","\n")
    }
}

#draw a card card from the top of the deck and put it at the end of the deck
draw_card = function(){
  card<<-deck[1]
  deck<<-deck[-1]
  deck<<-append(deck,card)
  return(card)
}



shuffle= function(deck){
  return(sample(deck))
}

draw_hand = function(){
  return(c(draw_card(),draw_card()))
  }


# total of the hand  ,   add condition for usable ace
sum_hand = function(hand){
  return(sum(hand))
}

#Is the hand higher than 21?
is_bust = function(hand){
  if (sum_hand(hand)>21){
   bust = TRUE
   }
  else {bust=FALSE}
return(bust)
}

#Score of the hand (0 if bust)
score = function(hand){
  if (is_bust(hand)){
    return(0)
  }
  else {
    return(sum_hand(hand))
  }
}

if(d_hand[1] %in% c(10)){counter<<-counter-1
cat("modify dhand bcose of initial -1 \n")
}
if(d_hand[1] %in% c(1,2,3,4)){counter<<-counter+1
cat("modify dhand bcose of initial +1","\n")
}

step = function(infos = "quiet",method = "Q"){
    
    state <<- row_Qmatrix()

    if (infos == "loud"){
      cat("old state: ",state,"\n")
    }
  if (infos == "loud"){
  cat("player cards:",p_hand,"\n")
  cat("dealer cards:",d_hand[1],"\n")
  cat("counter: ",counter,"\n")  
  }
    if (sum_hand(p_hand)==21){
      end <<- TRUE
      reward <<- 1
    }
    else{
      if (method == "Q"){
      action <<- choose_action(state)
      }
      else if (method =="R"){
        action <<- random_action()
      }
      else if (method =="C"){
        action <<- cautious_strategy(state)
      }
      
      cautious_strategy(state)
      
      if (action=="D"){
        p_hand <<- append(p_hand,draw_card())
        
        if (infos == "loud"){
        cat("draw a card, hand is",p_hand,"\n")
        }
        
        #update the counter each time the player draw a card
        if(p_hand[length(p_hand)] %in% c(10)){
          counter<<-counter-1
          
          if (infos == "loud"){
            cat("modify phand bcose of draw,-1 \n counter: ",counter)
          }
        }
        
        if(p_hand[length(p_hand)] %in% c(1,2,3,4)){
          counter<<-counter+1
          
          if (infos == "loud"){
            cat("modify phand bcose of draw,+1 \n counter: ",counter)
          }}
        
        
        if (is_bust(p_hand)){
          end <<- TRUE
          reward <<- -1
          if (infos == "loud"){
          print("bust")
          }
        }
        else if (sum_hand(p_hand)==21){
          end <<- TRUE
          reward <<- 1
          if (infos == "loud"){
          print("blackjack")
          }
        }
        else{
          end <<- FALSE
          reward <<- 0
          if (infos == "loud"){
          print('continue')
          }
        }
      }
    
      else{
        if (infos == "loud"){
        print('stop')
        }
        end <<- TRUE
        while (sum_hand(d_hand) < 17){
          d_hand <<- append(d_hand,draw_card())
          
          
          if (infos == "loud"){
          cat("dealer add a cart, hand is",d_hand,"\n")
          }
          
          #update counter each time the dealer draw a card
          if(d_hand[length(d_hand)] %in% c(10)){
            counter<<-counter-1
            if (infos == "loud"){
              cat("modify dhand bcose of draw,-1 \n counter: ",counter)
            }
          }
          if(d_hand[length(d_hand)] %in% c(1,2,3,4)){
            counter<<-counter+1
            if (infos == "loud"){
              cat("modify dhand bcose of draw,+1 \n counter: ",counter)
            }
          }
        }
        if (score(p_hand)>score(d_hand)){
          reward <<- 1
          if (infos == "loud"){
          print("win \n")
          }
        }
        else if (score(p_hand)<score(d_hand)){
          reward <<- -1
          if (infos == "loud"){
          print("loose \n")
          }
        }
        else {reward <<- 0}
      }
    }
    if (infos == "loud"){
  cat(reward,score(p_hand),score(d_hand),end,"\n")
    }
    state1 <<- row_Qmatrix()
    if (infos == "loud"){
      cat("new state: ",state1,"\n\n")
    } 
}
  
#Count number of win,loss,draw and games
count = function(){
  if (reward == 1){
  n_win <<- n_win+1
  } else if (reward == -1){
    n_loss <<- n_loss+1
  } else{
    n_draw <<- n_draw+1
  }
  n_game <<- n_game+1
  }

# Initialize player and dealer hands

reset = function(){
  p_hand<<-draw_hand()
  d_hand<<-draw_hand()
}

reset_stat = function(){
  n_game <<- 0
  n_win <<- 0
  n_loss <<- 0
  n_draw <<- 0
}

# --------GAME SIMULATION----------


# Run one game of blackjack, can print index of the states in the Q-table,before and after the game if infos="loud"
party = function(infos = "quiet",method = "Q"){
  
  if (infos=="loud"){cat("New party","\n")}
  reset()
  Count() #update counter after hand have been distributed
  end <<- FALSE
  while(end == FALSE){ 
  step(infos, method)
  Qlearning()
  }
}


 
# Simulate x number of game and output the result of the chosen strategy
game = function(n_episodes,infos = "quiet",method = "Q",shuffle_every=0,d=1,res = TRUE){
  reset_stat()
  reset_Qmatrix()
  counter<<-0
  deck <<- c(rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10),4*d))
  deck <<- shuffle(deck)
  # if (shuffle_every==0){shuffle_every=1e+20}
  n_episodes<<-n_episodes
  for (i in 1:n_episodes){
    #shuffle the deck and reset  the counter
    # if ((n_episodes %% shuffle_every)==0){
    #   shuffle_deck(deck)
    #   counter<<-0
    #   }
    party(infos, method)
    count()
  }
  
   if (res == TRUE){
   cat(n_win/n_game,"win: ",n_win,"loss: ",n_loss,"game: ",n_game,"draw",n_draw,"\n payoff: ",n_win-n_loss,"\n")
   }
  else if(method =="C"){
    return(table_C())
  }
}

#to see what's happening use "loud":          game(1000,"loud")
# to simulate a drunk player (Random choice): game(1000,method="R")
# To benchmark with a careful strategy:      game(10000,method="C")
#shuffle_every=3  shuffle the deck every 3 game
#To never shuffle the deck put shuffle_every=0 (default)
game(n_episodes=1000,infos = "loud",method="Q",shuffle_every=100,d=1,res=TRUE)

