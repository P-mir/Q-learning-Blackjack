# install.packages("ggplot2")
# install.packages("knitr")
library(ggplot2)
library(knitr)

# 1 = Ace, 2-10 = Number cards, Jack/Queen/King = 10
deck = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10)
end=FALSE

draw_card = function(deck){
  return(sample(deck,1))
  }

draw_hand = function(){
  return(c(draw_card(deck),draw_card(deck)))
  }

# usable ace function

# usable_ace(hand){
#   if (1 %in% hand){
#     
#   }
# }

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


step = function(infos = "quiet",method = "Q"){
 
    state <<- row_Qmatrix()
    if (infos == "loud"){
      cat("old state: ",state,"\n")
    }
  if (infos == "loud"){
  cat("cards:",p_hand,"\n")
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
        p_hand <<- append(p_hand,draw_card(deck))
        if (infos == "loud"){
        cat("draw a card, hand is",p_hand)
        }
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
          d_hand <<- append(d_hand,draw_card(deck))
          if (infos == "loud"){
          cat("dealer add a cart, hand is",d_hand,"\n")
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
#give the index of the Q table row corresponding to current state
row_Qmatrix = function(){
  if( score(p_hand) == 0){
    
  }
  else if (score(p_hand) == 21){
    
  }
  else{
    state <<- score(p_hand) + 19 * d_hand[1] - 20
  }
return(state)
}
# --------GAME SIMULATION----------


# Run one game of blackjack, can print index of the states in the Q-table,before and after the game
party = function(infos = "quiet",method = "Q"){
  reset()
  
  end <<- FALSE
  while(end == FALSE){ 
  step(infos, method)
  Qlearning()
  }
}



game = function(n_episodes,infos = "quiet",method = "Q",res = TRUE){
  reset_stat()
  reset_Qmatrix()
  for (i in 1:n_episodes){
    party(infos, method)
    count()
  }
  
   if (res == TRUE){
   cat(n_win/n_game,"win: ",n_win,"loss: ",n_loss,"game: ",n_game,"draw",n_draw,"\n payoff: ",n_win-n_loss,"\n")
   }
  if(method =="Q"){
   table(Q) #table of learned policy
  }
  else if(method =="C"){
    table_C()
  }
}

#to see what's happening use "loud":          game(1000,"loud")
# to simulate a drunk player (Random choice): game(1000,method="R")
# To benchmark with a carefull strategy:      game(10000,method="C")
game(10000,method="Q")


#note work even without exploration... bug or interesting?
