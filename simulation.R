# 1 = Ace, 2-10 = Number cards, Jack/Queen/King = 10
deck = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 10)


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


step = function(){
  end = FALSE
  while(end == FALSE){  
  cat("cards:",p_hand,"\n")
  
    if (sum_hand(p_hand)==21){
      end = TRUE
      reward = 1
    }
    else{
      if (choose_action(state)=="D"){
        p_hand <<- append(p_hand,draw_card(deck))
        cat("draw a card, hand is",p_hand)
        if (is_bust(p_hand)){
          end = TRUE
          reward = -1
          print("bust")
        }
        else if (sum_hand(p_hand)==21){
          end = TRUE
          reward = 1
          print("blackjack")
        }
        else{
          end = FALSE
          reward = 0
          print('continue')
        }
      }
    
      else{
        print('stop')
        end = TRUE
        while (sum_hand(d_hand) < 17){
          d_hand <<- append(d_hand,draw_card(deck))
          cat("dealer add a cart, hand is",d_hand,"\n")
        }
        if (score(p_hand)>score(d_hand)){
          reward = 1
          print("win \n")
        }
        else if (score(p_hand)<score(d_hand)){
          reward = -1
          print("loose \n")
        }
        else {reward = 0}
      }
    }
  
  cat(reward,score(p_hand),score(d_hand),end,"\n")
  }
}
  
#Count number of win,loss,draw and games
count = function(){
  if (reward == 1){
  n_win <<- n_win+1
  }
  else if (reward == -1){
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

#give the index of the row corresponding to current state
row_Qmatrix = function(){
  if (d_hand[1]==1){
    state=sum_hand(p_hand)-1
  } else if (d_hand[1]==2){
    state=19+sum_hand(p_hand)
  } else if (d_hand[1]==3){
    state=39+sum_hand(p_hand)
  } else if (d_hand[1]==4){
    state=59+sum_hand(p_hand)
  } else if (d_hand[1]==5){
    state=79+sum_hand(p_hand)
  } else if (d_hand[1]==6){
    state=99+sum_hand(p_hand)
  } else if (d_hand[1]==7){
    state=119+sum_hand(p_hand)
  } else if (d_hand[1]==8){
    state=139+sum_hand(p_hand)
  } else if (d_hand[1]==9){
    state=159+sum_hand(p_hand)
  } else if (d_hand[1]==10){
    state=179+sum_hand(p_hand)
  }
return(state)
}
# --------GAME SIMULATION----------


n_win = 0
n_loss = 0
n_draw = 0
party = function(){
  reset()
  state=row_Qmatrix()
  step()
}



game = function(n_episodes,report_every=100){
  n_game <<- 0
  n_win <<- 0
  n_loss <<- 0
  n_draw <<- 0
  for (i in n_episodes){
    party()
    count()
  }
  cat("win: ",n_win,"loss: ",n_loss,"game: ",n_game)

}

game(1000)
