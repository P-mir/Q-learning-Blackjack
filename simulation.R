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
  if (sum_hand()>21){
   bust = TRUE
   }
  else {bust=FALSE}
  return(bust)
}

#Score of the hand (0 if bust)
score = function(hand){
  if (is_bust==TRUE){
    return('ye')
  }
  else {
    return(sum_hand)
  }
}


step = function(action){
  if (action()){
    hand = append(hand,draw_card(deck))
    if (is_bust()){
      end = TRUE
      reward = -1
    }  
    else{
      end = FALSE
      reward = 0
    }
  }

  else{
    end = TRUE
    while (sum_hand(dealer_hand) < 17){
      d_hand = append(d_hand,draw_card(deck))
    }
    if (score(p_hand)>score(d_hand)){
      reward = 1
    }
    else if (score(p_hand)>score(d_hand)){
      reward = -1
    }
    else {reward = 0}
  }
}
  

# Initialize player and dealer hands

reset = function(){
  p_hand<<-draw_hand()
  d_hand<<-draw_hand()
  }
# --------GAME SIMULATION----------

party = function(){
  reset()
}


game = function(num_episode,report_every=100){
  game = 0
  win = 0
  loss = 0
  draw = 0
  for (i in num_episode){
    
  }
}

