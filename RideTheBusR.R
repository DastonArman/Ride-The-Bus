library(mosaic)
next_card_func <- function(card_vector){
  next_card_picked = sample(card_vector, 1)
  next_card_picked_index = match(next_card_picked, card_vector)
  card_vector = card_vector[-next_card_picked_index]
  
  if (length(card_vector) == 1) {
    card_vector = c(seq(1,13), seq(1,13))
  }
  
  return (list("next_card_picked" = next_card_picked, "card_vector" = card_vector))
}

a=1/13
(a*12/13) + (a*11/13) + (a*10/13) + (a*9/13) + (a*8/13) + (a*7/13) + (a*6/13) + (a*7/13) + (a*12/13) + (a*11/13) + (a*10/13) + (a*9/13) + (a*8/13)

n=100000
drunk_ballz = do(n)*{
  cards = c(seq(1,13), seq(1,13))
  slots = rep(0,7)
  slot_number = 1
  drinks = 0
  
  for (slot_index in 1:length(slots)){
    init_slot_card = sample(cards, 1)
    init_slot_card_index = match(init_slot_card, cards)
    cards = cards[-init_slot_card_index]
    slots[slot_index] = init_slot_card
  }
  
  next_card_list = next_card_func(cards)
  next_card = next_card_list$next_card_picked
  cards = next_card_list$card_vector
  
  while (slot_number != 8){
    if (slot_number == 1){
      while (slot_number == 1){
        if ((next_card > slots[1] & slots[1] < 7) | (next_card < slots[1] & slots[1] >= 7)){
          slots[1] = next_card
          next_card_list = next_card_func(cards)
          next_card = next_card_list$next_card_picked
          cards = next_card_list$card_vector
          slot_number = 2
        } else if (slots[1] == next_card) {
          slots[1] = next_card
          next_card_list = next_card_func(cards)
          next_card = next_card_list$next_card_picked
          cards = next_card_list$card_vector
        } else {
          slots[1] = next_card
          drinks = drinks + 1
          next_card_list = next_card_func(cards)
          next_card = next_card_list$next_card_picked
          cards = next_card_list$card_vector
        }
      }
    } else if (slot_number == 2) {
      while (slot_number == 2){
        if ((next_card > slots[2] & slots[2] < 7) | (next_card < slots[2] & slots[2] >= 7)){
          slots[2] = next_card
          next_card_list = next_card_func(cards)
          next_card = next_card_list$next_card_picked
          cards = next_card_list$card_vector
          slot_number = 3
        } else if (slots[2] == next_card) {
          slots[2] = next_card
          next_card_list = next_card_func(cards)
          next_card = next_card_list$next_card_picked
          cards = next_card_list$card_vector
        } else {
          slots[2] = next_card
          drinks = drinks + 1
          next_card_list = next_card_func(cards)
          next_card = next_card_list$next_card_picked
          cards = next_card_list$card_vector
          slot_number = 1
        }
      }
    } else if (slot_number == 3) {
      while (slot_number == 3){
        if ((next_card > slots[3] & slots[3] < 7) | (next_card < slots[3] & slots[3] >= 7)){
          slots[3] = next_card
          next_card_list = next_card_func(cards)
          next_card = next_card_list$next_card_picked
          cards = next_card_list$card_vector
          slot_number = 4
        } else if (slots[3] == next_card) {
          slots[3] = next_card
          next_card_list = next_card_func(cards)
          next_card = next_card_list$next_card_picked
          cards = next_card_list$card_vector
        } else {
          slots[3] = next_card
          drinks = drinks + 1
          next_card_list = next_card_func(cards)
          next_card = next_card_list$next_card_picked
          cards = next_card_list$card_vector
          slot_number = 1
        }
      }
    } else if (slot_number == 4) {
      while (slot_number == 4){
        if ((next_card > slots[4] & slots[4] < 7) | (next_card < slots[4] & slots[4] >= 7)){
          slots[4] = next_card
          next_card_list = next_card_func(cards)
          next_card = next_card_list$next_card_picked
          cards = next_card_list$card_vector
          slot_number = 5
        } else if (slots[4] == next_card) {
          slots[4] = next_card
          next_card_list = next_card_func(cards)
          next_card = next_card_list$next_card_picked
          cards = next_card_list$card_vector
        } else {
          slots[4] = next_card
          drinks = drinks + 1
          next_card_list = next_card_func(cards)
          next_card = next_card_list$next_card_picked
          cards = next_card_list$card_vector
          slot_number = 1
        }
      }
    } else if (slot_number == 5) {
      while (slot_number == 5){
        if ((next_card > slots[5] & slots[5] < 7) | (next_card < slots[5] & slots[5] >= 7)){
          slots[5] = next_card
          next_card_list = next_card_func(cards)
          next_card = next_card_list$next_card_picked
          cards = next_card_list$card_vector
          slot_number = 6
        } else if (slots[5] == next_card) {
          slots[5] = next_card
          next_card_list = next_card_func(cards)
          next_card = next_card_list$next_card_picked
          cards = next_card_list$card_vector
        } else {
          slots[5] = next_card
          drinks = drinks + 1
          next_card_list = next_card_func(cards)
          next_card = next_card_list$next_card_picked
          cards = next_card_list$card_vector
          slot_number = 1
        }
      }
    } else if (slot_number == 6) {
      while (slot_number == 6){
        if ((next_card > slots[6] & slots[6] < 7) | (next_card < slots[6] & slots[6] >= 7)){
          slots[6] = next_card
          next_card_list = next_card_func(cards)
          next_card = next_card_list$next_card_picked
          cards = next_card_list$card_vector
          slot_number = 7
        } else if (slots[6] == next_card) {
          slots[6] = next_card
          next_card_list = next_card_func(cards)
          next_card = next_card_list$next_card_picked
          cards = next_card_list$card_vector
        } else {
          slots[6] = next_card
          drinks = drinks + 1
          next_card_list = next_card_func(cards)
          next_card = next_card_list$next_card_picked
          cards = next_card_list$card_vector
          slot_number = 1
        }
      }
    } else {
      while (slot_number == 7){
        if ((next_card > slots[7] & slots[7] < 7) | (next_card < slots[7] & slots[7] >= 7)){
          slots[7] = next_card
          slot_number = 8
        } else if (slots[7] == next_card) {
          slots[7] = next_card
          next_card_list = next_card_func(cards)
          next_card = next_card_list$next_card_picked
          cards = next_card_list$card_vector
        } else {
          slots[7] = next_card
          drinks = drinks + 1
          next_card_list = next_card_func(cards)
          next_card = next_card_list$next_card_picked
          cards = next_card_list$card_vector
          slot_number = 1
        }
      }
    }
  }
  drinks
}

hist(drunk_ballz$result[drunk_ballz$result<=32], breaks=30, freq=FALSE, col='light blue', 
     xlab='# of Drinks', main='Relative Frequency Histogram of # of Drinks while Riding the Bus')

num_drink_x = seq(0:30)
cum_dist_y = rep(0,31)

for (num_drink in 0:30){
  cum_dist = length(drunk_ballz$result[drunk_ballz$result >= num_drink]) / length(drunk_ballz$result)
  cum_dist_y[num_drink+1] = cum_dist
}

df = data.frame(num_drink_x, cum_dist_y)
plot(df$num_drink_x, df$cum_dist_y, pch=19, main='Cumulative propability of drinking a certain amount or more', 
     xlab='# of Drinks', ylab='Cumulative Probability', yaxs="i", xaxs='i', xlim=c(0,30))
lines(df$num_drink_x, df$cum_dist_y, lwd=1)
grid(15,10)
