# Case 2 WILMA

start.time = Sys.time()
set.seed(100)
total_t = 0
rates = list('rate0' = 0.7, 'rate1' = 0.1)
lugg_min = 5
lugg_max = 10

update_location = function(location_list, updated_Q, seats_per_row, i){ # update row where person is at
  if (i != 1) {
    # the person either goes to its row or stuck at the row of the person in front - 1
    location_list[updated_Q[i]] = min(ceiling(updated_Q[i]/seats_per_row), location_list[updated_Q[i-1]]-1)
  } else {
    # head of the queue always goes to its row
    location_list[updated_Q[i]] = ceiling(updated_Q[i]/seats_per_row)
  }
  return(location_list)
}

gen_sit0 = function(rate0=0.7, min_lugg=5, max_lugg=10){ # sitting behavior dist. for no one blocking
  return(rexp(1, rate0) + runif(1, min = min_lugg, max = max_lugg))
}

gen_sit1 = function(rate1=0.1, min_lugg=5, max_lugg=10){ # sitting behavior dist. for 1 blocking
  return(rexp(1, rate1) + runif(1, min = min_lugg, max = max_lugg))
}

gen_clear = function(sitdown_time_list, seat_num, rates=list('rate0' = 0.7, 'rate1' = 0.1), lugg_min=5, lugg_max=10){ # update time for no block
  rate0 = rates[['rate0']]
  sitdown_time_list[seat_num] = t + gen_sit0(rate0, lugg_min, lugg_max)
  return(sitdown_time_list)
}
gen_middle = function(sitdown_time_list, seat_num, rates=list('rate0' = 0.7, 'rate1' = 0.1), lugg_min=5, lugg_max=10){ # check and update time if 2nd seat in
  rate1 = rates[['rate1']]
  if (!standing_list[seat_num-1]) { # if one person blocking
    sitdown_time_list[seat_num] = t + gen_sit1(rate1, lugg_min, lugg_max)
  } else { # no one blocking
    sitdown_time_list = gen_clear(sitdown_time_list, seat_num, rates, lugg_min, lugg_max)
  }
  return(sitdown_time_list)
}

# number of seats per side and row
seats_per_side = 3
seats_per_row = seats_per_side*2
# the queue for ALL SEATS IN ECONOMY
original_Q = sample(seats_per_row*33)

# ECON CLASS
for (i in seq(3)) { # for 3 zones: {window or middle or aisle}
  # BASIC DEFINITIONS
  # simulated time
  t = 0
  
  # change queue for {window or middle or aisle}
  if (i == 1){ # window zone
    updated_Q = original_Q[which((original_Q-3)%%seats_per_row==0 | (original_Q-6)%%seats_per_row==0)]
  } else if (i == 2){ # middle
    updated_Q = original_Q[which((original_Q-2)%%seats_per_row==0 | (original_Q-5)%%seats_per_row==0)]
  } else { # aisle
    updated_Q = original_Q[which((original_Q-1)%%seats_per_row==0 | (original_Q-4)%%seats_per_row==0)]
  }
  # when each person that are sitting down really sits down (cum. time mark)
  sitdown_time_list = rep(Inf, length(original_Q))
  # where each person is during each iteration (row #)
  location_list = rep(NA, length(original_Q))
  # whether each person has already started sitting down (binary: 0 FALSE sitting, 1 TRUE not yet)
  standing_list = rep(TRUE, length(original_Q))
  
  # SIMULATION
  # start iteration
  while (TRUE) {
    # initialization (only triggered once)
    if (t == 0) {
      # check every person during each iteration for zone (run time acceptable)
      for (i in 1:length(updated_Q)) {
        # whether the person is able to proceed to its row or stuck
        location_list = update_location(location_list, updated_Q, seats_per_row, i)
        
        seat_num = updated_Q[i]
        # if at seating row
        if (location_list[seat_num] == ceiling(seat_num/seats_per_row)) {
          sitdown_time_list = gen_clear(sitdown_time_list, seat_num)
          standing_list[seat_num] = FALSE
        }
      } # end initialization for
      updated_Q = updated_Q[updated_Q != which.min(sitdown_time_list)] # remove sat from queue
      t = min(sitdown_time_list) # update time
      sitdown_time_list[which.min(sitdown_time_list)] = Inf # reset sit down time
    } # end if t == 0 (first person boarding)
    
    # termination (when there's no one)
    else if (length(updated_Q) == 0) {
      break
    }
    
    # general (happens most of the time)
    else {
      for (i in 1:length(updated_Q)) {
        seat_num = updated_Q[i]
        # check still standing (those who haven't started sitting down)
        if (standing_list[seat_num]) {
          
          # whether the person is able to proceed to its row or stuck
          location_list = update_location(location_list, updated_Q, seats_per_row, i)
          # if at seating row
          if (location_list[seat_num] == ceiling(seat_num/seats_per_row)) {
            sitdown_time_list = gen_clear(sitdown_time_list, seat_num)
            standing_list[seat_num] = FALSE
          }
        }
      } # end for check all in updated_Q
      
      updated_Q = updated_Q[updated_Q != which.min(sitdown_time_list)] # remove sat from queue
      t = min(sitdown_time_list) # update time
      sitdown_time_list[which.min(sitdown_time_list)] = Inf # reset sit down time
    } # end else
  } # end while
  total_t = total_t + t
}
# BUSINESS CLASS
for (i in seq(1)) {
  # BASIC DEFINITIONS
  # simulated time
  t = 0
  # number of seats per side and row
  seats_per_side = 2
  seats_per_row = seats_per_side*2
  # the queue
  original_Q = sample(seats_per_row*5)
  updated_Q = original_Q
  # when each person that are sitting down really sits down 
  sitdown_time_list = rep(Inf, length(original_Q))
  # when each person is during each iteration
  location_list = rep(NA, length(original_Q))
  # whether each person has already started sitting down
  standing_list = rep(TRUE, length(original_Q))
  # SIMULATION
  # start iteration
  while (TRUE) {
    # initialization (only triggered once)
    if (t == 0) {
      # check every person during each iteration (run time acceptable)
      for (i in 1:length(updated_Q)) {
        # whether the person is able to proceed to its row or stuck
        location_list = update_location(location_list, updated_Q, seats_per_row, i)
        seat_num = updated_Q[i]
        # if the person successfully goes to its row (it can then start sitting down)
        if (location_list[seat_num] == ceiling(seat_num/seats_per_row)) {
          sitdown_time_list = gen_clear(sitdown_time_list, seat_num, rates, lugg_min, lugg_max)
          standing_list[seat_num] = FALSE
        }
      }
      updated_Q = updated_Q[updated_Q != which.min(sitdown_time_list)] # remove sat from queue
      t = min(sitdown_time_list) # increment to next t
      sitdown_time_list[which.min(sitdown_time_list)] = Inf # reset sit down time for those who have sat
    }
    # termination (when there's no one)
    else if (length(updated_Q) == 0) {
      break
    }
    # general (happens most of the time)
    else {
      for (i in 1:length(updated_Q)) {
        seat_num = updated_Q[i]
        # only update the info of those who haven't started sitting down, the rest has already been taken care of
        if (standing_list[seat_num]) {
          # whether the person is able to proceed to its row or stuck, update where the person is
          location_list = update_location(location_list, updated_Q, seats_per_row, i)
          # if at seating row
          if (location_list[seat_num] == ceiling(seat_num/seats_per_row)) {
            if (seat_num%%seats_per_side == 1) {
              sitdown_time_list = gen_clear(sitdown_time_list, seat_num, rates, lugg_min, lugg_max)
            }
            else {
              sitdown_time_list = gen_middle(sitdown_time_list, seat_num, rates, lugg_min, lugg_max)
            }
            standing_list[seat_num] = FALSE
          }
        }
      }
      updated_Q = updated_Q[updated_Q != which.min(sitdown_time_list)]
      t = min(sitdown_time_list)
      sitdown_time_list[which.min(sitdown_time_list)] = Inf
    }
  }
  total_t = total_t + t
}
print(total_t)
end.time = Sys.time()
round(end.time - start.time,2)