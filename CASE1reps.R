# CASE 1 BASE 3 blocks econ

start.time = Sys.time()

n = 1000
times = rep(0, n)

rates = list('rate0' = 0.7, 'rate1' = 0.1, 'rate2' = 0.05)
lugg_min = 5
lugg_max = 10

init = function(location_list, sitdown_time_list, standing_list, updated_Q, seats_per_row){
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
  return(list(location_list, sitdown_time_list, standing_list))
}

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

gen_sit2 = function(rate2=0.05, min_lugg=5, max_lugg=10){ # sitting behavior dist. for 2 blocking
  return(rexp(1, rate2) + runif(1, min = min_lugg, max = max_lugg))
}

gen_clear = function(sitdown_time_list, seat_num, rates=list('rate0' = 0.7, 'rate1' = 0.1, 'rate2' = 0.05), lugg_min=5, lugg_max=10){ # update time for no block
  rate0 = rates[['rate0']]
  sitdown_time_list[seat_num] = t + gen_sit0(rate0, lugg_min, lugg_max)
  return(sitdown_time_list)
}
gen_middle = function(sitdown_time_list, seat_num, rates=list('rate0' = 0.7, 'rate1' = 0.1, 'rate2' = 0.05), lugg_min=5, lugg_max=10){ # check and update time if 2nd seat in
  rate1 = rates[['rate1']]
  if (!standing_list[seat_num-1]) { # if one person blocking
    sitdown_time_list[seat_num] = t + gen_sit1(rate1, lugg_min, lugg_max)
  } else { # no one blocking
    sitdown_time_list = gen_clear(sitdown_time_list, seat_num, rates, lugg_min, lugg_max)
  }
  return(sitdown_time_list)
}
gen_window = function(sitdown_time_list, seat_num, rates=list('rate0' = 0.7, 'rate1' = 0.1, 'rate2' = 0.05), lugg_min=5, lugg_max=10){ # check and update time if 3nd seat in
  rate1 = rates[['rate1']]
  rate2 = rates[['rate2']]
  if (!standing_list[seat_num-1] & !standing_list[seat_num-2]) { # two people blocking
    sitdown_time_list[seat_num] = t + gen_sit2(rate2, lugg_min, lugg_max)
  }
  else if (!standing_list[seat_num-1] | !standing_list[seat_num-2]) { # one person blocking
    sitdown_time_list[seat_num] = t + gen_sit1(rate1, lugg_min, lugg_max)
  }
  else { # no one blocking
    sitdown_time_list = gen_clear(sitdown_time_list, seat_num, rates, lugg_min, lugg_max)
  }
  return(sitdown_time_list)
}

for (rep in seq(n)){
  total_t = 0
  # ECON CLASS
  for (i in seq(3)) {
    # BASIC DEFINITIONS
    # simulated time
    t = 0
    # number of seats per side and row
    seats_per_side = 3
    seats_per_row = seats_per_side*2
    # the queue
    original_Q = sample(seats_per_row*11)
    updated_Q = original_Q
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
        all_lists = init(location_list, sitdown_time_list, standing_list, updated_Q, seats_per_row)
        location_list = all_lists[[1]]
        sitdown_time_list = all_lists[[2]]
        standing_list = all_lists[[3]]
        
        updated_Q = updated_Q[updated_Q != which.min(sitdown_time_list)] # remove sat from queue
        t = min(sitdown_time_list) # update time
        sitdown_time_list[which.min(sitdown_time_list)] = Inf # reset sit down time
      } # end if
      
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
            
            # whether the person is able to proceed to its row or stuck
            location_list = update_location(location_list, updated_Q, seats_per_row, i)
            
            # check if at seating row
            if (location_list[seat_num] == ceiling(seat_num/seats_per_row)) {
              if (seat_num%%seats_per_side == 1) {
                sitdown_time_list = gen_clear(sitdown_time_list, seat_num)
              } # end aisle
              else if (seat_num%%seats_per_side == 2) {
                sitdown_time_list = gen_middle(sitdown_time_list, seat_num)
              } # end middle
              else {
                sitdown_time_list = gen_window(sitdown_time_list, seat_num)
              } # end window
              standing_list[seat_num] = FALSE # update i sit
            } # end if at seating row and to be seated
          } # end if still standing (not sitting)
        } # end for check all in updated_Q
        
        updated_Q = updated_Q[updated_Q != which.min(sitdown_time_list)] # remove sat from queue
        t = min(sitdown_time_list) # update time
        sitdown_time_list[which.min(sitdown_time_list)] = Inf # reset sit down time
      }
    } # end while
    total_t = total_t + t
  } # end econ
  
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
        all_lists = init(location_list, sitdown_time_list, standing_list, updated_Q, seats_per_row)
        location_list = all_lists[[1]]
        sitdown_time_list = all_lists[[2]]
        standing_list = all_lists[[3]]
        
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
                sitdown_time_list = gen_clear(sitdown_time_list, seat_num)
              }
              else {
                sitdown_time_list = gen_middle(sitdown_time_list, seat_num)
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
  } # end business
  
  times[rep] = total_t
}

summary(times)
cat('variance: ', var(times), '\n')
cat('stdev.: ', sd(times), '\n')
conf_int = t.test(times)$conf.int
cat('conf.int.: ', conf_int, '\n')
end.time = Sys.time()
round(end.time - start.time,2)