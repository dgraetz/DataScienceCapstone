#v2: controlling for guessing probability
library(zoo)
library(tidyverse)

# result <- run_IE_sim(SwitchP = 0.1,
#            ITI = 0,
#            Blockdur = 150,
#            Win = 1,
#            Loss = 1,
#            RT_nC = 1,
#            RT_CC = 3,
#            ITI_randomness = 0,
#            reps = 10000)
# 
# agg <- result %>%
#   group_by(CheckP) %>%
#   summarize(rew = mean(final_reward)) %>%
#   ungroup() %>%
#   mutate(relrew = rew/max(rew))
# 
# ggplot(agg, aes(x = CheckP, y = relrew))+
#   geom_line()+
#   theme_classic()

# IE simulation ---

run_IE_sim <- function(SwitchP, 
                       ITI, 
                       Blockdur, 
                       Win, 
                       Loss, 
                       RT_nC, 
                       RT_CC, 
                       ITI_randomness = 0,
                       CheckP = seq(0, 1, 0.01),
                       reps = 100,
                       quiet = TRUE,
                       guess = 0,
                       seed = 04261997){
  
  # SwitchP = 0.1
  # ITI = 0.2
  # Blockdur = 10
  # Win = 1
  # Loss = 1
  # RT_nC =1
  # RT_CC = 1.5
  # ITI_randomness = 0
  # Check = seq(0, 1, 0.01)
  # reps = 10000
  # quiet = TRUE
  # 
  
  Loss <- abs(Loss)
  
  if (is.na(RT_nC)|is.na(RT_CC)){
    results <- NA
    if (!quiet){print("Reaction times contain NAs. Simulation halted.")}
    return(results)
  }
  
  Trials <- (Blockdur/min(RT_nC, RT_CC)) + 1 #max trials that can be run
  #Trials <- (Blockdur/RT_CC)+10
  
  
  #Start an Experiment
  
  set.seed(seed)
  
  # exp <- data.frame(
  #   Block = rep(1:(reps*length(SwitchP)*length(CheckP)*Check_p_reps), each = Trials) ,
  #   SwitchP = rep(SwitchP, each = (Trials*reps*length(CheckP)*Check_p_reps)),
  #   rep = rep(1:reps, each = Trials, length.out = (Trials*reps*length(SwitchP)*length(CheckP)*Check_p_reps)),
  #   CheckP = rep(CheckP, each = Trials*Check_p_reps, length.out = (Trials*reps*length(SwitchP)*length(CheckP)*Check_p_reps)),
  #   Check_p_reps = rep(1:Check_p_reps, each = Trials)
  # )
  
  Blocks <- length(SwitchP) * reps * length(CheckP)
  
  exp <- expand.grid(
    Trial = 1:Trials,
    rep = 1:reps,
    CheckP = CheckP
  )
  
  exp <- exp %>% 
    select(-Trial) %>%
    mutate(Block = rep(1:Blocks, each = Trials)) %>%
    relocate(Block, CheckP, rep) %>%
    group_by(Block) %>%
    mutate(Switches = c(1, sample(c(0, 1), (n()-1), replace = TRUE, prob = c(1-SwitchP, SwitchP))),
           Tasks = NA)
  
  exp$Tasks[exp$Switches == 1] <- rep(c("A", "B"), length.out = sum(exp$Switches))
  
  exp <- exp %>%
    group_by(Block) %>%
    fill(Tasks) %>%
    mutate(Checks = c(1, sample(c(0, 1), n() - 1, replace = TRUE, prob = c(1-CheckP[1], CheckP[1]))),
           Task_in_memory = NA)
  
  exp$Task_in_memory[exp$Checks == 1] <- exp$Tasks[exp$Checks == 1]
  
  exp <- exp %>%
    group_by(Block) %>%
    fill(Task_in_memory) %>%
    mutate(Correct = ifelse(Task_in_memory == Tasks, 1, 0),
           Reward = ifelse(Correct == 1, abs(Win), -abs(Loss)),
           Reward_cum = cumsum(Reward),
           RTs = ifelse(Checks == 1, RT_CC, RT_nC) + ITI, #+ rnorm(n(), mean = 0, sd = ITI_randomness),
           RTs_cum = cumsum(RTs),
           final_reward = Reward_cum[which(RTs_cum >= Blockdur)[1]],
           Trials_completed = which(RTs_cum >= Blockdur)[1])
  
  
  result <- exp %>%
    group_by(Block, CheckP, rep, final_reward, Trials_completed) %>%
    summarize(Error = 1-mean(Correct)) %>%
    slice(1) %>%
    select(final_reward, Trials_completed, Error) %>%
    group_by(CheckP) %>%
    summarize(final_reward = mean(final_reward)) %>%
    mutate(max_reward = max(final_reward),
           rel_reward = final_reward/max_reward)
  
  return(result)
  
}

sim_sidetask_vec <- function(RT_A_only,
                             RT_A_B_check,
                             RT_A_B_response,
                             ITI, 
                             Win_A,
                             Win_B,
                             Loss_B,
                             p_go_on,
                             p_go_off,
                             Time,
                             cr,
                             reps = 10){
  
  
  #FOR DEBUGGING
  # RT_A_only <- 1
  # RT_A_B_check <- 1.1
  # RT_A_B_response <- 1.5
  # 
  # ITI <- 0.2
  # 
  # Win_A <- 1
  # 
  # Win_B <- 1
  # Loss_B <- 10
  # 
  # p_go_on <- 0.9
  # p_go_off <- 0.5
  # 
  # Time <- 50
  # 
  # cr <- seq(0, 1, by = 0.1)
  # 
  # reps <- 100
  # 

  Loss_B <- abs(Loss_B)
  
  RT_min <- min(c(RT_A_only, RT_A_B_check, RT_A_B_check)) #only to determine the trials to simulate
  min_trials <- ceiling(Time/RT_min)
  
  Blocks <- reps * length(cr)
  
  exp <- expand.grid(
    Trial = 1:min_trials,
    rep = 1:reps,
    cr = cr
  )
  
  exp <- exp %>% 
    #select(-Trial) %>%
    mutate(Block = rep(1:Blocks, each = min_trials)) %>%
    relocate(Block, cr, rep) %>%
    group_by(Block)%>%
    mutate(checks = c(1, sample(c(0, 1), n() - 1, replace = TRUE, prob = c(1-cr[1], cr[1]))))
  
  
  state_B <- integer(nrow(exp))
  rand_vec <- runif(nrow(exp))
  
  for (i in 1:nrow(exp)) {
    # Every 'min_trials', we force a reset (start of new simulation)
    if (exp$Trial[i] == 1) {
      current_state <- 0
    } else {
      if (current_state == 0) {
        # Check transition 0 -> 1
        if (rand_vec[i] < p_go_on) {
          current_state <- 1
        }
      } else {
        # Check transition 1 -> 0
        if (rand_vec[i] < p_go_off) {
          current_state <- 0
        }
      }
    }
    state_B[i] <- current_state
  }
  
  exp$state_B <- state_B
  
  
  exp <- exp %>%
    group_by(Block) %>%
    mutate(state_on_first = ifelse(lag(state_B) == 0 & state_B == 1, 1, 0),
           state_id = cumsum(state_on_first),
           state_id = ifelse(state_B == 0, 0, state_id)) %>%
    group_by(Block, state_id) %>%
    mutate(state_checked = cumsum(checks),
           state_checked = ifelse(state_checked > 0, 1, 0),
           state_checked = ifelse(state_B == 0, 0, state_checked),
           on_missed_tmp = ifelse(state_checked[n()] == 0, 1, 0),
           on_missed_tmp = ifelse(state_B == 0, 0, on_missed_tmp),
           on_missed = 0,
           on_missed = replace(on_missed, n(), on_missed_tmp[1])) %>%
    group_by(Block) %>%
    mutate(Rew = case_when(checks == 0 & on_missed == 1 ~ Win_A - abs(Loss_B),
                           checks == 1 & state_B == 1 & state_checked == 0 ~ Win_A + Win_B, 
                           .default = Win_A),
           Rew_total = cumsum(Rew),
           RT = case_when(checks == 0 ~ RT_A_only + ITI,
                          checks == 1 & state_checked == 0 ~ RT_A_B_response + ITI,
                          checks == 1 & state_checked == 1 ~ RT_A_B_check + ITI),
           timeout = ifelse(cumsum(RT) > Time, 1, 0)) %>%
    filter(timeout == 0)
  
  result <- exp %>%
    group_by(rep, cr) %>%
    summarize(Rew = Rew_total[n()]) %>%
    group_by(cr) %>%
    summarize(final_reward = mean(Rew)) %>%
    ungroup() %>%
    mutate(max_reward = max(final_reward),
           rel_reward = final_reward/max_reward)
  
  
  return(result)
}


# comp_sidetask <- function(RT_A_only, 
#                           RT_A_B_check, 
#                           RT_A_B_response, 
#                           ITI, 
#                           Win_A, 
#                           Win_B, 
#                           Loss_B, 
#                           p_go_on, 
#                           p_go_off, 
#                           Time, 
#                           cr){
#   
#   Win_A <- abs(Win_A)
#   Win_B <- abs(Win_B)
#   Loss_B <- abs(Loss_B)
#   
#   results <- data.frame(cr = cr) %>%
#     mutate(final_reward = NA)
#   
#   for (i in 1:length(cr)){
#     
#     cr_i <- results$cr[i]
#     
#     # (Steady State Markov)
#     # Pi_off: Proportion of time world is in "OFF" state
#     # Pi_on:  Proportion of time world is in "ON" state
#     Pi_off <- p_go_off / (p_go_on + p_go_off)
#     Pi_on  <- 1 - Pi_off
#     
#     # p_event: Probabfility of a new event STARTING on any given trial
#     p_event <- Pi_off * p_go_on
#     
#     # A miss occurs if we fail to check (1-cr) for every trial the monster is alive, we're using a geometric interpolation heree
#     q <- 1 - cr_i
#     P_miss_event <- (p_go_off * q) / (1 - (1 - p_go_off) * q)
#     
#     # Handle edge case where cr = 1 (P_miss must be 0)
#     if(cr_i == 1) P_miss_event <- 0
#     
#     P_catch_event <- 1 - P_miss_event
#     
#     # Base Reward (A) + (Prob of Catch * Bonus) - (Prob of Miss * Penalty)
#     # Note: Win_A happens every trial. Bonus/Penalty happen once per event.
#     E_Reward_Trial <- Win_A + 
#       (p_event * P_catch_event * Win_B) - 
#       (p_event * P_miss_event * Loss_B)
#     
#     # We calculate the weighted average of time spent.
#     # T_base:  Time when NOT checking
#     # T_check: Time when Checking (standard check cost)
#     # T_extra: The extra cost of responding (RT_response - RT_check) happens 
#     #          exactly once per CAUGHT event.
#     
#     T_base  <- (1 - cr_i) * (RT_A_only + ITI)
#     T_check <- cr_i * (RT_A_B_check + ITI)
#     
#     #If we *do* catch a monster, one of those checks was actually a response.
#     # We add the difference between Response Time and Check Time.
#     T_response_adjustment <- p_event * P_catch_event * (RT_A_B_response - RT_A_B_check)
#     
#     E_Time_Trial <- T_base + T_check + T_response_adjustment
#     
# 
#     # How many trials fit in the total Time,. then apply proportinal expected rewards
#     Expected_Trials <- Time / E_Time_Trial
#     Total_Reward <- Expected_Trials * E_Reward_Trial
#     
#     results$final_reward[i] <- Total_Reward
#   
#   }
#   
#   results <- results %>%
#     mutate(max_reward = max(final_reward),
#            rel_reward = final_reward/max_reward)
#   
#   
#   return(results)
# }


# --- EarningsWhenChecking ---
EarningsWhenChecking <- function(
    BaselineRT, CheckRT, Win, Loss, Delay, SwitchP,
    TTime = 60, ITI = .2, k_max = 1000
) {
  
  Loss <- abs(Loss)
  stopifnot(is.finite(BaselineRT), BaselineRT > 0,
            is.finite(CheckRT),    CheckRT >= 0,
            is.finite(Delay),      Delay >= 0,
            is.finite(SwitchP),    SwitchP >= 0, SwitchP <= 1)
  ps   <- SwitchP
  k <- 1:k_max
  runP <- 0.5 * ( (1 - 2*ps)^k + 1 )
  payoff_per_trial <- runP * Win + (1 - runP) * Loss
  avg_payoff_by_k <- ifelse(
    k > 1,
    ( (k - 1) * payoff_per_trial + Win ) / k,
    Win
  )
  probabilities <- seq(1.00, 0.01, by = -0.01)
  earnings <- numeric(length(probabilities))
  for (i in seq_along(probabilities)) {
    p <- probabilities[i]
    w <- p * (1 - p)^(k - 1)
    w <- w / sum(w)
    avg_payoff_p <- sum(w * avg_payoff_by_k)
    denom_with_p <- TTime / (BaselineRT + ITI + p * ((CheckRT - BaselineRT) + Delay))
    earnings[i] <- avg_payoff_p * denom_with_p
  }
  list(
    earnings       = earnings,
    probabilities  = probabilities,
    avg_payoff_by_k = avg_payoff_by_k
  )
}




#UPDATED Side Task Computation!!

markov_state <- function(mat_initial_state,
                         mat_transition,
                         n) {
  
  library(expm)
  mat_initial_state %*% (mat_transition %^% n) %>%
    as.data.frame() %>%
    #rename(p_state_A = V1, p_state_A_detected = V2, p_state_B = V3) %>%
    return()
  
}

build_mats <- function(p_A2B = 0.1,
                       p_B2A = 0.1, 
                       cr_a = 0.2,
                       cr_b = 0.2,
                       start = "B_mB"){
  
  trans_mat <- matrix(0, 5, 5)
  
  # p_A2B <- 0.1
  # p_B2A <- 0.1
  # cr_a <- 0.1
  # cr_b <- 0.01
  
  names <- c("Au_mA", "Au_mB", "Ad_mA", "B_mA", "B_mB")
  
  colnames(trans_mat) <- names
  rownames(trans_mat) <- names
  
  trans_mat["Au_mA", "Au_mA"] <- (1 - p_A2B) * (1 - cr_a) #for it to remain undetected, I can't check
  trans_mat["Au_mA", "Au_mB"] <- 0 #memory can't update from A to B without 
  trans_mat["Au_mA", "Ad_mA"] <- (1 - p_A2B) * cr_a #for state to update from detected to undetected, you need to check
  trans_mat["Au_mA", "B_mA"] <- p_A2B * (1 - cr_a) # state changes from A to B, but your memory remains the same (you don't check)
  trans_mat["Au_mA", "B_mB"] <- p_A2B * cr_a #state changes from A to B, you check to update memory
  
  trans_mat["Au_mB", "Au_mA"] <- 0 #memory update impossible without checking 
  trans_mat["Au_mB", "Au_mB"] <- (1 - p_A2B) * (1 - cr_b) #state is not allowed to change, and you dont check
  trans_mat["Au_mB", "Ad_mA"] <- (1 - p_A2B) * cr_b # A state remains, you check
  trans_mat["Au_mB", "B_mA"] <- 0 # impossible transition (can't update memory)
  trans_mat["Au_mB", "B_mB"] <- p_A2B # it has to change to B, whether you check or not doesn't matter, the memory remains the same
  
  trans_mat["Ad_mA", "Au_mA"] <- 0 #impossible with just one transition
  trans_mat["Ad_mA", "Au_mB"] <- 0 #if I were to check, the memory wouldnt update to B in this case
  trans_mat["Ad_mA", "Ad_mA"] <- 1 - p_A2B #no matter whether you check or not, neither state nor memory change
  trans_mat["Ad_mA", "B_mA"] <- p_A2B * (1 - cr_a) # state changes without noticing (no checking)
  trans_mat["Ad_mA", "B_mB"] <- p_A2B * cr_a #(state chanegs and I update memory)
  
  trans_mat["B_mA", "Au_mA"] <- p_B2A * (1-cr_a) # state changes but you dont check (because otherwise state would be Ad)
  trans_mat["B_mA", "Au_mB"] <- 0 #impossible memory transition
  trans_mat["B_mA", "Ad_mA"] <- p_B2A * cr_a
  trans_mat["B_mA", "B_mA"] <- (1 - p_B2A) * (1 - cr_a)
  trans_mat["B_mA", "B_mB"] <- (1 - p_B2A) * cr_a
  
  trans_mat["B_mB", "Au_mA"] <- 0 #impossible to update memory from B to A if not also changing A undetected state to detected state
  trans_mat["B_mB", "Au_mB"] <- p_B2A * (1 - cr_b) # state changes, memory doesnt get updated, therefore no check allowed
  trans_mat["B_mB", "Ad_mA"] <- p_B2A * cr_b # state transitions AND I also check
  trans_mat["B_mB", "B_mA"] <- 0 #impossible to update memory from B to A if state doesnt change
  trans_mat["B_mB", "B_mB"] <- 1 - p_B2A #state doesnt change, regardless of whether i check or not
  
  initial_mat <- matrix(0, nrow = 1, ncol = 5)
  colnames(initial_mat) <- names
  
  initial_mat[1, "Au_mA"] <- 0
  initial_mat[1, "Au_mB"] <- 0
  initial_mat[1, "Ad_mA"] <- 0
  initial_mat[1, "B_mA"] <- 0
  initial_mat[1, "B_mB"] <- 0
  initial_mat[1, start] <- 1
  
  return(list(trans_mat = trans_mat,
              initial_mat = initial_mat))
}



get_states <- function(p_A2B = 0.1, 
                       p_B2A = 0.1, 
                       cr_a = seq(0, 1, by = 0.01),
                       cr_b = seq(0, 1, by = 0.01),
                       N_Trials = 90){
  
  
  # p_A2B = 0.1
  # p_B2A = 0.1
  # cr_a = seq(0, 1, by = 0.01)
  # cr_b = seq(0, 1, by = 0.01)
  # RT_nc = 0.5
  # RT_c = 1.5
  # ITI = 0.1
  # Win = 1
  # Loss = 10
  # TTime = 90
  #N_Trials = ceiling(TTime/RT_nc)
  
  data.frame(setup = 1) %>%
    mutate(trial = list(1:N_Trials),
           p_A2B = p_A2B, 
           p_B2A = p_B2A, 
           cr_a = list(cr_a = cr_a),
           cr_b = list(cr_b = cr_b),
    ) %>%
    select(-setup) %>%
    unnest(cr_a) %>%
    unnest(cr_b) %>%
    rowwise() %>%
    mutate(mats = list(build_mats(p_A2B = p_A2B, 
                                  p_B2A = p_B2A, 
                                  cr_a = cr_a, 
                                  cr_b = cr_b, 
                                  start = "B_mB"))) %>%
    unnest(trial) %>%
    rowwise() %>%
    arrange(cr_a, cr_b, trial) %>%
    mutate(states = list(markov_state(mat_initial_state = mats$initial_mat,
                                      mat_transition = mats$trans_mat, 
                                      n = trial))) %>%
    ungroup() %>%
    select(-mats) %>%
    #mutate(states = as.data.frame(states))
    unnest(states) %>%
    mutate(A_total = Au_mA + Au_mB + Ad_mA,
           Au_total = Au_mA + Au_mB,
           inst_miss = Au_total * p_A2B,
           B_total = B_mA + B_mB,
           Checks = (Au_mA + Ad_mA + B_mA) * cr_a + (Au_mB + B_mB) * cr_b)
  
}


get_payout <- function(states,
                       RT_c,
                       RT_nc,
                       ITI,
                       Win,
                       Loss,
                       TTime){
  
  
  # states <- x$states[1][[1]]
  # RT_c = 1
  # RT_nc= 1
  # ITI =1
  # Win= 1
  # Loss =1
  # TTime =1
  
  states %>%
    mutate(expected_RT = RT_nc + Checks * (RT_c - RT_nc) + ITI,
           expected_Rew = Win - (inst_miss * Loss)) %>%
    group_by(cr_a, cr_b) %>%
    mutate(cum_RT = cumsum(expected_RT),
           cum_Rew = cumsum(expected_Rew)) %>%
    filter(cum_RT > TTime) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(RT_correction = cum_RT - expected_RT, # subtract current RT
           Time_left = TTime - RT_correction, #how much time was left from previous trial to block end
           p_Trial_completed = Time_left/expected_RT, #this is the proportion of the last trial that was completed
           Rew_corrected = cum_Rew - ((1 - p_Trial_completed) * expected_Rew),#to correct, I am subtracting the reward associated with the incomplete portion of the trial 
           is_max = ifelse(cum_Rew == max(cum_Rew), 1, 0),
           is_max_same = ifelse(cr_a == cr_b & cum_Rew == max(cum_Rew[cr_a == cr_b]), 1, 0)) %>%
    select(cr_a, cr_b, Rew_corrected, is_max, is_max_same) %>%
    return()
  
}




# get_states_crgrid <- function(p_A2B = 0.1, 
#                               p_B2A = 0.1, 
#                               cr_grid,
#                               N_Trials = 90){
#   
#   
#   # p_A2B = 0.1
#   # p_B2A = 0.1
#   # cr_a = seq(0, 1, by = 0.01)
#   # cr_b = seq(0, 1, by = 0.01)
#   # RT_nc = 0.5
#   # RT_c = 1.5
#   # ITI = 0.1
#   # Win = 1
#   # Loss = 10
#   # TTime = 90
#   #N_Trials = ceiling(TTime/RT_nc)
#   
#   data.frame(setup = 1) %>%
#     mutate(trial = list(1:N_Trials),
#            p_A2B = p_A2B, 
#            p_B2A = p_B2A, 
#            cr_a = list(cr_a = cr_grid$cr_a),
#            cr_b = list(cr_b = cr_grid$cr_b),
#     ) %>%
#     select(-setup) %>%
#     unnest(cr_a) %>%
#     unnest(cr_b) %>%
#     rowwise() %>%
#     mutate(mats = list(build_mats(p_A2B = p_A2B, 
#                                   p_B2A = p_B2A, 
#                                   cr_a = cr_a, 
#                                   cr_b = cr_b, 
#                                   start = "B_mB"))) %>%
#     unnest(trial) %>%
#     rowwise() %>%
#     arrange(cr_a, cr_b, trial) %>%
#     mutate(states = list(markov_state(mat_initial_state = mats$initial_mat,
#                                       mat_transition = mats$trans_mat, 
#                                       n = trial))) %>%
#     ungroup() %>%
#     select(-mats) %>%
#     #mutate(states = as.data.frame(states))
#     unnest(states) %>%
#     mutate(A_total = Au_mA + Au_mB + Ad_mA,
#            Au_total = Au_mA + Au_mB,
#            inst_miss = Au_total * p_A2B,
#            B_total = B_mA + B_mB,
#            Checks = (Au_mA + Ad_mA + B_mA) * cr_a + (Au_mB + B_mB) * cr_b)
#   
# }


#this version actually uses the cr grid
get_states_crgrid <- function(p_A2B = 0.1, 
                              p_B2A = 0.1, 
                              cr_grid,
                              N_Trials = 90){
  
  
  # p_A2B = 0.1
  # p_B2A = 0.1
  # cr_a = seq(0, 1, by = 0.01)
  # cr_b = seq(0, 1, by = 0.01)
  # RT_nc = 0.5
  # RT_c = 1.5
  # ITI = 0.1
  # Win = 1
  # Loss = 10
  # TTime = 90
  #N_Trials = ceiling(TTime/RT_nc)
  
  data.frame(setup = 1) %>%
    mutate(trial = list(1:N_Trials),
           p_A2B = p_A2B, 
           p_B2A = p_B2A, 
           cr = list(cr_grid),
    ) %>%
    select(-setup) %>%
    unnest(cr) %>%
    rowwise() %>%
    mutate(mats = list(build_mats(p_A2B = p_A2B, 
                                  p_B2A = p_B2A, 
                                  cr_a = cr_a, 
                                  cr_b = cr_b, 
                                  start = "B_mB"))) %>%
    unnest(trial) %>%
    rowwise() %>%
    arrange(cr_a, cr_b, trial) %>%
    mutate(states = list(markov_state(mat_initial_state = mats$initial_mat,
                                      mat_transition = mats$trans_mat, 
                                      n = trial))) %>%
    ungroup() %>%
    select(-mats) %>%
    #mutate(states = as.data.frame(states))
    unnest(states) %>%
    mutate(A_total = Au_mA + Au_mB + Ad_mA,
           Au_total = Au_mA + Au_mB,
           inst_miss = Au_total * p_A2B,
           B_total = B_mA + B_mB,
           Checks = (Au_mA + Ad_mA + B_mA) * cr_a + (Au_mB + B_mB) * cr_b)
  
}


get_payout <- function(states,
                       RT_c,
                       RT_nc,
                       ITI,
                       Win,
                       Loss,
                       TTime){
  
  
  # states <- x$states[1][[1]]
  # RT_c = 1
  # RT_nc= 1
  # ITI =1
  # Win= 1
  # Loss =1
  # TTime =1
  
  Loss <- abs(Loss)
  
  states %>%
    mutate(expected_RT = RT_nc + Checks * (RT_c - RT_nc) + ITI,
           expected_Rew = Win - (inst_miss * Loss)) %>%
    group_by(cr_a, cr_b) %>%
    mutate(cum_RT = cumsum(expected_RT),
           cum_Rew = cumsum(expected_Rew)) %>%
    filter(cum_RT > TTime) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(RT_correction = cum_RT - expected_RT, # subtract current RT
           Time_left = TTime - RT_correction, #how much time was left from previous trial to block end
           p_Trial_completed = Time_left/expected_RT, #this is the proportion of the last trial that was completed
           Rew_corrected = cum_Rew - ((1 - p_Trial_completed) * expected_Rew),#to correct, I am subtracting the reward associated with the incomplete portion of the trial 
           is_max = ifelse(cum_Rew == max(cum_Rew), 1, 0),
           is_max_same = ifelse(cr_a == cr_b & cum_Rew == max(cum_Rew[cr_a == cr_b]), 1, 0)) %>%
    select(cr_a, cr_b, Rew_corrected, is_max, is_max_same) %>%
    return()
  
}


