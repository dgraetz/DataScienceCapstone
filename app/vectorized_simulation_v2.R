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
  ############
  
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


comp_sidetask <- function(RT_A_only, 
                          RT_A_B_check, 
                          RT_A_B_response, 
                          ITI, 
                          Win_A, 
                          Win_B, 
                          Loss_B, 
                          p_go_on, 
                          p_go_off, 
                          Time, 
                          cr){
  
  Win_A <- abs(Win_A)
  Win_B <- abs(Win_B)
  Loss_B <- abs(Loss_B)
  
  results <- data.frame(cr = cr) %>%
    mutate(final_reward = NA)
  
  for (i in 1:length(cr)){
    
    cr_i <- results$cr[i]
    
    # 1. State Probabilities (Steady State Markov)
    # Pi_off: Proportion of time world is in "OFF" state
    # Pi_on:  Proportion of time world is in "ON" state
    Pi_off <- p_go_off / (p_go_on + p_go_off)
    Pi_on  <- 1 - Pi_off
    
    # 2. Event Statistics
    # Lambda: Probability of a new event STARTING on any given trial
    lambda_event <- Pi_off * p_go_on
    
    # 3. Probability of Missing an Event
    # A miss occurs if we fail to check (1-cr) for every trial the monster is alive.
    # The duration of the monster follows a Geometric distribution (p = p_go_off).
    # P(Miss) = Sum[ P(Length=k) * (1-cr)^k ] for k=1 to infinity.
    # This resolves to a geometric series:
    q <- 1 - cr_i
    P_miss_event <- (p_go_off * q) / (1 - (1 - p_go_off) * q)
    
    # Handle edge case where cr = 1 (P_miss must be 0)
    if(cr_i == 1) P_miss_event <- 0
    
    P_catch_event <- 1 - P_miss_event
    
    # 4. Expected Reward Per Trial
    # Base Reward (A) + (Prob of Catch * Bonus) - (Prob of Miss * Penalty)
    # Note: Win_A happens every trial. Bonus/Penalty happen once per event.
    E_Reward_Trial <- Win_A + 
      (lambda_event * P_catch_event * Win_B) - 
      (lambda_event * P_miss_event * Loss_B)
    
    # 5. Expected Time Per Trial
    # We calculate the weighted average of time spent.
    # T_base:  Time when NOT checking
    # T_check: Time when Checking (standard check cost)
    # T_extra: The extra cost of responding (RT_response - RT_check) happens 
    #          exactly once per CAUGHT event.
    
    T_base  <- (1 - cr_i) * (RT_A_only + ITI)
    T_check <- cr_i * (RT_A_B_check + ITI)
    
    # Correction: If we catch a monster, one of those checks was actually a response.
    # We add the difference between Response Time and Check Time.
    T_response_adjustment <- lambda_event * P_catch_event * (RT_A_B_response - RT_A_B_check)
    
    E_Time_Trial <- T_base + T_check + T_response_adjustment
    
    # 6. Total Calculation
    # How many trials fit in the total Time?
    Expected_Trials <- Time / E_Time_Trial
    Total_Reward <- Expected_Trials * E_Reward_Trial
    
    results$final_reward[i] <- Total_Reward
  
  }
  
  results <- results %>%
    mutate(max_reward = max(final_reward),
           rel_reward = final_reward/max_reward)
  
  
  return(results)
}

