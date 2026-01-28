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

