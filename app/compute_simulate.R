# --- EarningsWhenChecking (unchanged) ---
EarningsWhenChecking <- function(
    BaselineRT, CheckRT, Win, Loss, Delay, SwitchP,
    TTime = 60, ITI = .2, k_max = 1000
) {
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
