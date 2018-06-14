generateEpsilon <- function(
  frame, 
  epsilon_start = 1, 
  epsilon_final = 0.001,
  epsilon_decay = 500
) {
  epsilon_final + (epsilon_start - epsilon_final) * 
    exp(-1 * frame / epsilon_decay)
}
