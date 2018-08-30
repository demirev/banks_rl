source("R/setup.R")

generateEpsilon <- function(
  frame, 
  epsilon_start = 0.02, 
  epsilon_final = 0.02,
  epsilon_decay = 5000
) {
  "Generate an exploration rate"
  epsilon_final + (epsilon_start - epsilon_final) * 
    exp(-1 * frame / epsilon_decay)
}

FilesToLoad <- c(
  "R/experiments/R1.RDS",
  "R/experiments/R2.RDS",
  "R/experiments/R3.RDS",
  "R/experiments/R4.RDS",
  "R/experiments/R5.RDS"
)
NamesToSave <- list(
  c("R/experiments/R1s1.RDS","R/experiments/R1s2.RDS","R/experiments/R1s3.RDS"),
  c("R/experiments/R2s1.RDS","R/experiments/R2s2.RDS","R/experiments/R2s3.RDS"),
  c("R/experiments/R3s1.RDS","R/experiments/R3s2.RDS","R/experiments/R3s3.RDS"),
  c("R/experiments/R4s1.RDS","R/experiments/R4s2.RDS","R/experiments/R4s3.RDS"),
  c("R/experiments/R5s1.RDS","R/experiments/R5s2.RDS","R/experiments/R5s3.RDS")
)
numEpisodes <- 350
numSimulations <- 3

for (ind in 1:length(FilesToLoad)) {
  for (sim in 1:numSimulations) {
    Economy <- readRDS(FilesToLoad[ind])
    Economy$FullHistory <- NULL
    Economy$file <- NamesToSave[[ind]][sim]
    Economy$reload(lossFunc = compute_td_loss)
    Economy$reset()
    Economy$train(
      numEpisodes = numEpisodes, 
      resetProb = 0, 
      verbose = 1,
      fixed = T,
      saveEvery = numEpisodes
    )  
  }
}
