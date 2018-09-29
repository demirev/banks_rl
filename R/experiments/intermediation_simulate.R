source("R/setup.R")

generateEpsilon <- function(
  frame, 
  epsilon_start = 0.05, 
  epsilon_final = 0.05,
  epsilon_decay = 5000
) {
  "Generate an exploration rate"
  epsilon_final + (epsilon_start - epsilon_final) * 
    exp(-1 * frame / epsilon_decay)
}

FilesToLoad <- c(
  "R/experiments/N2.RDS"
  ,"R/experiments/R1.RDS"
  ,"R/experiments/R2.RDS"
  ,"R/experiments/R3.RDS"
  ,"R/experiments/R4.RDS"
  ,"R/experiments/R5.RDS"
  ,"R/experiments/G2.RDS"
)
NamesToSave <- list(
  paste0("R/experiments/N2s",1:3,".RDS")#,
  ,paste0("R/experiments/R1s",2,".RDS")
  ,paste0("R/experiments/R2s",1:3,".RDS")
  ,paste0("R/experiments/R3s",1:3,".RDS")
  ,paste0("R/experiments/R4s",1:3,".RDS")
  ,paste0("R/experiments/R5s",1:3,".RDS")
  ,paste0("R/experiments/G2s",1,".RDS")
)
numEpisodes <- 500
numSimulations <- 1

for (ind in 1:length(FilesToLoad)) {
  for (sim in 1:numSimulations) {
    Economy <- readRDS(FilesToLoad[ind])
    Economy$EpisodeHistory <- NULL
    Economy$FullHistory <- NULL
    Economy$file <- NamesToSave[[ind]][sim]
    Economy$reload(lossFunc = compute_td_loss)
    Economy$reset()
    Economy$train(
      numEpisodes = numEpisodes, 
      resetProb = 0, 
      verbose = 1,
      fixed = FALSE, # let them adapt
      saveEvery = numEpisodes
    )  
  }
}
