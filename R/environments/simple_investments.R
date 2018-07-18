SimpleInvestments <- R6Class(
  "A Firm-Bank Investment Choice Environment",
  inherit = EconomyConstructor,
  public = list(
    
    createOptimizer = function() {
      "Initialize the optimizers"
      self$Optimizer$invest <- optim$Adam(self$DQN$invest$current$parameters())
      self$Optimizer$borrow <- optim$Adam(self$DQN$borrow$current$parameters())
      invisible(self)
    },
    
    createBuffer = function(bufferSize) {
      "Initialize the replay buffer"
      self$Buffer$invest <- ReplayBuffer(bufferSize)
      self$Buffer$borrow <- ReplayBuffer(bufferSize)
      invisible(self)
    },
    
    step = function(Investments, Borrowings) { 
      "Steps through one turn given decision vectors"
      
      # 0. Interest is paid
      self$repay()
      
      # 1. each firm applies for loans for their current investment
      self$processLoans(Borrowings)
      
      # 2. each firm keeps or discards their current opportunity
      self$invest(Investments)
      
      # 3. firms consume whatever is not invested
      Consumption <- lapply(
        self$Firms,
        function(firm) {
          firm$consume()
        }
      )
      
      # 4. Banks change their interests or fail
      bankOutcomes <- lapply(
        self$Banks,
        function(bank) {
          bank$act() # dummy actions by each bank
        }
      )
      self$defaults(bankOutcomes)
      
      # get new state and update history
      NewStates <- self$getInfoSet()
      
      self$updateHistory("episode")
      
      return(list(nextState = NewStates, reward = Consumption))
      
    },
    
    updateHistory = function(type = "episode") {
      # all deposits for history
      allProjects <- lapply(
        self$Firms,
        function(firm) {
          firm$ProjectLoans
        }
      ) %>%
        reduce(rbind)
      
      # bank details
      bankBalances <- lapply(
        self$Banks,
        function(bank) {
          tibble(
            type = bank$rating,
            depositRate = bank$depositRate,
            loanRate = bank$loanRate,
            defaultCounter = bank$defaultCounter,
            deposits = bank$deposits,
            loans = bank$loans,
            reserves = bank$reserves,
            capital = bank$capital
          )
        }
      ) %>%
        reduce(rbind)
      
      if (type == "episode") {
        # update episode history
        self$EpisodeHistory[[length(self$EpisodeHistory) + 1]] <- list(
          projects = allProjects,
          banks    = bankBalances
        )
      } else if (type == "full") {
        # add current state to full history
        self$FullHistory[[length(self$FullHistory) + 1]] <- self$EpisodeHistory
      }
      
      return(T)
    },
    
    train = function(
      numEpisodes = 10000, 
      resetProb = 0.001, 
      batch_size = 256,
      updateFreq = 200,
      verbose = 0
    ) {
      "Train the networks"
      # reset the economy
      
      Loss <- list(
        invest = rep(NA, numEpisodes), 
        borrow = rep(NA, numEpisodes)
      )
      
      InfoSets <- self$reset()$getInfoSet()
      
      # train
      for (episode in 1:numEpisodes) {
        # 0. set exploration rate
        epsilon <- generateEpsilon(episode)
        
        # 1. choose actions
        Investments <- lapply(
          InfoSets, 
          function(info) {
            decision <- self$DQN$invest$current$act(info, epsilon)
            vec <- c(0, 0)
            vec[decision + 1] <- 1
            return(decision)
          }
        )
        Borrowings <- lapply(
          InfoSets, 
          function(info) {
            decision <- self$DQN$borrow$current$act(info, epsilon)
            vec <- rep(0, length(self$Banks) + 1)
            vec[decision + 1] <- 1
            return(vec)
          }
        )
        
        # 2. interact with the economy
        Episode <- self$step(Investments, Borrowings)
        
        # push to buffers
        pmap(
          list(InfoSets, Investments, Borrowings, Episode$reward, Episode$nextState), 
          function(state, action_i, action_b, reward, next_state) {
            self$Buffer$invest$push(state, which.max(action_i) - 1L, reward, next_state)
            self$Buffer$borrow$push(state, which.max(action_b) - 1L, reward, next_state)
          }
        )
        
        if (runif(1) <= resetProb) {
          InfoSets <- self$reset()$getInfoSet()
          self$updateHistory("full")
          self$EpisodeHistory <- list()
        } else {
          InfoSets <- Episode$nextState
        }
        
        # 3. GD Step
        if (self$Buffer$invest$getLen() > batch_size) {
          Loss$invest[episode] <- self$loss(
            as.integer(batch_size),
            self$Buffer$invest,
            self$DQN$invest$current, 
            self$DQN$invest$target,
            self$Optimizer$invest,
            self$beta
          )$detach()$numpy()
          Loss$deposit[episode] <- self$loss(
            as.integer(batch_size), 
            self$Buffer$borrow,
            self$DQN$borrow$current, 
            self$DQN$borrow$target,
            self$Optimizer$borrow,
            self$beta
          )$detach()$numpy()
        }
        
        # 4. Update Target Network
        if (episode %% updateFreq == 0) {
          update_target(self$DQN$invest$current, self$DQN$invest$target)
          update_target(self$DQN$borrow$current, self$DQN$borrow$target)
        }
        
        if (verbose == 1) {
          cat(".")
        } else if (verbose == 2) {
          print(self$EpisodeHistory[[length(self$EpisodeHistory)]])
        }
      }
      
      return(Loss)
      
    },
    
    getInfoSet = function() {
      "Generates a state vector for each household"
      
      # Get some info for each bank
      bankStates <- lapply(
        self$Banks,
        function(bank) {
          bank$getState()
        }
      ) %>%
        reduce(c)
      
      # Combine it with the firm's holdings
      decisionStates <- lapply(
        self$Firms,
        function(firm) {
          c(bankStates, firm$getState())
        }
      )
      
      return(decisionStates)
    },
    
        
    repay = function() {
      "Loan Payments from Firms to Banks"
      repayments <- lapply(
        self$Firms,
        function(firm) {
          firm$payInterest(nBanks = length(self$Banks))
        }
      )
       
      principal <- lapply(
        repayments, 
        function(repayment) repayment$principal
      ) %>%
        reduce(cbind) %>%
        rowSums
      interest <- lapply(
        repayments, 
        function(repayment) repayment$interest
      ) %>%
        reduce(cbind) %>%
        rowSums 
      
      pmap(
        list(self$Banks, principal, interest),
        function(bank, principal, interest) {
          bank$loans    <- bank$loans - principal
          bank$reserves <- bank$reserves + principal + interest
        }
      )
      
      invisible(self)
    },
    
    processLoans = function(Borrowings) {  
      "Banks decide whether to accept or reject a loan" 
      
      # gather all project info in one list
      Applications <- lapply(
        self$Firms,
        function(firm) {
          firm$application
        }
      )
      
      # create a vector of accept / reject decisions and adjust bank balances
      Decisions <- map2(
        Borrowings, Applications,
        function(borrowing, application) {
          if (borrowing[length(borrowing)] == 1) {
            # last index is no loan
            return(0)
          } else {
            bankIndex <- which(borrowing == 1)
            return(self$Banks[[bankIndex]]$process(application))
          }
        }
      )
      
      # initiate approved projects
      pmap(
        list(self$Firms, Decisions, Borrowings),
        function(firm, decision, bankId) {
          firm$rollProject(decision, which(bankId == 1))
        }
      )
      
      return(self)
      
    },
    
    invest = function(Investments) {
      "Firms decide whether to invest in a project or discard it" 
      amounts <- map2(
        self$Firms, Investments,
        function(firm, decision) {
          # 1 is invest, 0 discard
          firm$invest(decision)
        }
      ) 
    },
    
    defaults = function(bankOutcomes) {
      "Decreases households outstanding deposits given a list of defaulted banks"
      lapply(
        self$Firms,
        function(firm) {
          firm$bankDefault(bankOutcomes)
        }
      )
    }
    
  )
)
