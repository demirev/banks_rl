ProjectPool <- R6Class(
  "Generates investment projects",
  public = list(
    duration = NULL,
    amount   = NULL,
    income   = NULL,
    income_sd = NULL,
    terminal_income = NULL,
    terminal_income_sd = NULL,
    default_prob = NULL,
    liquidation = NULL,
    
    initialize = function(
      duration = c(10,20),
      amount   = c(3, 9),
      income   = c(0, 1),
      income_sd = c(0, 3),
      terminal_income = c(0, 1.1),
      terminal_income_sd = c(0, 0.4),
      default_prob = c(0, .15),
      liquidation = c(0, 2) # as multiple of income
    ) {
      self$duration <- duration
      self$amount <- amount
      self$income <- income
      self$income_sd <- income_sd
      self$terminal_income <- terminal_income
      self$terminal_income_sd <- terminal_income_sd
      self$default_prob <- default_prob
      self$liquidation <- liquidation
    },
    
    draw = function() {
      "Generates a project"
      # to do - assertions
      tibble(
        bank = NA,
        amount = runif(1, self$amount[1], self$amount[2]),
        duration = sample(size = 1, x = seq(self$duration[1], self$duration[2])),
        principal = NA,
        interest = NA,
        payment = NA,
        outstanding = NA,
        income = runif(1, self$income[1], self$income[2]),
        income_sd = runif(1, self$income_sd[1], self$income_sd[2]),
        terminal_income = runif(1, self$terminal_income[1], self$terminal_income[2]),
        terminal_income_sd = runif(1, self$terminal_income_sd[1], self$terminal_income_sd[2]),
        default_prob = runif(1, self$default_prob[1], self$default_prob[2]),
        liquidation = runif(1, self$liquidation[1], self$liquidation[2])
      )
    }
    
  )
)

VanillaFirm <- R6Class(
  "A Class representing a simple firm",
  public = list(
    cash        = 0,
    endowment   = 0,
    opportunity = NULL,
    application = NULL,
    loans       = NULL,
    utilf       = NULL,
    ProjectPool = NULL,
    ProjectLoans = NULL,
    
    initialize = function(
      nBanks, 
      endowment = 100, 
      utilf = logUtility, 
      Pool = ProjectPool$new()
    ) {
      self$ProjectLoans <- tibble(
        bank = numeric(0),
        amount = numeric(0),
        duration = numeric(0),
        principal = numeric(0),
        interest = numeric(0),
        payment = numeric(0),
        outstanding = numeric(0),
        income = numeric(0),
        income_sd = numeric(0),
        terminal_income = numeric(0),
        terminal_income_sd = numeric(0),
        default_prob = numeric(0),
        liquidation = numeric(0)
      )
      self$cash <- self$cash + endowment
      self$endowment <- endowment
      self$utilf <- utilf
      self$ProjectPool <- Pool
      self$opportunity <- Pool$draw()
    },
    
    consume = function() {
      "Consume available cash to get utility"
      utility <- self$utilf(self$cash)
      self$cash <- 0
      
      return(utility)
    },
    
    getState = function() {
      
      # features relating to current investment opportunity
      opportunityFeatures <- self$opportunity %>% 
        t %>%
        na.omit %>%
        as.numeric
      
      # features realating to current loan application 
      if (is.null(self$application)) {
        applicationFeatures <- opportunityFeatures*0
      } else {
        applicationFeatures <- self$application %>%
          t %>%
          na.omit %>%
          as.numeric
      }
      
      # features realting to ongoing projects
      if (nrow(self$ProjectLoans) == 0) {
        projectFeatures <- rep(0, 34)
      } else {
        projectFeatures <- c(
          nrow(self$ProjectLoans),
          length(unique(self$ProjectLoans$bank)),
          max(table(self$ProjectLoans$bank)),
          mean(self$ProjectLoans$duration),
          min(self$ProjectLoans$duration),
          max(self$ProjectLoans$duration),
          mean(self$ProjectLoans$interest),
          min(self$ProjectLoans$interest),
          max(self$ProjectLoans$interest),
          sum(self$ProjectLoans$payment),
          mean(self$ProjectLoans$payment),
          min(self$ProjectLoans$payment),
          max(self$ProjectLoans$payment),
          sum(self$ProjectLoans$outstanding),
          mean(self$ProjectLoans$outstanding),
          min(self$ProjectLoans$outstanding),
          max(self$ProjectLoans$outstanding),
          sum(self$ProjectLoans$income),
          mean(self$ProjectLoans$income),
          min(self$ProjectLoans$income),
          max(self$ProjectLoans$income),
          mean(self$ProjectLoans$income_sd),
          sum(self$ProjectLoans$terminal_income),
          mean(self$ProjectLoans$terminal_income),
          min(self$ProjectLoans$terminal_income),
          max(self$ProjectLoans$terminal_income),
          mean(self$ProjectLoans$terminal_income_sd),
          mean(self$ProjectLoans$default_prob),
          min(self$ProjectLoans$default_prob),
          max(self$ProjectLoans$default_prob),
          sum(self$ProjectLoans$liquidation),
          mean(self$ProjectLoans$liquidation),
          min(self$ProjectLoans$liquidation),
          max(self$ProjectLoans$liquidation)
        )
      }
      
      return(c(opportunityFeatures, applicationFeatures, projectFeatures))
      
    },
    
    payInterest = function(nBanks) {
      "Resolve project outcomes and pay back loans" 
      
      if (nrow(self$ProjectLoans) == 0) {
        return(list(principal = rep(0,nBanks), interest = rep(0,nBanks)))
      }
      
      # some projects default
      defaults <- runif(nrow(self$ProjectLoans)) <= self$ProjectLoans$default_prob
      self$ProjectLoans$income[defaults] <- 0
      self$ProjectLoans$income_sd[defaults] <- 0
      self$ProjectLoans$terminal_income[defaults] <- 0
      self$ProjectLoans$terminal_income_sd[defaults] <- 0
      self$ProjectLoans$liquidation[defaults] <- 0
      
      # income is received (both ongoing and terminal)
      ongoing <- self$ProjectLoans$duration > 1
      income <- rnorm(
        sum(ongoing), 
        self$ProjectLoans$income[ongoing],
        self$ProjectLoans$income_sd[ongoing]
      )
      income[income <= 0] <- 0
      if (length(income) == 0) income <- 0
      
      terminal <- self$ProjectLoans$duration == 1
      terminal_income <- rnorm(
        sum(terminal),
        self$ProjectLoans$terminal_income[terminal],
        self$ProjectLoans$terminal_income[terminal]
      )
      terminal_income[terminal_income <= 0] <- 0
      if (length(terminal_income) == 0) terminal_income <- 0
      
      self$ProjectLoans$liquidation[terminal] <- 0 # these project are over
      
      self$cash <- self$cash + sum(income) + sum(terminal_income)
      
      # loan payments are made (principal + interest)
      paymentsDue <- sum(self$ProjectLoans$payment)
      if (paymentsDue > self$cash) {
        # firm defaults - cash equally split among banks, projects liquidated #
        
        # total residual payments
        payments <- self$ProjectLoans %>%
          group_by(bank) %>%
          summarise(payment = sum(liquidation * income)) %>%
          mutate(payment = payment + self$cash/nrow(.)) %>%
          right_join(tibble(bank = 1:nBanks), by = "bank") %>%
          select(payment) %>%
          t %>%
          as.numeric
        
        # principal to be written off banks' balance sheets
        principal <- self$ProjectLoans %>%
          group_by(bank) %>%
          summarise(principal = sum(principal * duration)) %>% # what's left
          right_join(tibble(bank = 1:nBanks), by = "bank") %>%
          select(principal) %>%
          t %>%
          as.numeric # this is not a payment - just a write off
        
        # reduce cash
        self$cash <- 0
        self$ProjectLoans <- self$ProjectLoans[0, ]
        
      } else {
        # firm repays #
        
        # total payment
        payments <- self$ProjectLoans %>%
          group_by(bank) %>%
          summarise(payment = sum(payment)) %>%
          right_join(tibble(bank = 1:nBanks), by = "bank") %>%
          select(payment) %>%
          t %>%
          as.numeric
        
        # the part of the repayment attributable to the principal is calculated
        principal <- self$ProjectLoans %>%
          group_by(bank) %>%
          summarise(principal = sum(principal)) %>% # equal installments
          right_join(tibble(bank = 1:nBanks), by = "bank") %>%
          select(principal) %>%
          t %>%
          as.numeric
        
        # reduce cash
        self$cash <- self$cash - sum(na.omit(payments))
        self$ProjectLoans <- self$ProjectLoans %>%
          filter(!terminal) %>%
          mutate(outstanding = outstanding - payment) %>%
          mutate(duration = duration - 1)
      }
      
      payments[is.na(payments)] <- 0
      principal[is.na(principal)] <- 0
      return(list(principal = principal, interest = payments - principal))
    },
    
    invest = function(investmentDecision) { 
      "Move up (or not) opportunity to investment slot"
      if (investmentDecision == 1) {
        # firm chooses to pursue current opportunity (and pays the price)
        self$application <- self$opportunity
        
        if (self$cash >= self$application$amount) {
          # firm can pay alone
          self$application <- self$application %>%
            mutate(
              bank = -1, # no bank
              outstanding = 0,
              payment = 0,
              principal = 0
            )
          self$ProjectLoans <- bind_rows(
            self$ProjectLoans,
            self$application
          )
          self$cash <- self$cash - self$application$amount
          self$application <- NULL
        } else {
          # firm pays part of the project and applies for loan
          self$application$amount <- self$application$amount - self$cash
          self$cash <- 0
        }
        
      } else {
        # firm chooses to forgo opportunity and consume the cash
        self$application <- NULL
      }
      
      # draw a new opportunity
      self$opportunity <- self$ProjectPool$draw()
    },
    
    rollProject = function(bankDecision, bankId) {
      "Add approved projects to ongoing"
      if (bankDecision != 0) { # borrowing
        self$application <- self$application %>%
          mutate(
            bank = bankId,
            outstanding = (amount * (1 + bankDecision)^duration),
            interest = bankDecision,
            payment = outstanding / duration,
            principal = amount / duration
          )
        
        self$ProjectLoans <- bind_rows(
          self$ProjectLoans,
          self$application
        )
        
      }
      self$application <- NULL
    },
    
    bankDefault = function(bankOutcomes) {
      "Settle loans with defaulted banks"
      defaults <- self$ProjectLoans$bank %in% which(as.logical(bankOutcomes))
      self$ProjectLoans$outstanding[defaults] <- 0
      self$ProjectLoans$payment[defaults] <- 0
      self$ProjectLoans$principal[defaults] <- 0
    }
    
  )
)
