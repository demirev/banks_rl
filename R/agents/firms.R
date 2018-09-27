#' A class for generating projcet opportunities for firms
#' 
#' @section Fields:
#' \code{duration} A 2-vector indicating the interval to which project durations
#' must belong
#' 
#' \code{amount} A 2-vector indicating the interval to which project loan amount
#' must belong
#' 
#' \code{income} A 2-vector indicating the interval to which project incomes
#' must belong
#' 
#' \code{income_sd} A 2-vector indicating the interval to which the 
#' standard deviation of income per period must belong
#' 
#' \code{terminal_income} A 2-vector indicating the interval to which terminal
#' income of a project must belong
#'
#' \code{terminal_icnome_sd} A 2-vector indicating the interval to which
#' the standard deviation of terminal income must belong
#' 
#' \code{default_prob} A 2-vector indicating the interval to which the default
#' probability (per period) of the project must belong
#' 
#' \code{liquidation} A 2-vector indicating the interval to which the 
#' liquidation multiple of the project must belong.
#' 
#' @section Methods:
#' \code{
#' $new(
#'   duration = c(6,20),
#'   amount   = c(10, 20),
#'   income   = c(0, 10),
#'   income_sd = c(0, 1),
#'   terminal_income = c(0, 8),
#'   terminal_income_sd = c(0, 1),
#'   default_prob = c(0, .03),
#'   liquidation = c(0, 0.6) 
#' )} Initialize the firm with some initial amounts of the capital and the
#' consumption goods.
#' 
#' \code{$draw()} Generates a 1-row tibble representing a project. The project
#' is characterized by a duration for which it will be active; a investment
#' amount required to undertake the project; average per-period income that
#' the project generates (in terms of the capital good); standard deviation
#' of this income; terminal incomer that the projects yields upon completion;
#' standard deviation of this income; a per-period default probability.
#' All these values are drawn uniformly from the intervals defined above. If a
#' project defaults it stops paying out income, but any loans taken to finance
#' it remain outstanding.
#' 
#' @name ProjectPool

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
      duration = c(6,20),
      amount   = c(10, 20),
      income   = c(0, 10),
      income_sd = c(0, 1),
      terminal_income = c(0, 8),
      terminal_income_sd = c(0, 1),
      default_prob = c(0, .03),
      liquidation = c(0, 0.6) # as multiple of amount
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


#' A Firm agent
#' 
#' Implements a firm in the ABM.
#'
#' @section Fields:
#' \code{capital} The amount of the capital good owned by the firm
#' 
#' \code{cash} The amount of the consumption good owned by the firm
#' 
#' \code{opportunity} The current project the firm is considering to take on
#' 
#' \code{application} The project for which the firm is applying for a loan
#' 
#' \code{loans} A tbl with details of each project that the firm has taken up. 
#' Includes both projects for which the firm has received a loan, and those
#' financced entirely with its own funds. Includes loan repayment amounts.
#' 
#' \code{utilf} The utility function used to convert cash to utils
#' 
#' \code{ProjectPool} An instance of the ProjectPool class. Generates new 
#' investment opportunities for the firm
#' 
#' \code{Projects} A tbl with details of projects that the firm has taken up. 
#' Includes both projects for which the firm has received a loan, and those
#' financced entirely with its own funds. Includes loan repayment amounts.
#' 
#' @section Methods:
#' \code{
#' $new(
#'   nBanks,
#'   endowment = 100,
#'   endowment_k = 100,
#'   utilf = logUtility,
#'   Pool = ProjectPool$new()
#' )} Initialize the firm with some initial amounts of the capital and the
#' consumption goods.
#' 
#' \code{consume()} Convert cash to utils
#' 
#' \code{getState()} Return a named vector of state information regarding the
#' firm. Includes holding of cash and capital, as well as a summary of the
#' projects undertaken by the firm.
#' 
#' \code{receiveRate(rate, depreciation)} Given a rate of return on capital and
#' a depreciation rate, increase the firm's cash by capital*rate and reduce
#' capital as per the depreciation rate.
#' 
#' \code{resolveProject()} Receive income of capital of all active projects.
#' Income is distributed as per the project's characteristics. Some projects
#' may default at this stage. Others may run out their lifetime.
#' 
#' \code{payInteres(nBanks)} Pays interest to all banks from which the firm 
#' has taken out loans. The interest is determined at the moment of issue, and
#' is fixed for the lifetime of the loan.
#' 
#' \code{invest(decision)} Given a decision to invest or not, either keep the
#' current investment opportunity or discard it.
#' 
#' \code{borrow(decision)} Given a deicison from which bank to borrow, apply
#' for a loan for the current opportunity (if not discarded and if cash is
#' unsufficient to finance the porject).
#' 
#' \code{rollProject(bankDecision)} Receive the bank's decision on the loan
#' application. If the decision is affirmitive, calculate per-period loan 
#' reapyments and add the application to `Projects`.
#' 
#' \code{bankDefaults(bankOutcomes)} Given a vector indicating which bank have
#' defaulted, set the payments on loans from defaulted banks to zero (don't
#' repay those loans)
#' 
#' @name Firm

Firm <- R6Class(
  "A Firm class for the intermediation environment",
  
  public = list(
    capital     = 0,
    cash        = 0,
    opportunity = NULL,
    application = NULL,
    utilf       = NULL,
    ProjectPool = NULL,
    Projects    = NULL,
    
    initialize = function(
      nBanks, 
      endowment = 100, # initial cash
      endowment_k = 100, # initial capital
      utilf = logUtility, 
      Pool = ProjectPool$new()
    ) {
      self$Projects <- tibble(
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
      self$capital <- self$capital + endowment_k
      self$utilf <- utilf
      self$ProjectPool <- Pool
      self$opportunity <- Pool$draw()
      self$application <- self$opportunity[0, ]
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
        na.omit
      nms <- rownames(opportunityFeatures) # some acrobatics to get a named vector
      opportunityFeatures <- as.numeric(opportunityFeatures)
      names(opportunityFeatures) <- nms 
      
      # features realating to current loan application 
      if (nrow(self$application) == 0) {
        applicationFeatures <- opportunityFeatures*0
      } else {
        applicationFeatures <- self$application %>%
          select(-bank) %>%
          t %>%
          na.omit %>%
          as.numeric
        names(applicationFeatures) <- nms
      }
      
      names(opportunityFeatures) %<>% paste0("_opportunity")
      names(applicationFeatures) %<>% paste0("_application")
      
      # features realting to ongoing projects
      if (nrow(self$Projects) == 0) {
        projectFeatures <- rep(0, 33)
        names(projectFeatures) <- c(
          "number_projects",
          "number_banks",
          "mean_duration",
          "min_duration",
          "max_duration",
          "mean_interest",
          "min_interest",
          "max_interest",
          "sum_payment",
          "mean_payment",
          "min_payment",
          "max_payment",
          "sum_outstanding",
          "mean_outstanding",
          "min_outstanding",
          "max_outstanding",
          "sum_income",
          "mean_income",
          "min_income",
          "max_income",
          "mean_income_sd",
          "sum_terminal_income",
          "mean_terminal_income",
          "min_terminal_income",
          "max_terminal_income",
          "mean_terminal_income_sd",
          "mean_default_prob",
          "min_default_prob",
          "max_default_prob",
          "sum_liquidation",
          "mean_liquidation",
          "min_liquidation",
          "max_liquidation"
        )
      } else {
        projectFeatures <- c(
          "number_projects" = nrow(self$Projects),
          "number_banks" = length(unique(self$Projects$bank)),
          "mean_duration" = mean(self$Projects$duration),
          "min_duration" = min(self$Projects$duration),
          "max_duration" = max(self$Projects$duration),
          "mean_interest" = mean(self$Projects$interest),
          "min_interest" = min(self$Projects$interest),
          "max_interest" = max(self$Projects$interest),
          "sum_payment" = sum(self$Projects$payment),
          "mean_payment" = mean(self$Projects$payment),
          "min_payment" = min(self$Projects$payment),
          "max_payment" = max(self$Projects$payment),
          "sum_outstanding" = sum(self$Projects$outstanding),
          "mean_outstanding" = mean(self$Projects$outstanding),
          "min_outstanding" = min(self$Projects$outstanding),
          "max_outstanding" = max(self$Projects$outstanding),
          "sum_income" = sum(self$Projects$income),
          "mean_income" = mean(self$Projects$income),
          "min_income" = min(self$Projects$income),
          "max_income" = max(self$Projects$income),
          "mean_income_sd" = mean(self$Projects$income_sd),
          "sum_terminal_income" = sum(self$Projects$terminal_income),
          "mean_terminal_income" = mean(self$Projects$terminal_income),
          "min_terminal_income" = min(self$Projects$terminal_income),
          "max_terminal_income" = max(self$Projects$terminal_income),
          "mean_terminal_income_sd" = mean(self$Projects$terminal_income_sd),
          "mean_default_prob" = mean(self$Projects$default_prob),
          "min_default_prob" = min(self$Projects$default_prob),
          "max_default_prob" = max(self$Projects$default_prob),
          "sum_liquidation" = sum(self$Projects$liquidation),
          "mean_liquidation" = mean(self$Projects$liquidation),
          "min_liquidation" = min(self$Projects$liquidation),
          "max_liquidation" = max(self$Projects$liquidation)
        )
      }
      
      return(c(opportunityFeatures, applicationFeatures, projectFeatures))
      
    },
    
    receiveRate = function(rate, depreciation) {
      "Receive the rate of return on capital"
      if (self$capital != 0) {
        self$cash <- self$cash + self$capital * rate
        self$capital <- self$capital * (1 - depreciation)
      } # if condition to avoid 0 * Inf = NaN situations
    },
    
    resolveProject = function() {
      "Resolve project outcomes"
      
      # some projects default
      defaults <- runif(nrow(self$Projects)) <= self$Projects$default_prob
      self$Projects$income[defaults] <- 0
      self$Projects$income_sd[defaults] <- 0
      self$Projects$terminal_income[defaults] <- 0
      self$Projects$terminal_income_sd[defaults] <- 0
      self$Projects$liquidation[defaults] <- 0
      
      # income is received (both ongoing and terminal)
      ongoing <- self$Projects$duration > 1
      income <- rnorm(
        sum(ongoing), 
        self$Projects$income[ongoing],
        self$Projects$income_sd[ongoing]
      )
      income[income <= 0] <- 0
      if (length(income) == 0) income <- 0
      
      terminal <- self$Projects$duration == 1
      terminal_income <- rnorm(
        sum(terminal),
        self$Projects$terminal_income[terminal],
        self$Projects$terminal_income[terminal]
      )
      terminal_income[terminal_income <= 0] <- 0
      if (length(terminal_income) == 0) terminal_income <- 0
      
      self$Projects$liquidation[terminal] <- 0 # these project are over
      
      self$capital <- self$capital + sum(income) + sum(terminal_income)
      invisible(self)
    },
    
    payInterest = function(nBanks) {
      "Pay back loans" 
      
      if (nrow(self$Projects) == 0) {
        return(list(principal = rep(0,nBanks), interest = rep(0,nBanks)))
      }
      
      # loan payments are made (principal + interest)
      paymentsDue <- sum(self$Projects$payment)
      terminal <- self$Projects$duration == 1
      
      if (paymentsDue > self$cash) {
        # firm defaults - cash equally split among banks, projects liquidated #
        
        # total residual payments
        payments <- self$Projects %>%
          group_by(bank) %>%
          summarise(payment = sum(liquidation * amount)) %>%
          mutate(payment = payment + self$cash/nrow(.)) %>%
          right_join(tibble(bank = 1:nBanks), by = "bank") %>%
          select(payment) %>%
          t %>%
          as.numeric
        
        # principal to be written off banks' balance sheets
        principal <- self$Projects %>%
          group_by(bank) %>%
          summarise(principal = sum(principal * duration)) %>% # what's left
          right_join(tibble(bank = 1:nBanks), by = "bank") %>%
          select(principal) %>%
          t %>%
          as.numeric # this is not a payment - just a write off
        
        # reduce cash
        self$cash <- 0
        self$Projects <- self$Projects[0, ]
        
      } else {
        # firm repays #
        
        # total payment
        payments <- self$Projects %>%
          group_by(bank) %>%
          summarise(payment = sum(payment)) %>%
          right_join(tibble(bank = 1:nBanks), by = "bank") %>%
          select(payment) %>%
          t %>%
          as.numeric
        
        # the part of the repayment attributable to the principal is calculated
        principal <- self$Projects %>%
          group_by(bank) %>%
          summarise(principal = sum(principal)) %>% # equal installments
          right_join(tibble(bank = 1:nBanks), by = "bank") %>%
          select(principal) %>%
          t %>%
          as.numeric
        
        # reduce cash
        self$cash <- self$cash - sum(na.omit(payments))
        self$Projects <- self$Projects %>%
          filter(!terminal) %>%
          mutate(outstanding = outstanding - payment) %>%
          mutate(duration = duration - 1)
      }
      
      payments[is.na(payments)] <- 0
      principal[is.na(principal)] <- 0
      return(list(principal = principal, interest = payments - principal))
    },
    
    borrow = function(decision) {
      "Firms choose to which bank to apply"
      if (!(nrow(self$application) == 0)) {
        self$application$bank <- which(decision == 1)
      }
      invisible(self)
    },
    
    invest = function(decision) { 
      "Firm chooses whether to discard or keep opportunity"
      # decision is a 2-vector. If element 1 is '1' then it means forgo project.
      # if element 2 is '1' it means invest with cash. If both are '0' it means
      # apply for loan
      if (decision[1] == 1) {
        # firm chooses to forgo opportunity and consume the cash
        self$application <- self$opportunity[0, ]
      } else if (decision[2] == 1) {
        # firm chooses to pursue current opportunity (and pays the price)
        self$application <- self$opportunity[0, ]
        
        if (self$cash >= self$opportunity$amount) { 
          # firm can pay alone
          opportunity <- self$opportunity %>%
            mutate(
              bank = -1, # no bank
              interest = 0,
              outstanding = 0,
              payment = 0,
              principal = 0
            )
          self$Projects <- bind_rows(
            self$Projects,
            opportunity
          )
          self$cash <- self$cash - self$opportunity$amount
        } else {
          # firm cannot pay - loses both project and money
          self$cash <- 0
        }
      } else {
        # firm applies for loan
        self$application <- self$opportunity
      }
      
      # draw new opportunity
      self$opportunity <- self$ProjectPool$draw()
      
      invisible(self)
    },
    
    rollProject = function(bankDecision) {
      "Add approved projects to ongoing"
      if (bankDecision != 0) { # borrowing
        self$application <- self$application %>%
          mutate(
            outstanding = (amount * (1 + bankDecision)^duration),
            interest = bankDecision,
            payment = outstanding / duration,
            principal = amount / duration
          )
        
        self$Projects <- bind_rows(
          self$Projects,
          self$application
        )
        
      }
      self$application <- self$opportunity[0, ]
    },
    
    bankDefaults = function(bankOutcomes) {
      "Settle loans with defaulted banks"
      defaults <- self$Projects$bank %in% which(as.logical(bankOutcomes))
      self$Projects$outstanding[defaults] <- 0
      self$Projects$payment[defaults] <- 0
      self$Projects$principal[defaults] <- 0
    }
  )
)