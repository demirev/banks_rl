This document presents some results of testing the behavior of
artificially intelligent agents in simplified versions of the economy
from the main text. Each experiment tries to isolate a single behavioral
trait of the agents and to verify that indeed the correct behavior has
been learnt.

In the first section I present experiments relating to the behavior of
savers, while the second section contains the experiments that study the
behavior of firms.

Savings
=======

In order to test savers' behavior I created a simplified environment,
where savers are the only intelligent agents. They have to make (as in
the main text) two decisions - whether to withdraw their deposits from
the banks and in which (if any) bank to deposit their savings.

Banks on the other hand evolve according to some hard-coded rules. Each
bank can be one of five types - indexed 1 to 5. Lower types are less
risky banks. Their deposits pay out lower interest, but they also have a
low chance of default. Higher types are on the other hand riskier -
paying high interest but also defaulting more frequently.

Each household observes the banks' balance sheets before making a
decision. The input vector to the deep Q network consists of the
interest rate, deposits, loans, capital and reserve quantities for each
bank, as well as the deposit holdings and cash for any given household.
Even though banks are not autonomous in this environment, their balance
sheets movements indicate their default probabilities (riskier banks
have more sudden decreases in loans and reserves).

Picking up the lowest interest
------------------------------

In this experiment there are 5 banks and 200 households. All of the five
banks have the same default risk (rating 1, signifying the lowest risk),
but 4 of them pay 5 interest on deposits, and one pays 15:

    Banks_S1 <- lapply(
      c(0.15, rep(0.05, 4)),
      function(int) {
        DummyBank$new(
          depositRate = int, 
          rating = 1, 
          fixedInterest = T,
          deposits = 0,
          loans = 400,
          capital = 500,
          reserves = 100,
          provisions = 0
        )
      }
    )

    Households_S1 <- lapply(
      1:200,
      function(i) VanillaHousehold$new(length(Banks_S1), income = 1)
    )


    DQN_S1 <- list(
      withdraw = list(
        current = DQN(41L, 6L), #$cuda(),
        target  = DQN(41L, 6L)
      ),
      deposit = list(
        current = DQN(41L, 6L),
        target  = DQN(41L, 6L)
      )
    )

    Economy_S1 <- SimpleSavings$new(
      households = Households_S1, 
      banks = Banks_S1, 
      dqns = DQN_S1, 
      lossFunction = compute_td_loss, 
      bufferSize = 1000L
    )

    Economy_S1$train(numEpisodes = 1024*6, resetProb = 0.004, verbose = 1)

The list `Banks_S1` contains objects of class `DummyBank`, which are the
rule-based banks that are used in this experiment. `Households_S1`
contains the cash and deposit holdings of each household (collected in
objects of class \`VanillaHousehold\`\`).

Finally, the neural networks for calculating the Q values are contained
in the list `DQN_S1`. Since households make two types of decisions -
whether to withdraw their savings and where to deposit their cash -
there are two distinct Q networks. Additionally, since agents use double
Q learning, each network has a current and target variants.

The figure below shows the share of total deposits held at each of the
five banks at each period. If the households have learned the correct
savings behavior, we would expect to see the bank with the highest
interest rate to pick up almost all deposits.

Indeed, we can see that this bank consistently has more than 50 of all
deposits. The fact that this number is below 100 however may indicate
that the agents need more training rounds.

![different deposit rates](visuals/S1.gif)

Picking up the lowest default risk
----------------------------------

In this experiment there are again 5 banks and 200 households. However
this time they all pay the same interest on deposits (10), but four are
of the riskiest type (type 5) while one is of the least risky type (1).

    Banks_S2 <- lapply(
      c(1, rep(5, 4)),
      function(rat) {
        DummyBank$new(
          depositRate = 0.1, 
          rating = rat, 
          fixedInterest = T,
          deposits = 0,
          loans = 400,
          capital = 500,
          reserves = 100,
          provisions = 0
        )
      }
    )

    Households_S2 <- lapply(
      1:200,
      function(i) VanillaHousehold$new(length(Banks_S2), income = 1)
    )


    DQN_S2 <- list(
      withdraw = list(
        current = DQN(41L, 6L), #$cuda(),
        target  = DQN(41L, 6L)
      ),
      deposit = list(
        current = DQN(41L, 6L),
        target  = DQN(41L, 6L)
      )
    )

    Economy_S2 <- SimpleSavings$new(
      households = Households_S2, 
      banks = Banks_S2, 
      dqns = DQN_S2, 
      lossFunction = compute_td_loss, 
      bufferSize = 1000L
    )

    Economy_S2$train(numEpisodes = 1024*6, resetProb = 0.004, verbose = 1)

The code setup here is similar to the previous experiment, the only
difference being in the `Banks_S2` list, where the rating now differs
among banks.

The results are presented in a similar fashion. Examining the simulation
we see that the bank with the least risky type (1) captures the majority
of the deposits early on.

Note also, that as the economy progresses there are no defaults (which
would have been visible in the animation as a sudden drop of deposits to
zero). In this simulation even riskier banks are able to accumulate
reserves and do not default. Correspondingly agents start investing
their savings in those banks as well.

![different risk types](visuals/S2.gif)

Investment
==========

In order to test the behavior of autonomous firms in the simulated
economy I created a simpler environment where firms (entrepreneurs) are
the only learning agents and banks act according to pre-defined rules
(akin to the test environment for consumers).

As before, banks are of five different types, with riskier types
offering more attractive loans. The banks' asset sheet reflects their
risk profile (so to make the task learnable).

Firms have a 'project pool'. At each turn a new project is generated
from the pool, characterized by expected income and volatility, default
risk, and termination value. All firms have the same project pool.

Each firm makes two decisions - whether to pursue the current project
drawn out of the pool or to discard it, and to which bank to apply for
funding (if cash holdings are insufficient).

Borrowing from the lowest interest rate bank
--------------------------------------------

In this experiment there are five banks and fifty firms. Banks share the
same risk profile, but one of them offers a thrice lower interest rate
on loans.

    Banks_I1 <- lapply(
      c(0.07, rep(0.21, 4)), 
      function(int) DummyBankInv$new(rating = 1, loanRate = int, fixedInterest = T)
    )

    Firms_I1 <- lapply(
      1:50,
      function(i) {
        VanillaFirm$new(nBanks = length(Banks_I1), endowment = 10, utilf = logUtility)
      }
    )

    DQN_I1 <- list(
      invest = list(
        current = DQN(90L, 2L), #$cuda(),
        target  = DQN(90L, 2L)
      ),
      borrow = list(
        current = DQN(90L, 6L),
        target  = DQN(90L, 6L)
      )
    )

    Economy_I1 <- SimpleInvestments$new(
      firms = Firms_I1, 
      banks = Banks_I1, 
      dqns = DQN_I1, 
      lossFunction = compute_td_loss, 
      bufferSize = 1000L
    )

    Economy_I1$train(numEpisodes = 1024*6, resetProb = 0.004, verbose = 1)

The code looks similar to the tests on the savers. Firms are represented
by the `VanillaFirm` class, which holds their assets and liabilities, as
well as a list of all projects undertaken by the firm (along with their
per-period revenue, default risk, and interest payment).

As before there are a total of four deep-Q-networks: two for the
investment/discard decision (a target and a current network), and two
for the bank choice decision.

The initial expectation is that firms will only borrow from the bank
that gives out the lowest interest. However upon examining the
simulation result an interesting pattern appears:

![different loan rates](visuals/I1.gif)

We see that indeed the preferred bank seems to be the one with the
lowest interest. However on top of that we notice that firms tend to
flock to the same bank. This leads to the chosen bank to over-lend and
pile up risky assets, which in this simple simulation always leads to
default (as can be seen by the sudden drop in loans).

This may indicate that firms in this scenario have learned to 'game the
system' and always borrow from the bank with the most issued loans. Thus
leading the bank to default and avoiding paying their debt.

Borrowing from more or less risky banks
---------------------------------------

In this experiment there are again 5 banks and 50 firms. This time one
the banks is more risky than the others (rating 5).

    Banks_I2 <- lapply(
      c(5, rep(1, 4)), 
      function(rat) DummyBankInv$new(rating = rat, loanRate = 0.1, fixedInterest = T)
    )

    Firms_I2 <- lapply(
      1:50,
      function(i) {
        VanillaFirm$new(nBanks = length(Banks_I2), endowment = 10, utilf = logUtility)
      }
    )

    DQN_I2 <- list(
      invest = list(
        current = DQN(90L, 2L), #$cuda(),
        target  = DQN(90L, 2L)
      ),
      borrow = list(
        current = DQN(90L, 6L),
        target  = DQN(90L, 6L)
      )
    )

    Economy_I2 <- SimpleInvestments$new(
      firms = Firms_I2, 
      banks = Banks_I2, 
      dqns = DQN_I2, 
      lossFunction = compute_td_loss, 
      bufferSize = 1000L
    )

    Economy_I2$train(numEpisodes = 1024*6, resetProb = 0.004, verbose = 1)

More than anything this experiment reconfirms the findings of the
previous one - all firms cluster together borrowing from a single bank
until this bank defaults because not loans repay:

![different risk types](visuals/I2.gif)

Overall these experiments indicate that the current economy setup is
conductive to learning by artificial agents, but are very sensitive to
initial conditions.
