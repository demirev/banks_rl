# An Agent Based Model of the Banking System Based on Deep Reinforcement Learning

This repository contains the code needed to reproduce my paper on RL-based Banking ABM. The paper itself can be found in the main directory.

## Overview 
The model consists of a multi agent environment for economic interactions, which is implemented in R. Interactions take place in discrete time intervals. There are three types of agents - banks, households, and firms. The code allows for an arbitrary number of agents of each type. 

At each period each agent observes an 'observation vector', which is some multi-dimensional (~100 dimensions) subset of the state space of the environment and chooses one of several available discrete actions according to a learned policy. The policy of each agent is entirely discovered through interaction with the environment using double Q learning. The learning mechanism uses a neural network as a value function approximator and is implemented in Python.

Depending on the actions chosen by the agents and the interaction rules of the environment, agents receive utility from consumption. Their goal is to maximize this utility. For full details please refer to the main text.

## Repository Structure
The code needed to reproduce the findings in the paper is structured as follows:

-> *R* - contains R code  
  -> **R/setup.R** - contains the setup of the R environment. Refer to this file for all required packages and libraries (Python dependencies are also listed here).  
  -> **R/makePlots.R** - contains the main analysis code used to produce all plots and figure in the paper  
  -> *R/agents* - contains objects implementing the different agent types  
    -> **R/agents/banks.R** - objects implementing the bank agents  
    -> **R/agents/firms.R** - objects implementing the firm agents  
    -> **R/agents/households.R** - objects implementing the household agents  
  -> *R/environments* - contains objects implementing the interaction environment  
    -> **R/agents/constructor.R** - a constructor object for the interaction environment  
    -> **R/agents/intermediation.R** - the object for the main interaction environment  
  -> *R/functions* - various helper functions  
    -> **R/functions/plotFunctions.R** - functions for creating visualizations  
    -> **R/functions/utils.R** - other functions  
  -> *R/experiments* - contains code for training the agents and running the simulations  
    -> **R/experiments/intermediation_train.R** - train the agetns for the baseline scenario  
    -> **R/experiments/intermediation_guarantee.R** - train the agents for the deposit guarantee scenario  
    -> **R/experiments/intermediation_rates.R** - train the agents for the changing reserve requirements scenario  
    -> **R/experiments/intermediation_simulate.R** - simulate the economy using the trained models  
-> *python* - contains Python code  
  -> **python/dqn.py** - contains objects implementing the deep q networks and replay buffers  

## Reproducability
To reproduce the findings of the paper run all scripts in the experiments folder in the order in which they are listed above. Next run the `makePlots.R` script interactively and save the resulting figures and tables.
