library(magrittr) # for %<>%
library(tidyverse)
library(R6)
library(reticulate)
library(grid)
library(gridExtra)

use_virtualenv("banks_abm")

math <- import("math") # probably not needed here
random <- import("random") # probably not needed here
np <- import("numpy") # probably not needed here
torch <- import("torch")
nn <- import("torch.nn")
optim <- import("torch.optim")
autograd <- import("torch.autograd")
F <- import("torch.nn.functional")

source("R/functions/utils.R")
source("R/functions/plotFunctions.R")
source("R/agents/banks.R")
source("R/agents/firms.R")
source("R/agents/households.R")
source("R/environments/constructor.R")
source("R/environments/intermediation.R")
source_python("python/dqn.py")
