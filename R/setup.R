library(magrittr) # for %<>%
library(tidyverse)
library(R6)
library(reticulate)

use_virtualenv("banks_abm")

math <- import("math") # probably not needed here
random <- import("random") # probably not needed here
np <- import("numpy") # probably not needed here
torch <- import("torch")
nn <- import("torch.nn")
optim <- import("torch.optim")
autograd <- import("torch.autograd")
F <- import("torch.nn.functional")
