library(magrittr) # for %<>%
library(tidyverse)
library(R6)
library(reticulate)

use_virtualenv("banks_abm")

torch <- import("torch")
nn <- import("torch.nn")
optim <- import("torch.optim")
autograd <- import("torch.autograd")
F <- import("torch.nn.functional")
