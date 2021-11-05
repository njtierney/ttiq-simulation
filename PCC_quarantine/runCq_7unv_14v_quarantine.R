# Script to consider 7 day quarantine for vaccinated (compared to 14 for unvaccinated)
rm(list = ls())
require(foreach)
require(doParallel)
source(here::here("PCC_quarantine", "CQ_process.R"))
setwd("~/Desktop/COVID_Misc/WP1")

the.state <- "NSW"

n.ind <- 100000
TP <- 5
k <- 0.2

p.vac.idx <- 0.723
p.vac.sc <- 0.723
vacc.cor <- 0.9

VE.trans <- 0.4
VE.inf <- 0.72

# quarantine.duration <- 14
# the.scenario <- "optimal"

min.space <- 2

set.seed(1)

ts <- "optimal"

# Simulate secondary cases — all vaccinated PCCs (p.vac.idx) and all not vaccinated
n.ind.1 <- (n.ind * p.vac.idx * (1 - VE.inf))
n.ind.0 <- (n.ind * (1 - p.vac.idx * (1 - VE.inf)))

stopifnot(n.ind.1 + n.ind.0 == n.ind)

tic <- Sys.time()
CQ.output.1 <-
  CQ.sim2(
    n.ind.1,
    TP,
    k,
    p.vac.idx = 1,
    p.vac.sc,
    vacc.cor,
    VE.trans,
    VE.inf,
    quarantine.duration = 7,
    the.scenario = ts,
    the.state = "NSW"
  )
CQ.output.0 <-
  CQ.sim2(
    n.ind.0,
    TP,
    k,
    p.vac.idx = 0,
    p.vac.sc,
    vacc.cor,
    VE.trans,
    VE.inf,
    quarantine.duration = 14,
    the.scenario = ts,
    the.state = "NSW"
  )

# 14-day quarantine, 3 test, partial delay: 1,3,6
#  7-day quarantine, 2 test, partial delay: 1,7

# Evaluate test times against those test times
output.1 <-
  CQ.sim.test.times(
    CQ.sim.output = CQ.output.1,
    n.ind = n.ind.1,
    test.times = c(1, 6),
    VE.trans = VE.trans,
    the.scenario = ts
  )

output.0 <-
  CQ.sim.test.times(
    CQ.sim.output = CQ.output.0,
    n.ind = n.ind.0,
    test.times = c(1, 3, 6),
    VE.trans = VE.trans,
    the.scenario = ts
  )


average.IPq <-
  (output.1$IPq * n.ind.1 + output.0$IPq * n.ind.0) / (n.ind.1 + n.ind.0)
# compare this to 14-day quarantine, 3-test for all individuals
toc <- Sys.time() - tic

save(
  list = ls(),
  file = paste0("./output/opt_times_", ts, "ttiq_7_and_14d_HQduration.RData")
)
