# Run community quarantine of PCC's simulation model with inputs
#
# Authors: David J Price
# Date: 26 October 2021
#

require(foreach)
require(doParallel)
setwd("~/Desktop/COVID_Misc/WP1")
source(here::here("PCC_quarantine", "CQ_process.R"))

the.state <- "NSW"

n.ind <- 100000
TP <- 1.2
k <- 0.2

p.vac.idx <- 0.723
p.vac.sc <- 0.723
vacc.cor <- 0.9

VE.trans <- 0.4
VE.inf <- 0.72



the.scenarios <- c("optimal", "partial", "current_nsw_case_init")
quarantine.durations <- c(14, 7)

times <- 1:max(quarantine.durations)
min.space <- 2

all.times3 <-
  test.designs(times, 3, min.space, t1.prior.to = max(quarantine.durations))
all.times2 <-
  test.designs(times, 2, min.space, t1.prior.to = max(quarantine.durations))
all.times1 <-
  test.designs(times, 1, min.space, t1.prior.to = max(quarantine.durations))

all.times <- all.times3 %>% filter(Var1 <= 5) %>%
  add_row(all.times2 %>% mutate(Var3 = Inf) %>% filter(Var1 <= 5)) %>%
  add_row(all.times1 %>% mutate(Var2 = Inf, Var3 = Inf))


# all.times <- all.times %>% slice(1:10)
nn <- nrow(all.times)


for (hq.d in quarantine.durations) {
  for (ts in the.scenarios) {
    if (hq.d == 7 & ncol(all.times) == 3) {
      all.times <-
        all.times %>% select(-Var3) %>% distinct() %>% filter(Var1 <= hq.d &
                                                                (Var2 <= hq.d |
                                                                   is.infinite(Var2)))
      nn <- nrow(all.times)
    }
    
    set.seed(1)
    # Simulate secondary cases
    CQ.output <-
      CQ.sim2(
        n.ind,
        TP,
        k,
        p.vac.idx = p.vac.idx * (1 - VE.inf),
        p.vac.sc = p.vac.sc,
        vacc.cor,
        VE.trans,
        VE.inf,
        quarantine.duration = hq.d,
        the.scenario = ts,
        the.state = "NSW"
      )
    
    # Evaluate test times against those test times
    cl <- makeCluster(4)
    registerDoParallel(cl)
    
    tic <- Sys.time()
    
    output <-
      foreach::foreach (
        i = 1:nn,
        .combine = rbind,
        .packages = c("tidyverse")
      ) %dopar% {
        CQ.sim.test.times(
          CQ.sim.output = CQ.output,
          n.ind = n.ind,
          test.times = all.times[i,] %>% as.numeric(),
          VE.trans = VE.trans,
          the.scenario = ts
        )
      }
    
    (toc <- Sys.time() - tic)
    print(toc)
    
    stopCluster(cl)
    
    save(
      list = ls(),
      file = paste0(
        "./output/opt_times_",
        ts,
        "ttiq_",
        hq.d,
        "HQduration_TP1.2.RData"
      )
    )
    
  }
}
