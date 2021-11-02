# run Nick’s ABM

# change max_days argument in get_valid_abm_sample.R
# and in compute_abm_metrics.R x2
# change initial infections argument in get_valid_abm_sample.R

future::plan(multisession(workers = 8))
#future::plan(sequential, split=TRUE)
sims <- expand_grid(
  vaccination_coverage = 0.74, #0.740,
  vaccination_test_seeking_multiplier = c(1),
  passive_detection_given_symptoms = c(0.5),
  rel_active_detection_vaccinated_source = c(1),
  rel_active_detection_vaccinated_contact = c(1,0),
  #ve_onward=0.639*c(0.9, 1, 1.1),
  ve_onward=0.639,
  isolation_days_vax=c(14),
  isolation_start_day=c('isolation'),
  do_ttiq = c(TRUE)
) %>%
  mutate(
    # tweak starting R to get optimal reproduction number at about 1
    R = case_when(
      vaccination_coverage == 0.9 ~ 5, # 4.6 fails <2.58
      vaccination_coverage == 0.8 ~ 4.6, # 4.6 fails < 1.9
      vaccination_coverage == 0.74 ~ 4.75 # 4.6 fails < 1.55
    )
    
  ) %>%
  rowwise() %>%
  mutate(
    parameters = list(
      setup_abm(
        R = R,
        vaccination_coverage = vaccination_coverage,
        vaccination_test_seeking_multiplier = vaccination_test_seeking_multiplier,
        passive_detection_given_symptoms = passive_detection_given_symptoms,
        rel_active_detection_vaccinated_source = rel_active_detection_vaccinated_source,
        rel_active_detection_vaccinated_contact = rel_active_detection_vaccinated_contact,
        ve_onward=ve_onward,
        isolation_days_vax=isolation_days_vax,
        isolation_start_day=isolation_start_day,
        contact_tracing = do_ttiq,
        screening = do_ttiq
      )
    )
  ) %>%
  mutate(
    simulations = list(
      get_valid_abm_samples(parameters, n_samples = 10)
    )
  )  


#calculate TPs
sry_tps <- sims %>%
  mutate(
    metrics = list(
      compute_abm_metrics(simulations)
    )
  ) %>%
  select(
    -parameters,
    -simulations
  ) %>%
  unnest(
    cols = metrics
  ) %>% 
  filter(metric=='TP') %>% 
  select(metric, statistic, value) %>% 
  print(n=Inf)


plot_df <- sims %>%
  filter(
    vaccination_coverage == 0.74,
    #vaccination_test_seeking_multiplier == 0,
    #passive_detection_given_symptoms == 0.5,
    #rel_active_detection_vaccinated_source == 0,
    rel_active_detection_vaccinated_contact == 0
  ) %>%
  select(-parameters) %>%
  unnest(simulations) 

# make spaghetti plots of sims to check they make sense
plot_df_line <- plot_df %>% 
  group_by(simulation, 
           infection_day,
           ) %>% 
  summarise(infections=n()
            ) #%>% 
  #filter(simulation %in% paste0('sim_', 1:50))
  
ggplot(plot_df_line,
    aes(
      x=infection_day, 
      y=infections,
      group=simulation)
  ) + 
    geom_line(aes(color=simulation)) +
  theme(legend.position = "none") 

#
ggsave(
  "freyas_outputs/no_quarantine_vax_PCCs.png",
  width = 8,
  height = 4.5,
  dpi = 500
)

# calculate TPs
full_tps <- sims %>%
  mutate(
    metrics = list(
      compute_abm_metrics_full(simulations)
    )
  ) %>%
  select(
    -parameters,
    -simulations
  ) %>%
  unnest(
    cols = metrics
  )

# D and H
write.csv(sry_tps,
           'freyas_outputs/sry_tps_G.csv',
           row.names=FALSE)
 
write.csv(full_tps,
           'freyas_outputs/full_tps_G.csv',
           row.names=FALSE)

source('R/plot_functions.R')

# save outputs
full_tps <- read.csv('freyas_outputs/full_tps_80pc_DandH.csv',
                     stringsAsFactors = FALSE)


# loop over simulations calculating ratio between reference and others
full_tps_plot <- full_tps %>% 
  filter(near(ve_onward, 0.6390)) # 0.5751 0.6390 0.7029

sims <- unique(full_tps_plot$simulation)

sim_plot <- data.frame()

for (i in sims){
  
  sim_dat <- full_tps_plot %>% filter(simulation==i)
  
  ref_tp <- as.numeric(sim_dat %>% filter(rel_active_detection_vaccinated_source==1,
                            rel_active_detection_vaccinated_contact==1) %>% 
    select(TP))
    
  sim_sorted <- sim_dat %>% 
    mutate(pc_change=(TP/ref_tp-1)*100) 
  
  sim_plot <- rbind(sim_plot, sim_sorted)
  
}

sim_plot <- sim_plot %>% 
  mutate(rel_active_detection_vaccinated_source=case_when(
    rel_active_detection_vaccinated_source==1 ~ 'Vax & unvax cases',
    rel_active_detection_vaccinated_source==0 ~ 'Unvax cases only'),
    rel_active_detection_vaccinated_contact=case_when(
      rel_active_detection_vaccinated_contact==1 ~ 'Vax & unvax PCCs',
      rel_active_detection_vaccinated_contact==0 ~ 'Unvax PCCs only')
    ) %>% 
  mutate(rel_active_detection_vaccinated_source=factor(rel_active_detection_vaccinated_source,
                                                        levels=c('Vax & unvax cases', 'Unvax cases only'))) %>% 

sim_sry <- sim_plot %>% 
  group_by(rel_active_detection_vaccinated_source,
           rel_active_detection_vaccinated_contact) %>% 
  summarise(mean=mean(pc_change), se=se(pc_change)) %>% 
  mutate(label=paste0('                    ', round(mean, 1), '%')) %>% 
  filter(mean != 0)
  
# generate dot plots of TP % changes by scenarios
ggplot(sim_plot) +
  #geom_boxplot(aes(x=factor(strategy),
  #                  y=pc_change)) +
  geom_point(aes(x=factor(1),
                 y=pc_change),
             position=position_jitter(width=0.1),
             colour='pink',
             size=1) +
  geom_pointrange(data = sim_sry,
                  aes(x=factor(1), 
                      y=mean, 
                      ymin = mean-2*se,
                      ymax = mean+2*se
                      ),
                  size=0.3) +
  geom_hline(yintercept=0,
             linetype='dashed') +
  geom_text(data=sim_sry, 
            aes(x=factor(1),
                y=mean+1,
                label=label),
            size=3) +
  facet_grid(rel_active_detection_vaccinated_contact~rel_active_detection_vaccinated_source) +
  theme_bw() + 
  theme(panel.grid=element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(size=0.2, color = 'black'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        aspect.ratio = 1,
        strip.background =element_rect(fill='white')) +
  labs(x='',
       y='% increase in TP') %>% 
  xlab()

ggsave(
  "freyas_outputs/res_DandH_pink.png",
  width = 4.5,
  height = 4.5,
  dpi = 500
)
 





