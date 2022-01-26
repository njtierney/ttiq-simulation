# Freya Shearer's file embedding 

## Loads all packages and defines how to handle NAMESPACE conflicts
source("./packages.R")

# 1)
# pull out numbers of new case per day 
# detected via symptomatic surveillance 
# detected via contact tracing

# matches to data collected by jurisdictions on cases 
# and reason for test

# 2)
# check trends in proportion of infections detected via each mode of detection over time

# 3)
# testing heuristic....
# estimated number of infections = average cases found by contact tracing + average cases found by symptomatic pres * correction factor

# correction factor = 1/(symptomatic fraction * test-seeking fraction)

# case ascertainment = average cases found by contact tracing + average cases found by symptomatic pres/ estimated number of infections

# compare case ascertainment to true case ascertainment

# multiple simulations - consider TTIQ response strategies
# generate cloud of true and estimated ascertainments

# 4) 
# repeat step 3) for different specified parameter values in the ABM i.e. symptomatic fraction and test-seeking fraction

# also vary parameters when computing correction factor i.e. different to 10% relative to ABM parameters

# 5) 
# repeat steps 3) and 4) with different values of R

# and consider case where R fluctuates above and below 1 

#####################3

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
  passive_detections=TRUE, # old do_screening
  contact_tracing=TRUE,
  workplace_screening=TRUE # revised do_screening
) %>%
  mutate(
    # tweak starting R to get optimal reproduction number at about 1
    R = case_when(
      vaccination_coverage == 0.9 ~ 5, # 4.6 fails <2.58
      vaccination_coverage == 0.8 ~ 4.6, # 4.6 fails < 1.9
      vaccination_coverage == 0.74 ~ 5.8 # 4.6 fails < 1.55
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
        passive_detections=passive_detections, # this is always on though, yeah?
        contact_tracing = contact_tracing,
        workplace_screening = workplace_screening
      )
    )
  ) %>%
  mutate(
    simulations = list(
      get_valid_abm_samples(parameters, n_samples = 200)
    )
  )  

# plot simulations to check that they make sense
plot_df <- sims %>%
  unnest(simulations) %>% 
  group_by(simulation, 
           infection_day,
  ) %>% 
  summarise(infections=n()
  ) #%>% 
#filter(simulation %in% paste0('sim_', 1:50))

ggplot(plot_df,
       aes(
         x=infection_day, 
         y=infections,
         group=simulation)
) +
  theme_cowplot() +
  geom_line(aes(color=simulation)) +
  theme(legend.position = "none") 
  
ggsave(
  'outputs/plots/infections_R5.8_pdetect0.5.png',
  height=6,
  width=8,
  bg='white'
)


# clean up simulations
trim_sims <- sims %>% 
  unnest(simulations) %>% 
  filter(
  # find sources to consider (exclude those during burn in and last two weeks
  # due to truncation of onward infection)
  !is.na(source_id) &
    infection_day > 20 & # 70% 20-50, 80% 50-80, 90% 10-300
    infection_day < (max(infection_day) - 14)) %>% 
  group_by(simulation,
           infection_day,
           case_found_by) %>% 
  summarise(infections=n())

# 2)
# check trends in mode of detection of cases over time
# plot the number of cases 
# detected via symptomatic surveillance
# detected via active detection
# neither
trim_sims <- trim_sims %>% 
  mutate(case_found_by=case_when(
    is.na(case_found_by)~'undetected',
    TRUE ~ case_found_by)
  )
  
ggplot(trim_sims) +
  aes(
    x=infection_day, 
    y=infections,
    group=simulation) + 
  facet_wrap(
    ~case_found_by,
    ncol=4
  ) +
  theme_cowplot() +
  geom_line(aes(color=simulation)) +
  theme(legend.position = "none") +
  labs(title="")
    
ggsave(
  'outputs/plots/infections_trajectories_by_mode_pdetect0.5.png',
  height=6,
  width=12,
  bg='white'
)

sry_sims <- trim_sims %>% 
  group_by(case_found_by,
        infection_day) %>% 
  summarise(mean=mean(infections)) %>% 
  group_by(infection_day)%>% 
  mutate(sum=sum(mean)) %>% 
  mutate(fraction=mean/sum)

ggplot(sry_sims) + 
  geom_col(aes
           (x = infection_day,
             y = fraction, 
             fill = case_found_by
           )
           )+
  scale_fill_manual(values = c('#b3cde3',
                               '#ccebc5',
                               '#decbe4',
                               "grey")) +
  theme_cowplot() +
  ylab('Fraction') +
  xlab("Infection day") 

ggsave(
  'outputs/plots/fraction_infections_by_mode_pdetect0.5.png',
  height=6,
  width=12,
  bg='white'
)

# 3)

# estimated number of infections = average cases found by contact tracing + average cases found by symptomatic pres * correction factor

# correction factor = 1/(symptomatic fraction * test-seeking fraction)
correction_factor <- 1/(0.307*0.2)

# first check that when contact tracing is switched off - that we can accurately estimate the number of detected infections using the correction factor
case_ascertainment <- trim_sims %>% 
  group_by(case_found_by,
           simulation) %>% # groups by detected/screening and simulation
  summarise(infections_by_sim_mode=sum(infections)) %>% # total infections by detection mode and sim
  group_by(simulation) %>% 
  mutate(all_infections=sum(infections_by_sim_mode)) %>% # total infections by sim
  filter(case_found_by=='passive_surveillance') %>% 
  mutate(estimated_infections=infections_by_sim_mode*correction_factor) %>% 
  rename(detected_infections=infections_by_sim_mode) %>% 
  mutate(case_ascertainment=detected_infections/estimated_infections) %>% 
  mutate(true_case_ascertainment=detected_infections/all_infections) %>% 
  distinct(simulation, case_ascertainment, true_case_ascertainment) %>% 
  pivot_longer(cols=c(case_ascertainment, true_case_ascertainment))


  

# case ascertainment = (average cases found by contact tracing + average cases found by symptomatic pres)/ estimated number of infections

# compare case ascertainment to true case ascertainment

# for each simulation, calculate the estimated number of infections
metrics <- trim_sims %>% 
  group_by(case_found_by,
           simulation) %>% 
  summarise(sim_infections=sum(infections)) %>% 
  group_by(simulation) %>% 
  mutate(all_infections=sum(sim_infections))
  
case_ascertainment <- metrics %>%  
  summarise(detected_infections=all_infections-sim_infections[case_found_by=='undetected']) %>%
  distinct() %>% 
  left_join(metrics, 
            by='simulation') %>% 
  # do some pivot_wider here
  pivot_wider(
    names_from = case_found_by,
    values_from = sim_infections
  ) %>%
  mutate(
    estimated_infections = contact_tracing + workplace_screening + (passive_surveillance * correction_factor)) %>% 
  # mutate(estimated_infections=sim_infections[case_found_by=='contact_tracing']+
  #          (sim_infections[case_found_by=='screening']*correction_factor)) %>% 
  mutate(case_ascertainment=detected_infections/estimated_infections) %>% 
  mutate(true_case_ascertainment=detected_infections/all_infections) %>% 
  distinct(simulation, case_ascertainment, true_case_ascertainment) %>% 
  pivot_longer(cols=c(case_ascertainment, true_case_ascertainment))



# multiple simulations - consider TTIQ response strategies
# generate cloud of true and estimated ascertainments  

ggplot(case_ascertainment) +
  geom_point(aes(x=factor(name),
                 y=value),
             position=position_jitter(width=0.1),
             colour='grey40',
             size=1) +
  # geom_pointrange(data = sim_sry,
  #                 aes(x=factor(1), 
  #                     y=mean, 
  #                     ymin = mean-2*se,
  #                     ymax = mean+2*se
  #                 ),
  #                 size=0.3) +
  #facet_grid(rel_active_detection_vaccinated_contact~rel_active_detection_vaccinated_source) +
  theme_cowplot() + 
  ylim(0,1) +
  theme(panel.grid=element_blank(), 
        panel.border = element_blank(),
        axis.line = element_line(size=0.2, color = 'black'),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        aspect.ratio = 1,
        axis.text.x = element_text(angle=45, hjust=1),
        strip.background =element_rect(fill='white')) +
  labs(x='',
       y='case ascertainment')  

ggsave(
  #paste0('outputs/plots/ca_pdetect_', passive_detection_given_symptoms, '.png'),
  'outputs/plots/ca_pdetect_0.2_no_tracing.png',
  height=8,
  width=5,
  bg='white'
)




