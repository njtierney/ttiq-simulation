#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# library(shinyjqui)
library(targets)
source("../../packages.R")
lapply(list.files("../../R", full.names = TRUE), source)
tar_load(derived_delay_distributions, store="../../_targets")

# Define arbitrary ranking functions
priority_ranking_priority_new_swab <- function(x, sim_day, notification_time) {
    x %>%
        arrange(
            # Whether case is eligible to be interviewed
            desc(eligible_for_interview),
            # Priorities, in order of appearance
            desc(priority_group & (notification_date + priority_info_delay) <= sim_day), # priority group when status is available after delay
            desc(notification_date >= sim_day), # notified today first (maximise day 0s)
            desc(swab_date), # newest first
        )
}

random_swab <- function(x, sim_day, notification_time) {
    x %>%
        mutate(ix = runif(n())) %>%
        arrange(
            # Whether case is eligible to be interviewed
            desc(eligible_for_interview),
            # Priorities, in order of appearance
            ix
        ) %>%
        select(-ix)
}


priority_ranking_priority_vaccine_old_swab <- function(x, sim_day, notification_time) {
    x %>%
        arrange(
            # Whether case is eligible to be interviewed
            desc(eligible_for_interview),
            # Priorities, in order of appearance
            desc(priority_group & (notification_date + priority_info_delay) <= sim_day), # priority group when status is available after delay
            vaccinated, # vaccinated FALSE first
            desc(notification_date >= sim_day), # notified today first (maximise day 0s)
            swab_date, # oldest first
        )
}

priority_ranking_priority_vaccine_new_swab <- function(x, sim_day, notification_time) {
    x %>%
        arrange(
            # Whether case is eligible to be interviewed
            desc(eligible_for_interview),
            # Priorities, in order of appearance
            desc(priority_group & (notification_date + priority_info_delay) <= sim_day), # priority group when status is available after delay
            vaccinated, # vaccinated FALSE first
            desc(notification_date >= sim_day), # notified today first (maximise day 0s)
            desc(swab_date), # newest first
        )
}

priority_ranking_vaccine_priority_old_swab <- function(x, sim_day, notification_time) {
    x %>%
        arrange(
            # Whether case is eligible to be interviewed
            desc(eligible_for_interview),
            # Priorities, in order of appearance
            vaccinated, # vaccinated FALSE first
            desc(priority_group & (notification_date + priority_info_delay) <= sim_day), # priority group when status is available after delay
            desc(notification_date >= sim_day), # notified today first (maximise day 0s)
            swab_date, # oldest first
        )
}

priority_ranking_vaccine_priority_new_swab <- function(x, sim_day, notification_time) {
    x %>%
        arrange(
            # Whether case is eligible to be interviewed
            desc(eligible_for_interview),
            # Priorities, in order of appearance
            vaccinated, # vaccinated FALSE first
            desc(priority_group & (notification_date + priority_info_delay) <= sim_day), # priority group when status is available after delay
            desc(notification_date >= sim_day), # notified today first (maximise day 0s)
            desc(swab_date), # newest first
        )
}
priority_ranking_vaccine_priority_old_notification_old_swab <- function(x, sim_day, notification_time) {
    x %>%
        arrange(
            # Whether case is eligible to be interviewed
            desc(eligible_for_interview),
            # Priorities, in order of appearance
            vaccinated, # vaccinated FALSE first
            desc(priority_group & (notification_date + priority_info_delay) <= sim_day), # priority group when status is available after delay
            notification_date >= sim_day, # notified today last
            swab_date, # oldest first
        )
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Queue results"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            numericInput("n_samples",
                         "Samples",
                         min = 1,
                         max = 1e6,
                         value = 1e3),
            sliderInput("capacity_ratio",
                        "Daily capacity",
                        min = 0,
                        max = 1,
                        value = 0.8),
            sliderInput("max_interview_delay",
                        "Drop swabs after",
                        min = 0,
                        max = 14,
                        value = 5),
            sliderInput("proportion_cases_vaccinated",
                        "Proportion cases vaccinated",
                        min = 0,
                        max = 1,
                        value = 0.05),
            radioButtons("ranking_function",
                         "Prioritise case interviews by",
                         choices = c("priority_vaccine_new_swab", "vaccine_priority_new_swab", "priority_vaccine_old", "random")),
            radioButtons("priority_delay_function",
                         "Discovery of priority groups",
                         choices = c("Instantaneous", "Mean 1d"),
                         selected = "Mean 1d")
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plot", width="100%")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observe(print(reactiveValuesToList(input)))
    
    priority_delay_function = reactive({
        if (input$priority_delay_function == "Instantaneous") {
            f = function(n) rep(0, n)
        } else {
            f = function(n) rpois(n, 1)
        }
        f
    })
    
    ranking_function = reactive({
        if (input$ranking_function == "priority_vaccine_new_swab") {
            f = priority_ranking_priority_vaccine_new_swab
        } else if (input$ranking_function == "vaccine_priority_new_swab") {
            f = priority_ranking_priority_vaccine_new_swab
        } else if (input$ranking_function == "priority_vaccine_old") {
            f = priority_ranking_priority_vaccine_old_swab
        } else if (input$ranking_function == "random") {
            f = random_swab
        }
        f
    })
    
    sim_tracing_output = reactive({
        withProgress({
            sim_tracing(
                derived_delay_distributions %>%
                    # only using 'optimal' for now
                    filter(scenario == "optimal"),
                capacity_ratio = input$capacity_ratio,
                prop_priority = 0.2,
                prop_time_delay = 0.2,
                max_interview_delay = input$max_interview_delay,
                priority_delay_distribution = priority_delay_function(),
                f_priority = ranking_function(),
                proportion_cases_vaccinated = input$proportion_cases_vaccinated,
                n_samples = input$n_samples
            )
        },
        message = "Running queue simulation")
    })
    
    output$plot <- renderPlot({
        gg_queue_scenarios(list(sim_tracing_output()) %>% setNames(input$ranking_function))[[1]]
    },
    height = 800,
    res = 108)
}

# Run the application 
shinyApp(ui = ui, server = server)
