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

# Define TP reductions
csv_path = "../../data-public/testing_delay_kretzschmar_table_2_extended.csv"
kretzschmar_tp_reduction = read_kretzschmar_tp_reduction(csv_path)
vaccine_tp_reduction = tribble(
    ~vaccine, ~vaccine_tp_reduction,
    "AZ", 0.36,
    "Pfizer", 0.65,
    "None", 0
)

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

priority_ranking_new_swab <- function(x, sim_day, notification_time) {
    x %>%
        arrange(
            # Whether case is eligible to be interviewed
            desc(eligible_for_interview),
            # Priorities, in order of appearance
            desc(notification_date >= sim_day), # notified today first (maximise day 0s)
            desc(swab_date), # newest first
        )
}

priority_ranking_vaccine_new_swab <- function(x, sim_day, notification_time) {
    x %>%
        arrange(
            # Whether case is eligible to be interviewed
            desc(eligible_for_interview),
            # Priorities, in order of appearance
            vaccinated, # vaccinated FALSE first
            desc(notification_date >= sim_day), # notified today first (maximise day 0s)
            desc(swab_date), # newest first
        )
}

priority_ranking_new_swab_vaccine <- function(x, sim_day, notification_time) {
    x %>%
        arrange(
            # Whether case is eligible to be interviewed
            desc(eligible_for_interview),
            # Priorities, in order of appearance
            desc(swab_date), # newest first
            vaccinated # vaccinated FALSE first
        )
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
            p(tags$b("Interviewable"), " case rate is hardcoded to 20 per day but the count doesn't matter, only the ratio of interviewable:capacity."),
            numericInput("n_samples",
                         "Samples",
                         min = 1,
                         max = 1e6,
                         value = 1e3,
                         step = 100),
            sliderInput("capacity_ratio",
                        "Daily capacity",
                        min = 0,
                        max = 2,
                        value = 0.8,
                        step = 0.05),
            helpText("Daily capacity is a proportion of cases to be interviewed (usually ~70% of cases), '1' when capacity=calls to make"),
            sliderInput("prop_time_delay",
                        "Proportion of cases pushed after COB",
                        min = 0,
                        max = 1,
                        value = 0.2,
                        step = 0.05),
            helpText("Fraction of cases per day that can't be interviewed that day because they are too late, e.g. >8pm"),
            sliderInput("max_interview_delay",
                        "Drop swabs after days",
                        min = 0,
                        max = 14,
                        value = 5),
            sliderInput("proportion_cases_vaccinated",
                        "Proportion cases vaccinated",
                        min = 0,
                        max = 1,
                        value = 0.05,
                        step = 0.05),
            radioButtons("vaccine_type", NULL,
                         choices = c("Pfizer", "AZ"),
                         inline = TRUE),
            sliderInput("prop_priority",
                        "Proportion cases in a priority group (after discovery)",
                        min = 0,
                        max = 1,
                        value = 0.2,
                        step = 0.05),
            radioButtons("ranking_function",
                         "Prioritise case interviews by",
                         choices = c("priority_vaccine_new_swab", "vaccine_priority_new_swab", "vaccine_new_swab", "new_swab_vaccine", "new_swab", "priority_vaccine_old", "random")),
            radioButtons("priority_delay_function",
                         "Discovery of priority groups",
                         choices = c("Instantaneous", "Mean 1d", "Never"),
                         selected = "Mean 1d")
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plot", width="100%", height="100%"),
            textOutput("tp_reduction"),
            downloadButton("download", "Download samples"),
            h4("Debug print"),
            verbatimTextOutput("print")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observe(print(reactiveValuesToList(input)))
    
    priority_delay_function = reactive({
        if (input$priority_delay_function == "Instantaneous") {
            f = function(n) rep(0, n)
        } else if (input$priority_delay_function == "Mean 1d") {
            f = function(n) rpois(n, 1)
        } else { # Never
            f = function(n) rep(Inf, n)
        }
        f
    })
    
    ranking_function = reactive({
        if (input$ranking_function == "priority_vaccine_new_swab") {
            f = priority_ranking_priority_vaccine_new_swab
        } else if (input$ranking_function == "vaccine_new_swab") {
            f = priority_ranking_vaccine_new_swab
        } else if (input$ranking_function == "new_swab_vaccine") {
            f = priority_ranking_new_swab_vaccine
        } else if (input$ranking_function == "new_swab") {
            f = priority_ranking_new_swab
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
                prop_priority = input$prop_priority,
                prop_time_delay = input$prop_time_delay,
                max_interview_delay = input$max_interview_delay,
                priority_delay_distribution = priority_delay_function(),
                f_priority = ranking_function(),
                proportion_cases_vaccinated = input$proportion_cases_vaccinated,
                n_samples = input$n_samples
            )
            
        },
        message = "Running queue simulation")
    })
    
    sim_tracing_output_processed = reactive({
        sim_tracing_output() %>%
            unnest() %>%
            # Calculate TP reductions
            mutate(
                samples_vaccinated = ifelse(samples_vaccinated, input$vaccine_type, "None")
            ) %>%
            left_join(vaccine_tp_reduction,
                      by = c("samples_vaccinated" = "vaccine")) %>%
            left_join(kretzschmar_tp_reduction,
                      by = c("samples_test_turnaround_time" = "Testing.delay",
                             "samples_time_to_interview" = "contact_tracing_delay")) %>%
            mutate(tp_reduction = 1 - (1-vaccine_tp_reduction) * (1-kretzschmar_tp_reduction))
    })
    
    
    output$download = downloadHandler(
        filename = function() "sim_output.csv",
        content = function(con) {
            write.csv(sim_tracing_output_processed(), con, row.names=FALSE)
        }
    )
    
    output$plot <- renderPlot({
        gg_queue_scenarios(list(sim_tracing_output()) %>% setNames(input$ranking_function))[[1]]
    },
    height = 800,
    res = 108)
    
    output$tp_reduction <- renderText({
        paste("Mean TP reduction:", round(mean(sim_tracing_output_processed()$tp_reduction), 3))
    })
    
    # Useful print output for general debugging
    output$print = renderPrint({
        sim_tracing_output_processed() %>%
            select(ends_with("reduction")) %>%
            sample_n(20) %>%
            as.data.frame()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
