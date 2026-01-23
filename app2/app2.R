library(bslib)
library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(viridis)

# Ensure these files are in your app directory
# source("compute_simulate.R")
# source("vectorized_simulation_v2.R")

# --- UTILITY FUNCTIONS ---
parse_nums <- function(txt) {
  if (is.null(txt) || txt == "") return(numeric(0))
  parts <- strsplit(txt, ",")[[1]] %>% trimws()
  vals <- suppressWarnings(as.numeric(parts))
  vals[!is.na(vals)]
}

# --- UI DEFINITION ---
ui <- page_navbar(
  title = "Attentional Decision-Making Lab",
  theme = bs_theme(
    version = 5,
    bootswatch = "lux", 
    primary = "#2c3e50"
  ),
  
  nav_panel(
    title = "Simulation Lab",
    icon = icon("microchip"),
    
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        title = "Model Parameters",
        
        helpText("Enter comma-separated values for parameter sets."),
        
        accordion(
          accordion_panel(
            "Timing & Probabilities",
            icon = icon("clock"),
            textInput("baseline", "Baseline RT", value = "1"),
            textInput("checkrt", "Check RT", value = "1.5"),
            textInput("switchp", "P(Switch)", value = "0.05"),
            textInput("iti", "ITI", value = "0.1")
          ),
          accordion_panel(
            "Rewards & Environment",
            icon = icon("dollar-sign"),
            textInput("win", "Win Value", value = "1"),
            textInput("loss", "Loss Value", value = "-1"),
            textInput("ttime", "Block Duration", value = "150")
          )
        ),
        
        hr(),
        radioButtons("absolute_or_relative", "Display Mode:", 
                     choices = c("Relative" = "relative", "Absolute" = "absolute"),
                     inline = TRUE),
        
        layout_columns(
          actionButton("recalc", "Compute", icon = icon("calculator"), class = "btn-primary"),
          actionButton("sim", "Simulate", icon = icon("play"), class = "btn-success"),
          col_widths = c(6, 6)
        )
      ),
      
      navset_card_underline(
        title = "Results Analysis",
        full_screen = TRUE,
        
        nav_panel(
          "Performance Curves",
          layout_columns(
            card(
              card_header("Theoretical (Computed)"),
              plotlyOutput("comp_plot")
            ),
            card(
              card_header("Stochastic (Simulated)"),
              plotlyOutput("sim_plot")
            ),
            col_widths = c(6, 6)
          )
        ),
        
        nav_panel(
          "Model Correlation",
          card(
            card_header("Simulated vs. Computed Accuracy"),
            plotlyOutput("cor_plot", height = "500px")
          )
        ),
        
        nav_panel(
          "Parameter Table",
          card(
            card_header("Optimization Summary"),
            dataTableOutput("cond_table")
          )
        )
      )
    )
  ),
  
  nav_panel("Empirical Findings", icon = icon("chart-line"), "Experimental data content..."),
  nav_panel("About", icon = icon("info-circle"), "Model details...")
)

# --- SERVER LOGIC ---
server <- function(input, output, session) {
  
  # Reactive parameters
  params <- eventReactive(input$recalc, {
    list(
      baseline = parse_nums(input$baseline),
      checkrt  = parse_nums(input$checkrt),
      switchp  = parse_nums(input$switchp),
      win      = parse_nums(input$win),
      loss     = parse_nums(input$loss),
      iti      = parse_nums(input$iti),
      ttime    = parse_nums(input$ttime)
    )
  }, ignoreNULL = FALSE)
  
  # Computed Model Logic
  computation_results <- reactive({
    p <- params()
    max_p <- max(lengths(p))
    
    data.frame(
      baseline = rep(p$baseline, length.out = max_p),
      checkrt  = rep(p$checkrt, length.out = max_p),
      switchp  = rep(p$switchp, length.out = max_p),
      win      = rep(p$win, length.out = max_p),
      loss     = rep(p$loss, length.out = max_p),
      iti      = rep(p$iti, length.out = max_p),
      ttime    = rep(p$ttime, length.out = max_p)
    ) %>%
      mutate(cond = 1:n(), color = magma(n(), begin = 0.2, end = 0.8)) %>%
      rowwise() %>%
      mutate(predictions = list(
        EarningsWhenChecking(baseline, checkrt, win, loss, 0, switchp, ttime, iti)[-3] %>% as.data.frame()
      )) %>%
      ungroup() %>%
      unnest(predictions) %>%
      group_by(cond) %>%
      mutate(reward_at_opt = max(earnings), rel_reward = earnings / reward_at_opt) %>%
      ungroup()
  })
  
  # Simulation Logic
  simulation_results <- eventReactive(input$sim, {
    p <- params()
    max_p <- max(lengths(p))
    
    data.frame(
      baseline = rep(p$baseline, length.out = max_p),
      checkrt  = rep(p$checkrt, length.out = max_p),
      switchp  = rep(p$switchp, length.out = max_p),
      win      = rep(p$win, length.out = max_p),
      loss     = rep(p$loss, length.out = max_p),
      iti      = rep(p$iti, length.out = max_p),
      ttime    = rep(p$ttime, length.out = max_p)
    ) %>%
      mutate(cond = 1:n(), color = magma(n(), begin = 0.2, end = 0.8)) %>%
      rowwise() %>%
      mutate(predictions = list(
        run_IE_sim(switchp, iti, ttime, win, loss, baseline, checkrt, reps = 100, CheckP = seq(0.01, 1, 0.02))
      )) %>%
      ungroup() %>%
      unnest(predictions)
  }, ignoreNULL = FALSE)
  
  # Output: Computed Plot
  output$comp_plot <- renderPlotly({
    df <- computation_results()
    y_var <- if(input$absolute_or_relative == "relative") "rel_reward" else "earnings"
    
    p <- ggplot(df, aes(x = probabilities, y = !!sym(y_var), group = cond, color = color)) +
      geom_line() + scale_color_identity() + theme_minimal() +
      labs(x = "Check Rate", y = input$absolute_or_relative)
    ggplotly(p)
  })
  
  # Output: Simulation Plot
  output$sim_plot <- renderPlotly({
    df <- simulation_results()
    y_var <- if(input$absolute_or_relative == "relative") "rel_reward" else "final_reward"
    
    p <- ggplot(df, aes(x = CheckP, y = !!sym(y_var), group = cond, color = color)) +
      geom_line() + scale_color_identity() + theme_minimal() +
      labs(x = "Check Rate", y = input$absolute_or_relative)
    ggplotly(p)
  })
  
  # Output: Correlation Plot
  output$cor_plot <- renderPlotly({
    s <- simulation_results() %>% select(cond, color, CheckP, final_reward, rel_reward) %>%
      rename(probabilities = CheckP, sim_abs = final_reward, sim_rel = rel_reward) %>%
      mutate(probabilities = round(probabilities, 2))
    
    c <- computation_results() %>% select(cond, color, probabilities, earnings, rel_reward) %>%
      rename(com_abs = earnings, com_rel = rel_reward) %>%
      mutate(probabilities = round(probabilities, 2))
    
    s_c <- left_join(s, c, by = c("cond", "color", "probabilities"))
    
    x_val <- if(input$absolute_or_relative == "relative") "com_rel" else "com_abs"
    y_val <- if(input$absolute_or_relative == "relative") "sim_rel" else "sim_abs"
    
    p <- ggplot(s_c, aes(x = !!sym(x_val), y = !!sym(y_val), color = color)) +
      geom_point(alpha = 0.5) + geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      scale_color_identity() + theme_minimal() + labs(title = "Computed vs. Simulated Correlation")
    ggplotly(p)
  })
  
  # Output: Table
  output$cond_table <- renderDataTable({
    computation_results() %>%
      group_by(cond) %>%
      filter(earnings == reward_at_opt) %>%
      ungroup() %>%
      select(color, baseline, checkrt, switchp, win, loss, iti, ttime, probabilities, earnings) %>%
      datatable(rownames = FALSE, options = list(searching = FALSE, lengthChange = FALSE)) %>%
      formatStyle("color", target = "cell", backgroundColor = styleValue(), color = "transparent")
  })
}

shinyApp(ui, server)
# 
# library(bslib)
# library(shiny)
# library(tidyverse)
# library(plotly)
# library(DT)
# 
# source("compute_simulate.R")
# source("vectorized_simulation_v2.R")
# 
# #this transforms from text to numeric
# parse_nums <- function(txt) {
#   if (is.null(txt) || txt == "") return(numeric(0))
#   parts <- strsplit(txt, ",")[[1]] %>% trimws()
#   vals <- suppressWarnings(as.numeric(parts))
#   vals
# }
# 
# # Define UI for application that draws a histogram
# ui <- fluidPage(
#   
#   # Application title
#   titlePanel("Computational Model for Attentional Decision-Making"),
#   
#   navset_pill( 
#     nav_panel("Computation/Simulation Lab", 
#               
#               # Sidebar with a slider input for number of bins 
#               sidebarLayout(
#                 sidebarPanel(
#                   helpText("Enter comma-separated values to plot multiple parameter sets. Inputs will be recycled if lengths differ."),
#                   textInput("baseline", "Baseline RT (comma separated, positive)", value = "1"),
#                   textInput("checkrt", "Check RT (comma separated, positive)", value = "1.5"),
#                   textInput("switchp", "P(Switch) (comma separated, 0-1)", value = "0.05"),
#                   # Now text inputs for Win and Loss (comma-separated)
#                   textInput("win", "Win (comma separated)", value = "1"),
#                   textInput("loss", "Loss (comma separated)", value = "-1"),
#                   textInput("iti", "ITI (comma separated)", value = "0.1"),
#                   textInput("ttime", "Block duration (comma separated)", value = "150"),
#                   
#                   radioButtons("absolute_or_relative", label = "Absolute or relative rewards?", choices = c("relative", "absolute")),
#                   
#                   actionButton("recalc", "Recalculate"),
#                   actionButton("sim", "Simulate")
#                 ),
#                 
#                 # # Show a plot of the generated distribution
#                 # mainPanel(
#                 #   plotlyOutput("comp_plot"),
#                 #   #tableOutput("cond_table"),
#                 #   dataTableOutput("cond_table")
#                 # ),
#                 
#                 mainPanel(
#                   layout_columns(
#                     card(
#                       card_header("Computational Model"),
#                       plotlyOutput("comp_plot")
#                     ),
#                     card(
#                       card_header("Simulation"),
#                       plotlyOutput("sim_plot")
#                     ),
#                     col_widths = c(6, 6) # Each takes half the width (out of 12)
#                   ),
#                   plotlyOutput("cor_plot"),
#                   dataTableOutput("cond_table")
#                 )
#               )
#     ), 
#     
#     nav_panel("Empirical Findings", 
#               navset_pill( 
#                 nav_panel("Experiment 1", "E1"),
#                 nav_panel("Experiment 2", "E2"),
#                 nav_panel("Experiment 3", "E3")
#               )
#     ), 
#     
#     nav_panel("About", 
#               "About content"
#     ), 
#     
#     nav_panel("People", 
#               "People content"
#     )
#   ),
#   
# )
# 
# 
# server <- function(input, output) {
#   
#   params <- eventReactive(input$recalc, {
#     list(
#       baseline = parse_nums(input$baseline),
#       checkrt  = parse_nums(input$checkrt),
#       switchp  = parse_nums(input$switchp),
#       win      = parse_nums(input$win),
#       loss     = parse_nums(input$loss),
#       iti      = parse_nums(input$iti),
#       ttime    = parse_nums(input$ttime)
#     )
#     
#   }, ignoreNULL = FALSE)
#   
#   
#   computation_results <- reactive({
#     
#     params <- params()
#     
#     max_parameters <- lapply(params, length) %>% unlist() %>% max()
#     
#     data.frame(baseline = rep(params$baseline, length.out = max_parameters),
#                checkrt  = rep(params$checkrt, length.out = max_parameters),
#                switchp  = rep(params$switchp, length.out = max_parameters),
#                win      = rep(params$win, length.out = max_parameters),
#                loss     = rep(params$loss, length.out = max_parameters),
#                iti      = rep(params$iti, length.out = max_parameters),
#                ttime    = rep(params$ttime, length.out = max_parameters)
#     ) %>%
#       mutate(cond = 1:n()) %>%
#       mutate(color = viridis::magma(n(), begin = 0.2, end = 0.8)) %>%
#       rowwise() %>%
#       mutate(
#         predictions = list(
#           EarningsWhenChecking(
#             BaselineRT = baseline,
#             CheckRT = checkrt,
#             Win = win,
#             Loss = loss,
#             Delay = 0, # I want the user to add that to the check rt themselves
#             SwitchP = switchp,
#             TTime = ttime,
#             ITI = iti
#           )[-3] %>% as.data.frame()
#         )
#       ) %>%
#       ungroup() %>%
#       unnest(predictions) %>%
#       group_by(cond) %>%
#       mutate(reward_at_opt = max(earnings),
#              rel_reward = earnings / reward_at_opt) %>%
#       ungroup()
#     
#   }) 
# 
#   output$comp_plot <- renderPlotly({
# 
#     computation_results <- computation_results()
#     
#     plt <- ggplot(computation_results) 
#     
#     if (input$absolute_or_relative == "relative"){
#       
#       plt <- plt +
#         geom_line(aes(x = probabilities, y = rel_reward, group = cond, color = color))+
#         labs(x = "Check Rate",
#              y = "Relative reward")
#       
#     } else {
#       
#       plt <- plt +
#         geom_line(aes(x = probabilities, y = earnings, group = cond, color = color))+
#         labs(x = "Check Rate",
#              y = "Absolute reward")
#       
#     }
#     
#     plt <- plt +
#       scale_color_identity()+
#       theme_classic()+
#       theme(legend.position = "none")
#     
#     ggplotly(plt)
#   })
#   
#   
#   simulation_results <- reactive({
#     
#     params <- params()
#     
#     max_parameters <- lapply(params, length) %>% unlist() %>% max()
#     
#     data.frame(baseline = rep(params$baseline, length.out = max_parameters),
#                checkrt  = rep(params$checkrt, length.out = max_parameters),
#                switchp  = rep(params$switchp, length.out = max_parameters),
#                win      = rep(params$win, length.out = max_parameters),
#                loss     = rep(params$loss, length.out = max_parameters),
#                iti      = rep(params$iti, length.out = max_parameters),
#                ttime    = rep(params$ttime, length.out = max_parameters)
#     ) %>%
#       mutate(cond = 1:n()) %>%
#       mutate(color = viridis::magma(n(), begin = 0.2, end = 0.8)) %>%
#       rowwise() %>%
#       mutate(
#         predictions = list(
#           run_IE_sim(RT_nC = baseline,
#                      RT_CC = checkrt, 
#                      Win = win, 
#                      Loss = loss, 
#                      SwitchP = switchp, 
#                      Blockdur = ttime, 
#                      ITI = iti, 
#                      ITI_randomness = 0, 
#                      guess = 0, 
#                      CheckP = seq(0.01, 1, 0.02),
#                      reps = 100,
#                      quiet = TRUE 
#           )
#         )
#       ) %>%
#       ungroup() %>%
#       unnest(predictions)
#     
#   }) 
#   
#   
#   
#   output$sim_plot <- renderPlotly({
#     # Call the reactive data (this creates the dependency)
#     simulation_results <- simulation_results()
#     
#     # Example: Accessing a value would be params$baseline
#     # For now, we just render your ggplot
#     
#     plt <- ggplot(simulation_results) 
#     
#     if (input$absolute_or_relative == "relative"){
#       
#       plt <- plt +
#         geom_line(aes(x = CheckP, y = rel_reward, group = cond, color = color))+
#         labs(x = "Check Rate",
#              y = "Relative reward")
#       
#     } else {
#       
#       plt <- plt +
#         geom_line(aes(x = CheckP, y = final_reward, group = cond, color = color))+
#         labs(x = "Check Rate",
#              y = "Absolute reward")
#       
#     }
#     
#     plt <- plt +
#       scale_color_identity()+
#       theme_classic()+
#       theme(legend.position = "none")
#     
#     ggplotly(plt)
#   })
#   
# 
#   output$cor_plot <- renderPlotly({
# 
#     s <- simulation_results()
#     c <- computation_results()
#     
#     s <- s %>% 
#       select(cond, color, CheckP, final_reward, rel_reward) %>%
#       rename(probabilities = CheckP, 
#              sim_abs_reward = final_reward,
#              sim_rel_reward = rel_reward) %>%
#       mutate(probabilities = round(probabilities, 2))
#     
#     c <- c %>% 
#       select(cond, color, probabilities, earnings, rel_reward) %>%
#       rename(com_abs_reward = earnings,
#              com_rel_reward = rel_reward)%>%
#       mutate(probabilities = round(probabilities, 2))
#     
#     s_c <- left_join(s, c)
#     
#     if (input$absolute_or_relative == "relative"){
#       plt <- ggplot(s_c, aes(com_rel_reward, sim_rel_reward, color = color))+
#         labs(x = "computed: relative reward",
#              y = "simulated: relative reward")
#     } else {
#       plt <- ggplot(s_c, aes(com_abs_reward, sim_abs_reward, color = color))+
#         labs(x = "computed: absolute reward",
#              y = "simulated: absolute reward")
#       
#     }
#     
#     plt <- plt +
#       scale_color_identity()+
#       geom_point()+
#       geom_smooth(method = "lm", se = FALSE)+
#       geom_abline(slope = 1, intercept = 0)+
#       theme_classic()+
#       theme(legend.position = "none")
#     plt
#   
#     ggplotly(plt)
#     
#   })
#   
#   output$cond_table <- renderDataTable({
#     
#     computation_results <- computation_results()
#     computation_results %>%
#       group_by(cond) %>%
#       filter(earnings == reward_at_opt) %>%
#       ungroup() %>%
#       select(-reward_at_opt, -rel_reward, -cond) %>%
#       relocate(color, baseline, checkrt, switchp, win, loss, iti, ttime, probabilities, earnings) %>%
#       mutate(earnings = round(earnings, 2)) %>%
#       rename("Win" = win,
#              "Loss" = loss,
#              "Baseline RT" = baseline,
#              "Check RT" = checkrt,
#              "P(Switch)" = switchp,
#              "ITI" = iti,
#              "Block duration" = ttime,
#              "Optimal Earnings" = earnings,
#              "Optimal Check Rate" = probabilities) %>%
#       datatable(
#         rownames = FALSE,
#         options = list(
#           searching = FALSE,    # Removes the search box
#           lengthChange = FALSE, # Removes the "Show XX entries" dropdown
#           pageLength = 10       # Optional: Set how many rows show by default
#         )
#       ) %>%
#       formatStyle(
#         "color",
#         target = "cell",
#         backgroundColor = styleValue()
#       ) %>%
#       formatStyle(
#         'color',
#         color = 'transparent' # The text is still there, but you can't see it
#       )
#     
#   })
#   
# }
# 
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
