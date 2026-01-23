

library(bslib)
library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(viridis)

source("compute_simulate.R")
source("vectorized_simulation_v2.R")

#this transforms from text to numeric
parse_nums <- function(txt) {
  if (is.null(txt) || txt == "") return(numeric(0))
  parts <- strsplit(txt, ",")[[1]] %>% trimws()
  vals <- suppressWarnings(as.numeric(parts))
  vals
}

# Define UI for application that draws a histogram
ui <- page_navbar(
  
  header = tagList(
    tags$div(
      style = "background-color: #000; color: #fff; width: 100%; z-index: 2000;",
      img(src = "cdl_logo.svg", height = "35px"),
      span("Cognitive Dynamics Lab", style = "margin-left: 15px; font-weight: bold; text-transform: uppercase;")
    ),
    # This adds a small gap so the nav bar doesn't touch the black bar
    tags$style(HTML(".navbar { border-top: 1px solid #eee; }"))
  ),
  
  
  title = "Attentional Decision-Making Lab",
  theme = bs_theme(
    version = 5,
    bootswatch = "lux",
    primary = "#2c3e50"
  ),
  
  # nav_panel("Welcome",
  #           icon = icon("house"),
  #           "Welcome"
  # ),
  
  nav_panel(
    title = "Computation/Simulation Lab",
    icon = icon("microchip"),
    
    # Sidebar with a slider input for number of bins
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        title = "Model Parameters",
        helpText("Enter comma-separated values for parameter sets."),
        
        accordion(
          accordion_panel(
            title = "Timing & Probabilities",
            icon = icon("clock"),
            textInput("baseline", "Baseline RT (comma separated, positive)", value = "1"),
            textInput("checkrt", "Check RT (comma separated, positive)", value = "1.5"),
            textInput("switchp", "P(Switch) (comma separated, 0-1)", value = "0.05"),
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
        
        radioButtons("absolute_or_relative", label = "Absolute or relative rewards?", choices = c("relative", "absolute")),
        
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
          ),
          layout_columns(
            #div(DT::dataTableOutput("cond_table1"), style = "font-size: 50%; width: 100%")
            DT::dataTableOutput("cond_table1", width = "100%", height = "10%")
          )
        ),
        
        nav_panel(
          "Model Correlation",
          card(
            card_header("Simulated vs. Computed Accuracy"),
            plotlyOutput("cor_plot", height = "500px")
          ),
          #dataTableOutput("cond_table2")
          div(DT::dataTableOutput("cond_table2"), style = "font-size: 15%; width: 35%")
        ),
        
        
      )
    )
  ),
  
  nav_panel("Empirical Findings",
            icon = icon("chart-line"),
            navset_pill(
              nav_panel("Experiment 1", "E1"),
              nav_panel("Experiment 2", "E2"),
              nav_panel("Experiment 3", "E3")
            )
  ),
  
  nav_panel("About",
            icon = icon("info-circle"),
            "About content"
  ),
  
  nav_panel("People",
            icon = icon("user-group"),
            "People content"
  )
)


server <- function(input, output) {
  
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
  
  
  computation_results <- reactive({
    
    params <- params()
    
    max_parameters <- lapply(params, length) %>% unlist() %>% max()
    
    data.frame(baseline = rep(params$baseline, length.out = max_parameters),
               checkrt  = rep(params$checkrt, length.out = max_parameters),
               switchp  = rep(params$switchp, length.out = max_parameters),
               win      = rep(params$win, length.out = max_parameters),
               loss     = rep(params$loss, length.out = max_parameters),
               iti      = rep(params$iti, length.out = max_parameters),
               ttime    = rep(params$ttime, length.out = max_parameters)
    ) %>%
      mutate(cond = 1:n()) %>%
      mutate(color = viridis::magma(n(), begin = 0.2, end = 0.8)) %>%
      rowwise() %>%
      mutate(
        predictions = list(
          EarningsWhenChecking(
            BaselineRT = baseline,
            CheckRT = checkrt,
            Win = win,
            Loss = loss,
            Delay = 0, # I want the user to add that to the check rt themselves
            SwitchP = switchp,
            TTime = ttime,
            ITI = iti
          )[-3] %>% as.data.frame()
        )
      ) %>%
      ungroup() %>%
      unnest(predictions) %>%
      group_by(cond) %>%
      mutate(reward_at_opt = max(earnings),
             rel_reward = earnings / reward_at_opt) %>%
      ungroup()
    
  })
  
  output$comp_plot <- renderPlotly({
    
    computation_results <- computation_results()
    
    plt <- ggplot(computation_results)
    
    if (input$absolute_or_relative == "relative"){
      
      plt <- plt +
        geom_line(aes(x = probabilities, y = rel_reward, group = cond, color = color))+
        labs(x = "Check Rate",
             y = "Relative reward")
      
    } else {
      
      plt <- plt +
        geom_line(aes(x = probabilities, y = earnings, group = cond, color = color))+
        labs(x = "Check Rate",
             y = "Absolute reward")
      
    }
    
    plt <- plt +
      scale_color_identity()+
      theme_classic()+
      theme(legend.position = "none")
    
    ggplotly(plt)
  })
  
  
  simulation_results <- eventReactive(input$sim, {
    
    params <- params()
    
    max_parameters <- lapply(params, length) %>% unlist() %>% max()
    
    data.frame(baseline = rep(params$baseline, length.out = max_parameters),
               checkrt  = rep(params$checkrt, length.out = max_parameters),
               switchp  = rep(params$switchp, length.out = max_parameters),
               win      = rep(params$win, length.out = max_parameters),
               loss     = rep(params$loss, length.out = max_parameters),
               iti      = rep(params$iti, length.out = max_parameters),
               ttime    = rep(params$ttime, length.out = max_parameters)
    ) %>%
      mutate(cond = 1:n()) %>%
      mutate(color = magma(n(), begin = 0.2, end = 0.8)) %>%
      rowwise() %>%
      mutate(
        predictions = list(
          run_IE_sim(RT_nC = baseline,
                     RT_CC = checkrt,
                     Win = win,
                     Loss = loss,
                     SwitchP = switchp,
                     Blockdur = ttime,
                     ITI = iti,
                     ITI_randomness = 0,
                     guess = 0,
                     CheckP = seq(0.01, 1, 0.02),
                     reps = 100,
                     quiet = TRUE
          )
        )
      ) %>%
      ungroup() %>%
      unnest(predictions)
    
  })
  
  
  
  output$sim_plot <- renderPlotly({
    
    simulation_results <- simulation_results()
    
    plt <- ggplot(simulation_results)
    
    if (input$absolute_or_relative == "relative"){
      
      plt <- plt +
        geom_line(aes(x = CheckP, y = rel_reward, group = cond, color = color))+
        labs(x = "Check Rate",
             y = "Relative reward")
      
    } else {
      
      plt <- plt +
        geom_line(aes(x = CheckP, y = final_reward, group = cond, color = color))+
        labs(x = "Check Rate",
             y = "Absolute reward")
      
    }
    
    plt <- plt +
      scale_color_identity()+
      theme_classic()+
      theme(legend.position = "none")
    
    ggplotly(plt)
  })
  
  
  output$cor_plot <- renderPlotly({
    
    s <- simulation_results()
    c <- computation_results()
    
    s <- s %>%
      select(cond, color, CheckP, final_reward, rel_reward) %>%
      rename(probabilities = CheckP,
             sim_abs_reward = final_reward,
             sim_rel_reward = rel_reward) %>%
      mutate(probabilities = round(probabilities, 2))
    
    c <- c %>%
      select(cond, color, probabilities, earnings, rel_reward) %>%
      rename(com_abs_reward = earnings,
             com_rel_reward = rel_reward)%>%
      mutate(probabilities = round(probabilities, 2))
    
    s_c <- left_join(s, c)
    
    if (input$absolute_or_relative == "relative"){
      plt <- ggplot(s_c, aes(com_rel_reward, sim_rel_reward, color = color))+
        labs(x = "computed: relative reward",
             y = "simulated: relative reward")
    } else {
      plt <- ggplot(s_c, aes(com_abs_reward, sim_abs_reward, color = color))+
        labs(x = "computed: absolute reward",
             y = "simulated: absolute reward")
      
    }
    
    plt <- plt +
      scale_color_identity()+
      geom_point()+
      geom_smooth(method = "lm", se = FALSE)+
      geom_abline(slope = 1, intercept = 0)+
      theme_classic()+
      theme(legend.position = "none")
    
    ggplotly(plt)
    
  })
  
  cond_table <- reactive({
    
    computation_results() %>%
      group_by(cond) %>%
      filter(earnings == reward_at_opt) %>%
      ungroup() %>%
      select(-reward_at_opt, -rel_reward, -cond) %>%
      relocate(color, baseline, checkrt, switchp, win, loss, iti, ttime, probabilities, earnings) %>%
      mutate(earnings = round(earnings, 2),
             probabilities = round(probabilities, 2)) %>%
      rename("Win" = win,
             "Loss" = loss,
             "Baseline RT" = baseline,
             "Check RT" = checkrt,
             "P(Switch)" = switchp,
             "ITI" = iti,
             "Block duration" = ttime,
             "Optimal Earnings" = earnings,
             "Optimal Check Rate" = probabilities) %>%
      datatable(
        class = 'cell-border stripe compact nowrap',
        rownames = FALSE,
        options = list(
          searching = FALSE, 
          lengthChange = FALSE, 
          dom = 't',           # 't' means ONLY the table (hides search/pagination info)
          scrollY = "200px",   # Fixes height and adds a scroll bar
          scrollX = TRUE,      # Allows horizontal scrolling for small screens
          pageLength = -1      # Shows all rows since we are using a scroll bar
        )
      ) %>%
      formatStyle("color", target = "cell", backgroundColor = styleValue()) %>%
      formatStyle("color", color = 'transparent')
  })
  
  output$cond_table1 <- renderDataTable({
    cond_table()
  })
  
  # Output for the second location (e.g., a sub-pane or footer)
  output$cond_table2 <- renderDataTable({
    cond_table()
  })
  
}


# Run the application
shinyApp(ui = ui, server = server)
