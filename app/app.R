
library(bslib)
library(shiny)
library(tidyverse)
library(plotly)
library(DT)

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
ui <- fluidPage(
  
  # Application title
  titlePanel("Computational Model for Attentional Decision-Making"),
  
  navset_pill( 
    nav_panel("Computation/Simulation Lab", 
              
              # Sidebar with a slider input for number of bins 
              sidebarLayout(
                sidebarPanel(
                  helpText("Enter comma-separated values to plot multiple parameter sets. Inputs will be recycled if lengths differ."),
                  textInput("baseline", "Baseline RT (comma separated, positive)", value = "1"),
                  textInput("checkrt", "Check RT (comma separated, positive)", value = "1.5"),
                  textInput("switchp", "P(Switch) (comma separated, 0-1)", value = "0.05"),
                  # Now text inputs for Win and Loss (comma-separated)
                  textInput("win", "Win (comma separated)", value = "1"),
                  textInput("loss", "Loss (comma separated)", value = "-1"),
                  textInput("iti", "ITI (comma separated)", value = "0.1"),
                  textInput("ttime", "Block duration (comma separated)", value = "150"),
                  
                  radioButtons("absolute_or_relative", label = "Absolute or relative rewards?", choices = c("relative", "absolute")),
                  
                  actionButton("recalc", "Recalculate"),
                  actionButton("sim", "Simulate")
                ),
                
                # # Show a plot of the generated distribution
                # mainPanel(
                #   plotlyOutput("comp_plot"),
                #   #tableOutput("cond_table"),
                #   dataTableOutput("cond_table")
                # ),
                
                mainPanel(
                  layout_columns(
                    card(
                      card_header("Computatinal Model"),
                      plotlyOutput("comp_plot")
                    ),
                    card(
                      card_header("Simulation"),
                      plotlyOutput("sim_plot")
                    ),
                    col_widths = c(6, 6) # Each takes half the width (out of 12)
                  ),
                  dataTableOutput("cond_table")
                )
              )
    ), 
    
    nav_panel("B", 
              "Page B content"
    ), 
    
    nav_panel("C", 
              "Page C content"
    )
  ),
  
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
  
  
  simulation_results <- reactive({
    
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
          run_IE_sim(RT_nC = baseline,
                     RT_CC = checkrt, 
                     Win = win, 
                     Loss = loss, 
                     SwitchP = switchp, 
                     Blockdur = ttime, 
                     ITI = iti, 
                     ITI_randomness = 0, 
                     guess = 0.25, 
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
    # Call the reactive data (this creates the dependency)
    simulation_results <- simulation_results()
    
    # Example: Accessing a value would be params$baseline
    # For now, we just render your ggplot
    
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
  

  
  
  output$cond_table <- renderDataTable({
    
    computation_results <- computation_results()
    computation_results %>%
      group_by(cond) %>%
      filter(earnings == reward_at_opt) %>%
      ungroup() %>%
      select(-reward_at_opt, -rel_reward, -cond) %>%
      relocate(color, baseline, checkrt, switchp, win, loss, iti, ttime, probabilities, earnings) %>%
      mutate(earnings = round(earnings, 2)) %>%
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
        rownames = FALSE,
        options = list(
          searching = FALSE,    # Removes the search box
          lengthChange = FALSE, # Removes the "Show XX entries" dropdown
          pageLength = 10       # Optional: Set how many rows show by default
        )
      ) %>%
      formatStyle(
        "color",
        target = "cell",
        backgroundColor = styleValue()
      ) %>%
      formatStyle(
        'color',
        color = 'transparent' # The text is still there, but you can't see it
      )
    
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
