#To do:
#fix the radio buttons
#fix background color of scenario selector
#tooltips for the sidetask
#include equations
#would be nice to highlight columns with different input values


library(bslib)
library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(viridis)
library(shinycssloaders) 

source("compute_simulate.R")
source("vectorized_simulation_v2.R")

#this transforms from text to numeric
parse_nums <- function(txt) {
  if (is.null(txt) || txt == "") return(numeric(0))
  
  units <- strsplit(txt, ",")[[1]] %>% trimws()
  
  all_vals <- lapply(units, function(u) {
    if (grepl(":", u)) {
      #  colons
      parts <- as.numeric(strsplit(u, ":")[[1]])
      
      if (length(parts) == 2) {
        return(seq(from = parts[1], to = parts[2]))
      } else if (length(parts) == 3) {
        return(seq(from = parts[1], to = parts[3], by = parts[2]))
      } else {
        return(parts) 
      }
      
      
    } else if (grepl("\\*", u)) {
      
      parts <- strsplit(u, "\\*")[[1]] %>% trimws() %>% as.numeric()
      if (length(parts) == 2) {
        return(rep(parts[1], times = as.integer(parts[2])))
      }
    } else {
      return(suppressWarnings(as.numeric(u)))
    }
  })
  
  vals <- unlist(all_vals)
  return(vals[!is.na(vals)])
}



# ui ----



# Define UI for application that draws a histogram
ui <- page_navbar(
  
  id = "main_nav",
  
  header = tags$div(
    class = "custom-super-header",
    style = "
      position: fixed; 
      top: 0; 
      left: 0; 
      width: 100%; 
      height: 55px; 
      z-index: 9999; 
      display: flex; 
      align-items: center; 
      padding: 0 30px;
    ",
    tags$a(
      href = "https://blogs.uoregon.edu/cognitivedynamics/home/",
      target = "_blank",
      style = "display: flex; align-items: center; text-decoration: none; color: inherit;",
      img(src = "cdl_logo.svg", height = "35px"),
      span(
        "Cognitive Dynamics Lab",
        style = "margin-left: 15px; font-weight: bold; text-transform: uppercase; letter-spacing: 1px;"
      )
    ),
    tags$div(
      style = "margin-left: auto; display: flex; align-items: center; gap: 20px;",
      input_dark_mode(id = "dark_mode"),
      tags$a(
        href = "https://uoregon.edu",
        target = "_blank",
        img(src = "oregon_logo.svg", height = "35px")
      )
    )
  ),
  
  title = "Attentional Decision Hub",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "lux",
    primary = "#2c3e50"
  ),
  
  ## css ----
  
  tags$head(
    tags$style(HTML("
      

      /* =========================================================
         1. FIXED HEADERS SETUP
         ========================================================= */
      
      .custom-super-header {
        position: fixed !important;
        top: 0;
        left: 0;
        right: 0;
        min-height: 55px; /* Allows growth if content wraps */
        z-index: 1050;
        background-color: #000 !important;
        color: #fff !important;
        display: flex;
        align-items: center;
      }

      .bslib-page-navbar .navbar {
        position: fixed !important;
        top: 55px !important; /* Start below super-header */
        left: 0;
        right: 0;
        width: 100%;
        z-index: 1040;
        border: none !important;
        margin: 0 !important;
        /* Remove hard height to allow responsive wrapping */
        min-height: 55px; 
      }
      
      /* ================================
         2. TOGGLE ICON FIX
         ================================ */
      
      [data-bs-theme='dark'] #dark_mode,
      [data-bs-theme='dark'] #dark_mode svg,
      [data-bs-theme='dark'] .bslib-light-dark-switch svg {
        fill: #000000 !important;
        filter: brightness(0) !important;
      }
      
      #dark_mode,
      #dark_mode svg,
      .bslib-light-dark-switch svg {
        fill: #ffffff !important;
        filter: brightness(0) invert(1) !important;
        opacity: 1 !important;
      }

      /* =========================================================
         3. DYNAMIC COLORS
         ========================================================= */
      /* Light Mode */
      .bslib-page-navbar .navbar { 
        background-color: #F2F4F7 !important; 
      }
      
      .bslib-page-navbar .navbar-brand, .bslib-page-navbar .nav-link { 
        color: #000000 !important; 
      }

      /* Dark Mode */
      [data-bs-theme='dark'] .bslib-page-navbar .navbar { 
        background-color: #2b3035 !important; 
      }
      
      [data-bs-theme='dark'] .navbar-brand, [data-bs-theme='dark'] .nav-link { 
        color: #ffffff !important; 
      }
      [data-bs-theme='dark'] .custom-super-header { 
        background-color: #fff !important; 
        color: #000 !important; 
      }

      /* =========================================================
         4. UI & TABLE
         ========================================================= */
         
         /* Accordion & Text Visibility */
      .accordion-button.collapsed, .accordion-button:not(.collapsed) {
        background-color: #000000 !important; 
        color: #ffffff !important;           
      }
         
      .form-control, input, textarea { 
        background-color: #ffffff !important; 
        color: #000000 !important; 
      }
      
      [data-bs-theme='dark'] label { 
        color: #ffffff !important; 
      }

      .compact-table-container table.dataTable {
        font-size: 12px !important;      /* Shrinks the data font */
        line-height: 1 !important;       /* Removes vertical spacing between text */
        margin: 0 !important;
      }

      .compact-table-container table.dataTable thead th {
        font-size: 12px !important;      /* Forces headers even smaller than data */
        padding: 4px 2px !important;     /* Removes the 'cushion' inside header cells */
        background-color: transparent !important;
      }
      
      .compact-table-container table.dataTable tbody td {
        padding: 2px 4px !important;     /* Minimal gap between individual numbers */
        height: 18px !important;         /* Hard limit on row height */
        vertical-align: middle !important;
      }
      
      strong, b { 
        font-weight: 900 !important; 
        
        
      }
      
      
    ")),
    
    # This script measures the headers and pushes the content down dynamically
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        var updateSpacing = function() {
          var h1 = $('.custom-super-header').outerHeight();
          var h2 = $('.navbar').outerHeight();
          $('body').css('padding-top', (h1 + h2) + 'px');
          $('.navbar').css('top', h1 + 'px');
        };
        
        // Run on load and whenever window resizes
        updateSpacing();
        $(window).on('resize', updateSpacing);
        
        // Observer to catch if the navbar expands due to menu toggling
        var observer = new MutationObserver(updateSpacing);
        observer.observe(document.querySelector('.navbar'), { attributes: true, childList: true, subtree: true });
      });
    "))
  ),
  
  
  ## Welcome page ----
  nav_panel("Welcome",
            icon = icon("house"),
            markdown("
                     # Welcome
                     ### Decision-Making about our Attention
                     
                     We are interested in how our cognitive system controls where to focus attention. The answer to this question will depend on the context, but our approach to this question is through the lens of rational decision-making. 
                     
                     For this project, I am highlighting one context in which humans decide on an attentional checking policy - and our research has found that they consider the costs and benefits of possible strategies at least to some degree to make a decision.  
                     
                     #### Decision Variables to consider
                     When faced with an ambiguous task to which the response rules may change over time, how frequently should we check cues that reveal the task rule? For example, when completing a time-limited, open-book exam, how do we make decisions about when to check out notes? Let's consider the two extreme strategies: When you're checking your notes 0 % of the time, so never, you will be very fast (and can potentially respond to all questions), although at a cost of accuracy. On the other hand, checking notes for every question, so 100 % of the time, is very slow, so it comes at a cost of speed, resulting in fewer prompts answered.
                     Let's look at the following context and individual variables ore systematically:
                     * __Payoff for correct responses:__ If a question is worth a lot of points, it's more important to get it right, making checking _more_ adaptive.
                     * __Uncertainty__: If you're unsure about the question (i. e., more liekly to make an error), it may be _more_ adaptive to go and check for the correct response.
                     * __Time it takes to scramble for the answer:__ If you're running out of time and the information you need is in a moving box in the basement, it might take too long to check the answer, making checking more costly, and, _less_ adaptive.
                     * __Your response speed__: If you need longer to respond to an individual question, then it's more important to get the few questions you can tackle right. When the time cost for checking remains constant, slower reaction times make it *more* adaptive to check.
                     
                     These considerations transfer to a wide range of scenarios, that all have in common that decisions must be made between attenting to a primary task and relevant information in the environment (e. g., navigating a new city and checking the GPS, giving a speech and checking notes, cooking and checking a recipe, making a purchase and comparing prices etc.).
                     
                     #### Computational Model and Simulations
                     
                     We have developed a __computational model__ and __Monte-Carlo simulation__ that allows us to calculate the payoff for different attentional strategies. From a rational decision-making perspective, humans should select an attentional strategy that aligns with the maxmimum payoff. However, as you can see under the Computation/Simulation Lab, many contexts yield an optimality curve that is fairly broad, i. e., a function where a realtively wide range of checking strategies come relatively close to the optimum. 
                     
                     ### This website
                     
                     The goal of this website is to showcase my work. 
                     * Under the __Computation/Simulation Lab__ section, you can explore how manipulating the different parameters affect the attentional decision-making landscape. 
    
                     * In the __Empirical Findings__ tab, you can read about the experiments we have conducted and to which degree empirical checking rates collected from human subjects line up with the predicitons from this computational model.
                     * Read more about the team in the __People__ tab. 
                     
                     
                     "),
            
            layout_columns(
              col_widths = c(9, 3), # Smaller column for photo, larger for text
              # The Photo Container
              # The Text Content
              markdown("
              ### Funding
              Funding for this work comes from NSF grant 2120712.
                       
                       "),
              tags$div(
                tags$img(src = "NSF.svg", 
                         style = "width: 100px; height: 100px;"),
                style = "display: flex; flex-direction: column; align-items: center; gap: 15px;"
                
              ),
            ),
            
            
            
  ),
  
  ## Computation/Simulation Tool ----
  nav_panel(
    title = "Computation/Simulation Tool",
    icon = icon("microchip"),
    
    ### Scenario 1 ----
    
    navset_pill(
      nav_panel("Task-Switching Scenario",
                # Sidebar with a slider input for number of bins
                
                #### sidebar ----
                
                layout_sidebar(
                  sidebar = sidebar(
                    width = 350,
                    title = "Model Parameters",
                    helpText("Enter comma-separated values for parameter sets, or, for a series of numbers, numbers in the format from:increment:to (e. g., 2:1:5 is understood as 2, 3, 4, 5)"),
                    
                    accordion(
                      accordion_panel(
                        title = "Timing & Probabilities",
                        icon = icon("clock"),
                        span(
                          "Baseline RT ",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The expected response time when the participant does not perform a check.")
                        ),
                        textInput("baseline", NULL, value = "1"),
                        
                        span(
                          "Check RT (comma separated, positive)",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The expected response time when the participant does perform a check.")
                        ),
                        textInput("checkrt", NULL, value = "1.5"),
                        
                        span(
                          "P(Switch) (comma separated, positive)",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The trial-by-trial probabilty that the task switches.")
                        ),
                        textInput("switchp", NULL, value = "0.05"),
                        
                        span(
                          "ITI (comma separated, positive)",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The time interval in between trials. Modulates the rate of task completion.")
                        ),
                        textInput("iti", NULL, value = "0.1")
                      ),
                      accordion_panel(
                        "Rewards & Environment",
                        icon = icon("dollar-sign"),
                        span(
                          "Win Value",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The win amount per correctly completed trial.")
                        ),
                        textInput("win", NULL, value = "1"),
                        
                        span(
                          "Loss Value",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The loss amount per incorrect trial.")
                        ),
                        textInput("loss", NULL, value = "-1"),
                        
                        span(
                          "Block Duration",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The total duration per block.")
                        ),
                        textInput("ttime", NULL, value = "120")
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
                    
                    #### performance curves ----
                    
                    nav_panel(
                      "Performance Curves",
                      layout_columns(
                        card(
                          card_header("Theoretical (Computed)"),
                          withSpinner(plotlyOutput("comp_plot"), type = 8)
                        ),
                        card(
                          card_header("Stochastic (Simulated)"),
                          withSpinner(plotlyOutput("sim_plot"), type = 8)
                        ),
                        col_widths = c(6, 6)
                      ),
                      
                      card(
                        card_header("Model Results"),
                        div(class = "compact-table-container", 
                            DT::dataTableOutput("cond_table1")
                        )
                      ),
                      
                    ),
                    
                    
                    #### Model correlation ----
                    nav_panel(
                      "Model Correlation",
                      card(
                        card_header("Simulated vs. Computed Rewards"),
                        plotlyOutput("cor_plot", height = "500px")
                      ),
                      card(
                        card_header("Model Results"),
                        div(class = "compact-table-container", 
                            DT::dataTableOutput("cond_table2")
                        )
                      ),
                    ),
                    
                    nav_panel(
                      "Help",
                    ),
                  )
                )
                
      ),
      
      ### Sidetask ----
      nav_panel("Task + State checking",
                
                #### sidebar ----
                
                layout_sidebar(
                  sidebar = sidebar(
                    width = 350,
                    title = "Model Parameters",
                    helpText("Enter comma-separated values for parameter sets."),
                    
                    accordion(
                      accordion_panel(
                        title = "Timing & Probabilities",
                        icon = icon("clock"),
                        span(
                          "Baseline RT ",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The expected response time when the participant does not perform a check.")
                        ),
                        textInput("baseline_sidetask", NULL, value = "0.6"),
                        
                        span(
                          "Check RT (comma separated, positive)",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The expected response time when the participant does perform a check.")
                        ),
                        textInput("checkrt_sidetask", NULL, value = "1.7"),
                        
                        span(
                          "P(go on) (comma separated, positive)",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The trial-by-trial probabilty that the task switches.")
                        ),
                        textInput("p_go_on_sidetask", NULL, value = "0.1"),
                        
                        span(
                          "P(go off) (comma separated, positive)",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The trial-by-trial probabilty that the task switches.")
                        ),
                        textInput("p_go_off_sidetask", NULL, value = "0.1"),
                        
                        span(
                          "ITI (comma separated, positive)",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The time interval in between trials. Modulates the rate of task completion.")
                        ),
                        textInput("iti_sidetask", NULL, value = "0.2")
                      ),
                      accordion_panel(
                        "Rewards & Environment",
                        icon = icon("dollar-sign"),
                        span(
                          "Win for Task A",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The win amount per correctly completed trial.")
                        ),
                        textInput("win_a_sidetask", NULL, value = "1"),
                        
                        span(
                          "Loss for Task B",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The loss amount per correctly completed trial.")
                        ),
                        textInput("loss_b_sidetask", NULL, value = "0"),
                        
                        
                        span(
                          "Win for Task B",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The loss amount per incorrect trial.")
                        ),
                        textInput("win_b_sidetask", NULL, value = "0"),
                        
                        
                        span(
                          "Loss for Task B",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The loss amount per incorrect trial.")
                        ),
                        textInput("loss_b_sidetask", NULL, value = "-10"),
                        
                        span(
                          "Block Duration",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The total duration per block.")
                        ),
                        textInput("ttime_sidetask", NULL, value = "120")
                      )
                    ),
                    
                    hr(),
                    
                    radioButtons("absolute_or_relative_sidetask", label = "Absolute or relative rewards?", choices = c("relative", "absolute")),
                    
                    layout_columns(
                      actionButton("recalc_sidetask", "Compute", icon = icon("calculator"), class = "btn-primary"),
                      actionButton("sim_sidetask", "Simulate", icon = icon("play"), class = "btn-success"),
                      col_widths = c(6, 6)
                    )
                  ),
                  
                  
                  navset_card_underline(
                    title = "Results Analysis",
                    full_screen = TRUE,
                    
                    #### performance curves ----
                    
                    nav_panel(
                      "Performance Curves",
                      layout_columns(
                        card(
                          card_header("Theoretical (Computed)"),
                          withSpinner(plotlyOutput("comp_plot_sidetask"), type = 8)
                        ),
                        card(
                          card_header("Stochastic (Simulated)"),
                          withSpinner(plotlyOutput("sim_plot_sidetask"), type = 8)
                        ),
                        col_widths = c(6, 6)
                      ),
                      
                      card(
                        card_header("Model Results"),
                        div(class = "compact-table-container", 
                            DT::dataTableOutput("cond_table1_sidetask")
                        )
                      ),
                      
                    ),
                    
                    #### Model correlation ----
                    
                    nav_panel(
                      "Model Correlation",
                      card(
                        card_header("Simulated vs. Computed Rewards"),
                        plotlyOutput("cor_plot_sidetask", height = "500px")
                      ),
                      card(
                        card_header("Model Results"),
                        div(class = "compact-table-container", 
                            DT::dataTableOutput("cond_table2_sidetask")
                        )
                      ),
                    ),
                    
                    nav_panel(
                      "Help",
                    )
                  )
                )
                
      )
    )
  ),
  
  ## Empirical Findings ----
  
  nav_panel("Empirical Findings",
            icon = icon("chart-line"),
            navset_pill(
              nav_panel("Experiment 1", "E1"),
              nav_panel("Experiment 2", "E2"),
              nav_panel("Experiment 3", "E3")
            )
  ),
  
  ## people page ----
  
  nav_panel("People",
            icon = icon("user-group"),
            
            tags$h3("People", 
                    style = "text-transform: uppercase; font-weight: 150; letter-spacing: 2px; margin-bottom: 40px;"),
            
            
            # --- Profile Row 1: Dominik ---
            layout_columns(
              col_widths = c(3, 9),
              # The Photo Container
              tags$div(
                tags$img(src = "Dominik.jpg", 
                         style = "width: 150px; height: 150px; object-fit: cover; border-radius: 50%; border: 2px solid #eee; object-position: 50% 10%;"),
                style = "display: flex; flex-direction: column; align-items: center; gap: 15px;",
                
                
                tags$div(
                  style = "display: flex; gap: 15px; font-size: 1.2rem;",
                  # LinkedIn Link
                  tags$a(
                    href = "https://www.linkedin.com/in/dgraetz", 
                    target = "_blank",
                    icon("linkedin"),
                    style = "color: #0077b5; text-decoration: none;" # LinkedIn Blue
                  ),
                  
                  # ResearchGate Link
                  tags$a(
                    href = "https://www.researchgate.net/profile/Dominik-Graetz", 
                    target = "_blank",
                    icon("researchgate"),
                    style = "color: #00ccbb; text-decoration: none;" # ResearchGate Teal
                  ),
                  
                  # Email Link (Optional but helpful)
                  tags$a(
                    href = "mailto:dgrtz@uoregon.edu", 
                    icon("envelope"),
                    style = "color: #343a40; text-decoration: none;" # Dark Grey
                  )
                ),
              ),
              # The Text Content
              markdown("
#### Dominik Graetz
*Doctoral Candidate*

Dominik is a researcher specialized in modeling attentional decision-making. 
He developed the simulation framework powering this application. 

His current work focuses on empirical research on top-down controlled, bottom-up-driven, and context-dependent human attention.
    ")
            ),
            
            
            
            # --- Profile Row 2: Ulrich Mayr ---
            layout_columns(
              col_widths = c(3, 9), # Smaller column for photo, larger for text
              # The Photo Container
              tags$div(
                tags$img(src = "Ulrich.png", 
                         style = "width: 150px; height: 150px; object-fit: cover; border-radius: 50%; border: 2px solid #eee; object-position: 90% 20%;"),
                style = "display: flex; flex-direction: column; align-items: center; gap: 15px;",
                
                
                tags$div(
                  style = "display: flex; gap: 15px; font-size: 1.2rem;",
                  
                  # ResearchGate Link
                  tags$a(
                    href = "https://www.researchgate.net/profile/Ulrich-Mayr", 
                    target = "_blank",
                    icon("researchgate"),
                    style = "color: #00ccbb; text-decoration: none;" # ResearchGate Teal
                  ),
                  
                  # Email Link (Optional but helpful)
                  tags$a(
                    href = "mailto:mayr@uoregon.edu", 
                    icon("envelope"),
                    style = "color: #343a40; text-decoration: none;" # Dark Grey
                  )
                ),
              ),
              # The Text Content
              markdown("
#### Ulrich Mayr
*Principal Investigator*

Ulrich Mayr is a **Professor of Psychology**. His research focuses on the 
architecture and developmental trajectory of executive control processes, 
as well as the intersection of **attention** and **decision-making**. 

He leads the *Cognitive Dynamics Lab* in exploring how humans navigate complex 
task environments.
    ")
            ),
            
  )
)


# server ----

server <- function(input, output) {
  
  ## standardtask ----
  ### params ----
  params <- reactive({
    list(
      baseline = parse_nums(input$baseline),
      checkrt  = parse_nums(input$checkrt),
      switchp  = parse_nums(input$switchp),
      win      = parse_nums(input$win),
      loss     = parse_nums(input$loss),
      iti      = parse_nums(input$iti),
      ttime    = parse_nums(input$ttime)
    )
  })
  
  ### computation_results ----
  computation_results <- eventReactive(input$recalc, {
    
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
             rel_reward = earnings / reward_at_opt,
             is_max = ifelse(rel_reward == 1, 1, 0)) %>%
      ungroup()
    
  })
  
  ### comp_plot ----
  
  output$comp_plot <- renderPlotly({
    
    computation_results <- computation_results()
    
    plt <- ggplot(computation_results)
    
    if (input$absolute_or_relative == "relative"){
      
      plt <- plt +
        geom_line(aes(x = probabilities, y = rel_reward, group = cond, color = color, 
                      text = paste0(
                        "Check Rate: ", round(probabilities, 2), "\n",
                        "Reward: ", round(rel_reward, 2)
                      )))+
        geom_point(data = computation_results %>% filter(is_max == 1), aes(x = probabilities, y = rel_reward, group = cond, color = color), size = 3, shape = 23)+
        labs(x = "Check Rate",
             y = "Relative reward")
      
    } else {
      
      plt <- plt +
        geom_line(aes(x = probabilities, y = earnings, group = cond, color = color,
                      text = paste0(
                        "Check Rate: ", round(probabilities, 2), "\n",
                        "Reward: ", round(earnings, 2)
                      )))+
        geom_point(data = computation_results %>% filter(is_max == 1), aes(x = probabilities, y = earnings, group = cond, color = color), size = 3, shape = 23)+
        labs(x = "Check Rate",
             y = "Absolute reward")
      
    }
    
    plt <- plt +
      scale_color_identity()+
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))+
      theme_classic()+
      theme(legend.position = "none")
    
    ggplotly(plt, tooltip = "text")
  })
  
  ### simulation_results ----
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
        tmp_seed = runif(n()),
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
                     quiet = TRUE,
                     seed = tmp_seed
          )
        )
      ) %>%
      select(-tmp_seed) %>%
      ungroup() %>%
      unnest(predictions) %>%
      group_by(cond) %>%
      mutate(is_max = ifelse(rel_reward == 1, 1, 0))
    
  })
  
  
  ### render sim_plot ----
  output$sim_plot <- renderPlotly({
    
    simulation_results <- simulation_results()
    
    plt <- ggplot(simulation_results)
    
    if (input$absolute_or_relative == "relative"){
      
      plt <- plt +
        geom_line(aes(x = CheckP, y = rel_reward, group = cond, color = color, 
                      text = paste0(
                        "Check Rate: ", round(CheckP, 2), "\n",
                        "Reward: ", round(rel_reward, 2)
                      )))+
        geom_point(data = simulation_results %>% filter(is_max == 1), aes(x = CheckP, y = rel_reward, group = cond, color = color), size = 3, shape = 23)+
        labs(x = "Check Rate",
             y = "Relative reward")
      
    } else {
      
      plt <- plt +
        geom_line(aes(x = CheckP, y = final_reward, group = cond, color = color, 
                      text = paste0(
                        "Check Rate: ", round(CheckP, 2), "\n",
                        "Reward: ", round(final_reward, 2)
                      )))+
        geom_point(data = simulation_results %>% filter(is_max == 1), aes(x = CheckP, y = rel_reward, group = cond, color = color), size = 3, shape = 23)+
        labs(x = "Check Rate",
             y = "Absolute reward")
      
    }
    
    plt <- plt +
      scale_color_identity()+
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))+
      theme_classic()+
      theme(legend.position = "none")
    
    ggplotly(plt, tooltip = "text")
  })
  
  ### render cor plot ----
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
  
  ### cond_table ----
  cond_table <- reactive({
    
    
    # params <- params()
    # 
    # n_parameters <- lapply(params, function(x){length(unique(x))}) %>% unlist(use.names = TRUE)
    # 
    # recode_vec <- setNames(c(names(n_parameters),
    #                          "earnings",
    #                          "probabilities"), 
    #                        c("Baseline RT",
    #                          "Check RT",
    #                          "P(Switch)",
    #                          "Win",
    #                          "Loss",
    #                          "ITI",
    #                          "Block duration",
    #                          "Optimal Earnings",
    #                          "Optimal Check Rate"
    #                        ))
    #                          
    # 
    # n_parameters_new <- n_parameters
    # 
    # 
    # names(n_parameters_new) <- names(recode_vec[which(recode_vec == names(n_parameters))])
    
    computation_results() %>%
      group_by(cond) %>%
      filter(earnings == reward_at_opt) %>%
      ungroup() %>%
      select(-reward_at_opt, -rel_reward, -cond, -is_max) %>%
      relocate(color, baseline, checkrt, switchp, win, loss, iti, ttime, probabilities, earnings) %>%
      mutate(earnings = round(earnings, 2),
             probabilities = round(probabilities, 2)) %>%
      rename("Win" = "win",
             "Loss" = loss,
             "Baseline RT" = baseline,
             "Check RT" = checkrt,
             "P(Switch)" = switchp,
             "ITI" = iti,
             "Block duration" = ttime,
             "Optimal Earnings" = earnings,
             "Optimal Check Rate" = probabilities) %>%
      #rename(any_of(recode_vec)) %>%
      datatable(
        class = 'cell-border stripe compact',
        rownames = FALSE,
        options = list(
          ordering = FALSE,
          searching = FALSE, 
          lengthChange = FALSE, 
          dom = 't',           # 't' means ONLY the table (hides search/pagination info)
          scrollY = "200px",   # Fixes height and adds a scroll bar
          scrollX = TRUE,      # Allows horizontal scrolling for small screens
          pageLength = -1,      # Shows all rows since we are using a scroll bar
          columnDefs = list(
            list(
              targets = 0,      # Targets the first column ('color')
              width = '5px',   # Forces it to be narrow
              title = ""        # Removes the header name
            )
          )
        ) 
      ) %>%
      formatStyle("color", target = "cell", backgroundColor = styleValue()) %>%
      formatStyle("color", color = 'transparent') 
  })
  
  ### cond_table1 ----
  output$cond_table1 <- renderDataTable({
    cond_table()
  })
  
  ### cond_table2 ----
  # Output for the second location (e.g., a sub-pane or footer)
  output$cond_table2 <- renderDataTable({
    cond_table()
  })
  
  
  
  
  
  # Sidetask ----------------------------------------------------------------
  
  ### params_sidetask ----
  params_sidetask <- reactive({
    list(
      baseline = parse_nums(input$baseline_sidetask),
      checkrt  = parse_nums(input$checkrt_sidetask),
      p_go_on  = parse_nums(input$p_go_on_sidetask),
      p_go_off = parse_nums(input$p_go_off_sidetask),
      win_a    = parse_nums(input$win_a_sidetask),
      win_b    = parse_nums(input$win_b_sidetask),
      loss_b   = parse_nums(input$loss_b_sidetask),
      iti      = parse_nums(input$iti_sidetask),
      ttime    = parse_nums(input$ttime_sidetask)
    )
  })
  
  ### computation_results_sidetask ----
  computation_results_sidetask <- eventReactive(input$recalc_sidetask, {
    
    params_sidetask <- params_sidetask()
    
    max_parameters <- lapply(params_sidetask, length) %>% unlist() %>% max()
    
    data.frame(baseline = rep(params_sidetask$baseline, length.out = max_parameters),
               checkrt  = rep(params_sidetask$checkrt, length.out = max_parameters),
               p_go_on  = rep(params_sidetask$p_go_on, length.out = max_parameters),
               p_go_off = rep(params_sidetask$p_go_off, length.out = max_parameters),
               win_a    = rep(params_sidetask$win_a, length.out = max_parameters),
               win_b    = rep(params_sidetask$win_b, length.out = max_parameters),
               loss_b   = rep(params_sidetask$loss_b, length.out = max_parameters),
               iti      = rep(params_sidetask$iti, length.out = max_parameters),
               ttime    = rep(params_sidetask$ttime, length.out = max_parameters)
    ) %>%
      mutate(cond = 1:n()) %>%
      mutate(color = viridis::magma(n(), begin = 0.2, end = 0.8)) %>%
      rowwise() %>%
      mutate(
        predictions = list(
          comp_sidetask(RT_A_only = baseline,
                        RT_A_B_check = checkrt,
                        RT_A_B_response = checkrt,
                        ITI = iti, 
                        Win_A = win_a,
                        Win_B = win_b,
                        Loss_B = loss_b,
                        p_go_on = p_go_on,
                        p_go_off = p_go_off,
                        Time = ttime,
                        cr = seq(0, 1, by = 0.01))
        )
      ) %>%
      ungroup() %>%
      unnest(predictions) %>%
      group_by(cond) %>%
      mutate(is_max = ifelse(rel_reward == 1, 1, 0)) %>%
      ungroup()
    
  })
  
  ### comp_plot_sidetask ----
  output$comp_plot_sidetask <- renderPlotly({
    
    computation_results_sidetask <- computation_results_sidetask()
    
    plt <- ggplot(computation_results_sidetask)
    
    if (input$absolute_or_relative_sidetask == "relative"){
      
      plt <- plt +
        geom_line(aes(x = cr, y = rel_reward, group = cond, color = color, 
                      text = paste0(
                        "Check Rate: ", round(cr, 2), "\n",
                        "Reward: ", round(rel_reward, 2)
                      )))+
        geom_point(data = computation_results_sidetask %>% filter(is_max == 1), aes(x = cr, y = rel_reward, group = cond, color = color), size = 3, shape = 23)+
        labs(x = "Check Rate",
             y = "Relative reward")
      
    } else {
      
      plt <- plt +
        geom_line(aes(x = cr, y = final_reward, group = cond, color = color,
                      text = paste0(
                        "Check Rate: ", round(cr, 2), "\n",
                        "Reward: ", round(final_reward, 2)
                      )))+
        geom_point(data = computation_results_sidetask %>% filter(is_max == 1), aes(x = cr, y = final_reward, group = cond, color = color), size = 3, shape = 23)+
        labs(x = "Check Rate",
             y = "Absolute reward")
      
    }
    
    plt <- plt +
      scale_color_identity()+
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))+
      theme_classic()+
      theme(legend.position = "none")
    
    ggplotly(plt, tooltip = "text")
  })
  
  ### simulation_results_sidetask ----
  simulation_results_sidetask <- eventReactive(input$sim_sidetask, {
    
    params_sidetask <- params_sidetask()
    
    max_parameters <- lapply(params_sidetask, length) %>% unlist() %>% max()
    
    data.frame(baseline = rep(params_sidetask$baseline, length.out = max_parameters),
               checkrt  = rep(params_sidetask$checkrt, length.out = max_parameters),
               p_go_on  = rep(params_sidetask$p_go_on, length.out = max_parameters),
               p_go_off = rep(params_sidetask$p_go_off, length.out = max_parameters),
               win_a    = rep(params_sidetask$win_a, length.out = max_parameters),
               win_b    = rep(params_sidetask$win_b, length.out = max_parameters),
               loss_b   = rep(params_sidetask$loss_b, length.out = max_parameters),
               iti      = rep(params_sidetask$iti, length.out = max_parameters),
               ttime    = rep(params_sidetask$ttime, length.out = max_parameters)
    ) %>%
      mutate(cond = 1:n()) %>%
      mutate(color = viridis::magma(n(), begin = 0.2, end = 0.8)) %>%
      rowwise() %>%
      mutate(
        predictions = list(
          sim_sidetask_vec(RT_A_only = baseline,
                           RT_A_B_check = checkrt,
                           RT_A_B_response = checkrt,
                           ITI = iti, 
                           Win_A = win_a,
                           Win_B = win_b,
                           Loss_B = loss_b,
                           p_go_on = p_go_on,
                           p_go_off = p_go_off,
                           Time = ttime,
                           cr = seq(0, 1, by = 0.01),
                           reps = 100)
        )
      ) %>%
      ungroup() %>%
      unnest(predictions) %>%
      group_by(cond) %>%
      mutate(is_max = ifelse(rel_reward == 1, 1, 0)) %>%
      ungroup()
    
  })
  
  
  ### sim_plot_sidetask ----
  output$sim_plot_sidetask <- renderPlotly({
    
    simulation_results_sidetask <- simulation_results_sidetask()
    
    plt <- ggplot(simulation_results_sidetask)
    
    if (input$absolute_or_relative_sidetask == "relative"){
      
      plt <- plt +
        geom_line(aes(x = cr, y = rel_reward, group = cond, color = color, 
                      text = paste0(
                        "Check Rate: ", round(cr, 2), "\n",
                        "Reward: ", round(rel_reward, 2)
                      )))+
        geom_point(data = simulation_results_sidetask %>% filter(is_max == 1), aes(x = cr, y = rel_reward, group = cond, color = color), size = 3, shape = 23)+
        labs(x = "Check Rate",
             y = "Relative reward")
      
    } else {
      
      plt <- plt +
        geom_line(aes(x = cr, y = final_reward, group = cond, color = color,
                      text = paste0(
                        "Check Rate: ", round(cr, 2), "\n",
                        "Reward: ", round(final_reward, 2)
                      )))+
        geom_point(data = simulation_results_sidetask %>% filter(is_max == 1), aes(x = cr, y = final_reward, group = cond, color = color), size = 3, shape = 23)+
        labs(x = "Check Rate",
             y = "Absolute reward")
      
    }
    
    plt <- plt +
      scale_color_identity()+
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))+
      theme_classic()+
      theme(legend.position = "none")
    
    ggplotly(plt, tooltip = "text")
  })
  
  ### cor_plot_sidetask ----
  output$cor_plot_sidetask <- renderPlotly({
    
    s <- simulation_results_sidetask()
    c <- computation_results_sidetask()
    
    s <- s %>%
      select(cond, color, cr, final_reward, rel_reward) %>%
      rename(probabilities = cr,
             sim_abs_reward = final_reward,
             sim_rel_reward = rel_reward) %>%
      mutate(probabilities = round(probabilities, 2))
    
    c <- c %>%
      select(cond, color, cr, final_reward, rel_reward) %>%
      rename(probabilities = cr,
             com_abs_reward = final_reward,
             com_rel_reward = rel_reward) %>%
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
  
  ### cond_table_sidetask ----
  cond_table_sidetask <- reactive({
    
    
    computation_results_sidetask() %>%
      group_by(cond) %>%
      filter(rel_reward == 1) %>%
      ungroup() %>%
      select(-rel_reward, -cond, -is_max, -max_reward) %>%
      relocate(color, cr, final_reward, baseline, checkrt, p_go_on, p_go_off, win_a, win_b, loss_b, iti, ttime) %>%
      mutate(final_reward = round(final_reward, 2),
             cr = round(cr, 2)) %>%
      rename("Win A" = win_a,
             "Win B" = win_b,
             "Loss B" = loss_b,
             "Baseline RT" = baseline,
             "Check RT" = checkrt,
             "P(go on)" = p_go_on,
             "P(go off)" = p_go_off,
             "ITI" = iti,
             "Block duration" = ttime,
             "Optimal Earnings" = final_reward,
             "Optimal Check Rate" = cr) %>%
      datatable(
        class = 'cell-border stripe compact',
        rownames = FALSE,
        options = list(
          ordering = FALSE,
          searching = FALSE, 
          lengthChange = FALSE, 
          dom = 't',           # 't' means ONLY the table (hides search/pagination info)
          scrollY = "200px",   # Fixes height and adds a scroll bar
          scrollX = TRUE,      # Allows horizontal scrolling for small screens
          pageLength = -1,      # Shows all rows since we are using a scroll bar
          columnDefs = list(
            list(
              targets = 0,      # Targets the first column ('color')
              width = '5px',   # Forces it to be narrow
              title = ""        # Removes the header name
            )
          )
        ) 
      ) %>%
      formatStyle("color", target = "cell", backgroundColor = styleValue()) %>%
      formatStyle("color", color = 'transparent')
  })
  
  ### cond_table1_sidetask ----
  output$cond_table1_sidetask <- renderDataTable({
    cond_table_sidetask()
  })
  
  ### cond_table2_sidetask ----
  output$cond_table2_sidetask <- renderDataTable({
    cond_table_sidetask()
  })
  
}



# Run the application
shinyApp(ui = ui, server = server)
