library(bslib)
library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(viridis)
library(shinycssloaders) 
library(lmerTest)

source("vectorized_simulation_v2.R")

shinyLink <- function(to, label) {
  actionLink(
    inputId = paste0("link_to_", to), # Create a unique ID
    label = label,
    class = "shiny__link"
  )
}

full_data <- readRDS("empirical_data/full_data.RDS")

# Build plots once at startup from pre-processed data frames ----

# E1 curves
e1_d <- full_data$e1$curves_data
e1_fig_curves_plotly <- (ggplot(e1_d$lines, aes(x = probabilities, y = rel_reward,
                                                group = interaction(ID, group), color = Rate,
                                                text = paste0(
                                                  "ID: ", ID, "\n",
                                                  "RT not checking: ", round(RT_nCC, 2), "\n",
                                                  "RT checking: ", round(RT_CC, 2), "\n",
                                                  "Cost: ", round((RT_CC-RT_nCC)/RT_nCC, 2), "\n",
                                                  "Optimum: ", round(check_at_opt, 2)
                                                ))) +
                           geom_line(alpha = 0.5, lwd = 0.2) +
                           geom_ribbon(data = e1_d$dens_slow, aes(x = x, ymin = ymin, ymax = ymax, fill = Rate), inherit.aes = FALSE) +
                           geom_ribbon(data = e1_d$dens_fast, aes(x = x, ymin = ymin, ymax = ymax, fill = Rate), inherit.aes = FALSE) +
                           geom_point(data = e1_d$agg %>% filter(Rate == 0), aes(x = mean_opt, y = 0.1), inherit.aes = FALSE, pch = 23, fill = "white", size = 2) +
                           geom_point(data = e1_d$agg %>% filter(Rate == 1), aes(x = mean_opt, y = 0.2), inherit.aes = FALSE, pch = 23, fill = "white", size = 2) +
                           scale_color_viridis_d(option = "magma", begin = 0.2, end = 0.8,
                                                 labels = c("0" = "fast", "1" = "slow"), name = "Rate:") +
                           scale_fill_viridis_d(option = "magma", begin = 0.2, end = 0.8,
                                                labels = c("0" = "fast", "1" = "slow"), name = "Rate:") +
                           geom_text(data = NULL, aes(x = 0.8, y = max(e1_d$dens_slow$ymax, na.rm = TRUE) + 0.01), label = "slow", color = "#FE9F6D") +
                           geom_text(data = NULL, aes(x = 0.7, y = max(e1_d$dens_fast$ymin, na.rm = TRUE) - 0.05), label = "fast", color = "#3B0F70") +
                           coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
                           labs(x = "Possible Check Rates", y = "Optimality") +
                           theme_classic() + theme(legend.position = "none")) %>%
  ggplotly(tooltip = "text")

# E1 slopes
e1_fig_slopes_plotly <- (ggplot(full_data$e1$slopes_data,
                                aes(x = check_at_opt, y = CC_pred, group = ID, color = ID,
                                    text = paste0("ID: ", ID, "\n", "slope: ", round(slope, 2)))) +
                           geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
                           geom_line() +
                           scale_color_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
                           coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
                           labs(x = "Model Optimal Rate", y = "Observed Rate") +
                           theme_classic() + theme(legend.position = "none")) %>%
  ggplotly(tooltip = "text")

# E2 curves
e2_d <- full_data$e2$curves_data
e2_fig_curves_plotly <- (ggplot(e2_d$lines, aes(x = probabilities, y = rel_reward,
                                                group = interaction(ID, group), color = Rate,
                                                text = paste0(
                                                  "ID: ", ID, "\n",
                                                  "RT not checking: ", round(RT_nCC, 2), "\n",
                                                  "RT checking: ", round(RT_CC, 2), "\n",
                                                  "Cost: ", round((RT_CC-RT_nCC)/RT_nCC, 2), "\n",
                                                  "Optimum: ", round(check_at_opt, 2)
                                                ))) +
                           geom_line(alpha = 0.5, lwd = 0.2) +
                           geom_ribbon(data = e2_d$dens_slow, aes(x = x, ymin = ymin, ymax = ymax, fill = Rate), inherit.aes = FALSE) +
                           geom_ribbon(data = e2_d$dens_fast, aes(x = x, ymin = ymin, ymax = ymax, fill = Rate), inherit.aes = FALSE) +
                           geom_point(data = e2_d$agg %>% filter(Rate == 0), aes(x = mean_opt, y = 0.1), inherit.aes = FALSE, pch = 23, fill = "white", size = 2) +
                           geom_point(data = e2_d$agg %>% filter(Rate == 1), aes(x = mean_opt, y = 0.2), inherit.aes = FALSE, pch = 23, fill = "white", size = 2) +
                           scale_color_viridis_d(option = "magma", begin = 0.2, end = 0.8,
                                                 labels = c("0" = "fast", "1" = "slow"), name = "Rate:") +
                           scale_fill_viridis_d(option = "magma", begin = 0.2, end = 0.8,
                                                labels = c("0" = "fast", "1" = "slow"), name = "Rate:") +
                           geom_text(data = NULL, aes(x = 0.8, y = max(e2_d$dens_slow$ymax, na.rm = TRUE) + 0.01), label = "slow", color = "#FE9F6D") +
                           geom_text(data = NULL, aes(x = 0.8, y = max(e2_d$dens_fast$ymin, na.rm = TRUE) - 0.05), label = "fast", color = "#3B0F70") +
                           coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
                           labs(x = "Possible Check Rates", y = "Optimality") +
                           theme_classic() + theme(legend.position = "none")) %>%
  ggplotly(tooltip = "text")

# E2 slopes
e2_fig_slopes_plotly <- (ggplot(full_data$e2$slopes_data,
                                aes(x = check_at_opt, y = CC_pred, group = ID, color = ID,
                                    text = paste0("ID: ", ID, "\n", "slope: ", round(slope, 2)))) +
                           geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
                           geom_line() +
                           scale_color_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
                           coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
                           labs(x = "Model Optimal Rate", y = "Observed Rate") +
                           theme_classic() + theme(legend.position = "none")) %>%
  ggplotly(tooltip = "text")

# E3 curves
e3_d <- full_data$e3$curves_data
e3_fig_curves_plotly <- (ggplot(e3_d$lines, aes(x = probabilities, y = rel_reward,
                                                group = interaction(ID, group), color = Rate,
                                                text = paste0(
                                                  "ID: ", ID, "\n",
                                                  "RT not checking: ", round(RT_nCC, 2), "\n",
                                                  "RT checking: ", round(RT_CC, 2), "\n",
                                                  "Cost: ", round((RT_CC-RT_nCC)/RT_nCC, 2), "\n",
                                                  "Optimum: ", round(check_at_opt, 2)
                                                ))) +
                           geom_line(alpha = 0.5, lwd = 0.2) +
                           geom_ribbon(data = e3_d$dens_slow, aes(x = x, ymin = ymin, ymax = ymax, fill = Rate), inherit.aes = FALSE) +
                           geom_ribbon(data = e3_d$dens_fast, aes(x = x, ymin = ymin, ymax = ymax, fill = Rate), inherit.aes = FALSE) +
                           geom_point(data = e3_d$agg %>% filter(Rate == 0), aes(x = mean_opt, y = 0.1), inherit.aes = FALSE, pch = 23, fill = "white", size = 2) +
                           geom_point(data = e3_d$agg %>% filter(Rate == 1), aes(x = mean_opt, y = 0.2), inherit.aes = FALSE, pch = 23, fill = "white", size = 2) +
                           scale_color_viridis_d(option = "magma", begin = 0.2, end = 0.8,
                                                 labels = c("0" = "fast", "1" = "slow"), name = "Rate:") +
                           scale_fill_viridis_d(option = "magma", begin = 0.2, end = 0.8,
                                                labels = c("0" = "fast", "1" = "slow"), name = "Rate:") +
                           geom_text(data = NULL, aes(x = 0.7, y = max(e3_d$dens_slow$ymax, na.rm = TRUE) + 0.01), label = "slow", color = "#FE9F6D") +
                           geom_text(data = NULL, aes(x = 0.6, y = max(e3_d$dens_fast$ymin, na.rm = TRUE) - 0.05), label = "fast", color = "#3B0F70") +
                           coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
                           labs(x = "Possible Check Rates", y = "Optimality") +
                           theme_classic() + theme(legend.position = "none")) %>%
  ggplotly(tooltip = "text")

# E3 slopes
e3_fig_slopes_plotly <- (ggplot(full_data$e3$slopes_data,
                                aes(x = check_at_opt, y = CC_pred, group = ID, color = ID,
                                    text = paste0("ID: ", ID, "\n", "slope: ", round(slope, 2)))) +
                           geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
                           geom_line() +
                           scale_color_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
                           coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
                           labs(x = "Model Optimal Rate", y = "Observed Rate") +
                           theme_classic() + theme(legend.position = "none")) %>%
  ggplotly(tooltip = "text")



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
  
  title = tags$div(
    style = "display: flex; flex-direction: column; justify-content: center; padding-top: 5px;",
    tags$span("Attentional Decision Hub", style = "line-height: 1.2;"),
    tags$span(
      "companion app for Graetz, Froeber, & Mayr (in prep)", 
      style = "font-size: 0.65rem; font-style: italic; font-weight: normal; opacity: 0.8; line-height: 1;"
    )
  ),
  
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
         
      /* Target inputs ONLY when Dark Mode is active */
      [data-bs-theme='dark'] input, 
      [data-bs-theme='dark'] textarea, 
      [data-bs-theme='dark'] .form-select {
        background-color: #000000 !important;  /* Keep background white */
        color: #ffffff !important;             /* Force text black */
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
  nav_panel(title = "Welcome",
            value = "welcome",
            icon = icon("house"),
            
            div(
              # 1. Add 'd-flex' to enable flexible positioning
              class = "alert alert-info d-flex", 
              style = "margin-top: 20px; border-left: 5px solid #2c3e50;",
              
              # 2. Add 'align-self-center' to the icon so it floats in the middle
              icon("circle-info", class = "me-3 align-self-center", style = "font-size: 1.5rem;"),
              
              div(
                h5("What is this about?", class = "alert-heading mb-1"),
                p("This website is a companion app for a journal article in preparation. We are researching how humans make decisions about where they place their attention, dependent on context variables (e. g., how quickly are demands changing and how rewarding are adaptations of behavior to the environment) and intraindividual variables (e. g., speed). This website features a discussion of the general framework and theoretical background here on the Welcome page. It also includes a Computation/Simulation tool for you to test the effect of different variables, and a presentation of our findings.", class = "mb-0 text-muted small")
              ),
              
              # 3. Add 'align-self-start' to the button so it stays at the top
              tags$button(
                type = "button", 
                class = "btn-close ms-auto align-self-start", 
                `data-bs-dismiss` = "alert", 
                `aria-label` = "Close"
              )
            ),
            
            div(markdown("

# Welcome

<br>

### Decision-Making about our Attention
                     
We are interested in how our cognitive system controls where to focus attention. The answer to this question depends on the specific context, but our general approach to this question is through the lens of rational decision-making. 
                     
For this project, **I am highlighting two contexts** in which humans decide on an attentional checking-for-information policy - and our research has found that they consider the costs and benefits of possible strategies to make an attentional decision.  

<br>

")),
            
            div(
              markdown("### Two Contexts of Attentional Decision-Making"),
              p("Choose a context below to explore its theoretical background:"),
              
              navset_underline( 
                
                # Tab 1
                nav_panel(
                  title = "1. Rule Updating",
                  icon = icon("signs-post"),
                  div(class = "bg-body-tertiary p-4 rounded shadow-sm mb-4",
                      markdown("
                      
> EXAMPLE SCENARIO: You're driving a car with the assistance of a GPS. If you use a GPS on a familiar route, it offers little information (~ the environment does not demand frequent updating of information, instead you can naviagte from memory alone). Attending to a GPS will also be rather risky if the encoding of the information takes rather long (~ time cost). This cost may be mitigated if you're moving rather slow and need to make few turns (~ long primary task duration). As another factor, if you're on your way to an important meeting, you might rely on the GPS more (~benefits of correct, costs of incorrect, performance).

<br>

#### Characteristics of the context of interest:

To more generally characterize the context that we present research on: 
1) The primary task must be ambiguous and might benefit at least to some degree from additional information
2) Completing a primary task takes time, and obtaining additional information costs additional time
3) Performing the primary task correctly leads to benefits, performing incorrectly leads to costs
4) Time for completing the task is limited.
        
<br>
        
#### Decision Variables to consider:

* __Payoff for correct responses vs. Losses for incorrect responses:__ High payoffs for correct responses and high losses for incorrect responses will bias optimal behavior to more checking.
* __Uncertainty__: Higher switch rates, or in other words, greater uncertainty about the rule that leads to correct task performance, will bias optimal behavior to more checking.
* __Information retrieval time:__ Lower time costs for obtaining additional information will bias optimal behavior to more checking.
* __Your response speed__: Slower primary task response rates will bias optimal behavior to more checking. This may appear counter-intuitive, but the intuition is that if tasks take longer while the information retrieval time remains constant (lower _relative checking cost_), it is more adaptive to check.
        ")
                  )
                ),
                
                # Tab 2
                nav_panel(
                  title = "2. State Checking",
                  icon = icon("satellite-dish"),
                  div(class = "bg-body-tertiary p-4 rounded shadow-sm mb-4",
                      markdown("
> EXAMPLE SCENARIO: You're sitting in a classroom and your main task is to pay attention to the instructor. However, you know that your classmates are talking about something that is relevant to you. You don't need to check their conversation for extended periods of time, but you want to briefly check what they are talking about.  And, they are not always talking about something relevant, so sometimes you will check and it doesn't come at any benefit, sometimes you check and it's useful information, but most importantly, if you miss something, then this can come at smaller or greater consequences or costs ('you're missing out').
        
<br>
        
#### Characteristics of the context of interest:

1) The primary task is completely independent of the state. 
2) Completing a primary task takes time, and obtaining additional information costs additional time
3) Performing the primary task correctly leads to benefits, and missing out on additional information (i. e., the active state) represents a cost.
4) Time for completing the task is limited.
        
<br>
        
#### Decision Variables to consider:

* __Payoff for correct responses vs. Losses for incorrect responses:__ High payoffs for correct responses and high losses missing an active state will bias optimal behavior to more checking.
* __Information retrieval time:__ Lower time costs for obtaining information about the state will bias optimal behavior to more checking.
* __Your response speed__: Slower primary task response rates will bias optimal behavior to more state checking. This may appear counter-intuitive, but the intuition is that if tasks take longer while the information retrieval time remains constant (lower _relative checking cost_), it is more adaptive to check.
        ")
                  )
                )
              )
            ),
            div(markdown(
              
              "
                     

<br>

#### Computational Model and Simulations

We have developed a __computational model__ and __Monte-Carlo simulation__ that allows us to calculate the payoff for different attentional strategies. From a rational decision-making perspective, humans should select an attentional strategy that aligns with the maxmimum payoff. However, as you can see in the Computation/Simulation Tool, many contexts yield an optimality curve that is fairly broad, i. e., a function where a realtively wide range of checking strategies come relatively close to the optimum. 

<br>

### This website

The goal of this website is to help readers of the paper explore how different settings of the value context change the decision landscape."),
              
              tags$ul(
                tags$li(
                  "Under the ", 
                  shinyLink(to = "comp_sim", label = "Computation/Simulation Lab"), 
                  " section, you can explore how parameters affect behavior."
                ),
                tags$li(
                  "In the ", 
                  shinyLink(to = "empirical", label = "Empirical Findings"), 
                  " tab, you can read about the experiments we conducted."
                ),
                tags$li(
                  "Read more about the team in the ", 
                  shinyLink(to = "people", label = "People"), 
                  " tab."
                )
              ),
              
              markdown("
You can read more about our work on our [lab website](https://blogs.uoregon.edu/cognitivedynamics/home/), and the pre-registrations for [experiment 1](https://osf.io/ayvp9), [experiment 2](https://osf.io/dar78), and [experiment 3](https://osf.io/tvjw7) on OSF. You can find posters that I presented on this work under my [ResearchGate](https://www.researchgate.net/profile/Dominik-Graetz) profile.
"
              )),
            
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
    value = "comp_sim",
    icon = icon("microchip"),
    
    ### Scenario 1 ----
    
    navset_underline(
      nav_panel("Rule Updating",
                icon = icon("signs-post"),
                hr(),
                div(
                  class = "alert alert-light border-0 shadow-sm d-flex align-items-center",
                  style = "margin-bottom: 20px;",
                  icon("circle-info", class = "me-3", style = "font-size: 1.5rem;"),
                  div(
                    h5("Scenario Context", class = "alert-heading mb-1"),
                    p("In this scenario, participants perform a task with an ambiguous task stimulus. The task rule that allows for correct (and rewarded) responses can change from trial to trial with a specified switch probability. To find out the correct task, task rule cues are present on the screen. In the most typical scenario, checking task rules reduce uncertainty abotu the current task, but also come at a time cost. Here, you can insert various parameters to identify the optimal strategy.", class = "mb-0 text-muted small")
                  ),
                  # The Close Button
                  tags$button(
                    type = "button", 
                    class = "btn-close ms-auto", 
                    `data-bs-dismiss` = "alert", 
                    `aria-label` = "Close"
                  )
                ),
                
                conditionalPanel(
                  condition = "input.recalc == 0",
                  div(
                    class = "alert alert-info",
                    style = "margin-top: 20px; border-left: 5px solid #2c3e50;",
                    h4("How to use this tool:"),
                    p("1. Tweak the 'Model Parameters' in the sidebar."),
                    p("2. Click the 'Compute' and/or 'Simulate' button below the parameters."),
                    p("3. The resulting graph will show you the optimal checking rate (the peak of the curve).")
                  )
                ),
                
                # Sidebar with a slider input for number of bins
                
                #### sidebar ----
                
                layout_sidebar(
                  sidebar = sidebar(
                    width = 350,
                    title = "Model Parameters",
                    helpText("Enter comma-separated values for parameter sets, or, for a series of numbers, numbers in the format from:increment:to (e. g., 2:0.2:3 is interpreted as 2, 2.02, 2.04, 2.06, 2.08, 2.1)"),
                    
                    accordion(
                      accordion_panel(
                        title = "Timing & Probabilities",
                        icon = icon("clock"),
                        span(
                          "Baseline RT ",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The expected response time when the participant does not perform a task cue check.")
                        ),
                        textInput("baseline", NULL, value = "1"),
                        
                        span(
                          "Check RT",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The expected response time when the participant does perform a task cue check.")
                        ),
                        textInput("checkrt", NULL, value = "1.5"),
                        
                        span(
                          "P(Switch)",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The trial-by-trial probabilty that the task switches.")
                        ),
                        textInput("switchp", NULL, value = "0.05"),
                        
                        span(
                          "Inter-Trial-Interval",
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
                                  "The win amount per correct trial.")
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
                    
                    #### help ----
                    nav_panel(
                      "Help & Model Info",
                      
                      withMathJax(
                        tags$div(
                          
                          markdown(
                            "This page is designed to compare a Monte-Carlo simulation with our computational model. The key idea of this model model is that the relative payoff for different run lengths of trials (e. g., run length of 1 means checking in every trial or 100 %, 2 would be 50 %, 3 would be 33 % and so forth is compared. The simulation and computational model agree very nicely in their predictions.
                            
                            ### Simulation
                            
                            The simulation that can be plotted here generates task sequences and the simulates an agent performing with a stochastic check rate, and assigns corresponding _RTs_ to simulated trials with and without cue checks, assigns gains and losses for simulated correct and incorrect trials, and then summarizes the iteration in the payoff.
                            
                            
                            ### Computational Model
                            
                            The model calculates the payoff for all attentional strategies such that one can visualize the optimality curve. It takes into account the probability of a task switch, _p_, the gains _g_ per correct trial and losses _l_ for incorrect trials. To compute the relative time cost, individual _RTs_ for trials with and without cue checks are needed. To interpolate over the entire block duration, the duration of the inter-trial-interval and the total block duration are needed."
                          ),
                          
                          # 1. Probability of task staying the same
                          p("First, we calculate the probability that the task remains the same at trial \\(n\\):"),
                          p("$$p_{same}(n) = \\frac{1}{2} [1 + (1 - 2p)^n]$$"),
                          
                          # 2. Payoff on Non-Check (PONC)
                          p("The expected payoff for a single trial \\(n\\) without checking:"),
                          p("$$PONC_n = p_{same}(n) g + (1 - p_{same}(n)) l$$"),
                          p("Where \\(g\\) is gain and \\(l\\) is loss."),
                          br(),
                          
                          # 3. Average Payoff on Non-Check (APONC)
                          p("The average payoff over a run of length \\(r\\) without checking:"),
                          p("$$APONC_r = \\frac{1}{r} \\sum_{n=1}^{r} PONC_n$$"),
                          
                          # 4. Average Payoff on Check (APOC)
                          p("If a check occurs on the last trial (guaranteeing a win), the average payoff is:"),
                          p("$$APOC_r = \\frac{1}{r} (\\sum_{n=1}^{r-1} PONC_n + g)$$"),
                          
                          # 5. Cost Coefficient (CC)
                          p("We calculate a time-cost multiplier based on RT, ITI, and check duration:"),
                          p("$$CC_r = \\frac{RT + ITI + \\frac{CT+D}{r}}{RT + ITI}$$"),
                          
                          # 6. Adjusted Payoff (APOCTA)
                          p("The average payoff of checking is adjusted by this time cost:"),
                          p("$$APOCTA_r = \\frac{APOC_r}{CC_r}$$"),
                          
                          # 7. Net Value (PNCC)
                          p("Finally, we compare the relative payoff of NOT checking vs. checking:"),
                          p("$$PNCC_r = APONC_r - APOCTA_r$$"),
                          
                          # 8. Decision Rule
                          
                          p("Decision Policy:"),
                          p("If \\(PNCC_r < 0\\), the optimal strategy is to check (Cue Fixation).")
                          
                        )
                      )
                    ),
                  )
                )
                
      ),
      
      ### Sidetask ----
      nav_panel(title = "State checking",
                icon = icon("satellite-dish"),
                
                hr(),
                div(
                  class = "alert alert-light border-0 shadow-sm d-flex align-items-center",
                  style = "margin-bottom: 20px;",
                  icon("circle-info", class = "me-3", style = "font-size: 1.5rem;"),
                  div(
                    h5("Scenario Context", class = "alert-heading mb-1"),
                    p("In this scenario, participants perform a simple task. Additionally, a state can be on or off. When an 'on-state' is not checked and missed entirely, this leads to a Loss (as specified in the parameters). You can manipulate the probability that the state turns on and off (increasing both will lead to relatively short states). Because checking on a state comes at a time cost, the payout depends on the frequency of checking. You can explore here the optimality of different startegies.", class = "mb-0 text-muted small")
                  ),
                  # The Close Button
                  tags$button(
                    type = "button", 
                    class = "btn-close ms-auto", 
                    `data-bs-dismiss` = "alert", 
                    `aria-label` = "Close"
                  )
                ),
                div(
                  # Changed to alert-warning for that 'Pilot/Under Development' look
                  class = "alert alert-warning border-0 shadow-sm d-flex align-items-center",
                  style = "margin-bottom: 20px;",
                  
                  # Changed icon to a warning triangle
                  icon("triangle-exclamation", class = "me-3", style = "font-size: 1.5rem;"),
                  
                  div(
                    h5("Pilot Mode: Model Under Development", class = "alert-heading mb-1 fw-bold"),
                    p("Please note that this specific computational model is currently in a pilot phase. While functional, the underlying parameters and simulation logic are still being refined. Use these preliminary results for exploration purposes only. You can check on the newest developments of the more comprehensive models", a("here.", href = "https://github.com/dgraetz/state_checking_attentional_allocation_model", target = "_blank"), 
                      class = "mb-0 small")
                  ),
                  
                  # The Close Button
                  tags$button(
                    type = "button", 
                    class = "btn-close ms-auto align-self-start", 
                    `data-bs-dismiss` = "alert", 
                    `aria-label` = "Close"
                  )
                ),
                
                conditionalPanel(
                  condition = "input.recalc == 0",
                  div(
                    class = "alert alert-info",
                    style = "margin-top: 20px; border-left: 5px solid #2c3e50;",
                    h4("How to use this tool:"),
                    p("1. Tweak the 'Model Parameters' in the sidebar."),
                    p("2. Click the 'Compute' and/or 'Simulate' button below the parameters."),
                    p("3. The resulting graph will show you the optimal checking rate (the peak of the curve).")
                  )
                ),
                
                #### sidebar ----
                
                
                layout_sidebar(
                  sidebar = sidebar(
                    width = 350,
                    title = "Model Parameters",
                    helpText("Enter comma-separated values for parameter sets, or, for a series of numbers, numbers in the format from:increment:to (e. g., 2:0.2:3 is interpreted as 2, 2.02, 2.04, 2.06, 2.08, 2.1)."),
                    
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
                          "Check RT",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The expected response time when the participant does perform a check.")
                        ),
                        textInput("checkrt_sidetask", NULL, value = "1.7"),
                        
                        span(
                          "P(go on)",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The trial-by-trial probabilty that a state turns on.")
                        ),
                        textInput("p_go_on_sidetask", NULL, value = "0.1"),
                        
                        span(
                          "P(go off)",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The trial-by-trial probabilty that a state turns off.")
                        ),
                        textInput("p_go_off_sidetask", NULL, value = "0.1"),
                        
                        span(
                          "ITI",
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
                          "Win for Task B",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The win amount for detecting a state.")
                        ),
                        textInput("win_b_sidetask", NULL, value = "0"),
                        
                        
                        span(
                          "Loss for Task B",
                          tooltip(icon("info-circle", style = "cursor: pointer; font-size: 0.8rem;"), 
                                  "The loss amount for each missed state.")
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
                    
                    #### help ----
                    
                    nav_panel(
                      "Help & Model Info",
                      withMathJax(
                        tags$div(
                          
                          markdown(
                            "This page is designed to compare a Monte-Carlo simulation with our computational model. The key idea of this model model is that the relative payoff for different run lengths of trials (e. g., run length of 1 means checking in every trial or 100 %, 2 would be 50 %, 3 would be 33 % and so forth) is compared. 
                            
                            ### Simulation
                            
                            The simulation that can be generated here plots generates task sequences and then simulates an agent performing with a stochastic check rate, and assigns corresponding _RTs_ to simulated trials with and without cue checks, assigns gains and losses for simulated correct and incorrect trials, and then summarizes the iteration in the payoff.
                            
                            
                            ### Computational Model
                            
                            The model calculates the payoff for all attentional strategies such that one can visualize the optimality curve. It takes into account the probability of a state change, _p_, the gains _g_ per correct trial and losses _l_ for incorrect trials. To compute the relative time cost, individual _RTs_ for trials with and without cue checks are needed. To interpolate over the entire block duration, the duration of the inter-trial-interval and the total block duration are needed. Calculating the probability of a state change is here based on the steady state of a Markov Chain with known probbilities (here, P(Go On) and P (Go Off)). In reality the probability of a given state and accurate reward calculations depend on the frequency of checking and the trial number of a block, but for simplicity, I am here using the steady state. I am currently working on a more accurate model that more accurately calculates reward based on these additional factors."
                          ),
                          
                          # 1. Probability of task staying the same
                          p("First, we calculate the probability that the task remains the same at trial \\(n\\):"),
                          p("$$p_{\\text{steady state - off}} = \\frac{p_{\\text{go off}}}{p_{\\text{go off}} + p_{\\text{go on}}}$$"),
                          p("Therefore, $$p_{\\text{steady state - on}} = 1 - p_{\\text{steady state - off}}$$"),
                          p("Because the states are instantaneous, a Loss will happen with the probability that a state is on and turns off in the next trial:"),
                          p("$$p_{\\text{Loss}} = p_{\\text{steady state - on}} * p_{\\text{go off}$$"),
                          
                          # 2. Payoff on Non-Check (PONC)
                          p("The expected payoff for a single trial \\(n\\) without checking:"),
                          p("$$PONC_n = r * g +  r * p_{\text{Loss}} * l$$"),
                          p("Where \\(g\\) is gain and \\(l\\) is loss."),
                          br(),
                          
                          # 3. Average Payoff on Non-Check (APONC)
                          p("The average payoff over a run of length \\(r\\) without checking:"),
                          p("$$APONC_r = \\frac{1}{r} \\sum_{n=1}^{r} PONC_n$$"),
                          
                          # 4. Average Payoff on Check (APOC)
                          p("If a check occurs on the last trial (guaranteeing a win), the average payoff is:"),
                          p("$$APOC_r = \\frac{1}{r} (\\sum_{n=1}^{r-1} PONC_n + g + (p_{\text{Loss}} * l))$$"),
                          
                          # 5. Cost Coefficient (CC)
                          p("We calculate a time-cost multiplier based on RT, ITI, and check duration:"),
                          p("$$CC_r = \\frac{RT + ITI + \\frac{CT+D}{r}}{RT + ITI}$$"),
                          
                          # 6. Adjusted Payoff (APOCTA)
                          p("The average payoff of checking is adjusted by this time cost:"),
                          p("$$APOCTA_r = \\frac{APOC_r}{CC_r}$$"),
                          
                          # 7. Net Value (PNCC)
                          p("Finally, we compare the relative payoff of NOT checking vs. checking:"),
                          p("$$PNCC_r = APONC_r - APOCTA_r$$"),
                          
                          # 8. Decision Rule
                          
                          p("Decision Policy:"),
                          p("If \\(PNCC_r < 0\\), the optimal strategy is to check (Cue Fixation).")
                          
                        )
                      )
                    )
                  )
                )
                
      )
    )
  ),
  
  ## Empirical Findings ----
  
  nav_panel("Empirical Findings", 
            value = "empirical",
            icon = icon("chart-line"),
            navset_underline(
              
              
              ### E1 ----
              nav_panel("Experiment 1", 
                        layout_columns(
                          col_widths = c(6, 5),
                          card(
                            card_header("Project Overview"),
                            markdown("


In Experiment 1, we manipulated four variables:
* Task completion rate (perceptual difficulty was low vs. high)
  - Rate was manipulated by making the primary task easier or more difficult to encode. The response-relevant information needed to be decoded from a random dot motion task. In the slow condition, dot coherence built up over the course of one second, significantly delaying the availability of taks-relevant information. The normative model predicts higher check rates when task response rates are slow because the absolute time check cost is relatively small when the primary task takes longer to predict.
* Task rule switch probability (10 % vs. 25 %)
* Cue onset delay (task cue shows up instantly or with 1 s delay)
* Placeholders (cues were present on the screen, or absent)
  - We manipulated bototm-up salience of cues, we implemented two conditions that determined whether cues were present on the screen by default, or absent. We expected that participants would check the task cues more frequently when they were present, supported by no or only a small difference in check rates. 

Participants had 150 seconds to complete a block of trials. 

<br>


# Findings

The normative model makes individual predictions for each person in each condition, given individual RT parameters. These predictions are displayed in the upper graph, here colored by the level of the rate manipulation. As can be seen, when RTs are relatively slow, the optima are shifted to the right. The violin plots indicate the distribution of optimal check rates.


The lower figure plots the empirically measured check rates (y axis) against the optimal check rates predicted by the model. Nearly all subjects have positive slopes, though there is quite some variance in slope intercepts. 

## Main Effects

As perdicted, participants checked task cues more frequently when a) the task completion rate was slow, the task switch probability high, task cue delays short and placeholders present on the screen. 

Interestingly when placeholders were present, this led to a small, but significant, relative cost benefit (due to the easier findability of task cues). However, the main effect of this manipulation on observed checking rates was drastically higher than on the optima. 


# Participants

We recruited 46 subjects (43 were included in the analysis) from the human subjects pool at the University of Oregon. Participants were compensated with course credit and paid according to rewards accrued in the study.

                                     
                                     "
                            ),
                          ),
                          
                          # Column 2: The Plots stacked vertically
                          # Wrapping them in a div or a card keeps them together in the second column
                          card(
                            card_header("Visualizations"),
                            withSpinner(plotlyOutput("e1_optimality_curves"), type = 8),
                            withSpinner(plotlyOutput("e1_correlation"), type = 8)
                          )
                        )),
              
              ### E2 ----
              nav_panel("Experiment 2", 
                        layout_columns(
                          col_widths = c(6, 5),
                          card(
                            card_header("Project Overview"),
                            markdown("


In Experiment 2, we manipulated three variables:
* Task completion rate (response difficulty was low vs. high)
  - Rate was manipulated by making the primary task easier or more difficult to respond to. While the critical stimulus display remained the same, the response had to be made by dragging a cursor from the center of the screen to one of four rectangles that corresponded to the choice, but that varied in size and distance. In the difficult (and slow) condition, these 'response squares' were small and relatively far from the center of the screen, whereas in the easy (and fast condition), they were close to the respective option and big in size.
* Task rule switch probability (5 vs. 15 %)
* Placeholders (cues were present on the screen, or absent)
  - We again manipulated bottom-up salience of cues and implemented two conditions that determined whether cues were present on the screen by default, or absent. We expected that participants would check the task cues more frequently when they were present, supported by no or only a small difference in check rates. 

Participants had 120 seconds to complete a block of trials. 

<br>


# Findings

The first figure again shows the prediction for the levels of the response difficulty/rate manipulation.

The lower figure plots the empirically measured check rates (y axis) against the optimal check rates predicted by the model. Nearly all subjects have positive slopes, though there is quite some variance in slope intercepts. 

## Main Effects

Again, as perdicted, participants checked task cues more frequently when a) the task completion rate was slow, the task switch probability high, and placeholders present on the screen. 

Similar to experiment 1, when placeholders were present, this led to a small, but significant, relative cost benefit (due to the easier findability of task cues). However, the main effect of this manipulation on observed checking rates was drastically higher than on the optima. 


# Participants

We recruited 41 subjects from the human subjects pool at the University of Oregon. Participants were compensated with course credit and paid according to rewards accrued in the study.

                                     
                                     "
                            ),
                          ),
                          
                          # Column 2: The Plots stacked vertically
                          # Wrapping them in a div or a card keeps them together in the second column
                          card(
                            card_header("Visualizations"),
                            withSpinner(plotlyOutput("e2_optimality_curves"), type = 8),
                            
                            withSpinner(plotlyOutput("e2_correlation"), type = 8)
                          )
                        )),
              
              ### E3 ----
              nav_panel("Experiment 3", 
                        layout_columns(
                          col_widths = c(6, 5),
                          card(
                            card_header("Project Overview"),
                            markdown("


In Experiment 3, we manipulated two variables:
* Task completion rate (inter-trial-interval was short (0.1 s) vs. long (1.6 s))
  - Rate was manipulated by varying the inter-trial interval duration. Longer ITIs affect lower the costs because per unit of time, fewer trials can be completed, reducing the opportunity cost experienced during cue checks.
* Placeholders (cues were present on the screen, or absent)
  - Given the consisten placeholder salience effect in Experiments 1 and 2 on both, the optimal check rate and the outcome, we decided to manipulate salience on three levels. Here, as a new level we introduced a sudden onset placeholder that flickered at the beginning of the trial. Given the results of prior experiments, we were interested in whether this additional level is associated ieht more checking and a reduced relative cost. Finding increased check rates and lower relative costs could have implications on how salience-related increases of 'capture' are interpreted.

Participants had 120 seconds to complete a block of trials. 

<br>


# Findings

The first figure again shows the prediction for the levels of the response difficulty/rate manipulation.

The lower figure plots the empirically measured check rates (y axis) against the optimal check rates predicted by the model. Nearly all subjects have positive slopes, though there is quite some variance in slope intercepts. 

## Main Effects

Again, as perdicted, participants checked task cues more frequently when a) the task completion rate was slow, and as placeholder salience increased. 

As placeholder salience increaed, we again replicated the effect of salience from absent to present task cues on both, reduced relative costs and increased checking rates. However, while sudden onsets resulted in more checking of task cues, it was not accompanied by a further reduction in relative checking costs. 

# Participants

We recruited 44 subjects (41 subjects were included in the final sample) from the human subjects pool at the University of Oregon. Participants were compensated with course credit and paid according to rewards accrued in the study.

                                     
                                     "
                            ),
                          ),
                          
                          card(
                            card_header("Visualizations"),
                            withSpinner(plotlyOutput("e3_optimality_curves"), type = 8),
                            withSpinner(plotlyOutput("e3_correlation"), type = 8)
                          )
                        )
              )
            )
  ),
  
  ## people page ----
  
  nav_panel("People", 
            value = "people",
            icon = icon("user-group"),
            
            tags$h3("People", 
                    style = "text-transform: uppercase; font-weight: 150; letter-spacing: 2px; margin-bottom: 40px;"),
            
            
            ### Profile Dominik ----
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
                  
                  # ResearchGate Link
                  tags$a(
                    href = "https://github.com/dgraetz", 
                    target = "_blank",
                    icon("github"),
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
            
            hr(),
            
            
            
            ### Profile Ulrich ----
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

server <- function(input, output, session) {
  
  
  ## links ----
  
  observeEvent(input$link_to_welcome, {
    updateNavbarPage(session, "main_nav", selected = "welcome")
  })
  
  observeEvent(input$link_to_people, {
    updateNavbarPage(session, "main_nav", selected = "people")
  })
  
  observeEvent(input$link_to_compsim, {
    updateNavbarPage(session, "main_nav", selected = "compsim")
  })
  
  observeEvent(input$link_to_empirical, {
    updateNavbarPage(session, "main_nav", selected = "empirical")
  })
  
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
    
    df_display <- computation_results() %>%
      group_by(cond) %>%
      filter(earnings == reward_at_opt) %>%
      ungroup() %>%
      select(-reward_at_opt, -rel_reward, -cond, -is_max) %>%
      mutate(earnings = round(earnings, 2),
             probabilities = round(probabilities, 2),
             rel_cost = ((checkrt - baseline)/baseline) %>% round(2)) %>%
      relocate(color, probabilities, earnings, baseline, checkrt, rel_cost, switchp, win, loss, iti, ttime) %>%
      rename("Win" = "win",
             "Loss" = loss,
             "Baseline RT" = baseline,
             "Check RT" = checkrt,
             "Relative Time Cost" = rel_cost,
             "P(Switch)" = switchp,
             "ITI" = iti,
             "Block duration" = ttime,
             "Optimal Earnings" = earnings,
             "Optimal Check Rate" = probabilities) 
    
    
    input_cols <- c("Baseline RT", "Check RT", "Relative Time Cost", 
                    "P(Switch)", "Win", "Loss", "ITI", "Block duration")
    
    #I want to highlight the columns with multiple user inputs 
    
    # Find which of these columns have more than 1 unique value
    idx_mul_val_cols <- lapply(input_cols, function(x){
      df_display[,x] %>% unique %>% nrow()
    }) %>% unlist()
    
    mul_val_col <- input_cols[idx_mul_val_cols > 1]
    
    
    table <- datatable(
      df_display,
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
    
    if (length(mul_val_col) > 0) {
      table <- table %>%
        formatStyle(
          columns = mul_val_col,
          backgroundColor = 'rgba(255, 193, 7, 0.35)'
        )
    }
    
    table
    
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
  
  
  
  
  
  ## Sidetask ----------------------------------------------------------------
  
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
    
    
    df_display <- computation_results_sidetask() %>%
      group_by(cond) %>%
      filter(rel_reward == 1) %>%
      ungroup() %>%
      select(-rel_reward, -cond, -is_max, -max_reward) %>%
      mutate(final_reward = round(final_reward, 2),
             cr = round(cr, 2),
             check_cost = (
               (checkrt - baseline)/baseline) %>% round(2)) %>%
      relocate(color, cr, final_reward, baseline, checkrt, check_cost, p_go_on, p_go_off, win_a, win_b, loss_b, iti, ttime) %>%
      rename("Win A" = win_a,
             "Win B" = win_b,
             "Loss B" = loss_b,
             "Baseline RT" = baseline,
             "Check RT" = checkrt,
             "Relative Time Cost" = check_cost,
             "P(go on)" = p_go_on,
             "P(go off)" = p_go_off,
             "ITI" = iti,
             "Block duration" = ttime,
             "Optimal Earnings" = final_reward,
             "Optimal Check Rate" = cr) 
    
    input_cols <- c("Baseline RT", "Check RT", "Relative Time Cost", 
                    "P(go on)", "P(go off)", "Win A", "Win B", "Loss B", "ITI", "Block duration")
    
    #I want to highlight the columns with multiple user inputs 
    
    # Find which of these columns have more than 1 unique value
    idx_mul_val_cols <- lapply(input_cols, function(x){
      df_display[,x] %>% unique %>% nrow()
    }) %>% unlist()
    
    mul_val_col <- input_cols[idx_mul_val_cols > 1]
    
    table <- df_display %>%
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
    
    if (length(mul_val_col) > 0) {
      table <- table %>%
        formatStyle(
          columns = mul_val_col,
          backgroundColor = 'rgba(255, 193, 7, 0.35)'
        )
    }
    
    table
    
    
  })
  
  ### cond_table1_sidetask ----
  output$cond_table1_sidetask <- renderDataTable({
    cond_table_sidetask()
  })
  
  ### cond_table2_sidetask ----
  output$cond_table2_sidetask <- renderDataTable({
    cond_table_sidetask()
  })
  
  
  
  
  ### e1_optimality_curves ----
  output$e1_optimality_curves <- renderPlotly({
    
    e1_fig_curves_plotly %>% toWebGL()
  })
  
  ### e1_correlation ----
  output$e1_correlation <- renderPlotly({
    
    e1_fig_slopes_plotly %>% toWebGL()
  })
  
  ### e2_optimality_curves ----
  output$e2_optimality_curves <- renderPlotly({
    
    e2_fig_curves_plotly %>% toWebGL()
  })
  
  ### e2_correlation ----
  output$e2_correlation <- renderPlotly({
    
    e2_fig_slopes_plotly %>% toWebGL()
    
  })
  
  
  ### e3_optimality_curves ----
  output$e3_optimality_curves <- renderPlotly({
    
    e3_fig_curves_plotly %>% toWebGL()
  })
  
  
  
  ### e3_correlation ----
  output$e3_correlation <- renderPlotly({
    
    e3_fig_slopes_plotly %>% toWebGL()
  })
  
  
  
  
  
}



# Run the application
shinyApp(ui = ui, server = server)
