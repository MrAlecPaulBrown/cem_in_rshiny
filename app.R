# install.packages("ggfortify")
# PLEASE INSTALL BELOW PACKAGES IF NOT ALREADY INSTALLED - EXAMPLE OF HOW TO DO SO SHOWN ABOVE

library(shiny)
library(shinydashboard)
library(hesim)
library(data.table)
library(ggplot2)
library(ggfortify)
library(DT)


ui <- dashboardPage(
  dashboardHeader(title = "Cost-Effectiveness Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Information", tabName = "info_tab", icon = icon("info-circle")),
      menuItem("State Probabilities", tabName = "state_tab", icon = icon("chart-area")),
      menuItem("CEA Scatter Plot", tabName = "ceplane_tab", icon = icon("chart-scatter")),
      menuItem("CEA Curve", tabName = "ceac_tab", icon = icon("chart-line")),
      menuItem("ICER", tabName = "icer_tab", icon = icon("calculator")),
      br(),
      actionButton("update", "Update Models", class = "btn-primary", icon = icon("refresh")),
      br(),
      actionButton("update_icer", "Update ICER", class = "btn-warning", icon = icon("refresh")),
      br(),
      numericInput("rr_mean", "Relative Risk Mean:", value = 0.509, min = 0, step = 0.01),
      numericInput("rr_lower", "Relative Risk Lower Bound:", value = 0.365, min = 0, step = 0.01),
      numericInput("rr_upper", "Relative Risk Upper Bound:", value = 0.710, min = 0, step = 0.01),
      numericInput("c_druga", "Cost of Zidovudine (Drug A):", value = 2278, min = 0),
      numericInput("c_drugb", "Cost of Lamivudine (Drug B):", value = 2086.5, min = 0),
      numericInput("u", "Utility:", value = 1, min = 0, step = 0.01),
      numericInput("discount_rate", "Discount Rate (Costs):", min = 0, max = 1, value = 0.06, step = 0.01),
      numericInput("discount_rate_qalys", "Discount Rate (QALYs):", min = 0, max = 1, value = 0.03, step = 0.01),
      numericInput("wtp", "Willingness-to-Pay Threshold:", value = 25000, min = 0, step = 1000),
      selectInput("integrate_method", "Integrate Method:", choices = c("riemann_right", "riemann_left", "trapezoid"), selected = "riemann_right")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .skin-blue .main-header .navbar {
          background-color: #3c8dbc;
        }
        .skin-blue .main-sidebar {
          background-color: #1c1c1c;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
          background-color: #ff851b;
          color: #ffffff;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu a {
          color: #b8c7ce;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
          background-color: #ff851b;
          color: #ffffff;
        }
        .skin-blue .main-header .logo {
          background-color: #3c8dbc;
          color: #ffffff;
        }
        .btn-primary {
          background-color: #1e90ff;
          border-color: #1e90ff;
        }
        .btn-warning {
          background-color: #1e90ff;
          border-color: #1e90ff;
        }
        body {
          background-color: #f7f7f7;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "info_tab",
              h2("Information and User Guide"),
              p("Welcome to the Cost-Effectiveness Analysis App. This application is designed to help you perform a cost-effectiveness analysis (CEA) for different healthcare strategies."),
              h3("Inputs in Sidebar"),
              p("You can adjust various parameters for the analysis directly from the sidebar. These include:"),
              tags$ul(
                tags$li("Relative Risk Mean, Lower Bound, and Upper Bound: These values represent the relative risk and its confidence interval for the strategies being compared."),
                tags$li("Cost of Zidovudine (Drug A) and Cost of Lamivudine (Drug B): Enter the costs associated with these drugs."),
                tags$li("Utility: The utility value for the health states."),
                tags$li("Discount Rate (Costs): The discount rate to be applied to the costs."),
                tags$li("Discount Rate (QALYs): The discount rate to be applied to the QALYs."),
                tags$li("Willingness-to-Pay Threshold: This is the maximum amount willing to be paid for a unit of health benefit (e.g., a QALY)."),
                tags$li("Integrate Method: Select the integration method for calculating costs and QALYs.")
              ),
              p("Once you have adjusted the inputs, click the 'Update Models' button to apply the changes."),
              h3("State Probabilities Tab"),
              p("This tab displays the state probabilities over time for the different strategies. The plot will update based on the inputs provided in the sidebar."),
              h3("CEA Scatter Plot Tab"),
              p("This tab provides a scatter plot of incremental costs versus incremental QALYs for the strategies. You can adjust the willingness-to-pay threshold value to see its effect on the plot."),
              h3("CEA Curve Tab"),
              p("This tab displays the Cost-Effectiveness Acceptability Curve (CEAC) based on the input parameters. The CEAC shows the probability that each strategy is cost-effective for a range of willingness-to-pay thresholds."),
              h3("ICER Tab"),
              p("This tab provides information about the Incremental Cost-Effectiveness Ratio (ICER) and related metrics."),
              tags$ul(
                tags$li("Incremental NMB: This field allows the user to input the Net Monetary Benefit (NMB)."),
                tags$li("Incremental QALYs: Enter the incremental QALYs and its confidence intervals."),
                tags$li("Incremental Costs: Enter the incremental costs and its confidence intervals."),
                tags$li("ICER: The Incremental Cost-Effectiveness Ratio is calculated and displayed."),
                tags$li("Error Message: Displays validation messages to ensure the entered values are consistent.")
              ),
              p("Click the 'Update ICER' button to recalculate the ICER based on the input values.")
      ),
      tabItem(tabName = "state_tab",
              fluidRow(
                box(
                  title = "State Probabilities", width = 12, status = "primary", solidHeader = TRUE,
                  plotOutput("statePlot")
                )
              )
      ),
      tabItem(tabName = "ceplane_tab",
              fluidRow(
                box(
                  title = "CEA Scatter Plot", width = 12, status = "primary", solidHeader = TRUE,
                  plotOutput("ceplanePlot")
                )
              )
      ),
      tabItem(tabName = "ceac_tab",
              fluidRow(
                box(
                  title = "CEA Curve", width = 12, status = "primary", solidHeader = TRUE,
                  plotOutput("ceacPlot")
                )
              )
      ),
      tabItem(tabName = "icer_tab",
              fluidRow(
                box(
                  title = "ICER Table", width = 12, status = "primary", solidHeader = TRUE,
                  numericInput("incremental_nmb", "Incremental NMB:", value = 40524, step = 1),
                  numericInput("inc_qalys", "Incremental QALYs:", value = 0.93, step = 0.01),
                  numericInput("inc_qalys_lower", "Incremental QALYs Lower Confidence Interval:", value = 0.56, step = 0.01),
                  numericInput("inc_qalys_upper", "Incremental QALYs Upper Confidence Interval:", value = 1.21, step = 0.01),
                  numericInput("inc_costs", "Incremental Costs:", value = 5827, step = 1),
                  numericInput("inc_costs_lower", "Incremental Costs Lower Confidence Interval:", value = 3013, step = 1),
                  numericInput("inc_costs_upper", "Incremental Costs Upper Confidence Interval:", value = 9737, step = 1),
                  infoBoxOutput("icer_info"),
                  textOutput("error_message")
                )
              )
      )
    )
  )
)


server <- function(input, output, session) {
  
  observeEvent(input$update, {
    tryCatch({
      strategies <- data.table(strategy_id = 1:2,
                               strategy_name = c("Monotherapy", "Combination therapy"))
      patients <- data.table(patient_id = 1)
      states <- data.table(state_id = 1:3,
                           state_name = c("State A", "State B", "State C"))
      hesim_dat <- hesim_data(strategies = strategies,
                              patients = patients,
                              states = states)
      labs <- get_labels(hesim_dat)
      
      trans_mono <- matrix(c(1251, 350, 116, 17,
                             0, 731, 512, 15,
                             0, 0, 1312, 437,
                             0, 0, 0, 469),
                           ncol = 4, nrow = 4, byrow = TRUE)
      colnames(trans_mono) <- rownames(trans_mono) <- c("A", "B", "C", "D")
      
      lrr_mean <- log(input$rr_mean)
      lrr_lower <- log(input$rr_lower)
      lrr_upper <- log(input$rr_upper)
      
      params <- list(
        alpha_mono = trans_mono,
        lrr_mean = lrr_mean,
        lrr_lower = lrr_lower,
        lrr_upper = lrr_upper,
        c_dmed_mean = c(A = 1701, B = 1774, C = 6948),
        c_cmed_mean = c(A = 1055, B = 1278, C = 2059),
        c_druga = input$c_druga,
        c_drugb = input$c_drugb,
        u = input$u
      )
      
      rng_def <- define_rng({
        lrr_se <- (params$lrr_upper - params$lrr_lower) / (2 * qnorm(0.975))
        list(
          p_mono = dirichlet_rng(params$alpha_mono),
          rr_comb = lognormal_rng(params$lrr_mean, lrr_se),
          c_druga = params$c_druga,
          c_drugb = params$c_drugb,
          c_dmed = gamma_rng(mean = params$c_dmed_mean, sd = params$c_dmed_mean),
          c_cmed = gamma_rng(mean = params$c_cmed_mean, sd = params$c_cmed_mean),
          u = params$u
        )
      }, n = 1000)
      
      input_data <- expand(hesim_dat, by = c("strategies", "patients"))
      
      tparams_def <- define_tparams({
        rr <- ifelse(strategy_name == "Monotherapy" | time > 2, 1, rr_comb)
        list(
          tpmatrix = tpmatrix(
            C, p_mono$A_B * rr, p_mono$A_C * rr, p_mono$A_D * rr,
            0, C, p_mono$B_C * rr, p_mono$B_D * rr,
            0, 0, C, p_mono$C_D * rr,
            0, 0, 0, 1
          ),
          utility = u,
          costs = list(
            drug = ifelse(strategy_name == "Monotherapy" | time > 2,
                          c_druga, c_druga + c_drugb),
            community_medical = c_cmed,
            direct_medical = c_dmed
          )
        )
      }, times = c(2, Inf))
      
      mod_def <- define_model(tparams_def = tparams_def,
                              rng_def = rng_def,
                              params = params)
      
      econmod <- create_CohortDtstm(mod_def, input_data)
      
      econmod$sim_stateprobs(n_cycles = 20)
      stateprobs <- econmod$stateprobs_
      
      output$statePlot <- renderPlot({
        autoplot(stateprobs, labels = labs,
                 ci = TRUE, ci_style = "ribbon")
      })
      
      econmod$sim_qalys(dr = input$discount_rate_qalys, integrate_method = "riemann_right")
      econmod$sim_costs(dr = input$discount_rate, integrate_method = input$integrate_method)
      
      ce_sim <- econmod$summarize()
      wtp <- seq(0, 25000, 500)
      cea_pw_out <- cea_pw(ce_sim, comparator = 1, dr_qalys = input$discount_rate_qalys, dr_costs = input$discount_rate, k = wtp)
      
      output$ceacPlot <- renderPlot({
        plot_ceac(cea_pw_out, labels = labs)
      })
      
      output$ceplanePlot <- renderPlot({
        plot_ceplane(cea_pw_out, k = input$wtp, labels = labs)
      })
      
      icer_table <- format(icer(cea_pw_out))
      
      updateNumericInput(session, "inc_qalys", value = as.numeric(icer_table[1, 2]))
      updateNumericInput(session, "inc_qalys_lower", value = as.numeric(icer_table[1, 3]))
      updateNumericInput(session, "inc_qalys_upper", value = as.numeric(icer_table[1, 4]))
      updateNumericInput(session, "inc_costs", value = as.numeric(icer_table[2, 2]))
      updateNumericInput(session, "inc_costs_lower", value = as.numeric(icer_table[2, 3]))
      updateNumericInput(session, "inc_costs_upper", value = as.numeric(icer_table[2, 4]))
      
      incremental_nmb <- input$wtp * as.numeric(input$inc_qalys) - as.numeric(input$inc_costs)
      updateNumericInput(session, "incremental_nmb", value = round(incremental_nmb))
      
      icer <- as.numeric(input$inc_costs) / as.numeric(input$inc_qalys)
      output$icer_info <- renderInfoBox({
        infoBox(
          "ICER",
          paste0(round(icer)),
          icon = icon("balance-scale"),
          color = "blue"
        )
      })
      
    }, error = function(e) {
      print(e)
    })
  })
  
  observeEvent(input$update_icer, {
    tryCatch({
      if (input$inc_qalys_lower <= input$inc_qalys && input$inc_qalys <= input$inc_qalys_upper) {
        if (input$inc_costs_lower <= input$inc_costs && input$inc_costs <= input$inc_costs_upper) {
          incremental_nmb <- input$wtp * as.numeric(input$inc_qalys) - as.numeric(input$inc_costs)
          updateNumericInput(session, "incremental_nmb", value = round(incremental_nmb))
          
          icer <- as.numeric(input$inc_costs) / as.numeric(input$inc_qalys)
          output$icer_info <- renderInfoBox({
            infoBox(
              "ICER",
              paste0(round(icer)),
              icon = icon("balance-scale"),
              color = "blue"
            )
          })
          output$error_message <- renderText("")
        } else {
          output$error_message <- renderText("Error: It must be that: Incremental Costs Lower Bound <= Incremental Costs <= Incremental Costs Upper Bound.")
        }
      } else {
        output$error_message <- renderText("Error: Incremental QALYs Lower Bound must be <= Incremental QALYs <= Incremental QALYs Upper Bound.")
      }
    }, error = function(e) {
      print(e)
    })
  })
}


# Run the app
shinyApp(ui = ui, server = server)
