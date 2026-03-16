# ============================================================
# M&A Valuation: Advanced Monte Carlo DCF Simulation
# Project Vitality - PulsePoint AI
# Acquirer: Global Health Systems (GHS)
# ============================================================
# install.packages(c("shiny", "triangle", "ggplot2", "dplyr",
#                    "scales", "DT", "plotly"))

library(shiny)
library(triangle)
library(ggplot2)
library(dplyr)
library(scales)
library(DT)
library(plotly)

# ==========================================
# HELPER: Base-case DCF for cash flow table
# ==========================================
run_base_case <- function(rev_growth, ebit_margin, beta,
                          pgr, rev_0 = 50, tax_rate = 0.21,
                          da_pct = 0.04, capex_pct = 0.08,
                          nwc_pct = 0.03, rf = 0.042, mrp = 0.055,
                          rd = 0.07, w_d = 0.15, w_e = 0.85,
                          net_debt = 15, shares_out = 5) {
  ke   <- rf + beta * mrp
  wacc <- w_e * ke + w_d * rd * (1 - tax_rate)

  revs  <- rev_0 * (1 + rev_growth)^(1:5)
  ebit  <- revs * ebit_margin
  nopat <- ebit * (1 - tax_rate)
  da    <- revs * da_pct
  capex <- revs * capex_pct
  nwc   <- revs * nwc_pct
  fcff  <- nopat + da - capex - nwc
  df    <- 1 / (1 + wacc)^(1:5)
  pv    <- fcff * df

  fcff_y5 <- revs[5] * ebit_margin * (1 - tax_rate) + revs[5] * da_pct -
             revs[5] * capex_pct - revs[5] * nwc_pct
  tv     <- fcff_y5 * (1 + pgr) / (wacc - pgr)
  pv_tv  <- tv * df[5]
  ev     <- sum(pv) + pv_tv
  eq_val <- ev - net_debt
  sp     <- eq_val / shares_out

  list(
    cf_table = data.frame(
      Year      = paste0("Year ", 1:5),
      Revenue   = round(revs, 2),
      EBIT      = round(ebit, 2),
      NOPAT     = round(nopat, 2),
      DA        = round(da, 2),
      CapEx     = round(capex, 2),
      Delta_NWC = round(nwc, 2),
      FCFF      = round(fcff, 2),
      PV_Factor = round(df, 4),
      PV_FCFF   = round(pv, 2)
    ),
    wacc    = wacc,
    ke      = ke,
    pv_fcff = sum(pv),
    tv      = tv,
    pv_tv   = pv_tv,
    ev      = ev,
    eq_val  = eq_val,
    sp      = sp
  )
}

# ==========================================
# UI
# ==========================================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;600&family=Lora:wght@400;600;700&family=IBM+Plex+Sans:wght@300;400;600;700&display=swap');

      /* ── Rose & Cream Palette ──
         bg:       #fdf6f0  (warm cream)
         surface:  #fff9f6  (soft white)
         border:   #f0d9d0  (blush border)
         primary:  #c0566b  (rose)
         accent:   #e8956d  (peach)
         text:     #3d2b2b  (dark rosewood)
         muted:    #9e7272  (muted mauve)
         success:  #6b9e7a  (sage green)
         warn:     #c07a3a  (warm amber)
      ── */

      body {
        background: #fdf6f0;
        color: #3d2b2b;
        font-family: 'IBM Plex Sans', sans-serif;
        margin: 0; padding: 0;
      }
      .navbar-default { display: none; }

      /* ── Header ── */
      .app-header {
        background: linear-gradient(135deg, #fceae4 0%, #fdf6f0 60%, #fce8f0 100%);
        border-bottom: 1px solid #f0d9d0;
        padding: 26px 40px 18px;
      }
      .app-header h1 {
        font-family: 'Lora', serif;
        font-size: 22px; font-weight: 700;
        color: #c0566b; margin: 0 0 4px;
        letter-spacing: -0.3px;
      }
      .app-header p { font-size: 12px; color: #9e7272; margin: 0; font-weight: 300; }
      .tag-badge {
        display: inline-block;
        background: rgba(192,86,107,0.10);
        border: 1px solid rgba(192,86,107,0.30);
        color: #c0566b; font-size: 10px;
        padding: 2px 8px; border-radius: 20px;
        margin-left: 10px; vertical-align: middle;
        font-family: 'IBM Plex Mono', monospace;
      }
      .deal-badge {
        display: inline-block;
        background: rgba(107,158,122,0.12);
        border: 1px solid rgba(107,158,122,0.30);
        color: #4e8c62; font-size: 10px;
        padding: 2px 8px; border-radius: 20px;
        margin-left: 6px; vertical-align: middle;
        font-family: 'IBM Plex Mono', monospace;
      }

      /* ── Sidebar ── */
      .sidebar-panel-custom {
        background: #fff9f6;
        border-right: 1px solid #f0d9d0;
        padding: 22px 18px;
        min-height: calc(100vh - 85px);
      }
      .sidebar-panel-custom h4 {
        font-size: 10px; text-transform: uppercase;
        letter-spacing: 1.6px; color: #c0566b;
        margin: 20px 0 10px; font-weight: 700;
        font-family: 'IBM Plex Mono', monospace;
      }
      .sidebar-panel-custom h4:first-child { margin-top: 0; }

      /* ── Sliders ── */
      .irs--shiny .irs-bar { background: #c0566b; border-top-color: #c0566b; border-bottom-color: #c0566b; }
      .irs--shiny .irs-handle { background: #c0566b; border-color: #99304a; }
      .irs--shiny .irs-single { background: #99304a; font-family: 'IBM Plex Mono', monospace; font-size: 10px; color: #fff; }
      .irs--shiny .irs-line { background: #f0d9d0; }
      .irs--shiny .irs-grid-text { color: #9e7272; }

      /* ── Main content ── */
      .main-content { padding: 0 22px 40px; background: #fdf6f0; }

      /* ── KPI cards ── */
      .kpi-row { display: flex; gap: 10px; padding: 18px 0 14px; flex-wrap: wrap; }
      .kpi-card {
        background: #fff9f6;
        border: 1px solid #f0d9d0;
        border-radius: 10px; padding: 13px 16px;
        flex: 1; min-width: 125px;
        box-shadow: 0 1px 4px rgba(192,86,107,0.06);
        transition: box-shadow 0.2s, border-color 0.2s;
      }
      .kpi-card:hover { border-color: #d9a0aa; box-shadow: 0 3px 10px rgba(192,86,107,0.10); }
      .kpi-card .kpi-label {
        font-size: 10px; color: #9e7272;
        text-transform: uppercase; letter-spacing: 1px;
        font-family: 'IBM Plex Mono', monospace;
      }
      .kpi-card .kpi-value {
        font-size: 21px; font-weight: 700;
        color: #3d2b2b; margin-top: 4px;
        font-family: 'IBM Plex Mono', monospace;
      }
      .kpi-card .kpi-sub { font-size: 10px; color: #4e8c62; margin-top: 2px; }
      .kpi-card.warn  .kpi-value { color: #c07a3a; }
      .kpi-card.info  .kpi-value { color: #c0566b; }
      .kpi-card.green .kpi-value { color: #4e8c62; }

      /* ── Tabs ── */
      .nav-tabs { border-bottom: 1px solid #f0d9d0; margin-bottom: 0; }
      .nav-tabs > li > a {
        color: #9e7272; background: transparent;
        border: none; border-bottom: 2px solid transparent;
        font-size: 13px; font-weight: 500; padding: 9px 15px;
        font-family: 'IBM Plex Sans', sans-serif; transition: color 0.15s;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li > a:hover {
        color: #3d2b2b; background: transparent;
        border-color: transparent transparent #c0566b;
      }
      .tab-content { padding-top: 18px; }

      /* ── Section headings ── */
      .section-title {
        font-family: 'IBM Plex Mono', monospace;
        font-size: 11px; font-weight: 600;
        color: #9e7272; text-transform: uppercase;
        letter-spacing: 1.2px; margin: 0 0 10px;
        padding-bottom: 7px; border-bottom: 1px solid #f0d9d0;
      }

      /* ── Plot panels ── */
      .plot-panel {
        background: #fff9f6; border: 1px solid #f0d9d0;
        border-radius: 10px; padding: 14px; margin-bottom: 14px;
        box-shadow: 0 1px 4px rgba(192,86,107,0.05);
      }

      /* ── DataTable ── */
      .dataTables_wrapper { color: #3d2b2b; font-size: 12px; }
      table.dataTable thead th {
        background: #fceae4; color: #c0566b;
        border-bottom: 1px solid #f0d9d0;
        font-family: 'IBM Plex Mono', monospace;
        font-size: 10px; text-transform: uppercase; letter-spacing: 0.8px;
      }
      table.dataTable tbody tr { background: #fff9f6; }
      table.dataTable tbody tr:hover { background: #fceae4 !important; }
      table.dataTable tbody td {
        border-top: 1px solid #f0d9d0;
        font-family: 'IBM Plex Mono', monospace; font-size: 11px;
        color: #3d2b2b;
      }
      .dataTables_info, .dataTables_length, .dataTables_filter { color: #9e7272; font-size: 11px; }
      .dataTables_paginate .paginate_button { color: #9e7272 !important; }
      .dataTables_paginate .paginate_button.current {
        background: #fceae4 !important; color: #c0566b !important; border-radius: 4px;
      }

      /* ── Equity bridge ── */
      .bridge-row {
        display: flex; align-items: center;
        padding: 9px 14px; border-radius: 8px;
        margin-bottom: 5px; font-family: 'IBM Plex Mono', monospace; font-size: 12px;
      }
      .bridge-row .label { flex: 1; color: #9e7272; }
      .bridge-row .amount { font-weight: 600; }
      .bridge-add   { background: rgba(107,158,122,0.09); }
      .bridge-sub   { background: rgba(192,86,107,0.08); }
      .bridge-total { background: rgba(232,149,109,0.12); border: 1px solid rgba(232,149,109,0.30); }
      .bridge-add   .amount { color: #4e8c62; }
      .bridge-sub   .amount { color: #c0566b; }
      .bridge-total .amount { color: #c07a3a; font-size: 14px; }

      /* ── Probability table ── */
      .prob-table { width: 100%; border-collapse: collapse; font-size: 12px; }
      .prob-table th {
        background: #fceae4; color: #c0566b;
        padding: 9px 13px; text-align: left;
        font-family: 'IBM Plex Mono', monospace; font-size: 10px;
        text-transform: uppercase; border-bottom: 1px solid #f0d9d0;
      }
      .prob-table td {
        padding: 9px 13px; border-bottom: 1px solid #f0d9d0;
        font-family: 'IBM Plex Mono', monospace; color: #3d2b2b;
      }
      .prob-table tr:last-child td { border-bottom: none; }

      /* ── Run button ── */
      #runBtn {
        background: #c0566b; color: #fff;
        border: 1px solid #99304a; border-radius: 8px;
        font-size: 13px; font-weight: 600;
        padding: 9px 20px; width: 100%; margin-top: 10px;
        cursor: pointer; font-family: 'IBM Plex Sans', sans-serif;
        transition: background 0.15s;
        box-shadow: 0 2px 6px rgba(192,86,107,0.20);
      }
      #runBtn:hover { background: #99304a; }

      hr { border-color: #f0d9d0; margin: 14px 0; }

      /* ── Assumption chips ── */
      .chip {
        display: inline-block; background: #fceae4;
        color: #9e7272; font-size: 10px;
        padding: 3px 8px; border-radius: 20px; margin: 2px;
        font-family: 'IBM Plex Mono', monospace;
        border: 1px solid #f0d9d0;
      }
      .chip span { color: #c0566b; font-weight: 600; }

      pre {
        background: #fff9f6; color: #3d2b2b;
        border: 1px solid #f0d9d0; border-radius: 8px;
        font-family: 'IBM Plex Mono', monospace; font-size: 11px; padding: 12px;
      }

      /* ── Insight box ── */
      .insight-box {
        background: rgba(232,149,109,0.07);
        border: 1px solid rgba(232,149,109,0.25);
        border-left: 3px solid #e8956d;
        border-radius: 8px; padding: 12px 16px;
        font-size: 12px; color: #7a5050;
        font-family: 'IBM Plex Sans', sans-serif;
        margin-bottom: 14px; line-height: 1.6;
      }
      .insight-box strong { color: #c0566b; }

      /* ── Scenario table ── */
      .scenario-table { width: 100%; border-collapse: collapse; font-size: 12px; }
      .scenario-table th {
        font-family: 'IBM Plex Mono', monospace; font-size: 10px;
        text-transform: uppercase; letter-spacing: 0.8px;
        color: #c0566b; padding: 8px 12px; text-align: right;
        border-bottom: 1px solid #f0d9d0; background: #fceae4;
      }
      .scenario-table th:first-child { text-align: left; }
      .scenario-table td {
        padding: 8px 12px; text-align: right;
        border-bottom: 1px solid #f0d9d0;
        font-family: 'IBM Plex Mono', monospace; color: #3d2b2b;
      }
      .scenario-table td:first-child { text-align: left; color: #9e7272; }

      /* ── Label text overrides ── */
      label { color: #3d2b2b !important; font-size: 12px; }
      .irs--shiny .irs-min, .irs--shiny .irs-max { color: #9e7272; }
      select, input { color: #3d2b2b !important; background: #fff9f6 !important; border-color: #f0d9d0 !important; }
    "))
  ),

  # ── Header ──
  div(class = "app-header",
      tags$h1(
        "PROJECT VITALITY",
        tags$span(class = "tag-badge", "Monte Carlo DCF"),
        tags$span(class = "deal-badge", "GHS ⟶ PulsePoint AI")
      ),
      tags$p("HealthTech Acquisition Standalone Valuation  |  GHS Corporate Development Team")
  ),

  # ── Layout ──
  fluidRow(
    # Sidebar
    column(3,
      div(class = "sidebar-panel-custom",

        h4("Simulation"),
        sliderInput("n_iter", "Iterations:", min = 1000, max = 50000, value = 10000, step = 1000),
        actionButton("runBtn", ">  Run Simulation"),
        hr(),

        h4("Revenue Drivers"),
        sliderInput("rev_growth_mean", "Mean Revenue Growth:", min = 0.05, max = 0.40, value = 0.20, step = 0.01),
        sliderInput("rev_growth_sd",   "Std Dev (Growth):",    min = 0.01, max = 0.10, value = 0.05, step = 0.005),
        hr(),

        h4("Margin Drivers"),
        sliderInput("ebit_mode",       "EBIT Margin (Mode):",  min = 0.08, max = 0.35, value = 0.15, step = 0.01),
        sliderInput("ebit_min_offset", "Min Offset below Mode:", min = 0.02, max = 0.10, value = 0.05, step = 0.005),
        sliderInput("ebit_max_offset", "Max Offset above Mode:", min = 0.01, max = 0.10, value = 0.07, step = 0.005),
        hr(),

        h4("Risk & Terminal Value"),
        sliderInput("beta_min", "Min Beta:", min = 0.8,  max = 1.5, value = 1.20, step = 0.05),
        sliderInput("beta_max", "Max Beta:", min = 1.3,  max = 2.5, value = 1.70, step = 0.05),
        sliderInput("pgr_mean", "Mean PGR:", min = 0.01, max = 0.05, value = 0.030, step = 0.005),
        sliderInput("pgr_sd",   "Std Dev (PGR):", min = 0.001, max = 0.015, value = 0.006, step = 0.001),
        hr(),

        h4("Hardcoded Constants"),
        div(class = "chip", "Year 0 Rev ",  tags$span("$50M")),
        div(class = "chip", "Tax ",         tags$span("21%")),
        div(class = "chip", "D&A ",         tags$span("4%")),
        div(class = "chip", "CapEx ",       tags$span("8%")),
        div(class = "chip", "DeltaNWC ",    tags$span("3%")),
        div(class = "chip", "Rf ",          tags$span("4.2%")),
        div(class = "chip", "MRP ",         tags$span("5.5%")),
        div(class = "chip", "Rd ",          tags$span("7.0%")),
        div(class = "chip", "D/V ",         tags$span("15%")),
        div(class = "chip", "Net Debt ",    tags$span("$15M")),
        div(class = "chip", "Shares ",      tags$span("5M"))
      )
    ),

    # Main panel
    column(9,
      div(class = "main-content",

        uiOutput("kpiRow"),

        tabsetPanel(id = "mainTabs",

          # ── Tab 1: Distribution ──
          tabPanel("Distribution",
            br(),
            div(class = "insight-box",
              tags$strong("Analyst Note: "),
              "The histogram below shows the simulated distribution of PulsePoint AI's intrinsic
               share price across all Monte Carlo iterations. The base-case answer key estimate is
               ~$9.62/share. A bid above $12/share would require synergy justification per caselet notes."
            ),
            div(class = "plot-panel", plotlyOutput("distPlot", height = "360px")),
            div(class = "plot-panel", plotlyOutput("cdfPlot",  height = "250px"))
          ),

          # ── Tab 2: Cash Flows ──
          tabPanel("Cash Flow Model",
            br(),
            fluidRow(
              column(12,
                p(class = "section-title", "Base-Case 5-Year FCFF Projections  (FCFF = NOPAT + D&A − CapEx − ΔNWC)"),
                DTOutput("cfTable")
              )
            ),
            br(),
            fluidRow(
              column(7,
                p(class = "section-title", "Annual Cash Flow Breakdown"),
                div(class = "plot-panel", plotlyOutput("cfBarPlot", height = "300px"))
              ),
              column(5,
                p(class = "section-title", "Equity Bridge (Base Case)"),
                uiOutput("equityBridge")
              )
            )
          ),

          # ── Tab 3: Sensitivity ──
          tabPanel("Sensitivity",
            br(),
            p(class = "section-title", "Share Price vs WACC × Perpetuity Growth Rate"),
            div(class = "plot-panel", plotlyOutput("sensPlot",  height = "370px")),
            br(),
            p(class = "section-title", "Share Price vs Revenue Growth × EBIT Margin"),
            div(class = "plot-panel", plotlyOutput("sensPlot2", height = "370px"))
          ),

          # ── Tab 4: Scenarios ──
          tabPanel("Scenarios",
            br(),
            div(class = "insight-box",
              tags$strong("Scenario Logic: "),
              "Bear case assumes slower wearable adoption and tighter margins. Bull case reflects
               successful global expansion post-acquisition. Base case follows caselet parameters exactly."
            ),
            p(class = "section-title", "Bull / Base / Bear Scenario Comparison"),
            tableOutput("scenarioTable"),
            br(),
            p(class = "section-title", "Simulated Percentile Bands"),
            div(class = "plot-panel", plotlyOutput("fanPlot", height = "340px"))
          ),

          # ── Tab 5: Statistics ──
          tabPanel("Statistics",
            br(),
            fluidRow(
              column(6,
                p(class = "section-title", "Share Price Distribution"),
                verbatimTextOutput("summaryStats"),
                br(),
                p(class = "section-title", "Probability Table"),
                tableOutput("probTable")
              ),
              column(6,
                p(class = "section-title", "WACC Distribution"),
                div(class = "plot-panel", plotlyOutput("waccDist",  height = "210px")),
                br(),
                p(class = "section-title", "FCFF Year 5 Distribution"),
                div(class = "plot-panel", plotlyOutput("fcffDist",  height = "210px")),
                br(),
                p(class = "section-title", "EV Composition (Base Case)"),
                div(class = "plot-panel", plotlyOutput("evPie",     height = "210px"))
              )
            )
          )
        )
      )
    )
  )
)

# ==========================================
# SERVER
# ==========================================
server <- function(input, output, session) {

  # ── Hardcoded constants ──
  REV_0     <- 50;  TAX <- 0.21; DA_PCT    <- 0.04
  CAPEX_PCT <- 0.08; NWC_PCT <- 0.03; RF  <- 0.042
  MRP <- 0.055; RD  <- 0.07; W_D <- 0.15; W_E <- 0.85
  NET_DEBT  <- 15;  SHARES <- 5

  # ── MONTE CARLO ──
  sim_data <- eventReactive(input$runBtn, ignoreNULL = FALSE, {
    n <- input$n_iter

    rev_growth  <- rnorm(n, input$rev_growth_mean, input$rev_growth_sd)

    ebit_a      <- input$ebit_mode - input$ebit_min_offset
    ebit_b      <- input$ebit_mode + input$ebit_max_offset
    ebit_margin <- rtriangle(n, a = ebit_a, b = ebit_b, c = input$ebit_mode)

    beta <- runif(n, input$beta_min, input$beta_max)
    pgr  <- rnorm(n, input$pgr_mean, input$pgr_sd)

    ke   <- RF + beta * MRP
    wacc <- W_E * ke + W_D * RD * (1 - TAX)

    fcff_margin <- ebit_margin * (1 - TAX) + DA_PCT - CAPEX_PCT - NWC_PCT

    pv_sum  <- rep(0, n)
    rev_cur <- REV_0
    for (yr in 1:5) {
      rev_cur <- rev_cur * (1 + rev_growth)
      fcff    <- rev_cur * fcff_margin
      pv_sum  <- pv_sum + fcff / (1 + wacc)^yr
    }

    rev_y5  <- REV_0 * (1 + rev_growth)^5
    fcff_y5 <- rev_y5 * fcff_margin
    tv      <- (fcff_y5 * (1 + pgr)) / (wacc - pgr)
    pv_tv   <- tv / (1 + wacc)^5

    ev <- pv_sum + pv_tv
    eq <- ev - NET_DEBT
    sp <- eq / SHARES

    data.frame(SharePrice = sp, WACC = wacc, FCFF_Y5 = fcff_y5,
               PV_FCFF = pv_sum, PV_TV = pv_tv, EV = ev)
  })

  # ── BASE CASE ──
  base_case <- reactive({
    run_base_case(
      rev_growth  = input$rev_growth_mean,
      ebit_margin = input$ebit_mode,
      beta        = (input$beta_min + input$beta_max) / 2,
      pgr         = input$pgr_mean
    )
  })

  # ── KPI ROW ──
  output$kpiRow <- renderUI({
    df  <- sim_data()
    bc  <- base_case()
    sp  <- df$SharePrice
    tags$div(class = "kpi-row",
      tags$div(class = "kpi-card info",
        tags$div(class = "kpi-label", "Mean Share Price"),
        tags$div(class = "kpi-value", dollar(mean(sp), 0.01)),
        tags$div(class = "kpi-sub",   paste0("Median: ", dollar(median(sp), 0.01)))
      ),
      tags$div(class = "kpi-card green",
        tags$div(class = "kpi-label", "Base Case Price"),
        tags$div(class = "kpi-value", dollar(bc$sp, 0.01)),
        tags$div(class = "kpi-sub",   paste0("WACC: ", percent(bc$wacc, 0.01)))
      ),
      tags$div(class = "kpi-card warn",
        tags$div(class = "kpi-label", "P10 / P90 Band"),
        tags$div(class = "kpi-value",
          paste0(dollar(quantile(sp, 0.1), 0.01), " – ", dollar(quantile(sp, 0.9), 0.01))
        ),
        tags$div(class = "kpi-sub", "80% confidence interval")
      ),
      tags$div(class = "kpi-card",
        tags$div(class = "kpi-label", "Base Case EV"),
        tags$div(class = "kpi-value", paste0("$", round(bc$ev, 1), "M")),
        tags$div(class = "kpi-sub",   paste0("TV/EV: ", percent(bc$pv_tv / bc$ev, 0.1)))
      ),
      tags$div(class = "kpi-card",
        tags$div(class = "kpi-label", "Sim Iterations"),
        tags$div(class = "kpi-value", format(input$n_iter, big.mark = ",")),
        tags$div(class = "kpi-sub",   paste0("SD: ", dollar(sd(sp), 0.01)))
      ),
      tags$div(class = "kpi-card info",
        tags$div(class = "kpi-label", "Bid Premium Trigger"),
        tags$div(class = "kpi-value", "$12.00"),
        tags$div(class = "kpi-sub", paste0(
          round(mean(sp > 12) * 100, 1), "% sims exceed"
        ))
      )
    )
  })

  # ── TAB 1: Distribution ──
  output$distPlot <- renderPlotly({
    sp       <- sim_data()$SharePrice
    mean_val <- mean(sp)
    p5       <- quantile(sp, 0.05)
    p95      <- quantile(sp, 0.95)
    ans_key  <- 9.62   # caselet answer key

    plot_ly(x = sp, type = "histogram", nbinsx = 80,
      marker = list(color = "rgba(192,86,107,0.35)",
                    line  = list(color = "rgba(192,86,107,0.12)", width = 0.5))) %>%
      add_segments(x = mean_val, xend = mean_val, y = 0, yend = input$n_iter * 0.12,
        line = list(color = "#c07a3a", dash = "dash", width = 2),
        name = paste0("Mean: ", dollar(mean_val, 0.01))) %>%
      add_segments(x = ans_key, xend = ans_key, y = 0, yend = input$n_iter * 0.10,
        line = list(color = "#9b6bb5", dash = "longdash", width = 2),
        name = "Answer Key: $9.62") %>%
      add_segments(x = p5, xend = p5, y = 0, yend = input$n_iter * 0.08,
        line = list(color = "#c0566b", dash = "dot", width = 1.5),
        name = paste0("P5: ", dollar(p5, 0.01))) %>%
      add_segments(x = p95, xend = p95, y = 0, yend = input$n_iter * 0.08,
        line = list(color = "#4e8c62", dash = "dot", width = 1.5),
        name = paste0("P95: ", dollar(p95, 0.01))) %>%
      add_segments(x = 12, xend = 12, y = 0, yend = input$n_iter * 0.07,
        line = list(color = "#c07a3a", dash = "dot", width = 1.5),
        name = "Bid Trigger: $12.00") %>%
      layout(
        paper_bgcolor = "#fff9f6", plot_bgcolor = "#fff9f6",
        font  = list(family = "IBM Plex Mono", color = "#9e7272", size = 11),
        title = list(text = "Simulated Intrinsic Value Per Share – PulsePoint AI",
                     font = list(color = "#3d2b2b", size = 13)),
        xaxis = list(title = "Share Price ($)", gridcolor = "#f0d9d0", zerolinecolor = "#e8c8c0"),
        yaxis = list(title = "Frequency",       gridcolor = "#f0d9d0"),
        legend = list(font = list(color = "#3d2b2b"), bgcolor = "rgba(255,255,255,0.7)"),
        bargap = 0.05
      )
  })

  output$cdfPlot <- renderPlotly({
    sp  <- sort(sim_data()$SharePrice)
    cdf <- seq_along(sp) / length(sp)
    plot_ly(x = sp, y = cdf * 100, type = "scatter", mode = "lines",
      line = list(color = "#c0566b", width = 2),
      fill = "tozeroy", fillcolor = "rgba(192,86,107,0.07)") %>%
      add_segments(x = 9.62, xend = 9.62, y = 0, yend = 100,
        line = list(color = "#9b6bb5", dash = "dash", width = 1.5),
        name = "Answer Key $9.62") %>%
      layout(
        paper_bgcolor = "#fff9f6", plot_bgcolor = "#fff9f6",
        font  = list(family = "IBM Plex Mono", color = "#9e7272", size = 11),
        title = list(text = "Cumulative Distribution Function (CDF) – Share Price",
                     font = list(color = "#3d2b2b", size = 13)),
        xaxis = list(title = "Share Price ($)", gridcolor = "#f0d9d0"),
        yaxis = list(title = "Probability (%)", gridcolor = "#f0d9d0", range = c(0, 100)),
        legend = list(font = list(color = "#3d2b2b"), bgcolor = "rgba(255,255,255,0.7)")
      )
  })

  # ── TAB 2: Cash Flows ──
  output$cfTable <- renderDT({
    bc <- base_case()
    df <- bc$cf_table
    colnames(df) <- c("Year", "Revenue ($M)", "EBIT ($M)", "NOPAT ($M)",
                      "D&A ($M)", "CapEx ($M)", "ΔNWC ($M)", "FCFF ($M)",
                      "Disc. Factor", "PV(FCFF) ($M)")
    datatable(df,
      options = list(dom = "t", pageLength = 5, ordering = FALSE),
      rownames = FALSE, class = "display") %>%
      formatRound(columns = 2:8, digits = 2) %>%
      formatRound(columns = 9, digits = 4) %>%
      formatStyle("FCFF ($M)",      color = "#a6e3a1", fontWeight = "bold") %>%
      formatStyle("PV(FCFF) ($M)",  color = "#89b4fa", fontWeight = "bold")
  })

  output$cfBarPlot <- renderPlotly({
    df  <- base_case()$cf_table
    yrs <- df$Year
    plot_ly() %>%
      add_bars(x = yrs, y = df$Revenue,  name = "Revenue",
               marker = list(color = "rgba(192,86,107,0.30)")) %>%
      add_bars(x = yrs, y = df$FCFF,     name = "FCFF",
               marker = list(color = "rgba(78,140,98,0.75)")) %>%
      add_bars(x = yrs, y = df$NOPAT,    name = "NOPAT",
               marker = list(color = "rgba(232,149,109,0.75)")) %>%
      add_bars(x = yrs, y = df$PV_FCFF,  name = "PV(FCFF)",
               marker = list(color = "rgba(158,114,114,0.50)")) %>%
      layout(
        barmode = "group",
        paper_bgcolor = "#fff9f6", plot_bgcolor = "#fff9f6",
        font  = list(family = "IBM Plex Mono", color = "#9e7272", size = 11),
        xaxis = list(gridcolor = "#f0d9d0"),
        yaxis = list(title = "$M", gridcolor = "#f0d9d0"),
        legend = list(font = list(color = "#3d2b2b"), bgcolor = "rgba(255,255,255,0.7)")
      )
  })

  output$equityBridge <- renderUI({
    bc <- base_case()
    mk <- function(v) paste0("$", formatC(round(v, 1), format = "f", digits = 1, big.mark = ","), "M")
    tagList(
      tags$div(class = "bridge-row bridge-add",
        tags$div(class = "label",   "+ PV of FCFF (Yrs 1–5)"),
        tags$div(class = "amount",  mk(bc$pv_fcff))),
      tags$div(class = "bridge-row bridge-add",
        tags$div(class = "label",   "+ PV of Terminal Value"),
        tags$div(class = "amount",  mk(bc$pv_tv))),
      tags$div(class = "bridge-row bridge-total",
        tags$div(class = "label",   "= Enterprise Value"),
        tags$div(class = "amount",  mk(bc$ev))),
      tags$div(class = "bridge-row bridge-sub",
        tags$div(class = "label",   "− Net Debt"),
        tags$div(class = "amount",  mk(NET_DEBT))),
      tags$div(class = "bridge-row bridge-total",
        tags$div(class = "label",   "= Equity Value"),
        tags$div(class = "amount",  mk(bc$eq_val))),
      tags$div(class = "bridge-row bridge-add",
        tags$div(class = "label",   "÷ Shares Outstanding"),
        tags$div(class = "amount",  paste0(SHARES, "M"))),
      tags$div(class = "bridge-row bridge-total",
        tags$div(class = "label",   "= Intrinsic Share Price"),
        tags$div(class = "amount",  dollar(bc$sp, 0.01))),
      br(),
      tags$div(style = "font-size:10px; color:#9e7272; font-family:'IBM Plex Mono'",
        paste0("Ke: ", percent(bc$ke, 0.01),
               "  |  WACC: ", percent(bc$wacc, 0.01),
               "  |  TV/EV: ", percent(bc$pv_tv / bc$ev, 0.1)))
    )
  })

  # ── TAB 3: Sensitivity ──
  output$sensPlot <- renderPlotly({
    waccs <- seq(0.07, 0.17, by = 0.01)
    pgrs  <- seq(0.01, 0.045, by = 0.005)
    grid  <- expand.grid(wacc = waccs, pgr = pgrs)

    grid$price <- mapply(function(w, g) {
      bc   <- run_base_case(input$rev_growth_mean, input$ebit_mode,
                            (input$beta_min + input$beta_max) / 2, g)
      df2  <- bc$cf_table
      pv_fc <- sum(df2$PV_FCFF * (1 + bc$wacc)^(1:5) / (1 + w)^(1:5))
      tv2   <- (df2$FCFF[5] * (1 + g)) / (w - g)
      pv_t2 <- tv2 / (1 + w)^5
      (pv_fc + pv_t2 - NET_DEBT) / SHARES
    }, grid$wacc, grid$pgr)

    z_mat <- matrix(grid$price, nrow = length(waccs), ncol = length(pgrs), byrow = FALSE)
    plot_ly(x = pgrs * 100, y = waccs * 100, z = z_mat,
      type = "heatmap",
      colorscale = list(c(0,"#c0566b"), c(0.5,"#e8956d"), c(1,"#4e8c62")),
      colorbar = list(title = "$/share",
                      tickfont = list(family = "IBM Plex Mono", size = 10))) %>%
      layout(
        paper_bgcolor = "#fff9f6", plot_bgcolor = "#fff9f6",
        font  = list(family = "IBM Plex Mono", color = "#9e7272", size = 11),
        xaxis = list(title = "PGR (%)",  gridcolor = "#f0d9d0"),
        yaxis = list(title = "WACC (%)", gridcolor = "#f0d9d0")
      )
  })

  output$sensPlot2 <- renderPlotly({
    growths <- seq(0.05, 0.40, by = 0.025)
    margins <- seq(0.08, 0.30, by = 0.02)
    grid    <- expand.grid(g = growths, m = margins)

    grid$price <- mapply(function(g, m) {
      run_base_case(g, m, (input$beta_min + input$beta_max) / 2, input$pgr_mean)$sp
    }, grid$g, grid$m)

    z_mat <- matrix(grid$price, nrow = length(growths), ncol = length(margins), byrow = FALSE)
    plot_ly(x = margins * 100, y = growths * 100, z = z_mat,
      type = "heatmap",
      colorscale = list(c(0,"#c0566b"), c(0.5,"#e8956d"), c(1,"#4e8c62")),
      colorbar = list(title = "$/share",
                      tickfont = list(family = "IBM Plex Mono", size = 10))) %>%
      layout(
        paper_bgcolor = "#fff9f6", plot_bgcolor = "#fff9f6",
        font  = list(family = "IBM Plex Mono", color = "#9e7272", size = 11),
        xaxis = list(title = "EBIT Margin (%)",  gridcolor = "#f0d9d0"),
        yaxis = list(title = "Revenue Growth (%)", gridcolor = "#f0d9d0")
      )
  })

  # ── TAB 4: Scenarios ──
  output$scenarioTable <- renderTable({
    scenarios <- list(
      Bear = list(rev = 0.10, ebit = 0.10, beta = 1.65, pgr = 0.020),
      Base = list(rev = input$rev_growth_mean, ebit = input$ebit_mode,
                  beta = (input$beta_min + input$beta_max) / 2, pgr = input$pgr_mean),
      Bull = list(rev = 0.30, ebit = 0.22, beta = 1.25, pgr = 0.038)
    )
    rows <- lapply(names(scenarios), function(nm) {
      s  <- scenarios[[nm]]
      bc <- run_base_case(s$rev, s$ebit, s$beta, s$pgr)
      data.frame(
        Scenario    = nm,
        Rev_Growth  = paste0(round(s$rev  * 100, 1), "%"),
        EBIT_Margin = paste0(round(s$ebit * 100, 1), "%"),
        Beta        = round(s$beta, 2),
        WACC        = paste0(round(bc$wacc * 100, 2), "%"),
        EV          = paste0("$", round(bc$ev, 1), "M"),
        Share_Price = paste0("$", round(bc$sp, 2)),
        vs_Base     = paste0(ifelse(nm == "Base", "—",
                       ifelse(bc$sp > 9.62,
                              paste0("+", round((bc$sp / 9.62 - 1) * 100, 1), "%"),
                              paste0(round((bc$sp / 9.62 - 1) * 100, 1), "%"))))
      )
    })
    do.call(rbind, rows)
  }, striped = FALSE, hover = TRUE, bordered = FALSE, digits = 2, align = "c",
     sanitize.text.function = identity)

  output$fanPlot <- renderPlotly({
    sp   <- sim_data()$SharePrice
    pcts <- c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95)
    vals <- quantile(sp, pcts)
    clrs <- c("#c0566b","#e8956d","#c07a3a","#9b6bb5","#4e8c62","#3a7a8a","#2d6b5e")

    plot_ly() %>%
      add_bars(x = paste0("P", pcts * 100), y = vals,
        marker = list(color = clrs),
        text  = dollar(vals, 0.01),
        textposition = "outside",
        textfont = list(color = "#3d2b2b", size = 11, family = "IBM Plex Mono")) %>%
      add_segments(x = -0.5, xend = 6.5, y = 9.62, yend = 9.62,
        line = list(color = "#9b6bb5", dash = "dash", width = 1.5),
        name = "Answer Key $9.62") %>%
      add_segments(x = -0.5, xend = 6.5, y = 12, yend = 12,
        line = list(color = "#c07a3a", dash = "dot", width = 1.5),
        name = "Bid Trigger $12.00") %>%
      layout(
        paper_bgcolor = "#fff9f6", plot_bgcolor = "#fff9f6",
        font  = list(family = "IBM Plex Mono", color = "#9e7272", size = 11),
        xaxis = list(title = "Percentile",      gridcolor = "#f0d9d0"),
        yaxis = list(title = "Share Price ($)",  gridcolor = "#f0d9d0"),
        legend = list(font = list(color = "#3d2b2b"), bgcolor = "rgba(255,255,255,0.7)"),
        showlegend = TRUE
      )
  })

  # ── TAB 5: Statistics ──
  output$summaryStats <- renderPrint({
    cat("PulsePoint AI – Share Price Statistical Summary\n")
    cat("------------------------------------------------\n")
    s <- summary(sim_data()$SharePrice)
    print(s)
    sp <- sim_data()$SharePrice
    cat("\nStd Dev:  ", round(sd(sp), 4), "\n")
    cat("Skewness:", round(mean((sp - mean(sp))^3) / sd(sp)^3, 3), "\n")
    cat("Kurtosis:", round(mean((sp - mean(sp))^4) / sd(sp)^4 - 3, 3), "(excess)\n")
  })

  output$probTable <- renderTable({
    prices <- sim_data()$SharePrice
    data.frame(
      Scenario    = c("P(Price < $5)", "P(Price > $9.62 — ans key)",
                      "P(Price > $12 — bid trigger)", "P(Price > $15)",
                      "P(Price > $20)"),
      Probability = c(
        paste0(round(mean(prices <  5.00) * 100, 2), "%"),
        paste0(round(mean(prices >  9.62) * 100, 2), "%"),
        paste0(round(mean(prices > 12.00) * 100, 2), "%"),
        paste0(round(mean(prices > 15.00) * 100, 2), "%"),
        paste0(round(mean(prices > 20.00) * 100, 2), "%")
      )
    )
  }, striped = FALSE, hover = TRUE, bordered = FALSE, align = "c")

  output$waccDist <- renderPlotly({
    plot_ly(x = sim_data()$WACC * 100, type = "histogram", nbinsx = 50,
      marker = list(color = "rgba(232,149,109,0.65)",
                    line  = list(color = "rgba(192,86,107,0.15)", width = 0.5))) %>%
      layout(paper_bgcolor = "#fff9f6", plot_bgcolor = "#fff9f6",
        font  = list(family = "IBM Plex Mono", color = "#9e7272", size = 10),
        xaxis = list(title = "WACC (%)",  gridcolor = "#f0d9d0"),
        yaxis = list(title = "Frequency", gridcolor = "#f0d9d0"),
        title = list(text = "WACC Distribution",
                     font = list(color = "#3d2b2b", size = 12)))
  })

  output$fcffDist <- renderPlotly({
    plot_ly(x = sim_data()$FCFF_Y5, type = "histogram", nbinsx = 50,
      marker = list(color = "rgba(78,140,98,0.55)",
                    line  = list(color = "rgba(78,140,98,0.15)", width = 0.5))) %>%
      layout(paper_bgcolor = "#fff9f6", plot_bgcolor = "#fff9f6",
        font  = list(family = "IBM Plex Mono", color = "#9e7272", size = 10),
        xaxis = list(title = "FCFF Yr5 ($M)", gridcolor = "#f0d9d0"),
        yaxis = list(title = "Frequency",     gridcolor = "#f0d9d0"),
        title = list(text = "Year 5 FCFF Distribution",
                     font = list(color = "#3d2b2b", size = 12)))
  })

  output$evPie <- renderPlotly({
    bc <- base_case()
    plot_ly(
      labels = c("PV of FCFF (Yrs 1–5)", "PV of Terminal Value"),
      values = c(bc$pv_fcff, bc$pv_tv),
      type   = "pie",
      marker = list(colors = c("#c0566b","#e8956d"),
                    line   = list(color = "#fdf6f0", width = 2)),
      textfont = list(family = "IBM Plex Mono", size = 11, color = "#fff"),
      insidetextorientation = "radial"
    ) %>%
      layout(
        paper_bgcolor = "#fff9f6",
        font   = list(family = "IBM Plex Mono", color = "#9e7272", size = 11),
        title  = list(text = "EV Composition (Base Case)",
                      font = list(color = "#3d2b2b", size = 12)),
        legend = list(font = list(color = "#3d2b2b"), bgcolor = "rgba(255,255,255,0.7)")
      )
  })
}

shinyApp(ui = ui, server = server)
