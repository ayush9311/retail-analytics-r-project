# ==========================================================
# PREMIUM SHINY DASHBOARD: Retail Transaction Analytics
# PURE R ANALYTICS (NO ROC / NO PREDICTION)
# FULLY UPDATED WITH:
# - Graph descriptions
# - Business Insights
# - Live Analytics tab
# - Interactive controls for live segment graph
#
# Folder required:
# dashboard/
#   app.R
#   outputs/*.csv
#   www/plots/*.png
# ==========================================================

library(shiny)
library(bslib)
library(DT)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(plotly)

# -------------------------
# 1) HELPERS
# -------------------------
safe_read_csv <- function(path) {
  if (file.exists(path)) {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    NULL
  }
}

safe_img <- function(src, height = NULL) {
  disk_path <- file.path("www", src)
  
  if (!file.exists(disk_path)) {
    return(
      bslib::card(
        bslib::card_header(div(class = "section-title", "Image Missing")),
        bslib::card_body(
          div(
            style = "padding:10px; color:#b91c1c; font-weight:700;",
            paste("Missing file:", disk_path)
          ),
          div(
            style = "opacity:.75; font-size:13px;",
            "Fix: ensure the PNG exists inside dashboard/www/plots/."
          )
        )
      )
    )
  }
  
  st <- "width:100%; border-radius:18px; box-shadow:0 14px 40px rgba(2,6,23,.12);"
  if (!is.null(height)) st <- paste0(st, "height:", height, "; object-fit:cover;")
  tags$img(src = src, style = st)
}

kpi_card <- function(title, value, icon = "📌", subtitle = NULL) {
  bslib::card(
    class = "kpi-card",
    bslib::card_body(
      div(
        class = "kpi-top",
        div(class = "kpi-icon", icon),
        div(
          div(class = "kpi-title", title),
          if (!is.null(subtitle)) div(class = "kpi-sub", subtitle)
        )
      ),
      div(class = "kpi-value", value)
    )
  )
}

insight_card <- function(title, value, icon = "✨", note = NULL) {
  bslib::card(
    class = "insight-card",
    bslib::card_body(
      div(
        style = "display:flex; align-items:flex-start; gap:12px;",
        div(style = "font-size:24px;", icon),
        div(
          div(style = "font-size:13px; font-weight:800; color:#334155;", title),
          div(style = "font-size:20px; font-weight:900; color:#0f172a; margin-top:4px;", value),
          if (!is.null(note)) div(style = "font-size:12px; opacity:.75; margin-top:4px;", note)
        )
      )
    )
  )
}

graph_desc <- function(text) {
  div(class = "graph-desc", text)
}

fmt_dollar_safe <- function(x) {
  if (length(x) == 0 || is.na(x)) return("NA")
  scales::dollar(as.numeric(x))
}

# -------------------------
# 2) DATA LOAD
# -------------------------
kpi_basic             <- safe_read_csv("outputs/kpi_summary.csv")
kpi_plus              <- safe_read_csv("outputs/kpi_summary_plus_segments.csv")
monthly               <- safe_read_csv("outputs/monthly_sales.csv")
monthly_segment_sales <- safe_read_csv("outputs/monthly_segment_sales.csv")
top_prod              <- safe_read_csv("outputs/top10_products_by_revenue.csv")
top_prod_qty          <- safe_read_csv("outputs/top10_products_by_quantity.csv")
top_country           <- safe_read_csv("outputs/top10_countries_by_revenue.csv")
top_customers         <- safe_read_csv("outputs/top10_customers.csv")
weekday_sales         <- safe_read_csv("outputs/sales_by_weekday.csv")
hourly_sales          <- safe_read_csv("outputs/sales_by_hour.csv")
segments              <- safe_read_csv("outputs/segment_summary_named.csv")
seg_impact            <- safe_read_csv("outputs/segment_impact_revenue_share.csv")
top_rules             <- safe_read_csv("outputs/top20_association_rules.csv")
reco_table            <- safe_read_csv("outputs/recommendations_top_products.csv")
pareto_sum            <- safe_read_csv("outputs/pareto_80_20_summary.csv")

# -------------------------
# 3) UI
# -------------------------
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  
  tags$head(
    tags$style(HTML("
      body{
        background: radial-gradient(1200px 500px at 10% 0%, rgba(79,70,229,.16), transparent 55%),
                    radial-gradient(900px 500px at 90% 10%, rgba(16,185,129,.14), transparent 55%),
                    #f6f7fb;
      }
      .hero{
        border-radius: 22px;
        padding: 18px 18px;
        background: linear-gradient(135deg, rgba(255,255,255,.92), rgba(255,255,255,.75));
        border: 1px solid rgba(15,23,42,.08);
        box-shadow: 0 18px 55px rgba(2,6,23,.10);
        backdrop-filter: blur(10px);
        margin: 14px 6px 14px 6px;
      }
      .app-title{
        font-weight: 900;
        font-size: 28px;
        letter-spacing: .2px;
        color: #0f172a;
      }
      .subtle{
        opacity:.72;
        font-size:13.5px;
        color:#334155;
      }
      .badge-pill{
        display:inline-flex;
        align-items:center;
        gap:8px;
        padding: 6px 10px;
        border-radius: 999px;
        background: rgba(79,70,229,.10);
        border: 1px solid rgba(79,70,229,.18);
        color:#1f2a65;
        font-weight: 700;
        font-size: 12.5px;
      }
      .card{
        border: 1px solid rgba(15,23,42,.08) !important;
        border-radius: 20px !important;
        box-shadow: 0 16px 46px rgba(2,6,23,.10) !important;
        overflow: hidden;
      }
      .card-header{
        background: rgba(255,255,255,.9) !important;
        border-bottom: 1px solid rgba(15,23,42,.06) !important;
        font-weight: 900;
      }
      .section-title{
        font-weight:900;
        font-size:16px;
        color:#0f172a;
      }
      .kpi-card, .insight-card{
        border-radius: 20px !important;
        background: linear-gradient(135deg, rgba(255,255,255,.98), rgba(248,250,252,.90)) !important;
      }
      .kpi-top{
        display:flex;
        gap:12px;
        align-items:center;
      }
      .kpi-icon{
        width:42px;
        height:42px;
        border-radius:14px;
        display:flex;
        align-items:center;
        justify-content:center;
        background: rgba(79,70,229,.12);
        border: 1px solid rgba(79,70,229,.18);
        font-size: 20px;
      }
      .kpi-title{
        font-size:13px;
        opacity:.8;
        font-weight:800;
        color:#0f172a;
      }
      .kpi-sub{
        font-size:12px;
        opacity:.65;
        margin-top:2px;
      }
      .kpi-value{
        margin-top:10px;
        font-size:26px;
        font-weight:950;
        color:#0f172a;
      }
      .nav-link{
        font-weight: 800 !important;
        font-size: 14px !important;
        border-radius: 14px !important;
        margin-right: 6px;
      }
      .nav-tabs .nav-link.active{
        background: rgba(79,70,229,.12) !important;
        border-color: rgba(79,70,229,.25) !important;
      }
      .control-card{
        border-radius: 20px;
        padding: 14px;
        background: rgba(255,255,255,.85);
        border: 1px solid rgba(15,23,42,.08);
        box-shadow: 0 14px 38px rgba(2,6,23,.08);
      }
      .small-note{
        font-size:12px;
        opacity:.72;
      }
      .insight-box{
        border-radius:18px;
        padding:14px;
        background:rgba(255,255,255,.9);
        border:1px solid rgba(15,23,42,.08);
        box-shadow:0 8px 26px rgba(2,6,23,.06);
        margin-bottom:12px;
      }
      .graph-desc{
        font-size:12.5px;
        margin-top:12px;
        padding:8px 12px;
        border-left:3px solid #4F46E5;
        background:rgba(79,70,229,.05);
        border-radius:8px;
        color:#334155;
        line-height:1.45;
      }
      table.dataTable{
        border-radius: 14px;
        overflow:hidden;
      }
    "))
  ),
  
  div(
    class = "hero",
    div(
      style = "display:flex; justify-content:space-between; align-items:center; gap:12px; flex-wrap:wrap;",
      div(
        div(class = "app-title", "Retail Transaction Analytics Dashboard"),
        div(
          class = "subtle",
          "An interactive retail intelligence system built in R for customer segmentation, product association mining, and sales performance analysis."
        ),
        div(
          style = "margin-top:10px; display:flex; gap:10px; flex-wrap:wrap;",
          span(class = "badge-pill", "✨ Executive Summary"),
          span(class = "badge-pill", "📈 Interactive Plotly"),
          span(class = "badge-pill", "🛒 Recommendation Engine"),
          span(class = "badge-pill", "🧠 Pure R Analytics")
        )
      ),
      div(
        style = "text-align:right;",
        div(class = "subtle", "Dataset: UCI Online Retail"),
        div(class = "subtle", "Built with R • Shiny • bslib • Plotly")
      )
    )
  ),
  
  navset_tab(
    id = "tabs",
    
    nav_panel(
      "📌 Overview",
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        uiOutput("kpi1"), uiOutput("kpi2"), uiOutput("kpi3"), uiOutput("kpi4")
      ),
      br(),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        uiOutput("exec1"), uiOutput("exec2"), uiOutput("exec3"), uiOutput("exec4")
      ),
      br(),
      layout_columns(
        col_widths = c(4, 8),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Quick Controls")),
          bslib::card_body(
            div(
              class = "control-card",
              selectInput("segment_filter", "Filter KPIs / Segments", choices = c("All"), selected = "All"),
              textInput("prod_search", "Recommendation Search", placeholder = "Type product name (HEART, TEA, BOX, LAMP...)"),
              downloadButton("download_kpi", "Download KPI CSV"),
              div(class = "small-note", "Tip: use the Business Insights tab for summary findings.")
            )
          )
        ),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Monthly Sales Trend (Interactive)")),
          bslib::card_body(
            plotlyOutput("monthly_plotly", height = 360),
            graph_desc("Shows revenue movement across months and helps identify growth trends and seasonal changes.")
          )
        )
      ),
      br(),
      layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Pareto (80/20) Revenue Concentration")),
          bslib::card_body(
            safe_img("plots/pareto_80_20_revenue.png"),
            graph_desc("Shows that a relatively small percentage of customers contributes a large share of total revenue."),
            br(),
            uiOutput("pareto_text")
          )
        ),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Customer Segments Donut (Revenue Share)")),
          bslib::card_body(
            plotlyOutput("segment_donut", height = 420),
            graph_desc("Displays how total revenue is distributed across customer segments.")
          )
        )
      )
    ),
    
    nav_panel(
      "📈 Sales & Products",
      layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Top 10 Products by Revenue (Interactive)")),
          bslib::card_body(
            plotlyOutput("top_products_plotly", height = 400),
            graph_desc("Highlights the products contributing the most revenue.")
          )
        ),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Top 10 Countries by Revenue (Interactive)")),
          bslib::card_body(
            plotlyOutput("top_countries_plotly", height = 400),
            graph_desc("Shows the countries generating the highest total sales revenue.")
          )
        )
      ),
      br(),
      layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Top 10 Products by Quantity Sold")),
          bslib::card_body(
            plotlyOutput("top_products_qty_plotly", height = 400),
            graph_desc("Identifies products with the highest number of units sold.")
          )
        ),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Transaction Value Distribution")),
          bslib::card_body(
            safe_img("plots/transaction_value_distribution.png"),
            graph_desc("Shows that most transactions are small, while a few high-value transactions drive major revenue.")
          )
        )
      )
    ),
    
    nav_panel(
      "🕒 Time Insights",
      layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Sales by Weekday")),
          bslib::card_body(
            plotlyOutput("weekday_plotly", height = 380),
            graph_desc("Compares revenue across weekdays to identify peak shopping days.")
          )
        ),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Sales by Hour")),
          bslib::card_body(
            plotlyOutput("hourly_plotly", height = 380),
            graph_desc("Shows how sales vary by hour and helps identify peak shopping hours.")
          )
        )
      ),
      br(),
      layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Monthly Trend Snapshot")),
          bslib::card_body(
            safe_img("plots/monthly_sales_trend.png"),
            graph_desc("Static monthly trend snapshot showing the overall revenue pattern over time.")
          )
        ),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Data Quality Snapshot")),
          bslib::card_body(
            safe_img("plots/missing_values.png"),
            graph_desc("Displays missing values in the raw dataset and supports data quality assessment.")
          )
        )
      )
    ),
    
    nav_panel(
      "👥 Segmentation",
      layout_columns(
        col_widths = c(4, 8),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Segment Summary")),
          bslib::card_body(
            DTOutput("segments_tbl"),
            graph_desc("Summarizes each RFM-based customer segment by average recency, frequency, and monetary value.")
          )
        ),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Segmentation Visuals")),
          bslib::card_body(
            layout_columns(
              col_widths = c(6, 6),
              div(
                safe_img("plots/customer_segments_scatter.png", height = "260px"),
                graph_desc("Scatter plot of customer frequency versus spending across segments.")
              ),
              div(
                safe_img("plots/segments_pca.png", height = "260px"),
                graph_desc("PCA view of customer clusters for easier visual separation.")
              )
            )
          )
        )
      ),
      br(),
      layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Revenue Share by Segment")),
          bslib::card_body(
            safe_img("plots/revenue_share_by_segment.png"),
            graph_desc("Shows which customer segments generate the greatest revenue share.")
          )
        ),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Customer Share by Segment")),
          bslib::card_body(
            safe_img("plots/customer_share_by_segment.png"),
            graph_desc("Compares the number of customers in each segment.")
          )
        )
      ),
      br(),
      layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header(div(class = "section-title", "RFM Correlation Heatmap")),
          bslib::card_body(
            safe_img("plots/rfm_correlation_heatmap.png"),
            graph_desc("Shows correlation among recency, frequency, and monetary features.")
          )
        ),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Cluster Selection Diagnostics")),
          bslib::card_body(
            layout_columns(
              col_widths = c(6, 6),
              div(
                safe_img("plots/elbow_method.png", height = "260px"),
                graph_desc("Helps determine the appropriate number of clusters using within-cluster sum of squares.")
              ),
              div(
                safe_img("plots/silhouette_method.png", height = "260px"),
                graph_desc("Evaluates cluster quality by measuring separation and cohesion.")
              )
            )
          )
        )
      )
    ),
    
    nav_panel(
      "🛒 Market Basket & Recommendations",
      layout_columns(
        col_widths = c(5, 7),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Choose Product for Recommendation")),
          bslib::card_body(
            selectInput("selected_product", "Select Product", choices = NULL),
            uiOutput("selected_product_reco"),
            br(),
            downloadButton("download_reco", "Download Recommendations CSV")
          )
        ),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Recommendations (Search Enabled)")),
          bslib::card_body(
            DTOutput("reco_tbl"),
            graph_desc("Shows recommended companion products based on association rule mining.")
          )
        )
      ),
      br(),
      layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Top 20 Association Rules")),
          bslib::card_body(
            DTOutput("rules_tbl"),
            graph_desc("Lists the strongest product association rules ranked by support, confidence, and lift.")
          )
        ),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Download Rules")),
          bslib::card_body(
            downloadButton("download_rules", "Download Rules CSV"),
            br(), br(),
            div(class = "small-note", "Rules are ranked using support, confidence, and lift.")
          )
        )
      ),
      br(),
      layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Rules Scatter Plot")),
          bslib::card_body(
            safe_img("plots/rules_scatter_support_confidence.png"),
            graph_desc("Each point represents an association rule. Higher confidence and lift indicate stronger product relationships.")
          )
        ),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Rules Matrix Plot")),
          bslib::card_body(
            safe_img("plots/rules_matrix.png"),
            graph_desc("Matrix view of product-to-product associations, where stronger rules show higher lift.")
          )
        )
      )
    ),
    
    nav_panel(
      "🏆 Top Customers",
      layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Top Customers by Revenue")),
          bslib::card_body(
            plotlyOutput("top_customers_plotly", height = 420),
            graph_desc("Shows the highest-value customers and supports Pareto-based customer prioritization.")
          )
        ),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Why This Matters")),
          bslib::card_body(
            div(class = "insight-box",
                strong("Pareto Insight"), br(),
                "A small set of top customers contributes a major share of revenue. These customers should be prioritized for retention and loyalty strategies."
            ),
            div(class = "insight-box",
                strong("Business Action"), br(),
                "Use this view to identify your highest-value customers and align promotions, VIP programs, and retention plans."
            )
          )
        )
      )
    ),
    
    # =====================================================
    # NEW TAB: LIVE ANALYTICS
    # =====================================================
    nav_panel(
      "📡 Live Analytics",
      layout_columns(
        col_widths = c(4, 8),
        
        bslib::card(
          bslib::card_header(div(class = "section-title", "Live Graph Controls")),
          bslib::card_body(
            div(
              class = "control-card",
              selectInput("live_segments", "Select Segment(s)", choices = NULL, multiple = TRUE),
              uiOutput("live_date_ui"),
              radioButtons(
                "live_chart_type",
                "Chart Type",
                choices = c("Line" = "line", "Bar" = "bar"),
                selected = "line",
                inline = TRUE
              ),
              checkboxInput("live_show_points", "Show Points", value = TRUE),
              div(class = "small-note", "This tab is fully reactive. Changing the input filters updates the graph instantly.")
            )
          )
        ),
        
        bslib::card(
          bslib::card_header(div(class = "section-title", "Monthly Sales by Segment (Live)")),
          bslib::card_body(
            plotlyOutput("monthly_segment_plotly", height = 460),
            graph_desc("This reactive graph changes based on selected segment, date range, and chart type.")
          )
        )
      ),
      br(),
      layout_columns(
        col_widths = c(4, 4, 4),
        uiOutput("live_kpi1"),
        uiOutput("live_kpi2"),
        uiOutput("live_kpi3")
      ),
      br(),
      bslib::card(
        bslib::card_header(div(class = "section-title", "Live Insight Summary")),
        bslib::card_body(
          uiOutput("monthly_segment_live_summary")
        )
      )
    ),
    
    nav_panel(
      "💡 Business Insights",
      layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Key Findings")),
          bslib::card_body(
            uiOutput("insights_text")
          )
        ),
        bslib::card(
          bslib::card_header(div(class = "section-title", "Project Workflow")),
          bslib::card_body(
            div(class = "insight-box", strong("1. Raw Data"), br(), "UCI Online Retail transaction dataset"),
            div(class = "insight-box", strong("2. Cleaning + Feature Engineering"), br(), "Removed cancellations, missing customer IDs, invalid quantities, and created revenue/time features"),
            div(class = "insight-box", strong("3. Analytics"), br(), "KPI analysis, Pareto 80/20, RFM segmentation, Apriori market basket analysis"),
            div(class = "insight-box", strong("4. Output"), br(), "Interactive Shiny dashboard for business insights and decision support")
          )
        )
      )
    )
  )
)

# -------------------------
# 4) SERVER
# -------------------------
server <- function(input, output, session) {
  
  observe({
    ch <- "All"
    if (!is.null(segments) && "SegmentName" %in% names(segments)) {
      ch <- c("All", sort(unique(segments$SegmentName)))
    }
    updateSelectInput(session, "segment_filter", choices = ch, selected = "All")
  })
  
  observe({
    ch <- character(0)
    if (!is.null(reco_table) && "For_Product" %in% names(reco_table)) {
      ch <- sort(unique(reco_table$For_Product))
    }
    updateSelectInput(session, "selected_product", choices = ch)
  })
  
  observe({
    seg_choices <- character(0)
    if (!is.null(monthly_segment_sales) && "SegmentName" %in% names(monthly_segment_sales)) {
      seg_choices <- sort(unique(monthly_segment_sales$SegmentName))
    }
    updateSelectInput(session, "live_segments", choices = seg_choices, selected = seg_choices)
  })
  
  output$live_date_ui <- renderUI({
    req(monthly_segment_sales)
    df <- monthly_segment_sales %>% mutate(YearMonth = as.Date(YearMonth))
    dateRangeInput(
      "live_date_range",
      "Select Month Range",
      start = min(df$YearMonth, na.rm = TRUE),
      end   = max(df$YearMonth, na.rm = TRUE),
      min   = min(df$YearMonth, na.rm = TRUE),
      max   = max(df$YearMonth, na.rm = TRUE)
    )
  })
  
  kpi_src <- if (!is.null(kpi_plus)) kpi_plus else kpi_basic
  
  get_kpi <- function(name, fallback = "NA") {
    if (is.null(kpi_src)) return(fallback)
    row <- kpi_src %>% filter(Metric == name) %>% slice(1)
    if (nrow(row) == 0) return(fallback)
    row$Value[[1]]
  }
  
  output$kpi1 <- renderUI({
    seglab <- if (!is.null(input$segment_filter) && input$segment_filter != "All") {
      paste("All segments")
    } else {
      "All segments"
    }
    kpi_card("Total Revenue", get_kpi("Total Revenue"), "💰", seglab)
  })
  output$kpi2 <- renderUI(kpi_card("Total Invoices", get_kpi("Total Invoices"), "🧾"))
  output$kpi3 <- renderUI(kpi_card("Total Customers", get_kpi("Total Customers"), "👥"))
  output$kpi4 <- renderUI(kpi_card("Average Order Value", get_kpi("Average Order Value (AOV)"), "🛍️"))
  
  output$exec1 <- renderUI({
    val <- if (!is.null(pareto_sum) && nrow(pareto_sum) >= 2) pareto_sum$Value[[2]] else "NA"
    insight_card("Revenue Concentration", val, "💡", "share of customers contributing 80% revenue")
  })
  
  output$exec2 <- renderUI({
    top_seg <- get_kpi("Top Segment (by Revenue Share)", "NA")
    insight_card("Top Segment", top_seg, "👑", "highest revenue-contributing segment")
  })
  
  output$exec3 <- renderUI({
    top_ctry <- get_kpi("Top Country (by Sales)", "NA")
    insight_card("Top Market", top_ctry, "🌍", "highest revenue country")
  })
  
  output$exec4 <- renderUI({
    peak_day <- "NA"
    if (!is.null(weekday_sales) && nrow(weekday_sales) > 0) {
      peak_day <- weekday_sales %>% slice_max(Sales, n = 1) %>% pull(Weekday)
    }
    insight_card("Peak Shopping Day", peak_day, "⏰", "best weekday by revenue")
  })
  
  output$monthly_plotly <- renderPlotly({
    req(monthly)
    df <- monthly %>% mutate(YearMonth = as.Date(YearMonth))
    
    p <- ggplot(df, aes(x = YearMonth, y = Sales)) +
      geom_line(linewidth = 1.2, color = "#4E79A7") +
      geom_point(size = 2, color = "#4E79A7") +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = "Month", y = "Revenue") +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(margin = list(l = 40, r = 20, t = 10, b = 40))
  })
  
  # -------------------------
  # LIVE ANALYTICS REACTIVE DATA
  # -------------------------
  live_monthly_segment_df <- reactive({
    req(monthly_segment_sales, input$live_date_range)
    
    df <- monthly_segment_sales %>%
      mutate(
        YearMonth = as.Date(YearMonth),
        Sales = as.numeric(Sales),
        SegmentName = as.character(SegmentName)
      ) %>%
      filter(!is.na(YearMonth), !is.na(Sales), !is.na(SegmentName))
    
    if (!is.null(input$live_segments) && length(input$live_segments) > 0) {
      df <- df %>% filter(SegmentName %in% input$live_segments)
    }
    
    df %>%
      filter(
        YearMonth >= as.Date(input$live_date_range[1]),
        YearMonth <= as.Date(input$live_date_range[2])
      )
  })
  
  output$monthly_segment_plotly <- renderPlotly({
    df <- live_monthly_segment_df()
    req(nrow(df) > 0)
    
    if (input$live_chart_type == "bar") {
      p <- ggplot(df, aes(x = YearMonth, y = Sales, fill = SegmentName)) +
        geom_col(position = "dodge") +
        scale_y_continuous(labels = scales::dollar_format()) +
        labs(x = "Month", y = "Revenue", fill = "Segment") +
        theme_minimal(base_size = 13) +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    } else {
      p <- ggplot(df, aes(x = YearMonth, y = Sales, color = SegmentName, group = SegmentName)) +
        geom_line(linewidth = 1.3) +
        {
          if (isTRUE(input$live_show_points)) geom_point(size = 2) else NULL
        } +
        scale_y_continuous(labels = scales::dollar_format()) +
        labs(x = "Month", y = "Revenue", color = "Segment") +
        theme_minimal(base_size = 13) +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    }
    
    ggplotly(p, tooltip = c("x", "y", "color", "fill")) %>%
      layout(
        legend = list(orientation = "h", x = 0, y = -0.25),
        margin = list(l = 50, r = 20, t = 20, b = 90)
      )
  })
  
  output$live_kpi1 <- renderUI({
    df <- live_monthly_segment_df()
    total_live_sales <- sum(df$Sales, na.rm = TRUE)
    kpi_card("Live Revenue", fmt_dollar_safe(total_live_sales), "📡", "filtered live view")
  })
  
  output$live_kpi2 <- renderUI({
    df <- live_monthly_segment_df()
    latest_month <- if (nrow(df) > 0) max(df$YearMonth, na.rm = TRUE) else NA
    latest_month_txt <- if (!is.na(latest_month)) format(latest_month, "%b %Y") else "NA"
    kpi_card("Latest Visible Month", latest_month_txt, "🗓️", "based on selected range")
  })
  
  output$live_kpi3 <- renderUI({
    df <- live_monthly_segment_df()
    top_seg <- "NA"
    if (nrow(df) > 0) {
      top_seg <- df %>%
        group_by(SegmentName) %>%
        summarise(Sales = sum(Sales), .groups = "drop") %>%
        arrange(desc(Sales)) %>%
        slice(1) %>%
        pull(SegmentName)
    }
    kpi_card("Top Visible Segment", top_seg, "🏅", "top performer in filtered graph")
  })
  
  output$monthly_segment_live_summary <- renderUI({
    df <- live_monthly_segment_df()
    req(nrow(df) > 0)
    
    latest_month <- max(df$YearMonth, na.rm = TRUE)
    
    latest_top <- df %>%
      filter(YearMonth == latest_month) %>%
      arrange(desc(Sales)) %>%
      slice(1)
    
    tagList(
      div(
        class = "insight-box",
        strong("Live Summary"), br(),
        paste(
          "For", format(latest_month, "%b %Y"),
          ", the highest-performing visible segment is",
          latest_top$SegmentName,
          "with revenue of",
          scales::dollar(latest_top$Sales), "."
        )
      ),
      div(
        class = "insight-box",
        strong("How this tab works"), br(),
        "Use the segment selector, date range, and chart type controls to dynamically change the graph and analyze segment-wise sales patterns."
      )
    )
  })
  
  output$pareto_text <- renderUI({
    if (is.null(pareto_sum)) {
      return(div(class = "subtle", "Pareto summary not found."))
    }
    div(
      class = "subtle",
      paste(paste0(pareto_sum$Metric, ": ", pareto_sum$Value), collapse = " • ")
    )
  })
  
  output$segment_donut <- renderPlotly({
    req(seg_impact)
    d <- seg_impact %>%
      mutate(
        SegmentName = as.character(SegmentName),
        RevenueShare = as.numeric(RevenueShare)
      ) %>%
      filter(!is.na(RevenueShare))
    
    plot_ly(
      d,
      labels = ~SegmentName,
      values = ~RevenueShare,
      type = "pie",
      hole = 0.55,
      textinfo = "label+percent",
      hoverinfo = "label+percent"
    ) %>%
      layout(
        title = list(text = "Revenue Distribution by Customer Segment", x = 0),
        showlegend = FALSE,
        margin = list(l = 10, r = 10, t = 50, b = 10)
      )
  })
  
  output$top_products_plotly <- renderPlotly({
    req(top_prod)
    df <- top_prod %>%
      mutate(
        Description = stringr::str_trunc(Description, 28),
        Description = reorder(Description, Revenue)
      )
    
    p <- ggplot(df, aes(x = Description, y = Revenue)) +
      geom_col(width = 0.72, fill = "#4E79A7") +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = NULL, y = "Revenue") +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$top_countries_plotly <- renderPlotly({
    req(top_country)
    df <- top_country %>% mutate(Country = reorder(Country, Revenue))
    
    p <- ggplot(df, aes(x = Country, y = Revenue)) +
      geom_col(width = 0.72, fill = "#59A14F") +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = NULL, y = "Revenue") +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$top_products_qty_plotly <- renderPlotly({
    req(top_prod_qty)
    df <- top_prod_qty %>%
      mutate(
        Description = as.character(Description),
        QuantitySold = as.numeric(QuantitySold)
      ) %>%
      filter(!is.na(QuantitySold)) %>%
      mutate(
        Description = stringr::str_trunc(Description, 28),
        Description = reorder(Description, QuantitySold)
      )
    
    p <- ggplot(df, aes(x = Description, y = QuantitySold)) +
      geom_col(width = 0.72, fill = "#F28E2B") +
      coord_flip() +
      labs(x = NULL, y = "Quantity Sold") +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$weekday_plotly <- renderPlotly({
    req(weekday_sales)
    df <- weekday_sales %>%
      mutate(
        Weekday = as.character(Weekday),
        Sales = as.numeric(Sales)
      )
    
    df$Weekday <- factor(
      df$Weekday,
      levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    )
    df <- df %>% arrange(Weekday)
    
    p <- ggplot(df, aes(x = Weekday, y = Sales)) +
      geom_col(fill = "#9C755F") +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = "Weekday", y = "Revenue") +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$hourly_plotly <- renderPlotly({
    req(hourly_sales)
    df <- hourly_sales %>%
      mutate(
        Hour = as.numeric(Hour),
        Sales = as.numeric(Sales)
      ) %>%
      arrange(Hour)
    
    p <- ggplot(df, aes(x = Hour, y = Sales)) +
      geom_line(linewidth = 1.2, color = "#E15759") +
      geom_point(size = 2, color = "#E15759") +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = "Hour", y = "Revenue") +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$top_customers_plotly <- renderPlotly({
    req(top_customers)
    nm <- names(top_customers)
    
    id_col <- if ("CustomerID" %in% nm) "CustomerID" else nm[1]
    val_col <- if ("Revenue" %in% nm) "Revenue" else nm[2]
    
    df <- top_customers
    df[[id_col]] <- as.character(df[[id_col]])
    df[[val_col]] <- as.numeric(df[[val_col]])
    df[[id_col]] <- reorder(df[[id_col]], df[[val_col]])
    
    p <- ggplot(df, aes(x = .data[[id_col]], y = .data[[val_col]])) +
      geom_col(width = 0.72, fill = "#B07AA1") +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = "Customer", y = "Revenue") +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$segments_tbl <- renderDT({
    req(segments)
    datatable(segments, options = list(pageLength = 7, scrollX = TRUE), rownames = FALSE)
  })
  
  output$rules_tbl <- renderDT({
    req(top_rules)
    datatable(top_rules, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })
  
  reco_filtered <- reactive({
    req(reco_table)
    q <- input$prod_search
    if (is.null(q) || !nzchar(trimws(q))) return(reco_table)
    keep <- apply(reco_table, 1, function(r) any(grepl(q, as.character(r), ignore.case = TRUE)))
    reco_table[keep, , drop = FALSE]
  })
  
  output$reco_tbl <- renderDT({
    datatable(reco_filtered(), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })
  
  output$selected_product_reco <- renderUI({
    req(reco_table, input$selected_product)
    df <- reco_table %>% filter(For_Product == input$selected_product)
    
    if (nrow(df) == 0) {
      return(div(class = "small-note", "No recommendations found for this product."))
    }
    
    tags$div(
      lapply(seq_len(min(5, nrow(df))), function(i) {
        tags$div(
          class = "insight-box",
          strong(paste0("Recommendation ", i, ": ", df$Bought_With[i])),
          br(),
          paste("Confidence:", round(as.numeric(df$Confidence[i]), 3),
                "| Lift:", round(as.numeric(df$Lift[i]), 3))
        )
      })
    )
  })
  
  output$insights_text <- renderUI({
    top_seg <- get_kpi("Top Segment (by Revenue Share)", "NA")
    top_country_val <- get_kpi("Top Country (by Sales)", "NA")
    top_product_val <- get_kpi("Top Product (by Revenue)", "NA")
    
    peak_day <- "NA"
    if (!is.null(weekday_sales) && nrow(weekday_sales) > 0) {
      peak_day <- weekday_sales %>% slice_max(Sales, n = 1) %>% pull(Weekday)
    }
    
    strongest_rule <- "NA"
    if (!is.null(top_rules) && "rules" %in% names(top_rules) && nrow(top_rules) > 0) {
      strongest_rule <- as.character(top_rules$rules[1])
    }
    
    tagList(
      div(class = "insight-box",
          strong("Revenue Concentration"), br(),
          "The Pareto analysis shows that a relatively small percentage of customers contributes a large share of total revenue."
      ),
      div(class = "insight-box",
          strong("Most Valuable Segment"), br(),
          paste("The top customer segment by revenue contribution is:", top_seg)
      ),
      div(class = "insight-box",
          strong("Top Market"), br(),
          paste("The country contributing the highest revenue is:", top_country_val)
      ),
      div(class = "insight-box",
          strong("Top Product"), br(),
          paste("The highest revenue-generating product is:", top_product_val)
      ),
      div(class = "insight-box",
          strong("Peak Shopping Day"), br(),
          paste("The strongest weekday by sales is:", peak_day)
      ),
      div(class = "insight-box",
          strong("Strong Product Association"), br(),
          paste("One of the strongest association rules identified is:", strongest_rule)
      ),
      div(class = "insight-box",
          strong("Customer Spending Pattern"), br(),
          "Transaction value distribution shows that most purchases are low-value, while a small number of transactions contribute significantly higher revenue."
      ),
      div(class = "insight-box",
          strong("High-Value Customer Dependence"), br(),
          "Top customers contribute a disproportionately large share of revenue, indicating strong dependence on high-spending buyers."
      )
    )
  })
  
  output$download_kpi <- downloadHandler(
    filename = function() "kpi_summary.csv",
    content = function(file) {
      src <- "outputs/kpi_summary_plus_segments.csv"
      if (!file.exists(src)) src <- "outputs/kpi_summary.csv"
      file.copy(src, file)
    }
  )
  
  output$download_reco <- downloadHandler(
    filename = function() "recommendations_top_products.csv",
    content = function(file) {
      file.copy("outputs/recommendations_top_products.csv", file)
    }
  )
  
  output$download_rules <- downloadHandler(
    filename = function() "top20_association_rules.csv",
    content = function(file) {
      file.copy("outputs/top20_association_rules.csv", file)
    }
  )
}

shinyApp(ui, server)