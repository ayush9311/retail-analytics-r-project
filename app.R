# ==========================================================
# PREMIUM SHINY DASHBOARD: Retail Transaction Analytics
# Reads outputs/ and uses www/plots/ images
# ==========================================================

# -------------------------
# 0) PACKAGES
# -------------------------
needed <- c(
  "shiny","bslib","DT","readr","dplyr","ggplot2","scales","stringr",
  "plotly","shinyWidgets","thematic"
)
installed <- rownames(installed.packages())
to_install <- setdiff(needed, installed)
if(length(to_install) > 0) install.packages(to_install)

library(shiny)
library(bslib)
library(DT)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(plotly)
library(shinyWidgets)
library(thematic)

# -------------------------
# 1) HELPERS
# -------------------------
safe_read_csv <- function(path) {
  if (file.exists(path)) readr::read_csv(path, show_col_types = FALSE) else NULL
}

fmt_money <- function(x) {
  if (is.null(x) || is.na(x)) return("NA")
  # x may come as character already
  suppressWarnings({
    if (is.character(x)) return(x)
    dollar(x, accuracy = 1)
  })
}

safe_img <- function(src, height = NULL) {
  # src is relative to www/ (e.g., "plots/file.png")
  # In Shiny, tags$img src is served from www automatically.
  st <- "width:100%; border-radius:18px; box-shadow:0 14px 40px rgba(2,6,23,.12);"
  if (!is.null(height)) st <- paste0(st, "height:", height, "; object-fit:cover;")
  tags$img(src = src, style = st)
}

kpi_card <- function(title, value, icon = "📌", subtitle = NULL) {
  bslib::card(
    class = "kpi-card",
    bslib::card_body(
      div(class="kpi-top",
          div(class="kpi-icon", icon),
          div(
            div(class="kpi-title", title),
            if (!is.null(subtitle)) div(class="kpi-sub", subtitle)
          )
      ),
      div(class="kpi-value", value)
    )
  )
}

# -------------------------
# 2) DATA LOAD (from outputs/)
# -------------------------
# NOTE: app.R is inside dashboard/, so outputs are ../outputs
kpi_basic   <- safe_read_csv("../outputs/kpi_summary.csv")
kpi_plus    <- safe_read_csv("../outputs/kpi_summary_plus_segments.csv")
monthly     <- safe_read_csv("../outputs/monthly_sales.csv")
top_prod    <- safe_read_csv("../outputs/top10_products_by_revenue.csv")
top_country <- safe_read_csv("../outputs/top10_countries_by_revenue.csv")
segments    <- safe_read_csv("../outputs/segment_summary_named.csv")
seg_impact  <- safe_read_csv("../outputs/segment_impact_revenue_share.csv")
top_rules   <- safe_read_csv("../outputs/top20_association_rules.csv")
reco_table  <- safe_read_csv("../outputs/recommendations_top_products.csv")
roc_auc     <- safe_read_csv("../outputs/roc_auc_value.csv")
pareto_sum  <- safe_read_csv("../outputs/pareto_80_20_summary.csv")

# -------------------------
# 3) UI
# -------------------------
thematic::thematic_shiny(font = "Inter")
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  
  tags$head(
    tags$style(HTML("
      /* ---- Premium background ---- */
      body{
        background: radial-gradient(1200px 500px at 10% 0%, rgba(79,70,229,.16), transparent 55%),
                    radial-gradient(900px 500px at 90% 10%, rgba(16,185,129,.14), transparent 55%),
                    #f6f7fb;
      }

      /* ---- Header ---- */
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
      .subtle{opacity:.72; font-size: 13.5px; color:#334155;}
      .badge-pill{
        display:inline-flex; align-items:center; gap:8px;
        padding: 6px 10px;
        border-radius: 999px;
        background: rgba(79,70,229,.10);
        border: 1px solid rgba(79,70,229,.18);
        color:#1f2a65;
        font-weight: 700;
        font-size: 12.5px;
      }

      /* ---- Cards ---- */
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
      .section-title{font-weight:900; font-size:16px; color:#0f172a;}

      /* ---- KPI ---- */
      .kpi-card{
        border-radius: 20px !important;
        background: linear-gradient(135deg, rgba(255,255,255,.98), rgba(248,250,252,.90)) !important;
      }
      .kpi-top{display:flex; gap:12px; align-items:center;}
      .kpi-icon{
        width:42px; height:42px; border-radius:14px;
        display:flex; align-items:center; justify-content:center;
        background: rgba(79,70,229,.12);
        border: 1px solid rgba(79,70,229,.18);
        font-size: 20px;
      }
      .kpi-title{font-size:13px; opacity:.8; font-weight:800; color:#0f172a;}
      .kpi-sub{font-size:12px; opacity:.65; margin-top:2px;}
      .kpi-value{margin-top:10px; font-size:26px; font-weight:950; color:#0f172a;}

      /* ---- Tabs ---- */
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

      /* ---- Inputs ---- */
      .control-card{
        border-radius: 20px;
        padding: 14px;
        background: rgba(255,255,255,.85);
        border: 1px solid rgba(15,23,42,.08);
        box-shadow: 0 14px 38px rgba(2,6,23,.08);
      }
      .small-note{font-size:12px; opacity:.72;}

      /* DT */
      table.dataTable{border-radius: 14px; overflow:hidden;}
    "))
  ),
  
  # HERO HEADER
  div(class="hero",
      div(style="display:flex; justify-content:space-between; align-items:center; gap:12px; flex-wrap:wrap;",
          div(
            div(class="app-title", "Retail Transaction Analytics Dashboard"),
            div(class="subtle",
                "Customer Segmentation • Market Basket (Apriori) • Pareto (80/20) • Repeat Purchase ROC"
            ),
            div(style="margin-top:10px; display:flex; gap:10px; flex-wrap:wrap;",
                span(class="badge-pill", "✨ Premium UI"),
                span(class="badge-pill", "📈 Interactive Plotly"),
                span(class="badge-pill", "🔎 Recommendation Search"),
                span(class="badge-pill", "🎯 KPI + Filters")
            )
          ),
          div(style="text-align:right;",
              div(class="subtle", "Dataset: UCI Online Retail"),
              div(class="subtle", "Built with R • Shiny • bslib • Plotly")
          )
      )
  ),
  
  navset_tab(
    id = "tabs",
    
    # ---------------- OVERVIEW ----------------
    nav_panel("📌 Overview",
              layout_columns(
                col_widths = c(3,3,3,3),
                uiOutput("kpi1"), uiOutput("kpi2"), uiOutput("kpi3"), uiOutput("kpi4")
              ),
              br(),
              
              layout_columns(
                col_widths = c(4,8),
                bslib::card(
                  bslib::card_header(div(class="section-title", "Quick Controls")),
                  bslib::card_body(
                    div(class="control-card",
                        pickerInput(
                          inputId = "segment_filter",
                          label = "Filter KPIs / Segments",
                          choices = if(!is.null(segments) && "SegmentName" %in% names(segments))
                            c("All", sort(unique(segments$SegmentName))) else c("All"),
                          selected = "All",
                          options = list(`live-search` = TRUE)
                        ),
                        textInput("prod_search", "Product Recommendation Search", placeholder = "Type product name (e.g., HEART, TEA, BOX, LAMP...)"),
                        div(class="small-note", "Tip: this search filters your recommendations table instantly.")
                    )
                  )
                ),
                bslib::card(
                  bslib::card_header(div(class="section-title", "Monthly Sales Trend (Interactive)")),
                  bslib::card_body(plotlyOutput("monthly_plotly", height = 360))
                )
              ),
              br(),
              
              layout_columns(
                col_widths = c(6,6),
                bslib::card(
                  bslib::card_header(div(class="section-title", "Pareto (80/20) Revenue Concentration")),
                  bslib::card_body(
                    # FIXED PATH: served from dashboard/www/plots/
                    safe_img("plots/pareto_80_20_revenue.png"),
                    br(),
                    uiOutput("pareto_text")
                  )
                ),
                bslib::card(
                  bslib::card_header(div(class="section-title", "Customer Segments Donut (Revenue Share)")),
                  bslib::card_body(plotlyOutput("segment_donut", height = 420))
                )
              )
    ),
    
    # ---------------- SALES & PRODUCTS ----------------
    nav_panel("📈 Sales & Products",
              layout_columns(
                col_widths = c(6,6),
                bslib::card(
                  bslib::card_header(div(class="section-title", "Top 10 Products by Revenue (Interactive)")),
                  bslib::card_body(plotlyOutput("top_products_plotly", height = 420))
                ),
                bslib::card(
                  bslib::card_header(div(class="section-title", "Top 10 Countries by Revenue (Interactive)")),
                  bslib::card_body(plotlyOutput("top_countries_plotly", height = 420))
                )
              )
    ),
    
    # ---------------- SEGMENTATION ----------------
    nav_panel("👥 Segmentation",
              layout_columns(
                col_widths = c(4,8),
                bslib::card(
                  bslib::card_header(div(class="section-title", "Segment Summary")),
                  bslib::card_body(DTOutput("segments_tbl"))
                ),
                bslib::card(
                  bslib::card_header(div(class="section-title", "Segmentation Visuals")),
                  bslib::card_body(
                    layout_columns(
                      col_widths = c(6,6),
                      safe_img("plots/customer_segments_scatter.png", height="260px"),
                      safe_img("plots/segments_pca.png", height="260px")
                    )
                  )
                )
              ),
              br(),
              layout_columns(
                col_widths = c(6,6),
                bslib::card(
                  bslib::card_header(div(class="section-title", "Revenue Share by Segment")),
                  bslib::card_body(safe_img("plots/revenue_share_by_segment.png"))
                ),
                bslib::card(
                  bslib::card_header(div(class="section-title", "Customer Share by Segment")),
                  bslib::card_body(safe_img("plots/customer_share_by_segment.png"))
                )
              )
    ),
    
    # ---------------- MARKET BASKET ----------------
    nav_panel("🛒 Market Basket & Recommendations",
              layout_columns(
                col_widths = c(6,6),
                bslib::card(
                  bslib::card_header(div(class="section-title", "Top 20 Association Rules")),
                  bslib::card_body(DTOutput("rules_tbl"))
                ),
                bslib::card(
                  bslib::card_header(div(class="section-title", "Recommendations (Search Enabled)")),
                  bslib::card_body(DTOutput("reco_tbl"))
                )
              ),
              br(),
              layout_columns(
                col_widths = c(6,6),
                bslib::card(
                  bslib::card_header(div(class="section-title", "Rules Scatter Plot")),
                  bslib::card_body(safe_img("plots/rules_scatter_support_confidence.png"))
                ),
                bslib::card(
                  bslib::card_header(div(class="section-title", "Rules Matrix Plot")),
                  bslib::card_body(safe_img("plots/rules_matrix.png"))
                )
              )
    ),
    
    # ---------------- ROC ----------------
    nav_panel("🎯 Model (ROC)",
              layout_columns(
                col_widths = c(6,6),
                bslib::card(
                  bslib::card_header(div(class="section-title", "ROC Curve: Repeat Purchase (30 days)")),
                  bslib::card_body(safe_img("plots/roc_curve_repeat_purchase.png"))
                ),
                bslib::card(
                  bslib::card_header(div(class="section-title", "AUC Score")),
                  bslib::card_body(uiOutput("auc_box"))
                )
              )
    )
  )
)

# -------------------------
# 4) SERVER
# -------------------------
server <- function(input, output, session) {
  
  # KPI source
  kpi_src <- if (!is.null(kpi_plus)) kpi_plus else kpi_basic
  
  get_kpi <- function(name, fallback = "NA") {
    if (is.null(kpi_src)) return(fallback)
    row <- kpi_src %>% filter(Metric == name) %>% slice(1)
    if (nrow(row) == 0) return(fallback)
    row$Value[[1]]
  }
  
  # ---- KPI cards (premium + segment selection label) ----
  output$kpi1 <- renderUI({
    seglab <- if (!is.null(input$segment_filter) && input$segment_filter != "All") paste("Segment:", input$segment_filter) else "All segments"
    kpi_card("Total Revenue", get_kpi("Total Revenue"), "💰", seglab)
  })
  output$kpi2 <- renderUI(kpi_card("Total Invoices", get_kpi("Total Invoices"), "🧾"))
  output$kpi3 <- renderUI(kpi_card("Total Customers", get_kpi("Total Customers"), "👥"))
  output$kpi4 <- renderUI(kpi_card("Average Order Value", get_kpi("Average Order Value (AOV)"), "🛍️"))
  
  # ---- Monthly trend: Plotly interactive ----
  output$monthly_plotly <- renderPlotly({
    req(monthly)
    df <- monthly %>%
      mutate(YearMonth = as.Date(YearMonth))
    
    p <- ggplot(df, aes(x = YearMonth, y = Sales)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = "Month", y = "Revenue") +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = c("x","y")) %>%
      layout(margin = list(l = 40, r = 20, t = 10, b = 40))
  })
  
  # ---- Pareto text ----
  output$pareto_text <- renderUI({
    if (is.null(pareto_sum)) {
      return(div(class="subtle", "Pareto summary not found. Run main.R after adding Pareto block."))
    }
    div(
      class="subtle",
      paste(paste0(pareto_sum$Metric, ": ", pareto_sum$Value), collapse = " • ")
    )
  })
  
  # ---- Segment donut: Plotly ----
  output$segment_donut <- renderPlotly({
    req(seg_impact)
    req("RevenueShare" %in% names(seg_impact))
    
    d <- seg_impact %>%
      mutate(
        SegmentName = if ("SegmentName" %in% names(seg_impact)) SegmentName else as.character(Segment),
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
  
  # ---- Top products: Plotly ----
  output$top_products_plotly <- renderPlotly({
    req(top_prod)
    df <- top_prod %>%
      mutate(Description = stringr::str_trunc(Description, 28),
             Description = reorder(Description, Revenue))
    
    p <- ggplot(df, aes(x = Description, y = Revenue)) +
      geom_col(width = 0.72) +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = NULL, y = "Revenue") +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = c("x","y")) %>%
      layout(margin = list(l = 10, r = 10, t = 10, b = 40))
  })
  
  # ---- Top countries: Plotly ----
  output$top_countries_plotly <- renderPlotly({
    req(top_country)
    df <- top_country %>%
      mutate(Country = reorder(Country, Revenue))
    
    p <- ggplot(df, aes(x = Country, y = Revenue)) +
      geom_col(width = 0.72) +
      coord_flip() +
      scale_y_continuous(labels = dollar_format()) +
      labs(x = NULL, y = "Revenue") +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = c("x","y")) %>%
      layout(margin = list(l = 10, r = 10, t = 10, b = 40))
  })
  
  # ---- Segment summary table ----
  output$segments_tbl <- renderDT({
    req(segments)
    datatable(
      segments,
      options = list(pageLength = 7, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # ---- Rules table ----
  output$rules_tbl <- renderDT({
    req(top_rules)
    datatable(
      top_rules,
      options = list(pageLength = 8, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # ---- Recommendations table WITH SEARCH BOX FILTER ----
  reco_filtered <- reactive({
    req(reco_table)
    q <- input$prod_search
    if (is.null(q) || !nzchar(trimws(q))) return(reco_table)
    
    # match across all columns safely
    keep <- apply(reco_table, 1, function(r) any(grepl(q, as.character(r), ignore.case = TRUE)))
    reco_table[keep, , drop = FALSE]
  })
  
  output$reco_tbl <- renderDT({
    datatable(
      reco_filtered(),
      options = list(pageLength = 8, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # ---- AUC box ----
  output$auc_box <- renderUI({
    if (is.null(roc_auc) || !("AUC" %in% names(roc_auc))) {
      return(kpi_card("AUC", "Not Found", "⚠️"))
    }
    kpi_card("AUC (ROC)", sprintf("%.4f", as.numeric(roc_auc$AUC[[1]])), "✅")
  })
}

shinyApp(ui, server)