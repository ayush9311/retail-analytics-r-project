# ==========================================================
# Retail Transaction Analytics (PURE R ANALYTICS)
# Customer Segmentation (RFM + K-means) + Market Basket (Apriori)
# Recommendation Engine + KPI Summary + Pareto (80/20)
# + Sales by Weekday + Sales by Hour + Product Quantity Analysis
# Dataset: UCI Online Retail (Online Retail.xlsx)
# ==========================================================

# -------------------------
# 0) PROJECT SETUP
# -------------------------
rm(list = ls())

dir.create("outputs", showWarnings = FALSE)
dir.create("plots", showWarnings = FALSE)

needed_pkgs <- c(
  "readxl","tidyverse","lubridate","cluster",
  "arules","arulesViz","factoextra",
  "ggcorrplot","scales","viridis"
)

installed <- rownames(installed.packages())
to_install <- setdiff(needed_pkgs, installed)
if (length(to_install) > 0) install.packages(to_install)

# -------------------------
# 1) LOAD LIBRARIES
# -------------------------
library(readxl)
library(tidyverse)
library(lubridate)
library(cluster)
library(arules)
library(arulesViz)
library(factoextra)
library(ggcorrplot)
library(scales)
library(viridis)
options(lifecycle_verbosity = "quiet")
# -------------------------
# GLOBAL THEME
# -------------------------
theme_set(
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 11, color = "gray30"),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )
)

cat("\n===== PROJECT STARTED =====\n")
print(Sys.time())

# -------------------------
# 2) LOAD DATA
# -------------------------
df_raw <- read_excel("data/Online Retail.xlsx")

cat("\n===== DATA LOADED =====\n")
cat("Rows:", nrow(df_raw), "Cols:", ncol(df_raw), "\n")

write.csv(df_raw, "outputs/raw_dataset.csv", row.names = FALSE)

# -------------------------
# 3) DATA CLEANING + FEATURE ENGINEERING
# -------------------------
df_clean <- df_raw %>%
  select(InvoiceNo, StockCode, Description, Quantity, InvoiceDate, UnitPrice, CustomerID, Country) %>%
  mutate(
    InvoiceDate = as.POSIXct(InvoiceDate),
    InvoiceNo = as.character(InvoiceNo),
    Description = as.character(Description),
    CustomerID = as.character(CustomerID)
  ) %>%
  filter(!is.na(CustomerID), !is.na(Description)) %>%
  filter(!str_detect(InvoiceNo, "^C")) %>%   # remove cancellations
  filter(Quantity > 0, UnitPrice > 0) %>%
  mutate(
    Description = str_squish(Description),
    TotalPrice = Quantity * UnitPrice,
    YearMonth = floor_date(InvoiceDate, "month"),
    Weekday = wday(InvoiceDate, label = TRUE, abbr = FALSE),
    Hour = hour(InvoiceDate)
  )

cat("\n===== CLEANING DONE =====\n")
cat("Raw rows:", nrow(df_raw), "\n")
cat("Clean rows:", nrow(df_clean), "\n")

write.csv(df_clean, "outputs/cleaned_online_retail.csv", row.names = FALSE)

# -------------------------
# 3.1) PARETO CHART (80/20): CUSTOMER REVENUE CONCENTRATION
# -------------------------
cust_rev <- df_clean %>%
  group_by(CustomerID) %>%
  summarise(Revenue = sum(TotalPrice), .groups = "drop") %>%
  arrange(desc(Revenue)) %>%
  mutate(
    Rank = row_number(),
    CumRevenue = cumsum(Revenue),
    TotalRevenue = sum(Revenue),
    CumRevenuePct = CumRevenue / TotalRevenue
  )

cut80 <- cust_rev %>% filter(CumRevenuePct >= 0.80) %>% slice(1)
n80 <- cut80$Rank
pct_customers_80 <- n80 / nrow(cust_rev)

write.csv(
  tibble(
    Metric = c("Customers needed for 80% Revenue", "Percent Customers for 80% Revenue"),
    Value = c(n80, scales::percent(pct_customers_80))
  ),
  "outputs/pareto_80_20_summary.csv",
  row.names = FALSE
)

max_rev <- max(cust_rev$Revenue, na.rm = TRUE)

p_pareto <- ggplot(cust_rev, aes(x = Rank)) +
  geom_col(aes(y = Revenue, fill = CumRevenuePct <= 0.80),
           show.legend = FALSE, alpha = 0.9, width = 0.8) +
  geom_line(aes(y = CumRevenuePct * max_rev), linewidth = 1.3, color = "#4E79A7") +
  geom_hline(yintercept = 0.80 * max_rev, linetype = 2, linewidth = 1, color = "gray40") +
  geom_vline(xintercept = n80, linetype = 2, linewidth = 1, color = "gray40") +
  annotate(
    "text",
    x = n80, y = 0.95 * max_rev,
    label = paste0("80% revenue reached by ~", scales::percent(pct_customers_80),
                   " customers\n(", n80, " customers)"),
    hjust = -0.05, vjust = 1, size = 4
  ) +
  scale_fill_manual(values = c("TRUE" = "#1f77b4", "FALSE" = "#ff7f0e")) +
  scale_y_continuous(
    labels = scales::dollar_format(),
    sec.axis = sec_axis(~ . / max_rev, labels = scales::percent, name = "Cumulative Revenue %")
  ) +
  labs(
    title = "Pareto (80/20 Rule): Customer Revenue Concentration",
    subtitle = "Bars = customer revenue (sorted). Line = cumulative revenue percentage.",
    x = "Customers ranked by spend (1 = highest)",
    y = "Revenue"
  )

ggsave("plots/pareto_80_20_revenue.png", p_pareto, width = 12, height = 6, dpi = 300)

cat("\n✅ PARETO CHART SAVED: plots/pareto_80_20_revenue.png\n")
cat("✅ PARETO SUMMARY SAVED: outputs/pareto_80_20_summary.csv\n")
cat("→ 80% revenue achieved by", n80, "customers (", scales::percent(pct_customers_80), ")\n")

# -------------------------
# 4) KPI + BUSINESS QUESTIONS SUMMARY
# -------------------------
total_revenue   <- sum(df_clean$TotalPrice)
total_invoices  <- n_distinct(df_clean$InvoiceNo)
total_customers <- n_distinct(df_clean$CustomerID)
aov <- total_revenue / total_invoices

top_month <- df_clean %>%
  group_by(YearMonth) %>%
  summarise(Sales = sum(TotalPrice), .groups="drop") %>%
  arrange(desc(Sales)) %>%
  slice(1)

top_country <- df_clean %>%
  group_by(Country) %>%
  summarise(Sales = sum(TotalPrice), .groups="drop") %>%
  arrange(desc(Sales)) %>%
  slice(1)

top_product_rev <- df_clean %>%
  group_by(Description) %>%
  summarise(Revenue = sum(TotalPrice), .groups="drop") %>%
  arrange(desc(Revenue)) %>%
  slice(1)

top_product_qty <- df_clean %>%
  group_by(Description) %>%
  summarise(QuantitySold = sum(Quantity), .groups="drop") %>%
  arrange(desc(QuantitySold)) %>%
  slice(1)

kpi_table <- tibble(
  Metric = c(
    "Total Revenue", "Total Invoices", "Total Customers", "Average Order Value (AOV)",
    "Top Month (by Sales)", "Top Month Sales",
    "Top Country (by Sales)", "Top Country Sales",
    "Top Product (by Revenue)", "Top Product Revenue",
    "Top Product (by Quantity)", "Top Product Quantity"
  ),
  Value = c(
    dollar(total_revenue),
    format(total_invoices, big.mark = ","),
    format(total_customers, big.mark = ","),
    dollar(aov),
    as.character(top_month$YearMonth),
    dollar(top_month$Sales),
    as.character(top_country$Country),
    dollar(top_country$Sales),
    as.character(top_product_rev$Description),
    dollar(top_product_rev$Revenue),
    as.character(top_product_qty$Description),
    format(top_product_qty$QuantitySold, big.mark = ",")
  )
)

write.csv(kpi_table, "outputs/kpi_summary.csv", row.names = FALSE)

# -------------------------
# 5) EDA GRAPHS
# -------------------------

# 5.1 Missing Values
na_counts <- colSums(is.na(df_raw))
na_df <- tibble(col = names(na_counts), na = as.numeric(na_counts))

p_na <- ggplot(na_df, aes(x = reorder(col, na), y = na, fill = na)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis(option = "C", direction = -1) +
  labs(
    title = "Missing Values (Raw Data)",
    subtitle = "Quick data quality check",
    x = NULL, y = "Missing Count"
  ) +
  theme(legend.position = "none")
ggsave("plots/missing_values.png", p_na, width = 9, height = 5, dpi = 300)

# 5.2 Monthly Sales Trend
monthly_sales <- df_clean %>%
  group_by(YearMonth) %>%
  summarise(Sales = sum(TotalPrice), .groups = "drop")

p_month <- ggplot(monthly_sales, aes(x = YearMonth, y = Sales)) +
  geom_line(color = "#E15759", linewidth = 1.3) +
  geom_point(color = "#E15759", size = 2.5) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Monthly Sales Trend",
    subtitle = "Revenue movement over time",
    x = "Month", y = "Revenue"
  )
ggsave("plots/monthly_sales_trend.png", p_month, width = 10, height = 5, dpi = 300)
write.csv(monthly_sales, "outputs/monthly_sales.csv", row.names = FALSE)

# 5.3 Top 10 Products by Revenue
top10_products_rev <- df_clean %>%
  group_by(Description) %>%
  summarise(Revenue = sum(TotalPrice), .groups="drop") %>%
  arrange(desc(Revenue)) %>%
  slice_head(n = 10)

p_top_prod <- ggplot(top10_products_rev, aes(x = reorder(Description, Revenue), y = Revenue, fill = Revenue)) +
  geom_col(width = 0.75) +
  coord_flip() +
  scale_fill_viridis(option = "D") +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Top 10 Products by Revenue",
    subtitle = "Best revenue drivers",
    x = NULL, y = "Revenue"
  ) +
  theme(legend.position = "none")
ggsave("plots/top10_products_by_revenue.png", p_top_prod, width = 10, height = 6, dpi = 300)
write.csv(top10_products_rev, "outputs/top10_products_by_revenue.csv", row.names = FALSE)

# 5.4 Top 10 Countries by Revenue
top10_countries <- df_clean %>%
  group_by(Country) %>%
  summarise(Revenue = sum(TotalPrice), .groups="drop") %>%
  arrange(desc(Revenue)) %>%
  slice_head(n = 10)

p_top_country <- ggplot(top10_countries, aes(x = reorder(Country, Revenue), y = Revenue, fill = Revenue)) +
  geom_col(width = 0.75) +
  coord_flip() +
  scale_fill_viridis(option = "B") +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Top 10 Countries by Revenue",
    subtitle = "Where sales come from",
    x = NULL, y = "Revenue"
  ) +
  theme(legend.position = "none")
ggsave("plots/top10_countries_by_revenue.png", p_top_country, width = 10, height = 6, dpi = 300)
write.csv(top10_countries, "outputs/top10_countries_by_revenue.csv", row.names = FALSE)

# 5.5 Transaction Value Distribution
p_dist <- ggplot(df_clean, aes(x = TotalPrice)) +
  geom_histogram(bins = 70, fill = "#59A14F", color = "white", alpha = 0.9) +
  scale_x_log10(labels = dollar_format()) +
  labs(
    title = "Transaction Value Distribution (Log Scale)",
    subtitle = "Most transactions are small; few are very large",
    x = "TotalPrice (log10)", y = "Count"
  )
ggsave("plots/transaction_value_distribution.png", p_dist, width = 9, height = 5, dpi = 300)

# 5.6 Sales by Weekday
weekday_sales <- df_clean %>%
  group_by(Weekday) %>%
  summarise(Sales = sum(TotalPrice), .groups = "drop")

weekday_sales$Weekday <- factor(
  weekday_sales$Weekday,
  levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
)

weekday_sales <- weekday_sales %>% arrange(Weekday)

p_weekday <- ggplot(weekday_sales, aes(x = Weekday, y = Sales, fill = Sales)) +
  geom_col() +
  scale_fill_viridis(option = "C") +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Sales by Weekday",
    subtitle = "Identify peak shopping days",
    x = "Day of Week", y = "Revenue"
  ) +
  theme(legend.position = "none")
ggsave("plots/sales_by_weekday.png", p_weekday, width = 9, height = 5, dpi = 300)
write.csv(weekday_sales, "outputs/sales_by_weekday.csv", row.names = FALSE)

# 5.7 Sales by Hour
hourly_sales <- df_clean %>%
  group_by(Hour) %>%
  summarise(Sales = sum(TotalPrice), .groups = "drop")

p_hour <- ggplot(hourly_sales, aes(x = Hour, y = Sales)) +
  geom_line(color = "#4E79A7", linewidth = 1.2) +
  geom_point(size = 2, color = "#4E79A7") +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Sales by Hour",
    subtitle = "Peak shopping hours",
    x = "Hour of Day", y = "Revenue"
  )
ggsave("plots/sales_by_hour.png", p_hour, width = 9, height = 5, dpi = 300)
write.csv(hourly_sales, "outputs/sales_by_hour.csv", row.names = FALSE)

# 5.8 Top 10 Products by Quantity
top10_products_qty <- df_clean %>%
  group_by(Description) %>%
  summarise(QuantitySold = sum(Quantity), .groups="drop") %>%
  arrange(desc(QuantitySold)) %>%
  slice_head(n = 10)

p_top_qty <- ggplot(top10_products_qty,
                    aes(x = reorder(Description, QuantitySold), y = QuantitySold, fill = QuantitySold)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis(option = "D") +
  labs(
    title = "Top 10 Products by Quantity Sold",
    subtitle = "Best-selling products by units",
    x = NULL, y = "Quantity"
  ) +
  theme(legend.position = "none")
ggsave("plots/top10_products_by_quantity.png", p_top_qty, width = 10, height = 6, dpi = 300)
write.csv(top10_products_qty, "outputs/top10_products_by_quantity.csv", row.names = FALSE)

cat("\n===== EDA DONE (GRAPHS SAVED) =====\n")

# -------------------------
# 6) CUSTOMER SEGMENTATION (RFM + K-MEANS) + SEGMENT NAMING
# -------------------------
ref_date <- max(df_clean$InvoiceDate)

rfm <- df_clean %>%
  group_by(CustomerID) %>%
  summarise(
    Recency = as.numeric(difftime(ref_date, max(InvoiceDate), units = "days")),
    Frequency = n_distinct(InvoiceNo),
    Monetary = sum(TotalPrice),
    .groups = "drop"
  )

write.csv(rfm, "outputs/rfm_table.csv", row.names = FALSE)

# RFM correlation heatmap
rfm_corr <- cor(rfm[, c("Recency", "Frequency", "Monetary")], use = "complete.obs")
png("plots/rfm_correlation_heatmap.png", width = 1200, height = 800)
print(
  ggcorrplot(rfm_corr, lab = TRUE, colors = c("#E15759", "#F2F2F2", "#4E79A7")) +
    ggtitle("RFM Correlation Heatmap")
)
dev.off()

rfm_scaled <- scale(rfm[, c("Recency", "Frequency", "Monetary")])

# Elbow Method
png("plots/elbow_method.png", width = 1200, height = 800)
print(fviz_nbclust(rfm_scaled, kmeans, method = "wss") + ggtitle("Elbow Method (WSS)"))
dev.off()

# Silhouette Method
png("plots/silhouette_method.png", width = 1200, height = 800)
print(fviz_nbclust(rfm_scaled, kmeans, method = "silhouette") + ggtitle("Silhouette Method"))
dev.off()

set.seed(123)
k <- 4
km <- kmeans(rfm_scaled, centers = k, nstart = 25)
rfm$Segment <- factor(km$cluster)

segment_summary <- rfm %>%
  group_by(Segment) %>%
  summarise(
    Customers = n(),
    Avg_Recency = mean(Recency),
    Avg_Frequency = mean(Frequency),
    Avg_Monetary = mean(Monetary),
    .groups = "drop"
  ) %>%
  arrange(desc(Avg_Monetary), desc(Avg_Frequency), Avg_Recency)

segment_summary <- segment_summary %>%
  mutate(SegmentName = case_when(
    row_number() == 1 ~ "VIP / Champions",
    row_number() == 2 ~ "Loyal Customers",
    row_number() == 3 ~ "Potential Customers",
    TRUE              ~ "At-Risk / Lost"
  ))

write.csv(segment_summary, "outputs/segment_summary_named.csv", row.names = FALSE)

seg_map <- segment_summary %>% select(Segment, SegmentName)
rfm_named <- rfm %>% left_join(seg_map, by = "Segment")
write.csv(rfm_named, "outputs/customers_with_segments.csv", row.names = FALSE)
# ==============================
# Monthly Sales by Segment (LIVE DASHBOARD DATA)
# ==============================
# Monthly Sales by Segment (for live dashboard graph)
monthly_segment_sales <- df_clean %>%
  left_join(rfm_named %>% select(CustomerID, SegmentName), by = "CustomerID") %>%
  group_by(YearMonth, SegmentName) %>%
  summarise(Sales = sum(TotalPrice), .groups = "drop")

write.csv(monthly_segment_sales, "outputs/monthly_segment_sales.csv", row.names = FALSE)
# PCA cluster plot
png("plots/segments_pca.png", width = 1200, height = 800)
print(
  fviz_cluster(list(data = rfm_scaled, cluster = km$cluster),
               geom = "point", ellipse.type = "norm") +
    ggtitle("Customer Segments (PCA View)") +
    theme(legend.position = "right")
)
dev.off()

# Scatter plot
p_seg_scatter <- ggplot(rfm_named, aes(x = Frequency, y = Monetary, color = SegmentName)) +
  geom_point(alpha = 0.75, size = 2.2) +
  scale_color_viridis_d(option = "D", end = 0.9) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Customer Segments: Frequency vs Spending",
    subtitle = "Each color = segment (K-means on RFM)",
    x = "Frequency (#Invoices)", y = "Monetary (Total Spend)"
  )
ggsave("plots/customer_segments_scatter.png", p_seg_scatter, width = 10, height = 6, dpi = 300)

cat("\n===== SEGMENTATION DONE =====\n")
print(segment_summary)

# -------------------------
# 7) SEGMENT BUSINESS IMPACT (REVENUE SHARE)
# -------------------------
cust_revenue <- df_clean %>%
  group_by(CustomerID) %>%
  summarise(CustomerRevenue = sum(TotalPrice), .groups = "drop")

segment_impact <- rfm_named %>%
  left_join(cust_revenue, by = "CustomerID") %>%
  group_by(SegmentName) %>%
  summarise(
    Customers = n(),
    Revenue = sum(CustomerRevenue),
    .groups = "drop"
  ) %>%
  mutate(
    CustomerShare = Customers / sum(Customers),
    RevenueShare = Revenue / sum(Revenue)
  ) %>%
  arrange(desc(Revenue))

write.csv(segment_impact, "outputs/segment_impact_revenue_share.csv", row.names = FALSE)

p_rev_share <- ggplot(segment_impact, aes(x = reorder(SegmentName, RevenueShare), y = RevenueShare, fill = SegmentName)) +
  geom_col(width = 0.75) +
  coord_flip() +
  scale_fill_viridis_d(option = "C") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Revenue Share by Customer Segment",
    subtitle = "Business impact of each segment",
    x = NULL, y = "Revenue Share"
  ) +
  theme(legend.position = "none")
ggsave("plots/revenue_share_by_segment.png", p_rev_share, width = 10, height = 6, dpi = 300)

p_cust_share <- ggplot(segment_impact, aes(x = reorder(SegmentName, CustomerShare), y = CustomerShare, fill = SegmentName)) +
  geom_col(width = 0.75) +
  coord_flip() +
  scale_fill_viridis_d(option = "A") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Customer Share by Segment",
    subtitle = "How many customers fall into each segment",
    x = NULL, y = "Customer Share"
  ) +
  theme(legend.position = "none")
ggsave("plots/customer_share_by_segment.png", p_cust_share, width = 10, height = 6, dpi = 300)

vip_row <- segment_impact %>% arrange(desc(RevenueShare)) %>% slice(1)
kpi_extra <- tibble(
  Metric = c("Top Segment (by Revenue Share)", "Top Segment Revenue Share"),
  Value  = c(vip_row$SegmentName, percent(vip_row$RevenueShare))
)

write.csv(bind_rows(kpi_table, kpi_extra), "outputs/kpi_summary_plus_segments.csv", row.names = FALSE)

# -------------------------
# 8) MARKET BASKET ANALYSIS (APRIORI) + RECOMMENDATIONS
# -------------------------
df_mba <- df_clean %>%
  filter(Country == "United Kingdom") %>%
  select(InvoiceNo, Description) %>%
  filter(Description != "") %>%
  distinct(InvoiceNo, Description)

basket_list <- split(df_mba$Description, df_mba$InvoiceNo)
trans <- as(basket_list, "transactions")

cat("\n===== TRANSACTIONS SUMMARY =====\n")
print(summary(trans))

rules <- apriori(
  trans,
  parameter = list(supp = 0.005, conf = 0.30, minlen = 2)
)

if (length(rules) == 0) {
  cat("\n⚠️ No association rules found. Try lowering support/confidence.\n")
  write.csv(data.frame(), "outputs/top20_association_rules.csv", row.names = FALSE)
  write.csv(data.frame(), "outputs/recommendations_top_products.csv", row.names = FALSE)
} else {
  rules_sorted <- sort(rules, by = "lift", decreasing = TRUE)
  
  top_rules_20 <- head(rules_sorted, 20)
  write.csv(as(top_rules_20, "data.frame"), "outputs/top20_association_rules.csv", row.names = FALSE)
  saveRDS(rules_sorted, "outputs/apriori_rules_all.rds")
  
  png("plots/rules_scatter_support_confidence.png", width = 1200, height = 800)
  plot(head(rules_sorted, 50), measure = c("support", "confidence"), shading = "lift")
  dev.off()
  
  png("plots/rules_matrix.png", width = 1200, height = 800)
  plot(head(rules_sorted, 50), method = "matrix", measure = "lift")
  dev.off()
  
  # Recommendation Engine
  rules_df <- as(rules_sorted, "data.frame") %>%
    tidyr::separate(.data$rules, into = c("lhs", "rhs"), sep = " => ", remove = FALSE) %>%
    mutate(
      lhs_clean = gsub("\\{|\\}", "", as.character(lhs)),
      rhs_clean = gsub("\\{|\\}", "", as.character(rhs)),
      lhs_clean = stringr::str_squish(lhs_clean),
      rhs_clean = stringr::str_squish(rhs_clean)
    )
  
  recommend_for_item <- function(item_name, top_n = 5) {
    item_name <- stringr::str_squish(as.character(item_name))
    rules_df %>%
      dplyr::filter(stringr::str_detect(lhs_clean, stringr::fixed(item_name, ignore_case = TRUE))) %>%
      dplyr::arrange(dplyr::desc(lift), dplyr::desc(confidence)) %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::transmute(
        Bought_With = rhs_clean,
        Support = support,
        Confidence = confidence,
        Lift = lift
      )
  }
  
  top_items <- top10_products_rev$Description
  
  reco_table <- purrr::map_dfr(top_items, function(it) {
    recos <- recommend_for_item(it, top_n = 5)
    if (nrow(recos) == 0) return(NULL)
    recos %>% dplyr::mutate(For_Product = it) %>% dplyr::select(For_Product, dplyr::everything())
  })
  
  write.csv(reco_table, "outputs/recommendations_top_products.csv", row.names = FALSE)
  
  cat("\n===== MARKET BASKET DONE + RECOMMENDATIONS SAVED =====\n")
  cat("Saved: outputs/recommendations_top_products.csv\n")
}
# -------------------------
# 8.1) TOP CUSTOMERS BY REVENUE
# -------------------------
top10_customers <- df_clean %>%
  group_by(CustomerID) %>%
  summarise(Revenue = sum(TotalPrice), .groups = "drop") %>%
  arrange(desc(Revenue)) %>%
  slice_head(n = 10)

write.csv(top10_customers, "outputs/top10_customers.csv", row.names = FALSE)
# -------------------------
# 9) FINAL SUMMARY
# -------------------------
cat("\n\n==================================================\n")
cat("FINAL PROJECT SUMMARY\n")
cat("==================================================\n")

cat("\nKey Files to Show Professor:\n")
cat("- outputs/kpi_summary_plus_segments.csv\n")
cat("- outputs/segment_impact_revenue_share.csv\n")
cat("- outputs/top20_association_rules.csv\n")
cat("- outputs/recommendations_top_products.csv\n")
cat("- outputs/pareto_80_20_summary.csv\n")
cat("- outputs/sales_by_weekday.csv\n")
cat("- outputs/sales_by_hour.csv\n")
cat("- outputs/top10_products_by_quantity.csv\n")
cat("- plots/pareto_80_20_revenue.png\n")
cat("- plots/monthly_sales_trend.png\n")
cat("- plots/sales_by_weekday.png\n")
cat("- plots/sales_by_hour.png\n")
cat("- plots/top10_products_by_quantity.png\n")
cat("- plots/revenue_share_by_segment.png\n")
cat("- plots/customer_segments_scatter.png\n")
cat("- plots/segments_pca.png\n")
cat("- plots/rules_matrix.png\n")
cat("- plots/rules_scatter_support_confidence.png\n")

cat("\nDone ✅\n")
cat("==================================================\n")

cat("\n===== PROJECT FINISHED =====\n")
print(Sys.time())