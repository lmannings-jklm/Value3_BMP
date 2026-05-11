library(tidyverse)
library(jsonlite)
library(httr)
library(scales)

# --- 1. Global Setup ---
val3_watchlist <- tibble::tribble(
    ~symbol, ~business_name,            ~sector,
    "AMZN",  "Amazon",                 "Consumer Discretionary",
    "GOOGL", "Alphabet",               "Communication Services",
    "INTU",  "Intuit",                 "Information Technology",
    "MSFT",  "Microsoft",              "Information Technology",
    "HEICO", "HEICO Corp",             "Industrials",
    "NVDA",  "NVIDIA",                 "Information Technology"
)

# Robust helper for null/empty safety
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a)) a else b

# --- 2. Analyst Logic: Data-Driven Hints ---
get_business_suggestions <- function(financials) {
    if (is.null(financials) || nrow(financials) == 0) {
        return(list(b1=F, b2=F, b3=F, summary="Waiting for API data..."))
    }
    
    rev <- as.numeric(financials$revenue[1]) %||% 0
    margin <- as.numeric(financials$adj_margin[1]) %||% 0
    growth <- as.numeric(financials$growth_est[1]) %||% 0
    
    b1_hint <- rev < 50000 
    b2_hint <- growth >= 0.10
    b3_hint <- margin > 0.25
    
    summary <- case_when(
        margin > 0.35 ~ "Strong 'Toll Bridge' signature (Moat: High).",
        margin > 0.20 ~ "Standard Value 3.0 margin profile.",
        TRUE ~ "Caution: Margin profile resembles legacy business."
    )
    
    list(b1 = b1_hint, b2 = b2_hint, b3 = b3_hint, summary = summary)
}

# --- 3. API Connectors ---
get_fmp_segments <- function(ticker) {
    fmp_key <- Sys.getenv("FMP_API_KEY")
    av_key  <- Sys.getenv("ALPHAVANTAGE_API_KEY")
    
    url_fmp <- paste0("https://financialmodelingprep.com/stable/income-statement?symbol=", ticker)
    message(paste(">>> FMP Stable API: Requesting fundamentals for", ticker))
    
    res <- GET(url_fmp, query = list(limit = 1, apikey = fmp_key))
    
    if (status_code(res) == 200) {
        raw <- fromJSON(content(res, "text", encoding = "UTF-8"))
        if (is.data.frame(raw) && nrow(raw) > 0) {
            inc <- raw[1, ]
            rev <- (as.numeric(inc$revenue) %||% 0)/1e6
            oi  <- (as.numeric(inc$operatingIncome) %||% 0)/1e6
            rd  <- (as.numeric(inc$researchAndDevelopmentExpenses) %||% 0)/1e6
            return(tibble(symbol=ticker, segment="Consolidated (FMP Stable)", revenue=rev, 
                          adj_margin=round((oi + (0.5 * rd)) / rev, 3), growth_est=0.10, source="API"))
        }
    }
    
    # Failover to Alpha Vantage
    url_av <- "https://www.alphavantage.co/query"
    res_av <- GET(url_av, query = list(`function` = "INCOME_STATEMENT", symbol = ticker, apikey = av_key))
    if (status_code(res_av) == 200) {
        raw_av <- fromJSON(content(res_av, "text", encoding = "UTF-8"))
        if (!is.null(raw_av$annualReports) && length(raw_av$annualReports) > 0) {
            inc <- raw_av$annualReports[1, ]
            rev <- (as.numeric(inc$totalRevenue) %||% 0)/1e6
            oi  <- (as.numeric(inc$operatingIncome) %||% 0)/1e6
            rd  <- (as.numeric(inc$researchAndDevelopment) %||% 0)/1e6
            return(tibble(symbol=ticker, segment="Consolidated (AV Failover)", revenue=rev, 
                          adj_margin=round((oi + (0.5 * rd)) / rev, 3), growth_est=0.10, source="API"))
        }
    }
    return(tibble(symbol=ticker, segment="Manual Input Required", revenue=1000, adj_margin=0.15, growth_est=0.10, source="Manual"))
}

get_management_score <- function(ticker) {
    api_key <- Sys.getenv("FMP_API_KEY")
    url <- paste0("https://financialmodelingprep.com/stable/key-metrics?symbol=", ticker)
    res <- GET(url, query = list(limit = 1, apikey = api_key))
    roic <- 0.12 
    if(status_code(res) == 200) {
        d <- fromJSON(content(res, "text", encoding = "UTF-8"))
        if (is.data.frame(d) && nrow(d) > 0) roic <- (as.numeric(d$roic[1]) %||% 0.12)
    }
    list(roic = roic, score = if_else(roic > 0.15, 2, 1))
}

get_tiingo_price <- function(ticker) {
    api_key <- Sys.getenv("TIINGO_API_KEY")
    url <- paste0("https://api.tiingo.com/tiingo/daily/", ticker, "/prices")
    res <- GET(url, query = list(token = api_key))
    if (status_code(res) != 200) return(100)
    data <- fromJSON(content(res, "text", encoding = "UTF-8"))
    if (length(data) == 0) return(100)
    return(as.numeric(data$adjClose[1]) %||% 100)
}

get_av_shares <- function(ticker) {
    av_key <- Sys.getenv("ALPHAVANTAGE_API_KEY")
    fallbacks <- list("NVDA" = 24.6, "AMZN" = 10.4, "MSFT" = 7.4, "AAPL" = 15.4, "GOOGL" = 13.9)
    url <- "https://www.alphavantage.co/query"
    res <- GET(url, query = list(`function` = "OVERVIEW", symbol = ticker, apikey = av_key))
    if (status_code(res) != 200) return(as.numeric(fallbacks[[ticker]] %||% 1.0))
    data <- fromJSON(content(res, "text", encoding = "UTF-8"))
    shares <- as.numeric(data$SharesOutstanding) / 1e9
    return(shares %||% as.numeric(fallbacks[[ticker]] %||% 1.0))
}

# --- 4. BMP Engine ---
calculate_bmp_score <- function(ticker, segment_df, b_score, m_score, price, shares_billions) {
    ep_calc <- segment_df %>% 
        mutate(rev_3yr = revenue * ((1 + growth_est)^3), adj_oi = rev_3yr * adj_margin)
    total_net_inc_mil <- sum(ep_calc$adj_oi, na.rm = TRUE) * 0.79 
    shares_mil <- (as.numeric(shares_billions) %||% 1) * 1000
    eps_power <- total_net_inc_mil / shares_mil
    yield <- eps_power / price
    list(yield = yield, eps = eps_power, 
         status = if_else(yield >= 0.05, "VALUE 3.0 BUY", "VETOED (High Price)"),
         total_bm = b_score + m_score, price = price, 
         max_buy = eps_power / 0.05, p_pass = (yield >= 0.05))
}