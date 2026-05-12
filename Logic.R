library(tidyverse)
library(jsonlite)
library(httr)
library(scales)

# Helper for null/empty safety
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a)) a else b

# --- 1. Business Quality Automation (The "B" in BMP) ---
# Logic designed to assist qualitative assessment via financial proxies.
get_business_suggestions <- function(ticker, financials) {
    api_key <- Sys.getenv("FMP_API_KEY")
    
    # Fetch Growth Data for Proxy #2 (Ch 8, Page 131)
    growth_url <- paste0("https://financialmodelingprep.com/stable/financial-growth?symbol=", ticker)
    growth_res <- GET(growth_url, query = list(limit = 1, apikey = api_key))
    
    rev_growth <- 0.08 
    if(status_code(growth_res) == 200) {
        g_data <- fromJSON(content(growth_res, "text", encoding = "UTF-8"))
        if(is.data.frame(g_data) && nrow(g_data) > 0) {
            rev_growth <- as.numeric(g_data$revenueGrowth[1]) %||% 0.08
        }
    }
    
    # 1. Low Market Share Proxy (Page 91)
    # Rationale: Single-digit shares of a massive market. 
    # Logic: Total Revenue < $50B (50,000M)
    total_rev <- sum(as.numeric(financials$revenue), na.rm = TRUE)
    b1_auto <- total_rev < 50000 
    
    # 2. Large/Growing Market Proxy (Page 131)
    # Rationale: CAGR > 10% indicates a "rising tide" or escape velocity.
    b2_auto <- rev_growth > 0.10
    
    # 3. Sustainable Moat Proxy (Page 80)
    # Rationale: Adjusted margins > 25% are the signature of a digital "Toll Bridge".
    max_margin <- max(as.numeric(financials$adj_margin), na.rm = TRUE)
    b3_auto <- max_margin > 0.25
    
    summary_text <- case_when(
        b3_auto && b2_auto ~ "Strong Value 3.0 Signature: High-margin growth engine.",
        b3_auto ~ "Moat signature confirmed by margins.",
        b2_auto ~ "High growth detected; verify moat durability manually.",
        TRUE ~ "Legacy/Commodity profile: Requires deep manual 10-K review."
    )
    
    list(b1 = b1_auto, b2 = b2_auto, b3 = b3_auto, summary = summary_text)
}

# --- 2. API Connectors (Stable Premium) ---

get_fmp_segments <- function(ticker) {
    fmp_key <- Sys.getenv("FMP_API_KEY")
    
    # v4 Unpacking (Ch 8 Method)
    url_v4 <- paste0("https://financialmodelingprep.com/stable/revenue-product-segmentation?symbol=", ticker)
    message(paste(">>> FMP STABLE: Unpacking", ticker))
    res_v4 <- GET(url_v4, query = list(period = "annual", structure = "flat", apikey = fmp_key))
    
    if (status_code(res_v4) == 200) {
        raw_v4 <- fromJSON(content(res_v4, "text", encoding = "UTF-8"))
        if (is.data.frame(raw_v4) && nrow(raw_v4) > 0) {
            meta_keys <- c("date", "symbol", "period", "fiscalYear", "reportedCurrency", "fillingDate", "acceptedDate", "cik")
            return(raw_v4[1, ] %>% as.list() %>% unlist() %>% enframe(name = "segment", value = "revenue") %>%
                       filter(!segment %in% meta_keys) %>% mutate(revenue = as.numeric(revenue)/1e6) %>%
                       filter(!is.na(revenue), revenue > 0) %>%
                       mutate(symbol = ticker, adj_margin = 0.25, growth_est = 0.10, source = "API"))
        }
    }
    
    # v3 Fallover
    url_v3 <- paste0("https://financialmodelingprep.com/stable/income-statement?symbol=", ticker)
    res_v3 <- GET(url_v3, query = list(limit = 1, apikey = fmp_key))
    if (status_code(res_v3) == 200) {
        raw <- fromJSON(content(res_v3, "text", encoding = "UTF-8"))
        if (is.data.frame(raw) && nrow(raw) > 0) {
            inc <- raw[1, ]; rev <- as.numeric(inc$revenue)/1e6; oi <- as.numeric(inc$operatingIncome)/1e6
            rd <- as.numeric(inc$researchAndDevelopmentExpenses %||% 0)/1e6
            return(tibble(symbol=ticker, segment="Consolidated (Adjusted)", revenue=rev, 
                          adj_margin=round((oi + (0.5*rd))/rev, 3), growth_est=0.10, source="API"))
        }
    }
    return(tibble(symbol=ticker, segment="Manual Input Required", revenue=1000, adj_margin=0.15, growth_est=0.10, source="Manual"))
}

get_management_score <- function(ticker) {
    api_key <- Sys.getenv("FMP_API_KEY")
    url <- paste0("https://financialmodelingprep.com/stable/key-metrics?symbol=", ticker)
    res <- GET(url, query = list(limit = 1, apikey = api_key))
    roic_val <- 0.12
    if(status_code(res) == 200) {
        d <- fromJSON(content(res, "text", encoding = "UTF-8"))
        if (is.data.frame(d) && nrow(d) > 0) {
            roic_val <- as.numeric(d$roic[1] %||% d$returnOnInvestedCapital[1] %||% 0.12)
        }
    }
    list(roic = roic_val, score = if_else(roic_val > 0.15, 2, 1))
}

get_tiingo_price <- function(ticker) {
    api_key <- Sys.getenv("TIINGO_API_KEY")
    url <- paste0("https://api.tiingo.com/tiingo/daily/", ticker, "/prices")
    res <- GET(url, query = list(token = api_key))
    if (status_code(res) != 200) return(100)
    data <- fromJSON(content(res, "text", encoding = "UTF-8"))
    return(as.numeric(data$adjClose[1]) %||% 100)
}

get_av_shares <- function(ticker) {
    av_key <- Sys.getenv("ALPHAVANTAGE_API_KEY")
    fallbacks <- list("NVDA" = 24.6, "AMZN" = 10.4, "MSFT" = 7.4, "AAPL" = 15.4, "GOOGL" = 13.9)
    url <- "https://www.alphavantage.co/query"
    res <- GET(url, query = list(`function` = "OVERVIEW", symbol = ticker, apikey = av_key))
    if (status_code(res) != 200) return(as.numeric(fallbacks[[ticker]] %||% 1.0))
    data <- fromJSON(content(res, "text", encoding = "UTF-8"))
    shares <- as.numeric(data$SharesOutstanding %||% (fallbacks[[ticker]]*1e9)) / 1e9
    return(shares)
}

calculate_bmp_score <- function(ticker, segment_df, b_score, m_score, price, shares_billions) {
    ep_calc <- segment_df %>% mutate(rev_3yr = revenue * ((1 + growth_est)^3), adj_oi = rev_3yr * adj_margin)
    total_net_inc_mil <- sum(ep_calc$adj_oi, na.rm = TRUE) * 0.79 
    shares_mil <- (as.numeric(shares_billions) %||% 1) * 1000
    eps_power <- total_net_inc_mil / shares_mil
    yield <- eps_power / price
    list(yield = yield, eps = eps_power, status = if_else(yield >= 0.05, "VALUE 3.0 BUY", "VETOED (High Price)"),
         total_bm = b_score + m_score, price = price, max_buy = eps_power / 0.05, p_pass = (yield >= 0.05))
}