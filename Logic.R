library(tidyverse)
library(jsonlite)
library(httr)
library(scales)

# Helper for null/empty safety
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a)) a else b

# --- 1. Analyst Logic: B-Score Hints ---
get_business_suggestions <- function(ticker, financials) {
    if (is.null(financials) || nrow(financials) == 0) return(list(b1=F, b2=F, b3=F, summary="Waiting..."))
    total_rev <- sum(as.numeric(financials$revenue), na.rm = TRUE)
    max_margin <- max(as.numeric(financials$adj_margin), na.rm = TRUE)
    list(b1 = total_rev < 50000, b2 = TRUE, b3 = max_margin > 0.25, 
         summary = if_else(max_margin > 0.30, "Strong Moat signature detected.", "Standard profile."))
}

# --- 2. The Unpacking Engine ---

get_fmp_segments <- function(ticker, fmp_key) {
    clean_key <- gsub("[[:space:]]", "", fmp_key)
    
    # ATTEMPT 1: Stable Unpacking (Premium)
    url_stable <- "https://financialmodelingprep.com/stable/revenue-product-segmentation"
    message(paste(">>> FMP STABLE REQ:", ticker))
    
    res <- GET(url_stable, query = list(symbol = ticker, period = "annual", structure = "flat", apikey = clean_key))
    .GlobalEnv$last_api_status <- status_code(res)
    
    if (status_code(res) == 200) {
        raw <- fromJSON(content(res, "text", encoding = "UTF-8"), simplifyVector = TRUE)
        if (length(raw) > 0) {
            # Take first row and flatten list-columns
            row_data <- if(is.data.frame(raw)) raw[1, ] else raw[[1]]
            
            meta_keys <- c("date", "symbol", "period", "fiscalYear", "reportedCurrency", "fillingDate", "acceptedDate", "cik")
            
            # Process and scale revenue correctly
            latest <- row_data %>% as.list() %>% unlist() %>% enframe(name = "segment", value = "revenue") %>%
                filter(!segment %in% meta_keys) %>% 
                mutate(revenue = as.numeric(revenue)/1e6) %>% # Standardize to Millions
                filter(!is.na(revenue), revenue > 0) %>%
                mutate(symbol = ticker, adj_margin = 0.25, growth_est = 0.10, source = "API")
            
            return(latest)
        }
    }
    
    # ATTEMPT 2: Fundamentals Fallback (Stable)
    url_v3 <- "https://financialmodelingprep.com/stable/income-statement"
    res_v3 <- GET(url_v3, query = list(symbol = ticker, limit = 1, apikey = clean_key))
    
    if (status_code(res_v3) == 200) {
        raw_v3 <- fromJSON(content(res_v3, "text", encoding = "UTF-8"))
        if (is.data.frame(raw_v3) && nrow(raw_v3) > 0) {
            inc <- raw_v3[1, ]; rev <- as.numeric(inc$revenue)/1e6; oi <- as.numeric(inc$operatingIncome)/1e6
            rd <- as.numeric(inc$researchAndDevelopmentExpenses %||% 0)/1e6
            return(tibble(symbol=ticker, segment="Consolidated (Stable API)", revenue=rev, 
                          adj_margin=round((oi + (0.5 * rd)) / rev, 3), growth_est=0.10, source="API"))
        }
    }
    return(tibble(symbol=ticker, segment="Manual Input Mode", revenue=1000, adj_margin=0.15, growth_est=0.10, source="Manual"))
}

# --- 3. Key Metrics ---
get_management_score <- function(ticker, fmp_key) {
    url <- "https://financialmodelingprep.com/stable/key-metrics"
    res <- GET(url, query = list(symbol = ticker, limit = 1, apikey = trimws(fmp_key)))
    roic_val <- 0.12
    if(status_code(res) == 200) {
        d <- fromJSON(content(res, "text", encoding = "UTF-8"))
        if (length(d) > 0) {
            df_m <- if(is.data.frame(d)) d[1, ] else d[[1]]
            roic_val <- as.numeric(df_m$roic %||% df_m$returnOnInvestedCapital %||% 0.12)
        }
    }
    list(roic = roic_val, score = if_else(roic_val > 0.15, 2, 1))
}

# Tiingo & Alpha Vantage remain the same (Hard-coded fallbacks for shares)
get_tiingo_price <- function(ticker, tiingo_key) {
    url <- paste0("https://api.tiingo.com/tiingo/daily/", ticker, "/prices?token=", trimws(tiingo_key))
    res <- GET(url)
    if (status_code(res) != 200) return(150)
    data <- fromJSON(content(res, "text", encoding = "UTF-8"))
    return(as.numeric(data$adjClose[1]) %||% 150)
}

get_av_shares <- function(ticker, av_key) {
    url <- paste0("https://www.alphavantage.co/query?function=OVERVIEW&symbol=", ticker, "&apikey=", trimws(av_key))
    res <- GET(url)
    fallbacks <- list("NVDA" = 24.6, "AMZN" = 10.4, "MSFT" = 7.4, "AAPL" = 15.4, "GOOGL" = 13.9)
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