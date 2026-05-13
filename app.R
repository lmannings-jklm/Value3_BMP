# --- PRODUCTION KEYS (Updated with your verified working key) ---
KEYS <- list(
    fmp = "PERSONAL KEY REMOVED",
    tiingo = "PERSONAL KEY REMOVED",
    av = "PERSONAL KEY REMOVED"
)

library(shiny)
library(tidyverse)
library(bslib)
library(bsicons)
library(scales)
library(jsonlite)
library(httr)

source("logic.R", local = TRUE)

ui <- page_navbar(
    title = "Value 3.0 Analyst Terminal",
    theme = bs_theme(version = 5, bootswatch = "zephyr", primary = "#2C3E50"),
    nav_panel("Deep Dive Analysis",
              layout_sidebar(
                  sidebar = sidebar(
                      textInput("ticker", "Ticker", value = "AMZN"),
                      actionButton("run_analysis", "Execute Stable Analysis", icon = icon("rocket"), class = "btn-primary w-100"),
                      hr(),
                      uiOutput("api_status_check"),
                      uiOutput("sec_link_ui"),
                      hr(),
                      h6("Business Quality (B)"),
                      uiOutput("b_hints"),
                      checkboxInput("b1", "Low Market Share?", FALSE),
                      checkboxInput("b2", "Large/Growing Market?", FALSE),
                      checkboxInput("b3", "Sustainable Moat?", FALSE),
                      hr(),
                      uiOutput("mgmt_info")
                  ),
                  layout_column_wrap(
                      width = 1/2,
                      card(
                          card_header("Earnings Power Adjustments (Conglomerate Unpacking)"), 
                          card_body(uiOutput("segment_sliders"))
                      ),
                      layout_column_wrap(
                          width = 1,
                          value_box(title = "Final BMP Decision", value = textOutput("final_status"), showcase = bsicons::bs_icon("clipboard-check"), theme = "dark"),
                          card(
                              card_header("Price Veto Analysis"),
                              card_body(
                                  layout_column_wrap(width = 1/2,
                                                     div(h6("Earnings Yield (EP)"), h3(textOutput("yield_pct"), class="text-primary")),
                                                     div(h6("Veto Status"), uiOutput("veto_badge"))
                                  ),
                                  hr(),
                                  p(strong("Target Buy Price (5% Yield):"), h3(textOutput("target_price"), class="text-success"))
                              )
                          ),
                          layout_column_wrap(width = 1/2,
                                             value_box(title = "Current Price", value = textOutput("current_p"), showcase = bsicons::bs_icon("currency-dollar"), theme = "info"),
                                             value_box(title = "Quality Score", value = textOutput("bm_total"), showcase = bsicons::bs_icon("shield-shaded"), theme = "secondary")
                          )
                      )
                  )
              )
    ),
    nav_panel("Methodology & How-To", p("Refer to Seessel's 'Where the Money Is' (2022)"))
)

server <- function(input, output, session) {
    data_store <- reactiveValues(
        segments = tibble(symbol="AMZN", segment="Consolidated", revenue=1000, adj_margin=0.15, growth_est=0.10, source="Manual"), 
        price = 100, shares = 1, mgmt = list(roic = 0.12, score = 1)
    )
    
    observeEvent(input$run_analysis, {
        ticker <- toupper(input$ticker)
        showNotification(paste("Connecting to FMP Stable for", ticker), type="message")
        
        data_store$segments <- get_fmp_segments(ticker, KEYS$fmp)
        data_store$mgmt     <- get_management_score(ticker, KEYS$fmp)
        data_store$price    <- get_tiingo_price(ticker, KEYS$tiingo)
        data_store$shares   <- get_av_shares(ticker, KEYS$av)
        
        hints <- get_business_suggestions(ticker, data_store$segments)
        updateCheckboxInput(session, "b1", value = hints$b1)
        updateCheckboxInput(session, "b3", value = hints$b3)
    })
    
    # Manual Unpacking Logic
    observeEvent(input$add_segment, {
        new_row <- tibble(symbol = toupper(input$ticker), segment = paste("New Unit", nrow(data_store$segments) + 1),
                          revenue = 0, adj_margin = 0.20, growth_est = 0.10, source = "Manual")
        data_store$segments <- bind_rows(data_store$segments, new_row)
    })
    
    output$segment_sliders <- renderUI({
        req(data_store$segments)
        df <- data_store$segments
        tagList(
            actionButton("add_segment", "Manual Unpack Unit (Use 10-K)", icon = icon("plus"), class = "btn-outline-primary btn-sm mb-3"),
            lapply(1:nrow(df), function(i) {
                div(class = "p-3 mb-2 border rounded bg-light", 
                    textInput(paste0("name_", i), NULL, value = df$segment[i]),
                    layout_column_wrap(width = 1/3,
                                       numericInput(paste0("rev_", i), "Rev ($M)", value = round(df$revenue[i], 0)),
                                       sliderInput(paste0("margin_", i), "Margin", 0, 1, df$adj_margin[i], step = 0.01),
                                       sliderInput(paste0("growth_", i), "Growth", 0, 0.6, df$growth_est[i], step = 0.01))
                )
            })
        )
    })
    
    calc <- reactive({
        req(input$margin_1, data_store$segments)
        df <- data_store$segments
        for(i in 1:nrow(df)) { 
            df$segment[i] <- input[[paste0("name_", i)]] %||% df$segment[i]
            df$revenue[i] <- input[[paste0("rev_", i)]] %||% df$revenue[i]
            df$adj_margin[i] <- input[[paste0("margin_", i)]] %||% df$adj_margin[i]
            df$growth_est[i] <- input[[paste0("growth_", i)]] %||% df$growth_est[i]
        }
        calculate_bmp_score(input$ticker, df, sum(input$b1, input$b2, input$b3), length(input$m_checks), data_store$price, data_store$shares)
    })
    
    output$api_status_check <- renderUI({
        code <- .GlobalEnv$last_api_status %||% "N/A"
        if(code == 200) span(bsicons::bs_icon("check-circle-fill"), " Stable API Linked", class="text-success small")
        else span(bsicons::bs_icon("x-circle-fill"), paste(" API Error:", code), class="text-danger small")
    })
    
    output$final_status <- renderText({ calc()$status })
    output$yield_pct <- renderText({ scales::percent(calc()$yield, accuracy = 0.1) })
    output$current_p <- renderText({ scales::dollar(calc()$price) })
    output$target_price <- renderText({ scales::dollar(calc()$max_buy) })
    output$bm_total <- renderText({ paste0(calc()$total_bm, " / 5") })
    
    output$mgmt_info <- renderUI({
        req(data_store$mgmt)
        tagList(h6("Management Quality (M)"),
                p(paste("Auto-ROIC:", scales::percent(data_store$mgmt$roic, accuracy = 0.1)), class = ifelse(data_store$mgmt$roic > 0.15, "text-success", "text-warning")),
                checkboxGroupInput("m_checks", "Manual Overrides", choices = c("Think/Act like Owners" = "m1", "Understands Value" = "m2"),
                                   selected = if(isTRUE(data_store$mgmt$score == 2)) c("m1", "m2") else "m1"))
    })
    
    output$sec_link_ui <- renderUI({
        tags$a(href = paste0("https://www.sec.gov/cgi-bin/browse-edgar?ticker=", input$ticker, "&action=getcompany&type=10-K"), 
               target = "_blank", class = "btn btn-outline-secondary btn-sm w-100", bsicons::bs_icon("file-earmark-text"), " Find Segment Data (10-K)")
    })
}

shinyApp(ui, server)