library(shiny)
library(tidyverse)
library(bslib)
library(bsicons)

source("logic.R")

ui <- page_navbar(
    title = "Value 3.0 Analyst Terminal",
    theme = bs_theme(version = 5, bootswatch = "zephyr", primary = "#2C3E50"),
    
    nav_panel("Deep Dive Analysis",
              layout_sidebar(
                  sidebar = sidebar(
                      title = "Equity Controls",
                      textInput("ticker", "Ticker", value = "AMZN"),
                      actionButton("run_analysis", "Execute Stable Analysis", icon = icon("rocket"), class = "btn-primary w-100"),
                      hr(),
                      uiOutput("sec_link_ui"),
                      hr(),
                      h6("Business Quality (B) Scorecard"),
                      uiOutput("b_hints"),
                      checkboxInput("b1", "Low Market Share?", TRUE),
                      checkboxInput("b2", "Large/Growing Market?", TRUE),
                      checkboxInput("b3", "Sustainable Moat?", TRUE),
                      hr(),
                      uiOutput("mgmt_info")
                  ),
                  
                  layout_column_wrap(
                      width = 1/2,
                      card(card_header("Earnings Power Adjustments"), uiOutput("segment_sliders")),
                      layout_column_wrap(
                          width = 1,
                          value_box(title = "Final BMP Decision", value = textOutput("final_status"), showcase = bsicons::bs_icon("clipboard-check"), theme = "dark"),
                          card(
                              card_header("Price Veto Analysis (The 'P' in BMP)"),
                              card_body(
                                  layout_column_wrap(width = 1/2,
                                                     div(h6("Earnings Yield (EP)"), h3(textOutput("yield_pct"), class="text-primary")),
                                                     div(h6("Veto Status"), uiOutput("veto_badge"))
                                  ),
                                  hr(),
                                  p(strong("Value 3.0 Entry Math:"), "To achieve a 5% yield, do not pay more than:"),
                                  h3(textOutput("target_price"), class="text-success")
                              )
                          ),
                          layout_column_wrap(width = 1/2,
                                             value_box(title = "Current Price", value = textOutput("current_p"), showcase = bsicons::bs_icon("currency-dollar"), theme = "info"),
                                             value_box(title = "Quality (BM) Score", value = textOutput("bm_total"), showcase = bsicons::bs_icon("shield-shaded"), theme = "secondary")
                          )
                      )
                  )
              )
    ),
    
    nav_panel("Methodology & How-To",
              layout_column_wrap(
                  width = 1,
                  card(
                      card_header("The Value 3.0 Scoring System"),
                      card_body(
                          h5("Business Quality (B) - Data-Driven Suggestions"),
                          p("The app uses these proxies based on Seessel's methodology to assist your qualitative judgment:"),
                          tags$table(class = "table table-sm table-bordered",
                                     tags$thead(tags$tr(tags$th("Question"), tags$th("Data Proxy"), tags$th("Rationale"))),
                                     tags$tbody(
                                         tags$tr(tags$td("Low Market Share?"), tags$td("Revenue < $50B"), tags$td("Seessel looks for single-digit shares of massive markets (Page 91).")),
                                         tags$tr(tags$td("Growing Market?"), tags$td("Growth CAGR > 10%"), tags$td("Indicates the company is in 'Escape Velocity' (Page 131).")),
                                         tags$tr(tags$td("Sustainable Moat?"), tags$td("Harvest Margin > 25%"), tags$td("Signature of a high-margin 'Toll Bridge' business (Page 80)."))
                                     )
                          ),
                          h5("Management (M)"),
                          p("Determined by ROIC. If the auto-pulled ROIC is > 15%, the app pre-checks 'Understands Value' as management is effectively creating wealth (Page 109)."),
                          h5("Price (P)"),
                          p("Calculated using Earnings Power. GAAP is adjusted by adding back R&D spend. If the resulting yield is < 5%, the stock is VETOED (Page 79).")
                      )
                  ),
                  card(
                      card_header("How to Use This Terminal"),
                      card_body(
                          tags$ol(
                              tags$li("Enter ticker and click Execute Analysis."),
                              tags$li("Use the 'Find Segment Data' button to open the SEC 10-K and verify Revenue/Margins."),
                              tags$li("Adjust Sliders to reflect 'Harvest Mode' profitability."),
                              tags$li("Check the Price Veto analysis to find your Target Entry Price.")
                          )
                      )
                  )
              )
    ),
    nav_spacer(),
    nav_item(tags$a(bsicons::bs_icon("book"), " Seessel Text", href="https://www.google.com/search?q=Adam+Seessel+Where+the+Money+Is", target="_blank"))
)

server <- function(input, output, session) {
    # Safe initialization
    data_store <- reactiveValues(segments = NULL, price = 100, shares = 1, mgmt = list(roic = 0.12, score = 1))
    
    observeEvent(input$run_analysis, {
        ticker <- toupper(input$ticker)
        showNotification(paste("Accessing FMP Stable for", ticker), type="message")
        data_store$segments <- get_fmp_segments(ticker)
        data_store$mgmt <- get_management_score(ticker)
        data_store$price <- get_tiingo_price(ticker)
        data_store$shares <- get_av_shares(ticker)
    })
    
    output$b_hints <- renderUI({
        req(data_store$segments)
        hints <- get_business_suggestions(data_store$segments)
        span(bsicons::bs_icon("lightbulb"), hints$summary, style = "font-size: 0.75rem; color: #d35400; font-style: italic;")
    })
    
    output$mgmt_info <- renderUI({
        req(data_store$mgmt)
        # CRITICAL FIX: Ensure the score is treated as a safe scalar
        current_score <- as.numeric(data_store$mgmt$score %||% 1)
        
        tagList(
            h6("Management Quality (M)"),
            p(paste("Auto-ROIC:", percent(data_store$mgmt$roic, accuracy = 0.1)), 
              class = ifelse(data_store$mgmt$roic > 0.15, "text-success", "text-warning")),
            checkboxGroupInput("m_checks", "Manual Overrides", 
                               choices = c("Think/Act like Owners" = "m1", "Understands Value" = "m2"),
                               selected = if(isTRUE(current_score == 2)) c("m1", "m2") else "m1")
        )
    })
    
    output$segment_sliders <- renderUI({
        req(data_store$segments)
        df <- data_store$segments
        lapply(1:nrow(df), function(i) {
            div(class = "p-3 mb-2 border rounded bg-light", 
                strong(df$segment[i], class="text-primary"),
                numericInput(paste0("rev_", i), "Base Revenue (Millions $)", value = round(df$revenue[i], 0)),
                sliderInput(paste0("margin_", i), "Adj. Harvest Margin", 0, 1, df$adj_margin[i], step = 0.01),
                sliderInput(paste0("growth_", i), "3yr Growth CAGR", 0, 0.6, df$growth_est[i], step = 0.01))
        })
    })
    
    calc <- reactive({
        req(input$margin_1, data_store$segments)
        df <- data_store$segments
        for(i in 1:nrow(df)) { 
            df$revenue[i] <- input[[paste0("rev_", i)]] %||% df$revenue[i]
            df$adj_margin[i] <- input[[paste0("margin_", i)]] %||% df$adj_margin[i]
            df$growth_est[i] <- input[[paste0("growth_", i)]] %||% df$growth_est[i]
        }
        calculate_bmp_score(input$ticker, df, sum(input$b1, input$b2, input$b3), 
                            length(input$m_checks), data_store$price, data_store$shares)
    })
    
    output$veto_badge <- renderUI({
        req(calc())
        if(calc()$p_pass) span(bsicons::bs_icon("check-circle-fill"), " PRICE PASSES", class="badge bg-success") else span(bsicons::bs_icon("x-circle-fill"), " PRICE VETOED", class="badge bg-danger")
    })
    
    output$sec_link_ui <- renderUI({
        tags$a(href = paste0("https://www.sec.gov/cgi-bin/browse-edgar?ticker=", input$ticker, "&action=getcompany&type=10-K"), 
               target = "_blank", class = "btn btn-outline-secondary btn-sm w-100", bsicons::bs_icon("file-earmark-text"), " Find Segment Data (10-K)")
    })
    
    output$final_status <- renderText({ calc()$status })
    output$yield_pct <- renderText({ percent(calc()$yield, accuracy = 0.1) })
    output$current_p <- renderText({ dollar(calc()$price) })
    output$target_price <- renderText({ dollar(calc()$max_buy) })
    output$bm_total <- renderText({ paste0(calc()$total_bm, " / 5") })
}

shinyApp(ui, server)