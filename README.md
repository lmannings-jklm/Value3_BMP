
# Value 3.0 Analyst Terminal 📈

An automated equity analysis platform built in **R Shiny**, based on the "Value 3.0" methodology established by Adam Seessel in his book, *Where the Money Is: Value Investing in the Digital Age*.

## 📖 Methodology Overview
This terminal moves beyond traditional value investing metrics (Value 1.0/2.0) to evaluate modern digital enterprises. It follows the **BMP (Business, Management, Price)** framework:

*   **Business (B):** Identifies "Toll Bridge" businesses with high switching costs and massive market opportunities.
*   **Management (M):** Evaluates capital allocation efficiency via Return on Invested Capital (ROIC) and owner-oriented mindsets.
*   **Price (P):** Applies the "Lincoln Cabinet" Veto. We calculate **Earnings Power** by adding back R&D/Marketing spend and rolling revenue 3 years forward. If the resulting yield is below **5%**, the investment is vetoed regardless of quality.

## 🚀 Key Features
- **Segmented Earnings Power:** Unpacks complex corporate structures (e.g., AWS vs. Retail) to find hidden profitability.
- **Multi-API Failover:** Integrated with **Financial Modeling Prep (FMP)**, **Alpha Vantage**, and **Tiingo** for high-resolution fundamental and market data.
- **Analyst Intelligence Layer:** Automatically suggests qualitative scores based on financial proxies (Revenue CAGR, Gross Margins, and ROIC).
- **Manual Override Mode:** High-precision research capability allowing users to input data directly from SEC 10-K filings when API limits are reached.
- **Interactive Veto Math:** Real-time calculation of "Maximum Buy Price" to achieve a 5% entry yield.

## 🛠️ Technical Setup

### Prerequisites
You will need an R environment and three API keys:
1.  **Financial Modeling Prep (FMP):** Used for fundamentals and ROIC.
2.  **Alpha Vantage:** Primary source for share counts and fundamental failover.
3.  **Tiingo:** Used for accurate, adjusted stock prices.

### Installation
1.  **Clone the Repository:**
    ```bash
    git clone https://github.com/YOUR_USERNAME/Value3_Analyst_Terminal.git
    ```
2.  **Install Dependencies:**
    Run the following in your R console:
    ```r
    install.packages(c("shiny", "tidyverse", "bslib", "bsicons", "jsonlite", "httr", "scales", "tidyquant"))
    ```
3.  **Configure API Keys:**
    Create a `.Renviron` file in your root directory (`usethis::edit_r_environ()`) and add your keys:
    ```text
    FMP_API_KEY=your_key_here
    TIINGO_API_KEY=your_key_here
    ALPHAVANTAGE_API_KEY=your_key_here
    ```
    *Restart R for changes to take effect.*

## 📊 Data-Driven "B" Score Logic
The terminal assists your qualitative judgment with the following data proxies:

| Question | Data Proxy | Rationale |
| :--- | :--- | :--- |
| **Low Market Share?** | Revenue < $50B | Targets single-digit players in massive TAMs. |
| **Growing Market?** | 3yr CAGR > 10% | Confirms the business is in "Escape Velocity." |
| **Sustainable Moat?** | Harvest Margin > 25% | Signature of a high-margin digital "Toll Bridge." |

## 🌐 Deployment
This application is optimized for deployment to **shinyapps.io**. 
> **Note:** When deploying, remember to set your API keys in the shinyapps.io dashboard under **Settings > Advanced > Environment Variables** to ensure connectivity in the cloud.

## ⚠️ Disclaimer
This terminal is a tool for financial analysis and is intended for educational and research purposes only. It does not constitute financial advice. Always verify data via official SEC filings (provided via the in-app link) before making investment decisions.

---
**Author:** [Your Name/GitHub Handle]  
**Methodology:** Adam Seessel, *Where the Money Is* (2022)
