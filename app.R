# ============================================================================
# 0050 投資組合監控儀表板 (終極優化版：修復0%Bug、新增個股查詢、標示截止日)
# ============================================================================
library(shiny); library(shinydashboard); library(dplyr); library(tidyr)
library(plotly); library(DT); library(lubridate)

stock_names <- c("0050"="元大50","2330"="台積電","2454"="聯發科","2308"="台達電",
                 "2317"="鴻海","3711"="日月光投控","2303"="聯電","2383"="台光電",
                 "3037"="欣興","2881"="富邦金","1303"="南亞","2891"="中信金",
                 "3017"="奇鋐","2882"="國泰金","2887"="台新新光金","2345"="智邦",
                 "2327"="國巨","2382"="廣達","2360"="致茂","2885"="元大金",
                 "6669"="緯穎","2884"="玉山金","2059"="川湖","2357"="華碩",
                 "3008"="大立光","2408"="南亞科","2883"="凱基金","2886"="兆豐金",
                 "2301"="光寶科","2890"="永豐金","2344"="華邦電","3231"="緯創",
                 "2412"="中華電","3443"="創意","3653"="健策","2892"="第一金",
                 "2880"="華南金","1216"="統一","4958"="臻鼎-KY","7769"="鴻勁",
                 "2368"="金像電","3665"="貿聯-KY","6446"="藥華藥","2449"="京元電子",
                 "2395"="研華","5880"="合庫金","2603"="長榮","8046"="南電",
                 "4904"="遠傳","3045"="台灣大","6505"="台塑化")

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "0050 投資組合監控儀表板"),
  dashboardSidebar(sidebarMenu(
    menuItem("市場總覽", tabName = "ov",  icon = icon("chart-line")),
    menuItem("個股監控", tabName = "stk", icon = icon("table")),
    menuItem("個股走勢查詢", tabName = "pick_stk", icon = icon("search")), # [P2] 新增
    menuItem("資料明細", tabName = "raw", icon = icon("download"))
  )),
  dashboardBody(
    # [P2] 明確標示資料截止日
    fluidRow(
      column(12, h4(textOutput("as_of", inline = TRUE), style = "text-align: right; color: gray; margin-bottom: 15px;"))
    ),
    tabItems(
      tabItem("ov",
        fluidRow(
          valueBoxOutput("vb_price", 3), valueBoxOutput("vb_chg", 3),
          valueBoxOutput("vb_up", 3),    valueBoxOutput("vb_ytd", 3)
        ),
        fluidRow(box(width = 12, status = "primary", solidHeader = TRUE,
                     title = "0050 調整後收盤價走勢",
                     plotlyOutput("p_price", height = 320))),
        # [P2] 標題改為近一年
        fluidRow(box(width = 6, title = "0050 近一年日報酬率", status = "primary",
                     plotlyOutput("p_ret", height = 280)),
                 box(width = 6, title = "近 20 日漲跌幅排行（前/後 10 名）",
                     status = "primary", plotlyOutput("p_rank", height = 280)))
      ),
      tabItem("stk",
        fluidRow(box(width = 12, title = "51 檔最新行情（依持股比例排序）",
                     status = "primary", DTOutput("tbl")))
      ),
      # [P2] 個股走勢查詢頁面
      tabItem("pick_stk",
        fluidRow(
          box(width = 4, status = "primary", title = "選擇個股",
              selectInput("pick", NULL, choices = setNames(names(stock_names), paste(names(stock_names), stock_names)), selected = "0050"))
        ),
        fluidRow(
          box(width = 12, status = "primary", title = "調整後收盤價走勢", plotlyOutput("p_pick_price", height = 300)),
          box(width = 12, status = "primary", title = "近一年日報酬率", plotlyOutput("p_pick_ret", height = 300))
        )
      ),
      tabItem("raw",
        fluidRow(box(width = 12, title = "原始日資料",
                     status = "primary",
                     downloadButton("dl", "下載 CSV"), br(), br(),
                     DTOutput("raw_tbl")))
      )
    )
  )
)

server <- function(input, output, session) {
  
  raw <- reactiveFileReader(3600000, NULL, "stock_daily.rds", readRDS)
  
  df <- reactive({
    req(raw())
    raw() %>% mutate(name = stock_names[symbol])
  })
  
  # [P2] 渲染資料截止日
  output$as_of <- renderText({
    req(nrow(df()) > 0)
    paste("資料截止：", max(df()$date), "｜每交易日 19:00 自動更新")
  })

  # === [P0 修正] latest() 變數覆寫問題 ===
  latest <- reactive({
    req(nrow(df()) > 0)
    df() %>% arrange(symbol, date) %>% group_by(symbol, name) %>%
      summarise(
        last_date  = max(date),
        close_last = last(close),
        close_prev = nth(close, n() - 1),
        adj_last   = last(adjusted),
        adj_prev   = nth(adjusted, n() - 1),
        adj_20     = nth(adjusted, max(n() - 20, 1)),
        volume     = last(volume),
        .groups   = "drop"
      ) %>%
      mutate(
        close    = close_last,
        adjusted = adj_last,
        chg_pct  = (close_last / close_prev - 1) * 100,
        ret      = log(adj_last / adj_prev),
        ret20    = (adj_last / adj_20 - 1) * 100,
        label    = paste(symbol, name)
      )
  })
  
  etf <- reactive({
    req(nrow(df()) > 0)
    df() %>% filter(symbol == "0050") %>% arrange(date)
  })
  
  l51 <- reactive({
    req(nrow(latest()) > 0)
    latest() %>% filter(symbol == "0050")
  })
  
  # === KPI 卡片 ([P0] 防呆改為顯示 — ) ===
  output$vb_price <- renderValueBox({
    req(nrow(l51()) > 0)
    price <- l51()$close
    if (length(price) == 0 || is.na(price)) price <- 0
    dt <- l51()$last_date
    if (length(dt) == 0 || is.na(dt)) dt <- "-"
    
    valueBox(
      paste0(round(price, 2), " 元"), paste("0050 收盤價（", dt, "）"),
      icon = icon("dollar-sign"), color = "blue"
    )
  })
  
  output$vb_chg <- renderValueBox({
    req(nrow(l51()) > 0)
    chg <- l51()$chg_pct
    if (length(chg) == 0 || is.na(chg)) {
      return(valueBox("—", "0050 日漲跌幅（資料異常）", icon = icon("exclamation-triangle"), color = "yellow"))
    }
    color_str <- ifelse(chg >= 0, "red", "green")
    icon_str <- ifelse(chg >= 0, "arrow-up", "arrow-down")
    
    valueBox(
      paste0(round(chg, 2), " %"), "0050 日漲跌幅",
      icon = icon(icon_str), color = color_str
    )
  })
  
  output$vb_up <- renderValueBox({
    req(nrow(latest()) > 0)
    up_count <- sum(latest()$chg_pct > 0, na.rm = TRUE)
    down_count <- sum(latest()$chg_pct < 0, na.rm = TRUE)
    valueBox(
      paste0(up_count, " / ", down_count),
      "上漲家數 / 下跌家數", icon = icon("exchange-alt"), color = "purple"
    )
  })
  
  output$vb_ytd <- renderValueBox({
    req(nrow(etf()) > 0)
    e <- etf() %>% filter(year(date) == year(max(df()$date)))
    if (nrow(e) == 0) {
      ytd <- 0
    } else {
      ytd <- (last(e$adjusted) / first(e$adjusted) - 1) * 100
      if (is.na(ytd)) ytd <- 0
    }
    valueBox(paste0(round(ytd, 2), " %"), "0050 今年以來報酬",
             icon = icon("chart-line"),
             color = ifelse(ytd >= 0, "red", "green"))
  })
  
  # === 圖表 ===
  output$p_price <- renderPlotly({
    req(nrow(etf()) > 0)
    plot_ly(etf(), x = ~date, y = ~adjusted, type = "scatter", mode = "lines",
            line = list(color = "#1f77b4")) %>%
      layout(yaxis = list(title = "調整後收盤價"), xaxis = list(title = ""))
  })
  
  # [P2] 限制近一年，加快載入速度
  output$p_ret <- renderPlotly({
    req(nrow(etf()) > 0)
    e <- etf() %>% mutate(ret = c(NA, diff(log(adjusted)))) %>%
      filter(date >= max(date) - 365)
    plot_ly(e, x = ~date, y = ~ret, type = "bar",
            marker = list(color = ifelse(e$ret >= 0, "red", "green"))) %>%
      layout(yaxis = list(title = "日對數報酬率"), xaxis = list(title = ""))
  })
  
  output$p_rank <- renderPlotly({
    req(nrow(latest()) > 0)
    rk <- latest() %>% filter(symbol != "0050", !is.na(ret20)) %>%
      arrange(ret20) %>% slice(c(1:10, (n()-9):n())) %>%
      mutate(label = factor(label, levels = label))
    req(nrow(rk) > 0)
    plot_ly(rk, x = ~ret20, y = ~label, type = "bar", orientation = "h",
            marker = list(color = ifelse(rk$ret20 >= 0, "red", "green"))) %>%
      layout(xaxis = list(title = "近 20 日報酬率 (%)"), yaxis = list(title = ""))
  })

  # === [P2] 個股走勢查詢 ===
  pick_etf <- reactive({
    req(nrow(df()) > 0, input$pick)
    df() %>% filter(symbol == input$pick) %>% arrange(date)
  })
  
  output$p_pick_price <- renderPlotly({
    req(nrow(pick_etf()) > 0)
    plot_ly(pick_etf(), x = ~date, y = ~adjusted, type = "scatter", mode = "lines",
            line = list(color = "#1f77b4")) %>%
      layout(yaxis = list(title = "調整後收盤價"), xaxis = list(title = ""))
  })
  
  output$p_pick_ret <- renderPlotly({
    req(nrow(pick_etf()) > 0)
    e <- pick_etf() %>% mutate(ret = c(NA, diff(log(adjusted)))) %>%
      filter(date >= max(date) - 365)
    req(nrow(e) > 0)
    plot_ly(e, x = ~date, y = ~ret, type = "bar",
            marker = list(color = ifelse(e$ret >= 0, "red", "green"))) %>%
      layout(yaxis = list(title = "日對數報酬率"), xaxis = list(title = ""))
  })

  # === 資料表格 ===
  output$tbl <- renderDT({
    req(nrow(latest()) > 0)
    df_tbl <- latest() %>%
      mutate(symbol = factor(symbol, levels = names(stock_names))) %>%
      arrange(symbol) %>%
      select(代號 = symbol, 名稱 = name, 日期 = last_date, 收盤價 = close,
             漲跌幅_pct = chg_pct, 成交量 = volume, 近20日_pct = ret20) %>%
      mutate(across(c(收盤價, 漲跌幅_pct, 近20日_pct), ~ round(., 2)))
    
    datatable(df_tbl, options = list(pageLength = 51, dom = "ftp")) %>%
      formatStyle("漲跌幅_pct",
                  color = styleInterval(0, c("green", "red")))
  })
  
  output$raw_tbl <- renderDT({
    req(nrow(df()) > 0)
    df() %>% arrange(desc(date))
  }, options = list(pageLength = 20, scrollX = TRUE))
  
  output$dl <- downloadHandler(
    filename = function() { "stock_daily.csv" },
    content = function(f) { write.csv(df(), f, row.names = FALSE) }
  )
}

shinyApp(ui, server)
