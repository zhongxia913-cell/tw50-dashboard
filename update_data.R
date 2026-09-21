# ============================================================================
# 每日更新（v4）：批次抓取 + 新成分股補齊歷史 + 拉長重試間隔
# ============================================================================
library(tidyquant); library(dplyr); library(tidyr); library(lubridate)

tickers <- paste0(c("0050","2330","2454","2308","2317","3711","2303","2383",
                    "3037","2881","1303","2891","3017","2882","2887","2345",
                    "2327","2382","2360","2885","6669","2884","2059","2357",
                    "3008","2408","2883","2886","2301","2890","2344","3231",
                    "2412","3443","3653","2892","2880","1216","4958","7769",
                    "2368","3665","6446","2449","2395","5880","2603","8046",
                    "4904","3045","6505"), ".TW")

rds_file <- "stock_daily.rds"

# --- 帶重試的批次抓取：失敗時依序等待 30／60／90 秒 ---
fetch_with_retry <- function(syms, from_d, tries = 3) {
  for (i in seq_len(tries)) {
    res <- tryCatch(
      tq_get(syms, get = "stock.prices", from = from_d, to = Sys.Date()),
      error = function(e) { cat("抓取錯誤：", e$message, "\n"); NULL }
    )
    if (is.data.frame(res) && nrow(res) > 0) {
      return(res %>% mutate(symbol = substr(symbol, 1, 4)))
    }
    cat("第", i, "次未取得有效資料，等待", 30 * i, "秒後重試...\n")
    Sys.sleep(30 * i)
  }
  return(NULL)
}

old <- if (file.exists(rds_file)) readRDS(rds_file) else NULL
cur <- substr(tickers, 1, 4)

if (is.null(old)) {
  new <- fetch_with_retry(tickers, as.Date("2016-01-01"))
} else {
  # (1) 無歷史的新成分股（如 6446）：先補齊完整歷史
  missing <- cur[!cur %in% unique(old$symbol)]
  backfill <- NULL
  if (length(missing) > 0) {
    cat("補齊新成分股歷史：", paste(missing, collapse = ", "), "\n")
    backfill <- fetch_with_retry(paste0(missing, ".TW"), as.Date("2016-01-01"))
    Sys.sleep(30)
  }
  # (2) 每日增量：自「全部成分股中最早的缺口日」起抓（可自動修補落後個股，如 0050）
  from_d <- old %>%
    filter(symbol %in% cur) %>%
    group_by(symbol) %>%
    summarise(d = max(date), .groups = "drop") %>%
    pull(d) %>% min() + 1
  incr <- NULL
  if (from_d <= Sys.Date()) {
    incr <- fetch_with_retry(tickers, from_d)
  }
  new <- bind_rows(backfill, incr)
}

if (is.null(new) || nrow(new) == 0) {
  cat(Sys.time(), "：無新交易日資料（週末/假日），或 Yahoo 仍拒絕連線，結束。\n")
  quit(save = "no")
}

new <- new %>%
  filter(!(is.na(open) & is.na(high) & is.na(low) & is.na(close))) %>%
  filter(!wday(date) %in% c(1, 7)) %>%
  filter(open > 0, high > 0, low > 0, close > 0) %>%
  select(symbol, date, open, high, low, close, volume, adjusted)

updated <- bind_rows(old, new) %>%
  filter(symbol %in% cur) %>%            # 剔除已刪除的成分股（如 3661）
  distinct(symbol, date, .keep_all = TRUE) %>%
  arrange(symbol, date)

saveRDS(updated, rds_file)
cat(Sys.time(), "：共新增", nrow(new), "筆，目前最新日期 =",
    as.character(max(updated$date)), "\n")
