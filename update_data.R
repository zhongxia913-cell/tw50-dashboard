# ============================================================================
# 每日更新（v3）：逐檔增量抓取、新成分股自動補齊歷史、自動剔除已刪除成分股
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

if (file.exists(rds_file)) {
  old <- readRDS(rds_file)
} else {
  old <- NULL
}

new_list <- list()
for (tk in tickers) {
  code <- substr(tk, 1, 4)
  from_d <- if (!is.null(old) && code %in% old$symbol) {
    max(old$date[old$symbol == code]) + 1
  } else {
    as.Date("2016-01-01")   # 新成分股：回溯補齊完整歷史
  }
  if (from_d > Sys.Date()) next
  tmp <- NULL
  for (i in 1:3) {
    tmp <- tryCatch(
      tq_get(tk, get = "stock.prices", from = from_d, to = Sys.Date()) %>%
        mutate(symbol = substr(symbol, 1, 4)),
      error = function(e) { cat("抓取", tk, "第", i, "次失敗：", e$message, "\n"); NULL }
    )
    if (!is.null(tmp) && nrow(tmp) > 0) break
    Sys.sleep(15)
  }
  if (!is.null(tmp) && nrow(tmp) > 0) {
    new_list[[code]] <- tmp
    cat(Sys.time(), "：", tk, "新增", nrow(tmp), "筆\n")
  }
  Sys.sleep(1)
}

if (length(new_list) == 0) {
  cat(Sys.time(), "：無新交易日資料（週末/國定假日），或抓取全數失敗，結束。\n")
  quit(save = "no")
}

new <- bind_rows(new_list) %>%
  filter(!(is.na(open) & is.na(high) & is.na(low) & is.na(close))) %>%
  filter(!wday(date) %in% c(1, 7)) %>%
  filter(open > 0, high > 0, low > 0, close > 0) %>%
  select(symbol, date, open, high, low, close, volume, adjusted)

updated <- bind_rows(old, new) %>%
  filter(symbol %in% substr(tickers, 1, 4)) %>%   # 剔除已刪除的成分股
  distinct(symbol, date, .keep_all = TRUE) %>%
  arrange(symbol, date)

saveRDS(updated, rds_file)
cat(Sys.time(), "：共新增", nrow(new), "筆，目前最新日期 =",
    as.character(max(updated$date)), "\n")
