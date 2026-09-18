# ============================================================================
# 每日更新：只抓「最後日期+1 ~ 今天」的新資料，附加到 stock_daily.rds (含重試機制)
# ============================================================================
library(tidyquant); library(dplyr); library(tidyr)

tickers <- paste0(c("0050","2330","2454","2308","2317","3711","2303","2383",
                    "3037","2881","1303","2891","3017","2882","2887","2345",
                    "2327","2382","2360","2885","6669","2884","2059","2357",
                    "3008","2408","2883","2886","2301","2890","2344","3231",
                    "2412","3443","3653","2892","2880","1216","4958","7769",
                    "2368","3665","3661","2449","2395","5880","2603","8046",
                    "4904","3045","6505"), ".TW")

rds_file <- "stock_daily.rds"

if (file.exists(rds_file)) {
  old <- readRDS(rds_file)
  last_date <- max(old$date)
} else {
  old <- NULL
  last_date <- as.Date("2016-01-01")
}

# [P1 修正] 加入重試機制，避免 Yahoo 暫時斷線導致當日無資料
new <- NULL
for (i in 1:3) {
  cat("嘗試第", i, "次抓取資料...\n")
  new <- tryCatch(
    tq_get(tickers, get = "stock.prices",
           from = last_date + 1, to = Sys.Date()) %>%
      mutate(symbol = substr(symbol, 1, 4)),
    error = function(e) { cat("第", i, "次抓取失敗：", e$message, "\n"); NULL }
  )
  if (!is.null(new) && nrow(new) > 0) break
  if (i < 3) Sys.sleep(30)
}

if (is.null(new) || nrow(new) == 0) {
  cat(Sys.time(), "：無新交易日資料（週末/國定假日/颱風假），或抓取失敗，結束。\n")
  quit(save = "no")
}

new <- new %>%
  filter(!(is.na(open) & is.na(high) & is.na(low) & is.na(close))) %>%
  filter(!lubridate::wday(date) %in% c(1, 7)) %>%
  filter(open > 0, high > 0, low > 0, close > 0) %>%
  select(symbol, date, open, high, low, close, volume, adjusted)

updated <- bind_rows(old, new) %>%
  distinct(symbol, date, .keep_all = TRUE) %>%
  arrange(symbol, date)

saveRDS(updated, rds_file)
cat(Sys.time(), "：新增", nrow(new), "筆，目前最新日期 =",
    as.character(max(updated$date)), "\n")
