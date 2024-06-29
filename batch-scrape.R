library(tidyverse)
library(rvest)
library(lubridate)
library(cli)

start_date <- ymd("2024-01-08")
end_date <- today() - 1

dates <- seq(start_date, end_date, by = "1 day")
dofw <- wday(dates)
dates <- dates[dofw %in% 2:6] # remove Saturday/Sunday
hac_date_strings <- tolower(paste(
  wday(dates, label = TRUE, abbr = FALSE),
  month(dates, label = TRUE, abbr = FALSE),
  day(dates),
  year(dates),
  sep = "-"
))

baseurl <- "https://hoosierathleticclub.com/"
grab_wods <- function(hds) {
  wods <- list()
  cli_progress_bar("Downloading HCF wods", total = length(hds))
  for (d in seq_along(hds)) {
    cli_progress_update()
    page <- tryCatch(
      read_html(paste0(baseurl, hds[d])) |>
        html_elements("div.entry-content") |>
        html_text2() |>
        str_trim(),
      error = function(e) NA,
      finally = NA
    )
    wods[[d]] <- tibble(page = page)
    Sys.sleep(1)
  }
  cli_process_done()
  wods
}

wods_2024 <- grab_wods(hac_date_strings)
names(wods_2024) <- dates
wods_2024 <- wods_2024[!is.na(wods_2024)]
wods <- wods_2024 |> list_rbind(names_to = "Date")
write_rds(wods, "2024-hcf-wods.rds")


