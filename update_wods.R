library(lubridate)
library(dplyr)
library(rvest)
library(tibble)
library(purrr)
library(stringr)


all_wods <- readRDS("hcf-wods.rds")
last_date <- tail(all_wods$Date, 1L)
dates_to_grab <- seq(from = last_date + 1, to = today(), by = 1L)
dofw <- wday(dates_to_grab)
dates <- dates_to_grab[dofw %in% 2:6] # remove Saturday/Sunday

if (length(dates) == 0L) {
  print("No new wods to download.")
} else {
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
    # cli_progress_bar("Downloading HCF wods", total = length(hds))
    for (d in seq_along(hds)) {
      # cli_progress_update()
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
    # cli_process_done()
    wods
  }

  new_wods <- grab_wods(hac_date_strings)
  names(new_wods) <- dates
  new_wods <- new_wods[!is.na(new_wods)]
  new_wods <- new_wods |> list_rbind(names_to = "Date") |>
    mutate(Date = ymd(Date))
  all_wods <- bind_rows(all_wods, new_wods) |>
    arrange(Date)
  saveRDS(all_wods, "hcf-wods.rds")
}
