library(tidyverse)
library(lubridate)
# devtools::install_github("PMassicotte/gtrendsR")
library(gtrendsR)


# https://www.axios.com/news-cycle-2020-google-trends-chart-8a27fc67-2dd0-45b6-ae23-2672c2771c50.html
# https://www.r-bloggers.com/2019/10/vignette-google-trends-with-the-gtrendsr-package/
# https://medium.com/@bewerunge.franz/google-trends-how-to-acquire-daily-data-for-broad-time-frames-b6c6dfe200e6


keywords <- c("Coronavirus", "Lockdown", "Pandemie",
              "Klopapier", "Maske", "Mund-Nasen-Schutz", "Desinfektionsmittel",
              "Schnelltest", "PCR-Test", "Querdenken", "Livestream", "Zoom",
              "Jogginghose", "Self Care", "Lieferdienst", "Brot backen", 
              "Urlaub", "Geisterspiele", "Amazon Prime", "Wein",
              "Kurzarbeit", "Home Office", 
              "Wuhan", "Christian Drosten", "Hendrik Streeck",
              "Impfstoff", "Curevac", "Biontech",
              "Bundestag", "Jens Spahn", "Armin Laschet", "Markus Söder", "Angela Merkel",
              "Friedrich Merz",
              "Thomas Kemmerich",
              "Donald Trump", "Joe Biden", "Kamala Harris", 
              "Black Lives Matters",
              "Beirut", 
              "Belarus",
              "Zoo Krefeld",
              "Bundesliga", "Bayern München", "Erling Haaland", "Robert Lewandowski",
              "Olympische Spiele", "EM 2020", 
              "Kobe Bryant", "Diego Maradona", "Eddie van Halen", "Albert Uderzo", "Christo", 
              "Sean Connery",
              "Fridays For Future", "Australien", 
              "Brexit", "Boris Johnson",
              "Wirecard",
              "Alexei Nawalny")

# get trends for 2020 (returns weekly data)
trends_year_raw <- map(keywords, gtrends, geo = "DE", time = "2015-01-01 2020-12-31", gprop = "web", onlyInterest = TRUE, tz = -120)
write_rds(trends_year_raw, file.path("data", "trends_year_raw.rds"))

trends_year <- flatten(trends_year_raw) %>% set_names(keywords)
# limit to 2020
trends_year_2020 <- map(trends_year, ~filter(.x, date >= as_date("2020-01-01")))
write_rds(trends_year_2020, file.path("data", "trends_year.rds"))

# create monthly intervals
start_date <- as_date("2020-01-01")
max_elems <- 12
range <- vector("list", max_elems)
for (month in seq(max_elems)) {
  # set end date (last day of month)
  end_date <- start_date + period("1 month") - 1
  
  # API will return an error if end date is in the future
  if (end_date >= today()) {
    end_date <- today()
    range[[month]] <- paste(start_date, end_date)
    break
  }
  range[[month]] <- paste(start_date, end_date)
  
  # set new start date
  start_date <- end_date + period("1 day")
}
range <- as.character(range)
range


# retrieve monthly data for all keywords
gtrends_sleep <- function(keyword, time, sleep = 1, geo = "DE", gprop = "web", onlyInterest = TRUE, tz = -120) {
  Sys.sleep(sleep)
  trends <- gtrends(keyword, geo = geo, time = time, gprop = gprop, onlyInterest = onlyInterest, tz = tz)
  # ensure "hits" is a character type
  trends$hits <- as.character(trends$hits) # NOT TESTED
  return(trends)
}

gtrends_sleep_safely <- safely(gtrends_sleep)

trends_daily_month_raw <- map(
  range[2],
  function(x) {
    trends <- map(keywords, 
                  gtrends_sleep, geo = "DE", time = x, gprop = "web", onlyInterest = TRUE)
    trends <- set_names(trends, keywords)
    # Show progress
    cat("|")
    # Wait a sec or 2...
    Sys.sleep(5)
    return(trends)
  }
)

write_rds(trends_daily_month_raw, file.path("data", "trends_daily_month_raw.rds"))

trends_daily_month <- 
  map(trends_daily_month_raw, flatten) %>%
  map(set_names, keywords) %>%
  transpose() %>%
  # There are empty list elements for some months for some keywords
  # remove those, otherwise nested map call will fail
  map(compact) %>%
  map( ~ map(.x, mutate_at, "hits", as.character) %>%
         bind_rows())
write_rds(trends_daily_month, file.path("data", "trends_daily_month.rds"))


replace_lessthan1 <- function(x, num_value = 0.5) {
  # new <- x
  # if (x == "<1") new <- as.character(num_value)
  # as.numeric(new)
  ifelse(x == "<1", num_value, as.numeric(x))
}

trends_year_2020_df <- map(trends_year_2020, mutate_at, "hits", as.character) %>% 
  bind_rows() %>% 
  mutate(month = date)

# multiply monthly and daily values to get the "true" daily hits value
trends_combined <- bind_rows(trends_daily_month) %>% 
  mutate(month = floor_date(date, "month")) %>% 
  inner_join(trends_year_2020_df, by = c("keyword", "month"), suffix = c(".d", ".m")) %>% 
  # if hits is "<1", replace it with a numeric value between >0 and <1
  mutate_at(vars(starts_with("hits.")), ~replace_lessthan1(., 0.5)) %>% 
  # if monthly hits value is 0, increase it slightly to 0.1 so that daily variations persist
  mutate(hits.m = ifelse(hits.m == 0, 0.1, hits.m)) %>% 
  mutate(hits = hits.d * hits.m / 100) %>% 
  select(everything(), 
         -starts_with("time."), -starts_with("time."), -starts_with("category."), -geo.m, -gprop.m, -date.m,
         geo = geo.d, gprop = gprop.d, date = date.d)

# visualize the values generated by multiplying daily and monthly rates vs. monthly rates only
trends_combined %>% 
  filter(keyword == "Coronavirus") %>% 
  select(-month, -geo, -gprop, -keyword, -hits.d) %>% 
  pivot_longer(cols = -date, names_to = "type", values_to = "value") %>% 
  ggplot(aes(date, value, col = type)) +
  geom_line()


#' Since monthly trends have been obtained from a longer period, hit scores have to be rescaled to a range of 0 to 100.

# show maximum hit value per keyword
trends_combined %>% 
  group_by(keyword) %>% 
  summarize(max = max(hits)) %>% 
  arrange(max)

# check calculation (every maximum value must be 100)
trends_combined %>% 
  group_by(keyword) %>% 
  mutate(hits_rescaled = hits * 100 / max(hits)) %>% 
  ungroup() %>% 
  group_by(keyword) %>% 
  summarize(max = max(hits_rescaled)) %>% 
  arrange(max)

trends_combined <- trends_combined %>% 
  group_by(keyword) %>% 
  mutate(hits_rescaled = hits * 100 / max(hits)) %>% 
  ungroup()

write_rds(trends_combined, "data/trends_2020_combined.rds")

