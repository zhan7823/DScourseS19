#question 3
library(rvest)

url <- "http://money.com/money/collection-post/3829776/heres-what-the-average-grad-makes-right-out-of-college/"

cast <- read_html(url) %>% html_nodes("td") %>% html_text()

cast

# question 4
install.packages("owmr")
library(owmr)

OWM_API_KEY = "c7c39cd727543e4258bb0c52a77e9265"
res <- get_current("OKLAHOMA", units = "metric") %>%
  owmr_as_tibble() %>% names()
rio <- search_city_list("Rio de Janeiro") %>%
  as.list()
get_current(rio$id, units = "metric") %>%
  owmr_as_tibble() %>% .[, 1:6]
res <- find_cities_by_geo_point(
  lat = rio$lat,
  lon = rio$lon,
  cnt = 5,
  units = "metric"
) %>% owmr_as_tibble()
idx <- c(names(res[1:6]), "OKLAHOMA")
forecast <- get_forecast("Oklahoma", units = "metric") %>%
  owmr_as_tibble()
funcs <- list( temp = round,wind_speed = round)
forecast %<>% parse_columns(funcs)
("{{dt_txt}}h {{temp}}°C, {{wind_speed}} m/s" %$$%
    forecast) %>% head(10)
                  
                  