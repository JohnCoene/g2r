library(dplyr)

temp_json <- '[{
    "city": "London",
    "month": "Jan.",
    "temp": 18.9
}, {
    "city": "London",
    "month": "Feb.",
    "temp": 28.8
}, {
    "city": "London",
    "month": "Mar.",
    "temp": 39.3
}, {
    "city": "London",
    "month": "Apr.",
    "temp": 81.4
}, {
    "city": "London",
    "month": "May",
    "temp": 47
}, {
    "city": "London",
    "month": "Jun.",
    "temp": 20.3
}, {
    "city": "London",
    "month": "Jul.",
    "temp": 24
}, {
    "city": "London",
    "month": "Aug.",
    "temp": 35.6
}, {
    "city": "Berlin",
    "month": "Jan.",
    "temp": 12.4
}, {
    "city": "Berlin",
    "month": "Feb.",
    "temp": 23.2
}, {
    "city": "Berlin",
    "month": "Mar.",
    "temp": 34.5
}, {
    "city": "Berlin",
    "month": "Apr.",
    "temp": 99.7
}, {
    "city": "Berlin",
    "month": "May",
    "temp": 52.6
}, {
    "city": "Berlin",
    "month": "Jun.",
    "temp": 35.5
}, {
    "city": "Berlin",
    "month": "Jul.",
    "temp": 37.4
}, {
    "city": "Berlin",
    "month": "Aug.",
    "temp": 42.4
}]'

temp <- jsonlite::fromJSON(temp_json) %>% 
  as_tibble()

fruits <- dplyr::tibble(
  fruit = c("Apples", "Bananas", "Pears", "Oranges"),
  value = c(.3, .2, .4, .1)
)

usethis::use_data(temp, fruits, internal = FALSE, overwrite = TRUE)