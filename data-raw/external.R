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

gaus <- jsonlite::fromJSON("https://raw.githubusercontent.com/antvis/g2/master/demos/data/gaussion-distribution.json")
gaus <- tibble::as_tibble(gaus)

library(g2r)

data <- jsonlite::fromJSON('[{
    "State": "AL",
    "Under 5 Years": 310504,
    "5 to 13 Years": 552339,
    "14 to 17 Years": 259034,
    "18 to 24 Years": 450818,
    "25 to 44 Years": 1231572,
    "45 to 64 Years": 1215966,
    "65 Years and Over": 641667
}, {
    "State": "AK",
    "Under 5 Years": 52083,
    "5 to 13 Years": 85640,
    "14 to 17 Years": 42153,
    "18 to 24 Years": 74257,
    "25 to 44 Years": 198724,
    "45 to 64 Years": 183159,
    "65 Years and Over": 50277
}, {
    "State": "AZ",
    "Under 5 Years": 515910,
    "5 to 13 Years": 828669,
    "14 to 17 Years": 362642,
    "18 to 24 Years": 601943,
    "25 to 44 Years": 1804762,
    "45 to 64 Years": 1523681,
    "65 Years and Over": 862573
}, {
    "State": "AR",
    "Under 5 Years": 202070,
    "5 to 13 Years": 343207,
    "14 to 17 Years": 157204,
    "18 to 24 Years": 264160,
    "25 to 44 Years": 754420,
    "45 to 64 Years": 727124,
    "65 Years and Over": 407205
}, {
    "State": "CA",
    "Under 5 Years": 2704659,
    "5 to 13 Years": 4499890,
    "14 to 17 Years": 2159981,
    "18 to 24 Years": 3853788,
    "25 to 44 Years": 10604510,
    "45 to 64 Years": 8819342,
    "65 Years and Over": 4114496
}, {
    "State": "CO",
    "Under 5 Years": 358280,
    "5 to 13 Years": 587154,
    "14 to 17 Years": 261701,
    "18 to 24 Years": 466194,
    "25 to 44 Years": 1464939,
    "45 to 64 Years": 1290094,
    "65 Years and Over": 511094
}, {
    "State": "CT",
    "Under 5 Years": 211637,
    "5 to 13 Years": 403658,
    "14 to 17 Years": 196918,
    "18 to 24 Years": 325110,
    "25 to 44 Years": 916955,
    "45 to 64 Years": 968967,
    "65 Years and Over": 478007
}]')

fields <- colnames(data)[!colnames(data) %in% "State"]

state <- alter(data, type = "fold", fields = fields, key = "name")

usethis::use_data(temp, fruits, gaus, state, internal = FALSE, overwrite = TRUE)