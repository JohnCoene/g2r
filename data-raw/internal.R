method_and_aes <- dplyr::tribble(
  ~"method",            ~"aes",
  "position",           c("x", "y"),
  "size",               "size",
  "color",              "color",
  "opacity",            "opacity",
  "shape",              "shape",
  "label",              "label",
  "tooltip",            "tooltip",
  "style",              "style",
  "adjust",             "adjust",
  "select",             "select"
)

map_names <- system.file("maps", package = "g2r")
map_names <- list.files(map_names)
map_names <- gsub("\\.json", "", map_names)

usethis::use_data(method_and_aes, map_names, internal = TRUE, overwrite = TRUE)
