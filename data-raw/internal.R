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
  "select",             "select",
  "active",             "active",
  "visible",            "visible"
)

usethis::use_data(method_and_aes, internal = TRUE, overwrite = TRUE)
