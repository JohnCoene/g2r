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

info_and_aes <- dplyr::tribble(
  ~"name",            ~"aes",
  "start",            c("x", "xend"),
  "end",              c("y", "yend"),
  "position",         c("x", "y"),
  "content",          "content",
  "offsetX",          "offset_x",
  "offsetY",          "offset_y",
  "html",             "html",
  "zIndex",           "z_index",
  "alignX",           "align_x",
  "alignY",           "align_y",
  "top",              "top",
  "color",            "color"
)

usethis::use_data(method_and_aes, info_and_aes, internal = TRUE, overwrite = TRUE)
