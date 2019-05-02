[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

# g2r

<img src="./man/figures/logo.png" align="right" height="200px"/>

A [ggplot2](https://ggplot2.tidyverse.org/)-inspired grammar of graphics for interactive visualisations via [g2](https://antv.alipay.com/zh-cn/g2/3.x/).

## Installation

``` r
# install.packages("remotes")
remotes::install_github("JohnCoene/g2r")
```

## Synthax

From ggplot2 to g2r:

1. `aes` -> `asp`
2. `geom_*` -> `fig_*`
3. `scale_*` -> `gauge_*`
4. `facet_*` -> `plane_*`
5. `theme_*` -> `style_*`

## Example


``` r
library(g2r)

iris %>%
  g2(plan(Petal.Length, Petal.Width, color = Species)) %>% 
  fig_point() %>%
  plane_wrap(planes(Species), type = "tree")
```

