# g2r

<img src="./man/figures/logo.png" align="right" style="max-height:180px;"/>

A [ggplot2](https://ggplot2.tidyverse.org/)-inspired grammar of graphics for interactive visualisations via [g2](https://antv.alipay.com/zh-cn/g2/3.x/).

## Installation

``` r
# install.packages("remotes")
remotes::install_github("JohnCoene/g2r")
```

## Synthax

From ggplot2 to g2r:

1. `aes` -> `plan`
2. `geom_*` -> `fig_*`
3. `scale_*` -> `gauge_*`

## Example

It's looks familiar to ggplot2.

### ggplot

``` r
library(ggplot2)

iris %>%
  ggplot(aes(Petal.Length, Petal.Width, color = Species)) +
  geom_point() +
  facet_wrap(.~Species)
```

### g2r

``` r
library(g2r)

iris %>%
  g2(plan(Petal.Length, Petal.Width, color = Species)) %>% 
  fig_point() %>%
  plane_wrap(planes(Species), type = "tree")
```

