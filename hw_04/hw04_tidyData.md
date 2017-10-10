Assignment 4
================
Fariha Khan
2017-10-09

``` r
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(gapminder))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(reshape2))
```

### Activity 1

#### Data reshaping cheatsheet

| reshape2 Function | tidyr Function |
|-------------------|----------------|
| `melt`            | `gather`       |
| `dcast`           | `spread`       |

### Activity 2

> Make a tibble with one row per year and columns for life expectancy for two or more countries. Use knitr::kable() to make this table look pretty in your rendered homework.

I chose six countries at random for this activity, mostly because I don't know too much about them. I first created a subset of the data without applying the spread() function, so I could later plot it more easily. I used paste() to rename the column names to add the string "lifeExp" to each country column before calling kable().

``` r
actv2_lifeExp <- as.tibble(gapminder %>% 
      filter(country %in% c("Benin","Iceland","Mozambique",
                            "Sierra Leone","Venezuela", "Zambia")) %>% 
      group_by(country) %>% 
      select(country, year, lifeExp))

actv2_lifeExp_spr <- actv2_lifeExp %>% 
      spread(country, lifeExp)
names(actv2_lifeExp_spr)[2:7] <- paste(names(actv2_lifeExp_spr)[2:7], "LifeExp", sep = " ")
kable(actv2_lifeExp_spr,
      align = "c", digits = 2, padding = 1,
      caption = "Life expectancy of a given country by year")
```

| year | Benin LifeExp | Iceland LifeExp | Mozambique LifeExp | Sierra Leone LifeExp | Venezuela LifeExp | Zambia LifeExp |
|:----:|:-------------:|:---------------:|:------------------:|:--------------------:|:-----------------:|:--------------:|
| 1952 |     38.22     |      72.49      |        31.29       |         30.33        |       55.09       |      42.04     |
| 1957 |     40.36     |      73.47      |        33.78       |         31.57        |       57.91       |      44.08     |
| 1962 |     42.62     |      73.68      |        36.16       |         32.77        |       60.77       |      46.02     |
| 1967 |     44.88     |      73.73      |        38.11       |         34.11        |       63.48       |      47.77     |
| 1972 |     47.01     |      74.46      |        40.33       |         35.40        |       65.71       |      50.11     |
| 1977 |     49.19     |      76.11      |        42.49       |         36.79        |       67.46       |      51.39     |
| 1982 |     50.90     |      76.99      |        42.80       |         38.45        |       68.56       |      51.82     |
| 1987 |     52.34     |      77.23      |        42.86       |         40.01        |       70.19       |      50.82     |
| 1992 |     53.92     |      78.77      |        44.28       |         38.33        |       71.15       |      46.10     |
| 1997 |     54.78     |      78.95      |        46.34       |         39.90        |       72.15       |      40.24     |
| 2002 |     54.41     |      80.50      |        44.03       |         41.01        |       72.77       |      39.19     |
| 2007 |     56.73     |      81.76      |        42.08       |         42.57        |       73.75       |      42.38     |

To plot this data:

``` r
ggplot(data = actv2_lifeExp, aes(x = year, y = lifeExp, color = country)) +
      geom_point(aes(shape = country)) +
      geom_smooth(size = 1, method = 'lm', se = FALSE) +
      scale_color_brewer(palette = "Set2") +
      ggtitle("Linear Model of Life Expectancy from 1952 - 2007")
```

![](hw04_tidyData_files/figure-markdown_github-ascii_identifiers/plot%20lifeExp%202+%20countries-1.png)

### Activity 3

> Compute some measure of life expectancy (mean? median? min? max?) for all possible combinations of continent and year. Reshape that to have one row per year and one variable for each continent. Or the other way around: one row per continent and one variable per year.

``` r
actv3_measures <-  gapminder %>% 
      select(continent, year, lifeExp) %>% 
      group_by(year, continent) %>% 
      summarise_each(funs(min, mean, max), lifeExp)
```

    ## `summarise_each()` is deprecated.
    ## Use `summarise_all()`, `summarise_at()` or `summarise_if()` instead.
    ## To map `funs` over a selection of variables, use `summarise_at()`

``` r
kable(actv3_measures[1:15,], align = "c")
```

| year | continent | lifeExp\_min | lifeExp\_mean | lifeExp\_max |
|:----:|:---------:|:------------:|:-------------:|:------------:|
| 1952 |   Africa  |    30.000    |    39.13550   |    52.724    |
| 1952 |  Americas |    37.579    |    53.27984   |    68.750    |
| 1952 |    Asia   |    28.801    |    46.31439   |    65.390    |
| 1952 |   Europe  |    43.585    |    64.40850   |    72.670    |
| 1952 |  Oceania  |    69.120    |    69.25500   |    69.390    |
| 1957 |   Africa  |    31.570    |    41.26635   |    58.089    |
| 1957 |  Americas |    40.696    |    55.96028   |    69.960    |
| 1957 |    Asia   |    30.332    |    49.31854   |    67.840    |
| 1957 |   Europe  |    48.079    |    66.70307   |    73.470    |
| 1957 |  Oceania  |    70.260    |    70.29500   |    70.330    |
| 1962 |   Africa  |    32.767    |    43.31944   |    60.246    |
| 1962 |  Americas |    43.428    |    58.39876   |    71.300    |
| 1962 |    Asia   |    31.997    |    51.56322   |    69.390    |
| 1962 |   Europe  |    52.098    |    68.53923   |    73.680    |
| 1962 |  Oceania  |    70.930    |    71.08500   |    71.240    |

``` r
gapminder %>% 
      select(continent, year, lifeExp) %>% 
      group_by(year) %>% 
      summarise_each(funs(min, median, mean, max), lifeExp)
```

    ## `summarise_each()` is deprecated.
    ## Use `summarise_all()`, `summarise_at()` or `summarise_if()` instead.
    ## To map `funs` over a selection of variables, use `summarise_at()`

    ## # A tibble: 12 x 5
    ##     year lifeExp_min lifeExp_median lifeExp_mean lifeExp_max
    ##    <int>       <dbl>          <dbl>        <dbl>       <dbl>
    ##  1  1952      28.801        45.1355     49.05762      72.670
    ##  2  1957      30.332        48.3605     51.50740      73.470
    ##  3  1962      31.997        50.8810     53.60925      73.680
    ##  4  1967      34.020        53.8250     55.67829      74.160
    ##  5  1972      35.400        56.5300     57.64739      74.720
    ##  6  1977      31.220        59.6720     59.57016      76.110
    ##  7  1982      38.445        62.4415     61.53320      77.110
    ##  8  1987      39.906        65.8340     63.21261      78.670
    ##  9  1992      23.599        67.7030     64.16034      79.360
    ## 10  1997      36.087        69.3940     65.01468      80.690
    ## 11  2002      39.193        70.8255     65.69492      82.000
    ## 12  2007      39.613        71.9355     67.00742      82.603
