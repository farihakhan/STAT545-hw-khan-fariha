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

Make a tibble with one row per year and columns for life expectancy for two or more countries. Use knitr::kable() to make this table look pretty in your rendered homework.

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
