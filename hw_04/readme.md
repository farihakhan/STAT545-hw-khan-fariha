# Fariha Khan 

## Homework 4: Tidy data and joins
#### Due date: Oct 11, 2017

#
#### **Links**

 - [Homework outline](http://stat545.com/hw04_tidy-data-joins.html)
 
 - Assignment in [.Rdm](https://github.com/farihakhan/STAT545-hw-khan-fariha/blob/master/hw_04/hw04_tidyData.Rmd) 
 
 - Assignment in [.md](https://github.com/farihakhan/STAT545-hw-khan-fariha/blob/master/hw_04/hw04_tidyData.md) 
 
##### Resources:

 - [ggplot cheatsheet](https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf)
 
 - [Efficient R programming](https://csgillespie.github.io/efficientR/5-5-dplyr.html#renaming-columns)
 
 - [An Introduction to reshape2](http://seananderson.ca/2013/10/19/reshape.html)
 
 - [Cheatsheet for dplyr join functions](http://stat545.com/bit001_dplyr-cheatsheet.html)
 

#
## **Overview:**

The goal of this homework is to solidify data wrangling skills by working some realistic problems in the grey area between data aggregation and data reshaping.

 - Choose your own adventure
 
 - General data reshaping and relationship to aggregation
 
      - Problem: You have data in one “shape” but you wish it were in another. Usually this is because the alternative shape is superior for presenting a table, making a figure, or doing aggregation and statistical analysis.
      
      - Solution: Reshape your data. For simple reshaping, gather() and spread() from tidyr will suffice. Do the thing that it possible / easier now that your data has a new shape.

 
## **Progress report:**

I noticed straight off the bad that my git pathway wasn't connecting with RStudio, so I wasn't able to push anything from this project. 

I wanted to replace the column names within the spread df without manually typing them out, but couldn't find a way to pipe it into the kable() function. I instead changed the column names in another step, which resulted in me having to create two data subsets.

The links I listed above really helped me understand how to use to spread() function. But when graphing the data, I found it easier to use the unspread data.
When I tried to graph a scatterplot based on the reshaped data, I couldn't figure out how to get a proper legen in place.