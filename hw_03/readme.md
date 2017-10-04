# Fariha Khan 

## Homework 3: Use dplyr to manipulate and explore data (also use ggplot2)
#### Due date: Oct 3, 2017

#
#### **Links**

 - [Homework outline](http://stat545.com/hw03_dplyr-and-more-ggplot2.html)
 
 - [My README](https://github.com/farihakhan/STAT545-hw-khan-fariha/blob/master/hw_03/readme.md)
 
 - My assignment in [.Rdm](https://github.com/farihakhan/STAT545-hw-khan-fariha/blob/master/hw_03/hw03_dplyr.Rmd) and [.md](https://github.com/farihakhan/STAT545-hw-khan-fariha/blob/master/hw_03/hw03_dplyr.md) formats.
 
##### Resources:

 - [ggplot cheatsheet](https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf)

#
## **Overview:**

 - Manipulate and explore a dataset with the dplyr package, complemented by visualizations made with ggplot2.
 
      - Gapminder data
       
 - Task menu
 
      - Get the maximum and minimum of GDP per capita for all continents.

      - Look at the spread of GDP per capita within the continents.

      - Compute a trimmed mean of life expectancy for different years. Or a weighted mean, weighting by population.

      - How is life expectancy changing over time on different continents?
       
 - Companion graphs
 
## **Progress report:**

This assignment definitely took longer than the previous too. The open ended questions were great because it really let me explore different functions and methods. It also made it harder to decide what to graph and what was meaningful. I spent 70% of the time playing with ggplot. Even though I didn't include too many variations of graphs, I learned a lot about the different types of graphs that are out there and what they're used for. Using dplyr to analyze the data was fairly straight forward, I think the most difficult part was trying to figure out what to graph. 

I did try using some addition packages to play with the data. This was my first time using grid an gridExtra. I just wanted to try formatting the tables all together. It was quite easy to use and I am happy with the result. I also used reshape to melt my data to allow easier plotting. I found for this assignment, I tended to write my code in a seperate .r file, because testing it within the .rmd because quite cluttered. This way I was able to go through the code more easily and then simply copy it into my .rmd.

Difficulties:
- I spent a lot of time trying to format a list into my README doc, but I was still unable to get the output I was looking for. This still is glitchy for me sadly.

- When making output tables using the kable() function, I found that using results='asis' displayed the info in plain linear text. I changed the chunk configuration to results='markup' to display the data is a cleaner format. I also found that I needed to seperate my code chunks in order to display a table and a plot. I eventually fixed this by changing the output of my rmd to a 'github_document'.

- I began to have some trouble with the gapminder.md file after trying to rename the markdown. The md wasn't updating, so I created a new r markdown and copy+pasted my work in. Not really sure what was going on but restarting the document seemed easier to do than debugging.

I would have tried working with the data more, I always lose track of time when I'm doing exploratory analysis. But I have a presentation to work on so I covered 4 of the suggested questions, and played with different forms of ggplots.
PS.. I may have forgotten labels on some of my graphs...