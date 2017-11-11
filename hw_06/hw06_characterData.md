# Homework 06: Data wrangling wrap up
Fariha Khan  
2017-11-07  




# 14.1 Introduction
Use the following packages:

```r
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(stringi))
suppressPackageStartupMessages(library(htmlwidgets))
```

# 14.2 String basics

 - String length
 - Combining strings
 - Subsetting strings
 - Locales
 - Exercises
 
### 14.2.5 Exercises

*1. In code that doesn’t use stringr, you’ll often see paste() and paste0(). What’s the difference between the two functions? What stringr function are they equivalent to? How do the functions differ in their handling of NA?*

The paste() function is used to concatenate strings. It converts character vectors into character strings and seperates strings by spaces by default, or by the parameter given to *sep*. The function takes three arguments: paste (..., sep = " ", collapse = NULL)

The paste0() function is similar to paste(), in that it also takes character vectors and concatenates them to a string. This functions takes two arguments: paste0(..., collapse = NULL), and assumes that that sep = "".


stringr::str_c() works by joining multiple strings into one string. It is similar to **paste0()**; it doesn't seperate strings with spaces by default. If any arguments are NA, str_c() will return NA, where are paste() and paste0() convert NAs into a string.

*2. In your own words, describe the difference between the sep and collapse arguments to str_c().*

str_c(..., sep = "", collapse = NULL)
The sep= argument dictates the string that is inserted between the input vectors. 
The collapse= argument used to collapse the the input vectors into a character vector of length 1 based on the string input.


*3. Use str_length() and str_sub() to extract the middle character from a string. What will you do if the string has an even number of characters?*

str_length(string)
str_sub(string, start = 1L, end = -1L)


```r
x <- c("1", "12", "123", "1234", "12345", "123456")
x_length <- str_length(x)
middle <- ceiling(x_length/2)
str_sub(x, middle, middle)
```

```
## [1] "1" "1" "2" "2" "3" "3"
```

Characters with even number of characters cannot return a single middle character; the str returned using ceiling will return the first of the two middle characters.

*4. What does str_wrap() do? When might you want to use it?*

str_wrap(string, width = 80, indent = 0, exdent = 0)

The str_wrap() function is used to wrap strings around formatted text for given dimensions. It can be used for formatting paragraphs and large text blocks.

*5. What does str_trim() do? What’s the opposite of str_trim()?*

str_trim(string, side = c("both", "left", "right"))
The str_trim() function is used to trim whitespace (ie. removes spaces/tabs) from the start and/or end of strings. 

str_pad(string, width, side = c("left", "right", "both"), pad = " ")
The opposite to this function is str_pad(), which adds whitespaces to the start and/or end of strings.

*6. Write a function that turns (e.g.) a vector c("a", "b", "c") into the string a, b, and c. Think carefully about what it should do if given a vector of length 0, 1, or 2.*


```r
return_abc <- function(x){
      x_length <- length(x)
      if(x_length > 1){
            str_c(str_c(x[-x_length], collapse = ", "),
                  x[x_length], sep= ", and ")
      } else {
            x
      }
}

return_abc("")
```

```
## [1] ""
```

```r
return_abc(letters[1])
```

```
## [1] "a"
```

```r
return_abc(letters[1:2])
```

```
## [1] "a, and b"
```

```r
return_abc(letters[1:4])
```

```
## [1] "a, b, c, and d"
```



# 14.3 Matching patterns with regular expressions

### 14.3.1.1 Exercises

*1. Explain why each of these strings don’t match a \: "\", "\\", "\\\".*

In R regexps use the backslash, \, to escape specific expression behaviours. A backslash will cause R to skip the the character following it.
 - The first string will escape to the next character - the statement will be incomplete if the string is " \ "
 - The second string will escape the first \ and print \
 - The second string will escape the first \ and print \\
 
*2. How would you match the sequence* "'\\ *?*

Not sure - this sequence won't be recognized as complete, since the \ will escape the next character. I suppose it can only be matched with itself or using indexes...

*3. What patterns will the regular expression \\..\\..\\.. match? How would you represent it as a string?*

The first \. creates the wildcard, followed by a dot. This expression will match (any character + .) 3 times

### 14.3.2.1 Exercises
*1. How would you match the literal string "\$^$" ?*

The ^ indicates the beginning of string and $ indicates the end of a string. 
Because they are both anchors, they need to be escaped to act as a string. 
The expression following expression will match the literal string.

```r
literal_string <- "$^$"
str_view(literal_string, "\\$\\^\\$")
```

<!--html_preserve--><div id="htmlwidget-1d3ad7827c396aac6ee6" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-1d3ad7827c396aac6ee6">{"x":{"html":"<ul>\n  <li><span class='match'>$^$<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

*2. Given the corpus of common words in stringr::words, create regular expressions that find all words that:*

 - Start with “y”: <!--html_preserve--><div id="htmlwidget-a8e53c2e306bf8799518" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-a8e53c2e306bf8799518">{"x":{"html":"<ul>\n  <li><span class='match'>y<\/span>ear<\/li>\n  <li><span class='match'>y<\/span>es<\/li>\n  <li><span class='match'>y<\/span>esterday<\/li>\n  <li><span class='match'>y<\/span>et<\/li>\n  <li><span class='match'>y<\/span>ou<\/li>\n  <li><span class='match'>y<\/span>oung<\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->
 
 - End with “x”: <!--html_preserve--><div id="htmlwidget-2d1cc9e35edd35447eb7" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-2d1cc9e35edd35447eb7">{"x":{"html":"<ul>\n  <li>bo<span class='match'>x<\/span><\/li>\n  <li>se<span class='match'>x<\/span><\/li>\n  <li>si<span class='match'>x<\/span><\/li>\n  <li>ta<span class='match'>x<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->
 
 - Are exactly three letters long: <!--html_preserve--><div id="htmlwidget-f3d4b9a43ce8d40f552f" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-f3d4b9a43ce8d40f552f">{"x":{"html":"<ul>\n  <li><span class='match'>act<\/span><\/li>\n  <li><span class='match'>add<\/span><\/li>\n  <li><span class='match'>age<\/span><\/li>\n  <li><span class='match'>ago<\/span><\/li>\n  <li><span class='match'>air<\/span><\/li>\n  <li><span class='match'>all<\/span><\/li>\n  <li><span class='match'>and<\/span><\/li>\n  <li><span class='match'>any<\/span><\/li>\n  <li><span class='match'>arm<\/span><\/li>\n  <li><span class='match'>art<\/span><\/li>\n  <li><span class='match'>ask<\/span><\/li>\n  <li><span class='match'>bad<\/span><\/li>\n  <li><span class='match'>bag<\/span><\/li>\n  <li><span class='match'>bar<\/span><\/li>\n  <li><span class='match'>bed<\/span><\/li>\n  <li><span class='match'>bet<\/span><\/li>\n  <li><span class='match'>big<\/span><\/li>\n  <li><span class='match'>bit<\/span><\/li>\n  <li><span class='match'>box<\/span><\/li>\n  <li><span class='match'>boy<\/span><\/li>\n  <li><span class='match'>bus<\/span><\/li>\n  <li><span class='match'>but<\/span><\/li>\n  <li><span class='match'>buy<\/span><\/li>\n  <li><span class='match'>can<\/span><\/li>\n  <li><span class='match'>car<\/span><\/li>\n  <li><span class='match'>cat<\/span><\/li>\n  <li><span class='match'>cup<\/span><\/li>\n  <li><span class='match'>cut<\/span><\/li>\n  <li><span class='match'>dad<\/span><\/li>\n  <li><span class='match'>day<\/span><\/li>\n  <li><span class='match'>die<\/span><\/li>\n  <li><span class='match'>dog<\/span><\/li>\n  <li><span class='match'>dry<\/span><\/li>\n  <li><span class='match'>due<\/span><\/li>\n  <li><span class='match'>eat<\/span><\/li>\n  <li><span class='match'>egg<\/span><\/li>\n  <li><span class='match'>end<\/span><\/li>\n  <li><span class='match'>eye<\/span><\/li>\n  <li><span class='match'>far<\/span><\/li>\n  <li><span class='match'>few<\/span><\/li>\n  <li><span class='match'>fit<\/span><\/li>\n  <li><span class='match'>fly<\/span><\/li>\n  <li><span class='match'>for<\/span><\/li>\n  <li><span class='match'>fun<\/span><\/li>\n  <li><span class='match'>gas<\/span><\/li>\n  <li><span class='match'>get<\/span><\/li>\n  <li><span class='match'>god<\/span><\/li>\n  <li><span class='match'>guy<\/span><\/li>\n  <li><span class='match'>hit<\/span><\/li>\n  <li><span class='match'>hot<\/span><\/li>\n  <li><span class='match'>how<\/span><\/li>\n  <li><span class='match'>job<\/span><\/li>\n  <li><span class='match'>key<\/span><\/li>\n  <li><span class='match'>kid<\/span><\/li>\n  <li><span class='match'>lad<\/span><\/li>\n  <li><span class='match'>law<\/span><\/li>\n  <li><span class='match'>lay<\/span><\/li>\n  <li><span class='match'>leg<\/span><\/li>\n  <li><span class='match'>let<\/span><\/li>\n  <li><span class='match'>lie<\/span><\/li>\n  <li><span class='match'>lot<\/span><\/li>\n  <li><span class='match'>low<\/span><\/li>\n  <li><span class='match'>man<\/span><\/li>\n  <li><span class='match'>may<\/span><\/li>\n  <li><span class='match'>mrs<\/span><\/li>\n  <li><span class='match'>new<\/span><\/li>\n  <li><span class='match'>non<\/span><\/li>\n  <li><span class='match'>not<\/span><\/li>\n  <li><span class='match'>now<\/span><\/li>\n  <li><span class='match'>odd<\/span><\/li>\n  <li><span class='match'>off<\/span><\/li>\n  <li><span class='match'>old<\/span><\/li>\n  <li><span class='match'>one<\/span><\/li>\n  <li><span class='match'>out<\/span><\/li>\n  <li><span class='match'>own<\/span><\/li>\n  <li><span class='match'>pay<\/span><\/li>\n  <li><span class='match'>per<\/span><\/li>\n  <li><span class='match'>put<\/span><\/li>\n  <li><span class='match'>red<\/span><\/li>\n  <li><span class='match'>rid<\/span><\/li>\n  <li><span class='match'>run<\/span><\/li>\n  <li><span class='match'>say<\/span><\/li>\n  <li><span class='match'>see<\/span><\/li>\n  <li><span class='match'>set<\/span><\/li>\n  <li><span class='match'>sex<\/span><\/li>\n  <li><span class='match'>she<\/span><\/li>\n  <li><span class='match'>sir<\/span><\/li>\n  <li><span class='match'>sit<\/span><\/li>\n  <li><span class='match'>six<\/span><\/li>\n  <li><span class='match'>son<\/span><\/li>\n  <li><span class='match'>sun<\/span><\/li>\n  <li><span class='match'>tax<\/span><\/li>\n  <li><span class='match'>tea<\/span><\/li>\n  <li><span class='match'>ten<\/span><\/li>\n  <li><span class='match'>the<\/span><\/li>\n  <li><span class='match'>tie<\/span><\/li>\n  <li><span class='match'>too<\/span><\/li>\n  <li><span class='match'>top<\/span><\/li>\n  <li><span class='match'>try<\/span><\/li>\n  <li><span class='match'>two<\/span><\/li>\n  <li><span class='match'>use<\/span><\/li>\n  <li><span class='match'>war<\/span><\/li>\n  <li><span class='match'>way<\/span><\/li>\n  <li><span class='match'>wee<\/span><\/li>\n  <li><span class='match'>who<\/span><\/li>\n  <li><span class='match'>why<\/span><\/li>\n  <li><span class='match'>win<\/span><\/li>\n  <li><span class='match'>yes<\/span><\/li>\n  <li><span class='match'>yet<\/span><\/li>\n  <li><span class='match'>you<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->
 
 - Have seven letters or more: <!--html_preserve--><div id="htmlwidget-22207598c49a016fc659" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-22207598c49a016fc659">{"x":{"html":"<ul>\n  <li><span class='match'>absolut<\/span>e<\/li>\n  <li><span class='match'>account<\/span><\/li>\n  <li><span class='match'>achieve<\/span><\/li>\n  <li><span class='match'>address<\/span><\/li>\n  <li><span class='match'>adverti<\/span>se<\/li>\n  <li><span class='match'>afterno<\/span>on<\/li>\n  <li><span class='match'>against<\/span><\/li>\n  <li><span class='match'>already<\/span><\/li>\n  <li><span class='match'>alright<\/span><\/li>\n  <li><span class='match'>althoug<\/span>h<\/li>\n  <li><span class='match'>america<\/span><\/li>\n  <li><span class='match'>another<\/span><\/li>\n  <li><span class='match'>apparen<\/span>t<\/li>\n  <li><span class='match'>appoint<\/span><\/li>\n  <li><span class='match'>approac<\/span>h<\/li>\n  <li><span class='match'>appropr<\/span>iate<\/li>\n  <li><span class='match'>arrange<\/span><\/li>\n  <li><span class='match'>associa<\/span>te<\/li>\n  <li><span class='match'>authori<\/span>ty<\/li>\n  <li><span class='match'>availab<\/span>le<\/li>\n  <li><span class='match'>balance<\/span><\/li>\n  <li><span class='match'>because<\/span><\/li>\n  <li><span class='match'>believe<\/span><\/li>\n  <li><span class='match'>benefit<\/span><\/li>\n  <li><span class='match'>between<\/span><\/li>\n  <li><span class='match'>brillia<\/span>nt<\/li>\n  <li><span class='match'>britain<\/span><\/li>\n  <li><span class='match'>brother<\/span><\/li>\n  <li><span class='match'>busines<\/span>s<\/li>\n  <li><span class='match'>certain<\/span><\/li>\n  <li><span class='match'>chairma<\/span>n<\/li>\n  <li><span class='match'>charact<\/span>er<\/li>\n  <li><span class='match'>colleag<\/span>ue<\/li>\n  <li><span class='match'>collect<\/span><\/li>\n  <li><span class='match'>college<\/span><\/li>\n  <li><span class='match'>comment<\/span><\/li>\n  <li><span class='match'>committ<\/span>ee<\/li>\n  <li><span class='match'>communi<\/span>ty<\/li>\n  <li><span class='match'>company<\/span><\/li>\n  <li><span class='match'>compare<\/span><\/li>\n  <li><span class='match'>complet<\/span>e<\/li>\n  <li><span class='match'>compute<\/span><\/li>\n  <li><span class='match'>concern<\/span><\/li>\n  <li><span class='match'>conditi<\/span>on<\/li>\n  <li><span class='match'>conside<\/span>r<\/li>\n  <li><span class='match'>consult<\/span><\/li>\n  <li><span class='match'>contact<\/span><\/li>\n  <li><span class='match'>continu<\/span>e<\/li>\n  <li><span class='match'>contrac<\/span>t<\/li>\n  <li><span class='match'>control<\/span><\/li>\n  <li><span class='match'>convers<\/span>e<\/li>\n  <li><span class='match'>correct<\/span><\/li>\n  <li><span class='match'>council<\/span><\/li>\n  <li><span class='match'>country<\/span><\/li>\n  <li><span class='match'>current<\/span><\/li>\n  <li><span class='match'>decisio<\/span>n<\/li>\n  <li><span class='match'>definit<\/span>e<\/li>\n  <li><span class='match'>departm<\/span>ent<\/li>\n  <li><span class='match'>describ<\/span>e<\/li>\n  <li><span class='match'>develop<\/span><\/li>\n  <li><span class='match'>differe<\/span>nce<\/li>\n  <li><span class='match'>difficu<\/span>lt<\/li>\n  <li><span class='match'>discuss<\/span><\/li>\n  <li><span class='match'>distric<\/span>t<\/li>\n  <li><span class='match'>documen<\/span>t<\/li>\n  <li><span class='match'>economy<\/span><\/li>\n  <li><span class='match'>educate<\/span><\/li>\n  <li><span class='match'>electri<\/span>c<\/li>\n  <li><span class='match'>encoura<\/span>ge<\/li>\n  <li><span class='match'>english<\/span><\/li>\n  <li><span class='match'>environ<\/span>ment<\/li>\n  <li><span class='match'>especia<\/span>l<\/li>\n  <li><span class='match'>evening<\/span><\/li>\n  <li><span class='match'>evidenc<\/span>e<\/li>\n  <li><span class='match'>example<\/span><\/li>\n  <li><span class='match'>exercis<\/span>e<\/li>\n  <li><span class='match'>expense<\/span><\/li>\n  <li><span class='match'>experie<\/span>nce<\/li>\n  <li><span class='match'>explain<\/span><\/li>\n  <li><span class='match'>express<\/span><\/li>\n  <li><span class='match'>finance<\/span><\/li>\n  <li><span class='match'>fortune<\/span><\/li>\n  <li><span class='match'>forward<\/span><\/li>\n  <li><span class='match'>functio<\/span>n<\/li>\n  <li><span class='match'>further<\/span><\/li>\n  <li><span class='match'>general<\/span><\/li>\n  <li><span class='match'>germany<\/span><\/li>\n  <li><span class='match'>goodbye<\/span><\/li>\n  <li><span class='match'>history<\/span><\/li>\n  <li><span class='match'>holiday<\/span><\/li>\n  <li><span class='match'>hospita<\/span>l<\/li>\n  <li><span class='match'>however<\/span><\/li>\n  <li><span class='match'>hundred<\/span><\/li>\n  <li><span class='match'>husband<\/span><\/li>\n  <li><span class='match'>identif<\/span>y<\/li>\n  <li><span class='match'>imagine<\/span><\/li>\n  <li><span class='match'>importa<\/span>nt<\/li>\n  <li><span class='match'>improve<\/span><\/li>\n  <li><span class='match'>include<\/span><\/li>\n  <li><span class='match'>increas<\/span>e<\/li>\n  <li><span class='match'>individ<\/span>ual<\/li>\n  <li><span class='match'>industr<\/span>y<\/li>\n  <li><span class='match'>instead<\/span><\/li>\n  <li><span class='match'>interes<\/span>t<\/li>\n  <li><span class='match'>introdu<\/span>ce<\/li>\n  <li><span class='match'>involve<\/span><\/li>\n  <li><span class='match'>kitchen<\/span><\/li>\n  <li><span class='match'>languag<\/span>e<\/li>\n  <li><span class='match'>machine<\/span><\/li>\n  <li><span class='match'>meaning<\/span><\/li>\n  <li><span class='match'>measure<\/span><\/li>\n  <li><span class='match'>mention<\/span><\/li>\n  <li><span class='match'>million<\/span><\/li>\n  <li><span class='match'>ministe<\/span>r<\/li>\n  <li><span class='match'>morning<\/span><\/li>\n  <li><span class='match'>necessa<\/span>ry<\/li>\n  <li><span class='match'>obvious<\/span><\/li>\n  <li><span class='match'>occasio<\/span>n<\/li>\n  <li><span class='match'>operate<\/span><\/li>\n  <li><span class='match'>opportu<\/span>nity<\/li>\n  <li><span class='match'>organiz<\/span>e<\/li>\n  <li><span class='match'>origina<\/span>l<\/li>\n  <li><span class='match'>otherwi<\/span>se<\/li>\n  <li><span class='match'>paragra<\/span>ph<\/li>\n  <li><span class='match'>particu<\/span>lar<\/li>\n  <li><span class='match'>pension<\/span><\/li>\n  <li><span class='match'>percent<\/span><\/li>\n  <li><span class='match'>perfect<\/span><\/li>\n  <li><span class='match'>perhaps<\/span><\/li>\n  <li><span class='match'>photogr<\/span>aph<\/li>\n  <li><span class='match'>picture<\/span><\/li>\n  <li><span class='match'>politic<\/span><\/li>\n  <li><span class='match'>positio<\/span>n<\/li>\n  <li><span class='match'>positiv<\/span>e<\/li>\n  <li><span class='match'>possibl<\/span>e<\/li>\n  <li><span class='match'>practis<\/span>e<\/li>\n  <li><span class='match'>prepare<\/span><\/li>\n  <li><span class='match'>present<\/span><\/li>\n  <li><span class='match'>pressur<\/span>e<\/li>\n  <li><span class='match'>presume<\/span><\/li>\n  <li><span class='match'>previou<\/span>s<\/li>\n  <li><span class='match'>private<\/span><\/li>\n  <li><span class='match'>probabl<\/span>e<\/li>\n  <li><span class='match'>problem<\/span><\/li>\n  <li><span class='match'>proceed<\/span><\/li>\n  <li><span class='match'>process<\/span><\/li>\n  <li><span class='match'>produce<\/span><\/li>\n  <li><span class='match'>product<\/span><\/li>\n  <li><span class='match'>program<\/span>me<\/li>\n  <li><span class='match'>project<\/span><\/li>\n  <li><span class='match'>propose<\/span><\/li>\n  <li><span class='match'>protect<\/span><\/li>\n  <li><span class='match'>provide<\/span><\/li>\n  <li><span class='match'>purpose<\/span><\/li>\n  <li><span class='match'>quality<\/span><\/li>\n  <li><span class='match'>quarter<\/span><\/li>\n  <li><span class='match'>questio<\/span>n<\/li>\n  <li><span class='match'>realise<\/span><\/li>\n  <li><span class='match'>receive<\/span><\/li>\n  <li><span class='match'>recogni<\/span>ze<\/li>\n  <li><span class='match'>recomme<\/span>nd<\/li>\n  <li><span class='match'>relatio<\/span>n<\/li>\n  <li><span class='match'>remembe<\/span>r<\/li>\n  <li><span class='match'>represe<\/span>nt<\/li>\n  <li><span class='match'>require<\/span><\/li>\n  <li><span class='match'>researc<\/span>h<\/li>\n  <li><span class='match'>resourc<\/span>e<\/li>\n  <li><span class='match'>respect<\/span><\/li>\n  <li><span class='match'>respons<\/span>ible<\/li>\n  <li><span class='match'>saturda<\/span>y<\/li>\n  <li><span class='match'>science<\/span><\/li>\n  <li><span class='match'>scotlan<\/span>d<\/li>\n  <li><span class='match'>secreta<\/span>ry<\/li>\n  <li><span class='match'>section<\/span><\/li>\n  <li><span class='match'>separat<\/span>e<\/li>\n  <li><span class='match'>serious<\/span><\/li>\n  <li><span class='match'>service<\/span><\/li>\n  <li><span class='match'>similar<\/span><\/li>\n  <li><span class='match'>situate<\/span><\/li>\n  <li><span class='match'>society<\/span><\/li>\n  <li><span class='match'>special<\/span><\/li>\n  <li><span class='match'>specifi<\/span>c<\/li>\n  <li><span class='match'>standar<\/span>d<\/li>\n  <li><span class='match'>station<\/span><\/li>\n  <li><span class='match'>straigh<\/span>t<\/li>\n  <li><span class='match'>strateg<\/span>y<\/li>\n  <li><span class='match'>structu<\/span>re<\/li>\n  <li><span class='match'>student<\/span><\/li>\n  <li><span class='match'>subject<\/span><\/li>\n  <li><span class='match'>succeed<\/span><\/li>\n  <li><span class='match'>suggest<\/span><\/li>\n  <li><span class='match'>support<\/span><\/li>\n  <li><span class='match'>suppose<\/span><\/li>\n  <li><span class='match'>surpris<\/span>e<\/li>\n  <li><span class='match'>telepho<\/span>ne<\/li>\n  <li><span class='match'>televis<\/span>ion<\/li>\n  <li><span class='match'>terribl<\/span>e<\/li>\n  <li><span class='match'>therefo<\/span>re<\/li>\n  <li><span class='match'>thirtee<\/span>n<\/li>\n  <li><span class='match'>thousan<\/span>d<\/li>\n  <li><span class='match'>through<\/span><\/li>\n  <li><span class='match'>thursda<\/span>y<\/li>\n  <li><span class='match'>togethe<\/span>r<\/li>\n  <li><span class='match'>tomorro<\/span>w<\/li>\n  <li><span class='match'>tonight<\/span><\/li>\n  <li><span class='match'>traffic<\/span><\/li>\n  <li><span class='match'>transpo<\/span>rt<\/li>\n  <li><span class='match'>trouble<\/span><\/li>\n  <li><span class='match'>tuesday<\/span><\/li>\n  <li><span class='match'>underst<\/span>and<\/li>\n  <li><span class='match'>univers<\/span>ity<\/li>\n  <li><span class='match'>various<\/span><\/li>\n  <li><span class='match'>village<\/span><\/li>\n  <li><span class='match'>wednesd<\/span>ay<\/li>\n  <li><span class='match'>welcome<\/span><\/li>\n  <li><span class='match'>whether<\/span><\/li>\n  <li><span class='match'>without<\/span><\/li>\n  <li><span class='match'>yesterd<\/span>ay<\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->
 
Since this list is long, you might want to use the match argument to str_view() to show only the matching or non-matching words.

### 14.3.3.1 Exercises

*1. Create regular expressions to find all words that:*

 - Start with a vowel: <!--html_preserve--><div id="htmlwidget-0cd8f112f04a933384d8" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-0cd8f112f04a933384d8">{"x":{"html":"<ul>\n  <li>a<\/li>\n  <li>able<\/li>\n  <li>about<\/li>\n  <li>absolute<\/li>\n  <li>accept<\/li>\n  <li>account<\/li>\n  <li>achieve<\/li>\n  <li>across<\/li>\n  <li>act<\/li>\n  <li>active<\/li>\n  <li>actual<\/li>\n  <li>add<\/li>\n  <li>address<\/li>\n  <li>admit<\/li>\n  <li>advertise<\/li>\n  <li>affect<\/li>\n  <li>afford<\/li>\n  <li>after<\/li>\n  <li>afternoon<\/li>\n  <li>again<\/li>\n  <li>against<\/li>\n  <li>age<\/li>\n  <li>agent<\/li>\n  <li>ago<\/li>\n  <li>agree<\/li>\n  <li>air<\/li>\n  <li>all<\/li>\n  <li>allow<\/li>\n  <li>almost<\/li>\n  <li>along<\/li>\n  <li>already<\/li>\n  <li>alright<\/li>\n  <li>also<\/li>\n  <li>although<\/li>\n  <li>always<\/li>\n  <li>america<\/li>\n  <li>amount<\/li>\n  <li>and<\/li>\n  <li>another<\/li>\n  <li>answer<\/li>\n  <li>any<\/li>\n  <li>apart<\/li>\n  <li>apparent<\/li>\n  <li>appear<\/li>\n  <li>apply<\/li>\n  <li>appoint<\/li>\n  <li>approach<\/li>\n  <li>appropriate<\/li>\n  <li>area<\/li>\n  <li>argue<\/li>\n  <li>arm<\/li>\n  <li>around<\/li>\n  <li>arrange<\/li>\n  <li>art<\/li>\n  <li>as<\/li>\n  <li>ask<\/li>\n  <li>associate<\/li>\n  <li>assume<\/li>\n  <li>at<\/li>\n  <li>attend<\/li>\n  <li>authority<\/li>\n  <li>available<\/li>\n  <li>aware<\/li>\n  <li>away<\/li>\n  <li>awful<\/li>\n  <li>baby<\/li>\n  <li>back<\/li>\n  <li>bad<\/li>\n  <li>bag<\/li>\n  <li>balance<\/li>\n  <li>ball<\/li>\n  <li>bank<\/li>\n  <li>bar<\/li>\n  <li>base<\/li>\n  <li>basis<\/li>\n  <li>be<\/li>\n  <li>bear<\/li>\n  <li>beat<\/li>\n  <li>beauty<\/li>\n  <li>because<\/li>\n  <li>become<\/li>\n  <li>bed<\/li>\n  <li>before<\/li>\n  <li>begin<\/li>\n  <li>behind<\/li>\n  <li>believe<\/li>\n  <li>benefit<\/li>\n  <li>best<\/li>\n  <li>bet<\/li>\n  <li>between<\/li>\n  <li>big<\/li>\n  <li>bill<\/li>\n  <li>birth<\/li>\n  <li>bit<\/li>\n  <li>black<\/li>\n  <li>bloke<\/li>\n  <li>blood<\/li>\n  <li>blow<\/li>\n  <li>blue<\/li>\n  <li>board<\/li>\n  <li>boat<\/li>\n  <li>body<\/li>\n  <li>book<\/li>\n  <li>both<\/li>\n  <li>bother<\/li>\n  <li>bottle<\/li>\n  <li>bottom<\/li>\n  <li>box<\/li>\n  <li>boy<\/li>\n  <li>break<\/li>\n  <li>brief<\/li>\n  <li>brilliant<\/li>\n  <li>bring<\/li>\n  <li>britain<\/li>\n  <li>brother<\/li>\n  <li>budget<\/li>\n  <li>build<\/li>\n  <li>bus<\/li>\n  <li>business<\/li>\n  <li>busy<\/li>\n  <li>but<\/li>\n  <li>buy<\/li>\n  <li>by<\/li>\n  <li>cake<\/li>\n  <li>call<\/li>\n  <li>can<\/li>\n  <li>car<\/li>\n  <li>card<\/li>\n  <li>care<\/li>\n  <li>carry<\/li>\n  <li>case<\/li>\n  <li>cat<\/li>\n  <li>catch<\/li>\n  <li>cause<\/li>\n  <li>cent<\/li>\n  <li>centre<\/li>\n  <li>certain<\/li>\n  <li>chair<\/li>\n  <li>chairman<\/li>\n  <li>chance<\/li>\n  <li>change<\/li>\n  <li>chap<\/li>\n  <li>character<\/li>\n  <li>charge<\/li>\n  <li>cheap<\/li>\n  <li>check<\/li>\n  <li>child<\/li>\n  <li>choice<\/li>\n  <li>choose<\/li>\n  <li>Christ<\/li>\n  <li>Christmas<\/li>\n  <li>church<\/li>\n  <li>city<\/li>\n  <li>claim<\/li>\n  <li>class<\/li>\n  <li>clean<\/li>\n  <li>clear<\/li>\n  <li>client<\/li>\n  <li>clock<\/li>\n  <li>close<\/li>\n  <li>closes<\/li>\n  <li>clothe<\/li>\n  <li>club<\/li>\n  <li>coffee<\/li>\n  <li>cold<\/li>\n  <li>colleague<\/li>\n  <li>collect<\/li>\n  <li>college<\/li>\n  <li>colour<\/li>\n  <li>come<\/li>\n  <li>comment<\/li>\n  <li>commit<\/li>\n  <li>committee<\/li>\n  <li>common<\/li>\n  <li>community<\/li>\n  <li>company<\/li>\n  <li>compare<\/li>\n  <li>complete<\/li>\n  <li>compute<\/li>\n  <li>concern<\/li>\n  <li>condition<\/li>\n  <li>confer<\/li>\n  <li>consider<\/li>\n  <li>consult<\/li>\n  <li>contact<\/li>\n  <li>continue<\/li>\n  <li>contract<\/li>\n  <li>control<\/li>\n  <li>converse<\/li>\n  <li>cook<\/li>\n  <li>copy<\/li>\n  <li>corner<\/li>\n  <li>correct<\/li>\n  <li>cost<\/li>\n  <li>could<\/li>\n  <li>council<\/li>\n  <li>count<\/li>\n  <li>country<\/li>\n  <li>county<\/li>\n  <li>couple<\/li>\n  <li>course<\/li>\n  <li>court<\/li>\n  <li>cover<\/li>\n  <li>create<\/li>\n  <li>cross<\/li>\n  <li>cup<\/li>\n  <li>current<\/li>\n  <li>cut<\/li>\n  <li>dad<\/li>\n  <li>danger<\/li>\n  <li>date<\/li>\n  <li>day<\/li>\n  <li>dead<\/li>\n  <li>deal<\/li>\n  <li>dear<\/li>\n  <li>debate<\/li>\n  <li>decide<\/li>\n  <li>decision<\/li>\n  <li>deep<\/li>\n  <li>definite<\/li>\n  <li>degree<\/li>\n  <li>department<\/li>\n  <li>depend<\/li>\n  <li>describe<\/li>\n  <li>design<\/li>\n  <li>detail<\/li>\n  <li>develop<\/li>\n  <li>die<\/li>\n  <li>difference<\/li>\n  <li>difficult<\/li>\n  <li>dinner<\/li>\n  <li>direct<\/li>\n  <li>discuss<\/li>\n  <li>district<\/li>\n  <li>divide<\/li>\n  <li>do<\/li>\n  <li>doctor<\/li>\n  <li>document<\/li>\n  <li>dog<\/li>\n  <li>door<\/li>\n  <li>double<\/li>\n  <li>doubt<\/li>\n  <li>down<\/li>\n  <li>draw<\/li>\n  <li>dress<\/li>\n  <li>drink<\/li>\n  <li>drive<\/li>\n  <li>drop<\/li>\n  <li>dry<\/li>\n  <li>due<\/li>\n  <li>during<\/li>\n  <li>each<\/li>\n  <li>early<\/li>\n  <li>east<\/li>\n  <li>easy<\/li>\n  <li>eat<\/li>\n  <li>economy<\/li>\n  <li>educate<\/li>\n  <li>effect<\/li>\n  <li>egg<\/li>\n  <li>eight<\/li>\n  <li>either<\/li>\n  <li>elect<\/li>\n  <li>electric<\/li>\n  <li>eleven<\/li>\n  <li>else<\/li>\n  <li>employ<\/li>\n  <li>encourage<\/li>\n  <li>end<\/li>\n  <li>engine<\/li>\n  <li>english<\/li>\n  <li>enjoy<\/li>\n  <li>enough<\/li>\n  <li>enter<\/li>\n  <li>environment<\/li>\n  <li>equal<\/li>\n  <li>especial<\/li>\n  <li>europe<\/li>\n  <li>even<\/li>\n  <li>evening<\/li>\n  <li>ever<\/li>\n  <li>every<\/li>\n  <li>evidence<\/li>\n  <li>exact<\/li>\n  <li>example<\/li>\n  <li>except<\/li>\n  <li>excuse<\/li>\n  <li>exercise<\/li>\n  <li>exist<\/li>\n  <li>expect<\/li>\n  <li>expense<\/li>\n  <li>experience<\/li>\n  <li>explain<\/li>\n  <li>express<\/li>\n  <li>extra<\/li>\n  <li>eye<\/li>\n  <li>face<\/li>\n  <li>fact<\/li>\n  <li>fair<\/li>\n  <li>fall<\/li>\n  <li>family<\/li>\n  <li>far<\/li>\n  <li>farm<\/li>\n  <li>fast<\/li>\n  <li>father<\/li>\n  <li>favour<\/li>\n  <li>feed<\/li>\n  <li>feel<\/li>\n  <li>few<\/li>\n  <li>field<\/li>\n  <li>fight<\/li>\n  <li>figure<\/li>\n  <li>file<\/li>\n  <li>fill<\/li>\n  <li>film<\/li>\n  <li>final<\/li>\n  <li>finance<\/li>\n  <li>find<\/li>\n  <li>fine<\/li>\n  <li>finish<\/li>\n  <li>fire<\/li>\n  <li>first<\/li>\n  <li>fish<\/li>\n  <li>fit<\/li>\n  <li>five<\/li>\n  <li>flat<\/li>\n  <li>floor<\/li>\n  <li>fly<\/li>\n  <li>follow<\/li>\n  <li>food<\/li>\n  <li>foot<\/li>\n  <li>for<\/li>\n  <li>force<\/li>\n  <li>forget<\/li>\n  <li>form<\/li>\n  <li>fortune<\/li>\n  <li>forward<\/li>\n  <li>four<\/li>\n  <li>france<\/li>\n  <li>free<\/li>\n  <li>friday<\/li>\n  <li>friend<\/li>\n  <li>from<\/li>\n  <li>front<\/li>\n  <li>full<\/li>\n  <li>fun<\/li>\n  <li>function<\/li>\n  <li>fund<\/li>\n  <li>further<\/li>\n  <li>future<\/li>\n  <li>game<\/li>\n  <li>garden<\/li>\n  <li>gas<\/li>\n  <li>general<\/li>\n  <li>germany<\/li>\n  <li>get<\/li>\n  <li>girl<\/li>\n  <li>give<\/li>\n  <li>glass<\/li>\n  <li>go<\/li>\n  <li>god<\/li>\n  <li>good<\/li>\n  <li>goodbye<\/li>\n  <li>govern<\/li>\n  <li>grand<\/li>\n  <li>grant<\/li>\n  <li>great<\/li>\n  <li>green<\/li>\n  <li>ground<\/li>\n  <li>group<\/li>\n  <li>grow<\/li>\n  <li>guess<\/li>\n  <li>guy<\/li>\n  <li>hair<\/li>\n  <li>half<\/li>\n  <li>hall<\/li>\n  <li>hand<\/li>\n  <li>hang<\/li>\n  <li>happen<\/li>\n  <li>happy<\/li>\n  <li>hard<\/li>\n  <li>hate<\/li>\n  <li>have<\/li>\n  <li>he<\/li>\n  <li>head<\/li>\n  <li>health<\/li>\n  <li>hear<\/li>\n  <li>heart<\/li>\n  <li>heat<\/li>\n  <li>heavy<\/li>\n  <li>hell<\/li>\n  <li>help<\/li>\n  <li>here<\/li>\n  <li>high<\/li>\n  <li>history<\/li>\n  <li>hit<\/li>\n  <li>hold<\/li>\n  <li>holiday<\/li>\n  <li>home<\/li>\n  <li>honest<\/li>\n  <li>hope<\/li>\n  <li>horse<\/li>\n  <li>hospital<\/li>\n  <li>hot<\/li>\n  <li>hour<\/li>\n  <li>house<\/li>\n  <li>how<\/li>\n  <li>however<\/li>\n  <li>hullo<\/li>\n  <li>hundred<\/li>\n  <li>husband<\/li>\n  <li>idea<\/li>\n  <li>identify<\/li>\n  <li>if<\/li>\n  <li>imagine<\/li>\n  <li>important<\/li>\n  <li>improve<\/li>\n  <li>in<\/li>\n  <li>include<\/li>\n  <li>income<\/li>\n  <li>increase<\/li>\n  <li>indeed<\/li>\n  <li>individual<\/li>\n  <li>industry<\/li>\n  <li>inform<\/li>\n  <li>inside<\/li>\n  <li>instead<\/li>\n  <li>insure<\/li>\n  <li>interest<\/li>\n  <li>into<\/li>\n  <li>introduce<\/li>\n  <li>invest<\/li>\n  <li>involve<\/li>\n  <li>issue<\/li>\n  <li>it<\/li>\n  <li>item<\/li>\n  <li>jesus<\/li>\n  <li>job<\/li>\n  <li>join<\/li>\n  <li>judge<\/li>\n  <li>jump<\/li>\n  <li>just<\/li>\n  <li>keep<\/li>\n  <li>key<\/li>\n  <li>kid<\/li>\n  <li>kill<\/li>\n  <li>kind<\/li>\n  <li>king<\/li>\n  <li>kitchen<\/li>\n  <li>knock<\/li>\n  <li>know<\/li>\n  <li>labour<\/li>\n  <li>lad<\/li>\n  <li>lady<\/li>\n  <li>land<\/li>\n  <li>language<\/li>\n  <li>large<\/li>\n  <li>last<\/li>\n  <li>late<\/li>\n  <li>laugh<\/li>\n  <li>law<\/li>\n  <li>lay<\/li>\n  <li>lead<\/li>\n  <li>learn<\/li>\n  <li>leave<\/li>\n  <li>left<\/li>\n  <li>leg<\/li>\n  <li>less<\/li>\n  <li>let<\/li>\n  <li>letter<\/li>\n  <li>level<\/li>\n  <li>lie<\/li>\n  <li>life<\/li>\n  <li>light<\/li>\n  <li>like<\/li>\n  <li>likely<\/li>\n  <li>limit<\/li>\n  <li>line<\/li>\n  <li>link<\/li>\n  <li>list<\/li>\n  <li>listen<\/li>\n  <li>little<\/li>\n  <li>live<\/li>\n  <li>load<\/li>\n  <li>local<\/li>\n  <li>lock<\/li>\n  <li>london<\/li>\n  <li>long<\/li>\n  <li>look<\/li>\n  <li>lord<\/li>\n  <li>lose<\/li>\n  <li>lot<\/li>\n  <li>love<\/li>\n  <li>low<\/li>\n  <li>luck<\/li>\n  <li>lunch<\/li>\n  <li>machine<\/li>\n  <li>main<\/li>\n  <li>major<\/li>\n  <li>make<\/li>\n  <li>man<\/li>\n  <li>manage<\/li>\n  <li>many<\/li>\n  <li>mark<\/li>\n  <li>market<\/li>\n  <li>marry<\/li>\n  <li>match<\/li>\n  <li>matter<\/li>\n  <li>may<\/li>\n  <li>maybe<\/li>\n  <li>mean<\/li>\n  <li>meaning<\/li>\n  <li>measure<\/li>\n  <li>meet<\/li>\n  <li>member<\/li>\n  <li>mention<\/li>\n  <li>middle<\/li>\n  <li>might<\/li>\n  <li>mile<\/li>\n  <li>milk<\/li>\n  <li>million<\/li>\n  <li>mind<\/li>\n  <li>minister<\/li>\n  <li>minus<\/li>\n  <li>minute<\/li>\n  <li>miss<\/li>\n  <li>mister<\/li>\n  <li>moment<\/li>\n  <li>monday<\/li>\n  <li>money<\/li>\n  <li>month<\/li>\n  <li>more<\/li>\n  <li>morning<\/li>\n  <li>most<\/li>\n  <li>mother<\/li>\n  <li>motion<\/li>\n  <li>move<\/li>\n  <li>mrs<\/li>\n  <li>much<\/li>\n  <li>music<\/li>\n  <li>must<\/li>\n  <li>name<\/li>\n  <li>nation<\/li>\n  <li>nature<\/li>\n  <li>near<\/li>\n  <li>necessary<\/li>\n  <li>need<\/li>\n  <li>never<\/li>\n  <li>new<\/li>\n  <li>news<\/li>\n  <li>next<\/li>\n  <li>nice<\/li>\n  <li>night<\/li>\n  <li>nine<\/li>\n  <li>no<\/li>\n  <li>non<\/li>\n  <li>none<\/li>\n  <li>normal<\/li>\n  <li>north<\/li>\n  <li>not<\/li>\n  <li>note<\/li>\n  <li>notice<\/li>\n  <li>now<\/li>\n  <li>number<\/li>\n  <li>obvious<\/li>\n  <li>occasion<\/li>\n  <li>odd<\/li>\n  <li>of<\/li>\n  <li>off<\/li>\n  <li>offer<\/li>\n  <li>office<\/li>\n  <li>often<\/li>\n  <li>okay<\/li>\n  <li>old<\/li>\n  <li>on<\/li>\n  <li>once<\/li>\n  <li>one<\/li>\n  <li>only<\/li>\n  <li>open<\/li>\n  <li>operate<\/li>\n  <li>opportunity<\/li>\n  <li>oppose<\/li>\n  <li>or<\/li>\n  <li>order<\/li>\n  <li>organize<\/li>\n  <li>original<\/li>\n  <li>other<\/li>\n  <li>otherwise<\/li>\n  <li>ought<\/li>\n  <li>out<\/li>\n  <li>over<\/li>\n  <li>own<\/li>\n  <li>pack<\/li>\n  <li>page<\/li>\n  <li>paint<\/li>\n  <li>pair<\/li>\n  <li>paper<\/li>\n  <li>paragraph<\/li>\n  <li>pardon<\/li>\n  <li>parent<\/li>\n  <li>park<\/li>\n  <li>part<\/li>\n  <li>particular<\/li>\n  <li>party<\/li>\n  <li>pass<\/li>\n  <li>past<\/li>\n  <li>pay<\/li>\n  <li>pence<\/li>\n  <li>pension<\/li>\n  <li>people<\/li>\n  <li>per<\/li>\n  <li>percent<\/li>\n  <li>perfect<\/li>\n  <li>perhaps<\/li>\n  <li>period<\/li>\n  <li>person<\/li>\n  <li>photograph<\/li>\n  <li>pick<\/li>\n  <li>picture<\/li>\n  <li>piece<\/li>\n  <li>place<\/li>\n  <li>plan<\/li>\n  <li>play<\/li>\n  <li>please<\/li>\n  <li>plus<\/li>\n  <li>point<\/li>\n  <li>police<\/li>\n  <li>policy<\/li>\n  <li>politic<\/li>\n  <li>poor<\/li>\n  <li>position<\/li>\n  <li>positive<\/li>\n  <li>possible<\/li>\n  <li>post<\/li>\n  <li>pound<\/li>\n  <li>power<\/li>\n  <li>practise<\/li>\n  <li>prepare<\/li>\n  <li>present<\/li>\n  <li>press<\/li>\n  <li>pressure<\/li>\n  <li>presume<\/li>\n  <li>pretty<\/li>\n  <li>previous<\/li>\n  <li>price<\/li>\n  <li>print<\/li>\n  <li>private<\/li>\n  <li>probable<\/li>\n  <li>problem<\/li>\n  <li>proceed<\/li>\n  <li>process<\/li>\n  <li>produce<\/li>\n  <li>product<\/li>\n  <li>programme<\/li>\n  <li>project<\/li>\n  <li>proper<\/li>\n  <li>propose<\/li>\n  <li>protect<\/li>\n  <li>provide<\/li>\n  <li>public<\/li>\n  <li>pull<\/li>\n  <li>purpose<\/li>\n  <li>push<\/li>\n  <li>put<\/li>\n  <li>quality<\/li>\n  <li>quarter<\/li>\n  <li>question<\/li>\n  <li>quick<\/li>\n  <li>quid<\/li>\n  <li>quiet<\/li>\n  <li>quite<\/li>\n  <li>radio<\/li>\n  <li>rail<\/li>\n  <li>raise<\/li>\n  <li>range<\/li>\n  <li>rate<\/li>\n  <li>rather<\/li>\n  <li>read<\/li>\n  <li>ready<\/li>\n  <li>real<\/li>\n  <li>realise<\/li>\n  <li>really<\/li>\n  <li>reason<\/li>\n  <li>receive<\/li>\n  <li>recent<\/li>\n  <li>reckon<\/li>\n  <li>recognize<\/li>\n  <li>recommend<\/li>\n  <li>record<\/li>\n  <li>red<\/li>\n  <li>reduce<\/li>\n  <li>refer<\/li>\n  <li>regard<\/li>\n  <li>region<\/li>\n  <li>relation<\/li>\n  <li>remember<\/li>\n  <li>report<\/li>\n  <li>represent<\/li>\n  <li>require<\/li>\n  <li>research<\/li>\n  <li>resource<\/li>\n  <li>respect<\/li>\n  <li>responsible<\/li>\n  <li>rest<\/li>\n  <li>result<\/li>\n  <li>return<\/li>\n  <li>rid<\/li>\n  <li>right<\/li>\n  <li>ring<\/li>\n  <li>rise<\/li>\n  <li>road<\/li>\n  <li>role<\/li>\n  <li>roll<\/li>\n  <li>room<\/li>\n  <li>round<\/li>\n  <li>rule<\/li>\n  <li>run<\/li>\n  <li>safe<\/li>\n  <li>sale<\/li>\n  <li>same<\/li>\n  <li>saturday<\/li>\n  <li>save<\/li>\n  <li>say<\/li>\n  <li>scheme<\/li>\n  <li>school<\/li>\n  <li>science<\/li>\n  <li>score<\/li>\n  <li>scotland<\/li>\n  <li>seat<\/li>\n  <li>second<\/li>\n  <li>secretary<\/li>\n  <li>section<\/li>\n  <li>secure<\/li>\n  <li>see<\/li>\n  <li>seem<\/li>\n  <li>self<\/li>\n  <li>sell<\/li>\n  <li>send<\/li>\n  <li>sense<\/li>\n  <li>separate<\/li>\n  <li>serious<\/li>\n  <li>serve<\/li>\n  <li>service<\/li>\n  <li>set<\/li>\n  <li>settle<\/li>\n  <li>seven<\/li>\n  <li>sex<\/li>\n  <li>shall<\/li>\n  <li>share<\/li>\n  <li>she<\/li>\n  <li>sheet<\/li>\n  <li>shoe<\/li>\n  <li>shoot<\/li>\n  <li>shop<\/li>\n  <li>short<\/li>\n  <li>should<\/li>\n  <li>show<\/li>\n  <li>shut<\/li>\n  <li>sick<\/li>\n  <li>side<\/li>\n  <li>sign<\/li>\n  <li>similar<\/li>\n  <li>simple<\/li>\n  <li>since<\/li>\n  <li>sing<\/li>\n  <li>single<\/li>\n  <li>sir<\/li>\n  <li>sister<\/li>\n  <li>sit<\/li>\n  <li>site<\/li>\n  <li>situate<\/li>\n  <li>six<\/li>\n  <li>size<\/li>\n  <li>sleep<\/li>\n  <li>slight<\/li>\n  <li>slow<\/li>\n  <li>small<\/li>\n  <li>smoke<\/li>\n  <li>so<\/li>\n  <li>social<\/li>\n  <li>society<\/li>\n  <li>some<\/li>\n  <li>son<\/li>\n  <li>soon<\/li>\n  <li>sorry<\/li>\n  <li>sort<\/li>\n  <li>sound<\/li>\n  <li>south<\/li>\n  <li>space<\/li>\n  <li>speak<\/li>\n  <li>special<\/li>\n  <li>specific<\/li>\n  <li>speed<\/li>\n  <li>spell<\/li>\n  <li>spend<\/li>\n  <li>square<\/li>\n  <li>staff<\/li>\n  <li>stage<\/li>\n  <li>stairs<\/li>\n  <li>stand<\/li>\n  <li>standard<\/li>\n  <li>start<\/li>\n  <li>state<\/li>\n  <li>station<\/li>\n  <li>stay<\/li>\n  <li>step<\/li>\n  <li>stick<\/li>\n  <li>still<\/li>\n  <li>stop<\/li>\n  <li>story<\/li>\n  <li>straight<\/li>\n  <li>strategy<\/li>\n  <li>street<\/li>\n  <li>strike<\/li>\n  <li>strong<\/li>\n  <li>structure<\/li>\n  <li>student<\/li>\n  <li>study<\/li>\n  <li>stuff<\/li>\n  <li>stupid<\/li>\n  <li>subject<\/li>\n  <li>succeed<\/li>\n  <li>such<\/li>\n  <li>sudden<\/li>\n  <li>suggest<\/li>\n  <li>suit<\/li>\n  <li>summer<\/li>\n  <li>sun<\/li>\n  <li>sunday<\/li>\n  <li>supply<\/li>\n  <li>support<\/li>\n  <li>suppose<\/li>\n  <li>sure<\/li>\n  <li>surprise<\/li>\n  <li>switch<\/li>\n  <li>system<\/li>\n  <li>table<\/li>\n  <li>take<\/li>\n  <li>talk<\/li>\n  <li>tape<\/li>\n  <li>tax<\/li>\n  <li>tea<\/li>\n  <li>teach<\/li>\n  <li>team<\/li>\n  <li>telephone<\/li>\n  <li>television<\/li>\n  <li>tell<\/li>\n  <li>ten<\/li>\n  <li>tend<\/li>\n  <li>term<\/li>\n  <li>terrible<\/li>\n  <li>test<\/li>\n  <li>than<\/li>\n  <li>thank<\/li>\n  <li>the<\/li>\n  <li>then<\/li>\n  <li>there<\/li>\n  <li>therefore<\/li>\n  <li>they<\/li>\n  <li>thing<\/li>\n  <li>think<\/li>\n  <li>thirteen<\/li>\n  <li>thirty<\/li>\n  <li>this<\/li>\n  <li>thou<\/li>\n  <li>though<\/li>\n  <li>thousand<\/li>\n  <li>three<\/li>\n  <li>through<\/li>\n  <li>throw<\/li>\n  <li>thursday<\/li>\n  <li>tie<\/li>\n  <li>time<\/li>\n  <li>to<\/li>\n  <li>today<\/li>\n  <li>together<\/li>\n  <li>tomorrow<\/li>\n  <li>tonight<\/li>\n  <li>too<\/li>\n  <li>top<\/li>\n  <li>total<\/li>\n  <li>touch<\/li>\n  <li>toward<\/li>\n  <li>town<\/li>\n  <li>trade<\/li>\n  <li>traffic<\/li>\n  <li>train<\/li>\n  <li>transport<\/li>\n  <li>travel<\/li>\n  <li>treat<\/li>\n  <li>tree<\/li>\n  <li>trouble<\/li>\n  <li>true<\/li>\n  <li>trust<\/li>\n  <li>try<\/li>\n  <li>tuesday<\/li>\n  <li>turn<\/li>\n  <li>twelve<\/li>\n  <li>twenty<\/li>\n  <li>two<\/li>\n  <li>type<\/li>\n  <li>under<\/li>\n  <li>understand<\/li>\n  <li>union<\/li>\n  <li>unit<\/li>\n  <li>unite<\/li>\n  <li>university<\/li>\n  <li>unless<\/li>\n  <li>until<\/li>\n  <li>up<\/li>\n  <li>upon<\/li>\n  <li>use<\/li>\n  <li>usual<\/li>\n  <li>value<\/li>\n  <li>various<\/li>\n  <li>very<\/li>\n  <li>video<\/li>\n  <li>view<\/li>\n  <li>village<\/li>\n  <li>visit<\/li>\n  <li>vote<\/li>\n  <li>wage<\/li>\n  <li>wait<\/li>\n  <li>walk<\/li>\n  <li>wall<\/li>\n  <li>want<\/li>\n  <li>war<\/li>\n  <li>warm<\/li>\n  <li>wash<\/li>\n  <li>waste<\/li>\n  <li>watch<\/li>\n  <li>water<\/li>\n  <li>way<\/li>\n  <li>we<\/li>\n  <li>wear<\/li>\n  <li>wednesday<\/li>\n  <li>wee<\/li>\n  <li>week<\/li>\n  <li>weigh<\/li>\n  <li>welcome<\/li>\n  <li>well<\/li>\n  <li>west<\/li>\n  <li>what<\/li>\n  <li>when<\/li>\n  <li>where<\/li>\n  <li>whether<\/li>\n  <li>which<\/li>\n  <li>while<\/li>\n  <li>white<\/li>\n  <li>who<\/li>\n  <li>whole<\/li>\n  <li>why<\/li>\n  <li>wide<\/li>\n  <li>wife<\/li>\n  <li>will<\/li>\n  <li>win<\/li>\n  <li>wind<\/li>\n  <li>window<\/li>\n  <li>wish<\/li>\n  <li>with<\/li>\n  <li>within<\/li>\n  <li>without<\/li>\n  <li>woman<\/li>\n  <li>wonder<\/li>\n  <li>wood<\/li>\n  <li>word<\/li>\n  <li>work<\/li>\n  <li>world<\/li>\n  <li>worry<\/li>\n  <li>worse<\/li>\n  <li>worth<\/li>\n  <li>would<\/li>\n  <li>write<\/li>\n  <li>wrong<\/li>\n  <li>year<\/li>\n  <li>yes<\/li>\n  <li>yesterday<\/li>\n  <li>yet<\/li>\n  <li>you<\/li>\n  <li>young<\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

 - That only contain consonants: <!--html_preserve--><div id="htmlwidget-9b8ca549640083fa3c4c" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-9b8ca549640083fa3c4c">{"x":{"html":"<ul>\n  <li><span class='match'>by<\/span><\/li>\n  <li><span class='match'>dry<\/span><\/li>\n  <li><span class='match'>fly<\/span><\/li>\n  <li><span class='match'>mrs<\/span><\/li>\n  <li><span class='match'>try<\/span><\/li>\n  <li><span class='match'>why<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

 - End with ed, but not with eed: <!--html_preserve--><div id="htmlwidget-1689d295adf6c23a40a5" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-1689d295adf6c23a40a5">{"x":{"html":"<ul>\n  <li><span class='match'>bed<\/span><\/li>\n  <li>hund<span class='match'>red<\/span><\/li>\n  <li><span class='match'>red<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

 - End with ing or ise: <!--html_preserve--><div id="htmlwidget-b2bcc6f7dc45eb09f508" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-b2bcc6f7dc45eb09f508">{"x":{"html":"<ul>\n  <li>advert<span class='match'>ise<\/span><\/li>\n  <li>br<span class='match'>ing<\/span><\/li>\n  <li>dur<span class='match'>ing<\/span><\/li>\n  <li>even<span class='match'>ing<\/span><\/li>\n  <li>exerc<span class='match'>ise<\/span><\/li>\n  <li>k<span class='match'>ing<\/span><\/li>\n  <li>mean<span class='match'>ing<\/span><\/li>\n  <li>morn<span class='match'>ing<\/span><\/li>\n  <li>otherw<span class='match'>ise<\/span><\/li>\n  <li>pract<span class='match'>ise<\/span><\/li>\n  <li>ra<span class='match'>ise<\/span><\/li>\n  <li>real<span class='match'>ise<\/span><\/li>\n  <li>r<span class='match'>ing<\/span><\/li>\n  <li>r<span class='match'>ise<\/span><\/li>\n  <li>s<span class='match'>ing<\/span><\/li>\n  <li>surpr<span class='match'>ise<\/span><\/li>\n  <li>th<span class='match'>ing<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

*2. Empirically verify the rule “i before e except after c”*

```r
## i before e, except after c
length(str_subset(words, "(cei|[^c]ie)"))
```

```
## [1] 14
```

```r
str_subset(words, "(cei|[^c]ie)")
```

```
##  [1] "achieve"    "believe"    "brief"      "client"     "die"       
##  [6] "experience" "field"      "friend"     "lie"        "piece"     
## [11] "quiet"      "receive"    "tie"        "view"
```

```r
## i after e
length(str_subset(words, "(cie|[^c]ei)"))
```

```
## [1] 3
```

```r
str_subset(words, "(cie|[^c]ei)")
```

```
## [1] "science" "society" "weigh"
```

*3. Is “q” always followed by a “u”?*

```r
## q before u
length(str_subset(words, "qu"))
```

```
## [1] 10
```

```r
str_subset(words, "qu")
```

```
##  [1] "equal"    "quality"  "quarter"  "question" "quick"    "quid"    
##  [7] "quiet"    "quite"    "require"  "square"
```

```r
## u before q
length(str_subset(words, "(uq|q[^u])"))
```

```
## [1] 0
```

```r
str_subset(words, "(uq|q[^u])")
```

```
## character(0)
```

*4. Write a regular expression that matches a word if it’s probably written in British English, not American English.*
Using spelling differences: 
|**American**|**British**|
|------------|-----------|
| -er        | -re       |
| -nse       | -nce      |
| -ize       | -ise      |
| -or        | -our      |


```r
str_view(words, "([^aeiou]re|nce|ise|our)$", match = TRUE)
```

<!--html_preserve--><div id="htmlwidget-342e409f8db3c79c22ba" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-342e409f8db3c79c22ba">{"x":{"html":"<ul>\n  <li>advert<span class='match'>ise<\/span><\/li>\n  <li>bala<span class='match'>nce<\/span><\/li>\n  <li>cen<span class='match'>tre<\/span><\/li>\n  <li>cha<span class='match'>nce<\/span><\/li>\n  <li>col<span class='match'>our<\/span><\/li>\n  <li>differe<span class='match'>nce<\/span><\/li>\n  <li>evide<span class='match'>nce<\/span><\/li>\n  <li>exerc<span class='match'>ise<\/span><\/li>\n  <li>experie<span class='match'>nce<\/span><\/li>\n  <li>fav<span class='match'>our<\/span><\/li>\n  <li>fina<span class='match'>nce<\/span><\/li>\n  <li>f<span class='match'>our<\/span><\/li>\n  <li>fra<span class='match'>nce<\/span><\/li>\n  <li>h<span class='match'>our<\/span><\/li>\n  <li>lab<span class='match'>our<\/span><\/li>\n  <li>o<span class='match'>nce<\/span><\/li>\n  <li>otherw<span class='match'>ise<\/span><\/li>\n  <li>pe<span class='match'>nce<\/span><\/li>\n  <li>pract<span class='match'>ise<\/span><\/li>\n  <li>ra<span class='match'>ise<\/span><\/li>\n  <li>real<span class='match'>ise<\/span><\/li>\n  <li>r<span class='match'>ise<\/span><\/li>\n  <li>scie<span class='match'>nce<\/span><\/li>\n  <li>si<span class='match'>nce<\/span><\/li>\n  <li>surpr<span class='match'>ise<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


*5. Create a regular expression that will match telephone numbers as commonly written in your country.*
Telephone numbers in canada consist of a 3-digit area code, followed by a 7-digit number:
(xxx-xxx-xxxx) or (xxx xxx xxxx)


```r
numbers <- c("123 456 7890", "113 432-4343", "444 3434", "111-444-3333", "11 1 4443333")
str_view(numbers, "^\\d{3}(\\s|-)\\d{3}(\\s|-)\\d{4}$", match = TRUE)
```

<!--html_preserve--><div id="htmlwidget-a45fe949f5e71eff3924" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-a45fe949f5e71eff3924">{"x":{"html":"<ul>\n  <li><span class='match'>123 456 7890<\/span><\/li>\n  <li><span class='match'>113 432-4343<\/span><\/li>\n  <li><span class='match'>111-444-3333<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


### 14.3.4.1 Exercises

*1. Describe the equivalents of * ?, +, * in {m,n} form.

 - ?: 0 or 1
 - +: 1 or more
 - *: 0 or more
 - {n}: exactly n
 - {n,}: n or more
 - {,m}: at most m
 - {n,m}: between n and m

 ? is equivalent to {,1} because it matches at most 1 time. 
 + is equivalent to {1,} because it matches 1 or more times. 
 * doesn't have an equivalent because can range from 0 up to infinity.
 
*2. Describe in words what these regular expressions match: (read carefully to see if I’m using a regular expression or a string that defines a regular expression.)*

 - ^.*$: Any string
 
 - "\\\\{.+\\}": A string that contains curly brackets surrounded by at least one character
 
 - \d{4}-\d{2}-\d{2}: 4Digits-2Digits-2Digits
 
 - "\\\\\\{4}": This will match a backlash four times

3. Create regular expressions to find all words that:

 - Start with three consonants: <!--html_preserve--><div id="htmlwidget-cb4af2f69d2bb9bf10c5" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-cb4af2f69d2bb9bf10c5">{"x":{"html":"<ul>\n  <li><span class='match'>Chr<\/span>ist<\/li>\n  <li><span class='match'>Chr<\/span>istmas<\/li>\n  <li><span class='match'>dry<\/span><\/li>\n  <li><span class='match'>fly<\/span><\/li>\n  <li><span class='match'>mrs<\/span><\/li>\n  <li><span class='match'>sch<\/span>eme<\/li>\n  <li><span class='match'>sch<\/span>ool<\/li>\n  <li><span class='match'>str<\/span>aight<\/li>\n  <li><span class='match'>str<\/span>ategy<\/li>\n  <li><span class='match'>str<\/span>eet<\/li>\n  <li><span class='match'>str<\/span>ike<\/li>\n  <li><span class='match'>str<\/span>ong<\/li>\n  <li><span class='match'>str<\/span>ucture<\/li>\n  <li><span class='match'>sys<\/span>tem<\/li>\n  <li><span class='match'>thr<\/span>ee<\/li>\n  <li><span class='match'>thr<\/span>ough<\/li>\n  <li><span class='match'>thr<\/span>ow<\/li>\n  <li><span class='match'>try<\/span><\/li>\n  <li><span class='match'>typ<\/span>e<\/li>\n  <li><span class='match'>why<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->
 
 - Have three or more vowels in a row.: <!--html_preserve--><div id="htmlwidget-51e855435fafcca57815" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-51e855435fafcca57815">{"x":{"html":"<ul>\n  <li>b<span class='match'>eau<\/span>ty<\/li>\n  <li>obv<span class='match'>iou<\/span>s<\/li>\n  <li>prev<span class='match'>iou<\/span>s<\/li>\n  <li>q<span class='match'>uie<\/span>t<\/li>\n  <li>ser<span class='match'>iou<\/span>s<\/li>\n  <li>var<span class='match'>iou<\/span>s<\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->
 
 - Have two or more vowel-consonant pairs in a row: 
 <!--html_preserve--><div id="htmlwidget-7f0daab55dc343965177" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-7f0daab55dc343965177">{"x":{"html":"<ul>\n  <li>abs<span class='match'>olut<\/span>e<\/li>\n  <li><span class='match'>agen<\/span>t<\/li>\n  <li><span class='match'>alon<\/span>g<\/li>\n  <li><span class='match'>americ<\/span>a<\/li>\n  <li><span class='match'>anot<\/span>her<\/li>\n  <li><span class='match'>apar<\/span>t<\/li>\n  <li>app<span class='match'>aren<\/span>t<\/li>\n  <li>auth<span class='match'>orit<\/span>y<\/li>\n  <li>ava<span class='match'>ilab<\/span>le<\/li>\n  <li><span class='match'>awar<\/span>e<\/li>\n  <li><span class='match'>away<\/span><\/li>\n  <li>b<span class='match'>alan<\/span>ce<\/li>\n  <li>b<span class='match'>asis<\/span><\/li>\n  <li>b<span class='match'>ecom<\/span>e<\/li>\n  <li>b<span class='match'>efor<\/span>e<\/li>\n  <li>b<span class='match'>egin<\/span><\/li>\n  <li>b<span class='match'>ehin<\/span>d<\/li>\n  <li>b<span class='match'>enefit<\/span><\/li>\n  <li>b<span class='match'>usines<\/span>s<\/li>\n  <li>ch<span class='match'>arac<\/span>ter<\/li>\n  <li>cl<span class='match'>oses<\/span><\/li>\n  <li>comm<span class='match'>unit<\/span>y<\/li>\n  <li>cons<span class='match'>ider<\/span><\/li>\n  <li>c<span class='match'>over<\/span><\/li>\n  <li>d<span class='match'>ebat<\/span>e<\/li>\n  <li>d<span class='match'>ecid<\/span>e<\/li>\n  <li>d<span class='match'>ecis<\/span>ion<\/li>\n  <li>d<span class='match'>efinit<\/span>e<\/li>\n  <li>d<span class='match'>epar<\/span>tment<\/li>\n  <li>d<span class='match'>epen<\/span>d<\/li>\n  <li>d<span class='match'>esig<\/span>n<\/li>\n  <li>d<span class='match'>evelop<\/span><\/li>\n  <li>diff<span class='match'>eren<\/span>ce<\/li>\n  <li>diff<span class='match'>icul<\/span>t<\/li>\n  <li>d<span class='match'>irec<\/span>t<\/li>\n  <li>d<span class='match'>ivid<\/span>e<\/li>\n  <li>d<span class='match'>ocumen<\/span>t<\/li>\n  <li>d<span class='match'>urin<\/span>g<\/li>\n  <li><span class='match'>econom<\/span>y<\/li>\n  <li><span class='match'>educat<\/span>e<\/li>\n  <li><span class='match'>elec<\/span>t<\/li>\n  <li><span class='match'>elec<\/span>tric<\/li>\n  <li><span class='match'>eleven<\/span><\/li>\n  <li>enco<span class='match'>urag<\/span>e<\/li>\n  <li>env<span class='match'>iron<\/span>ment<\/li>\n  <li>e<span class='match'>urop<\/span>e<\/li>\n  <li><span class='match'>even<\/span><\/li>\n  <li><span class='match'>evenin<\/span>g<\/li>\n  <li><span class='match'>ever<\/span><\/li>\n  <li><span class='match'>ever<\/span>y<\/li>\n  <li><span class='match'>eviden<\/span>ce<\/li>\n  <li><span class='match'>exac<\/span>t<\/li>\n  <li><span class='match'>exam<\/span>ple<\/li>\n  <li><span class='match'>exer<\/span>cise<\/li>\n  <li><span class='match'>exis<\/span>t<\/li>\n  <li>f<span class='match'>amil<\/span>y<\/li>\n  <li>f<span class='match'>igur<\/span>e<\/li>\n  <li>f<span class='match'>inal<\/span><\/li>\n  <li>f<span class='match'>inan<\/span>ce<\/li>\n  <li>f<span class='match'>inis<\/span>h<\/li>\n  <li>fr<span class='match'>iday<\/span><\/li>\n  <li>f<span class='match'>utur<\/span>e<\/li>\n  <li>g<span class='match'>eneral<\/span><\/li>\n  <li>g<span class='match'>over<\/span>n<\/li>\n  <li>h<span class='match'>oliday<\/span><\/li>\n  <li>h<span class='match'>ones<\/span>t<\/li>\n  <li>hosp<span class='match'>ital<\/span><\/li>\n  <li>h<span class='match'>owever<\/span><\/li>\n  <li><span class='match'>iden<\/span>tify<\/li>\n  <li><span class='match'>imagin<\/span>e<\/li>\n  <li>ind<span class='match'>ivid<\/span>ual<\/li>\n  <li>int<span class='match'>eres<\/span>t<\/li>\n  <li>intr<span class='match'>oduc<\/span>e<\/li>\n  <li><span class='match'>item<\/span><\/li>\n  <li>j<span class='match'>esus<\/span><\/li>\n  <li>l<span class='match'>evel<\/span><\/li>\n  <li>l<span class='match'>ikel<\/span>y<\/li>\n  <li>l<span class='match'>imit<\/span><\/li>\n  <li>l<span class='match'>ocal<\/span><\/li>\n  <li>m<span class='match'>ajor<\/span><\/li>\n  <li>m<span class='match'>anag<\/span>e<\/li>\n  <li>me<span class='match'>anin<\/span>g<\/li>\n  <li>me<span class='match'>asur<\/span>e<\/li>\n  <li>m<span class='match'>inis<\/span>ter<\/li>\n  <li>m<span class='match'>inus<\/span><\/li>\n  <li>m<span class='match'>inut<\/span>e<\/li>\n  <li>m<span class='match'>omen<\/span>t<\/li>\n  <li>m<span class='match'>oney<\/span><\/li>\n  <li>m<span class='match'>usic<\/span><\/li>\n  <li>n<span class='match'>atur<\/span>e<\/li>\n  <li>n<span class='match'>eces<\/span>sary<\/li>\n  <li>n<span class='match'>ever<\/span><\/li>\n  <li>n<span class='match'>otic<\/span>e<\/li>\n  <li><span class='match'>okay<\/span><\/li>\n  <li><span class='match'>open<\/span><\/li>\n  <li><span class='match'>operat<\/span>e<\/li>\n  <li>opport<span class='match'>unit<\/span>y<\/li>\n  <li>org<span class='match'>aniz<\/span>e<\/li>\n  <li><span class='match'>original<\/span><\/li>\n  <li><span class='match'>over<\/span><\/li>\n  <li>p<span class='match'>aper<\/span><\/li>\n  <li>p<span class='match'>arag<\/span>raph<\/li>\n  <li>p<span class='match'>aren<\/span>t<\/li>\n  <li>part<span class='match'>icular<\/span><\/li>\n  <li>ph<span class='match'>otog<\/span>raph<\/li>\n  <li>p<span class='match'>olic<\/span>e<\/li>\n  <li>p<span class='match'>olic<\/span>y<\/li>\n  <li>p<span class='match'>olitic<\/span><\/li>\n  <li>p<span class='match'>osit<\/span>ion<\/li>\n  <li>p<span class='match'>ositiv<\/span>e<\/li>\n  <li>p<span class='match'>ower<\/span><\/li>\n  <li>pr<span class='match'>epar<\/span>e<\/li>\n  <li>pr<span class='match'>esen<\/span>t<\/li>\n  <li>pr<span class='match'>esum<\/span>e<\/li>\n  <li>pr<span class='match'>ivat<\/span>e<\/li>\n  <li>pr<span class='match'>obab<\/span>le<\/li>\n  <li>pr<span class='match'>oces<\/span>s<\/li>\n  <li>pr<span class='match'>oduc<\/span>e<\/li>\n  <li>pr<span class='match'>oduc<\/span>t<\/li>\n  <li>pr<span class='match'>ojec<\/span>t<\/li>\n  <li>pr<span class='match'>oper<\/span><\/li>\n  <li>pr<span class='match'>opos<\/span>e<\/li>\n  <li>pr<span class='match'>otec<\/span>t<\/li>\n  <li>pr<span class='match'>ovid<\/span>e<\/li>\n  <li>qu<span class='match'>alit<\/span>y<\/li>\n  <li>re<span class='match'>alis<\/span>e<\/li>\n  <li>re<span class='match'>ason<\/span><\/li>\n  <li>r<span class='match'>ecen<\/span>t<\/li>\n  <li>r<span class='match'>ecog<\/span>nize<\/li>\n  <li>r<span class='match'>ecom<\/span>mend<\/li>\n  <li>r<span class='match'>ecor<\/span>d<\/li>\n  <li>r<span class='match'>educ<\/span>e<\/li>\n  <li>r<span class='match'>efer<\/span><\/li>\n  <li>r<span class='match'>egar<\/span>d<\/li>\n  <li>r<span class='match'>elat<\/span>ion<\/li>\n  <li>r<span class='match'>emem<\/span>ber<\/li>\n  <li>r<span class='match'>epor<\/span>t<\/li>\n  <li>repr<span class='match'>esen<\/span>t<\/li>\n  <li>r<span class='match'>esul<\/span>t<\/li>\n  <li>r<span class='match'>etur<\/span>n<\/li>\n  <li>s<span class='match'>atur<\/span>day<\/li>\n  <li>s<span class='match'>econ<\/span>d<\/li>\n  <li>secr<span class='match'>etar<\/span>y<\/li>\n  <li>s<span class='match'>ecur<\/span>e<\/li>\n  <li>s<span class='match'>eparat<\/span>e<\/li>\n  <li>s<span class='match'>even<\/span><\/li>\n  <li>s<span class='match'>imilar<\/span><\/li>\n  <li>sp<span class='match'>ecific<\/span><\/li>\n  <li>str<span class='match'>ateg<\/span>y<\/li>\n  <li>st<span class='match'>uden<\/span>t<\/li>\n  <li>st<span class='match'>upid<\/span><\/li>\n  <li>t<span class='match'>elep<\/span>hone<\/li>\n  <li>t<span class='match'>elevis<\/span>ion<\/li>\n  <li>th<span class='match'>erefor<\/span>e<\/li>\n  <li>tho<span class='match'>usan<\/span>d<\/li>\n  <li>t<span class='match'>oday<\/span><\/li>\n  <li>t<span class='match'>oget<\/span>her<\/li>\n  <li>t<span class='match'>omor<\/span>row<\/li>\n  <li>t<span class='match'>onig<\/span>ht<\/li>\n  <li>t<span class='match'>otal<\/span><\/li>\n  <li>t<span class='match'>owar<\/span>d<\/li>\n  <li>tr<span class='match'>avel<\/span><\/li>\n  <li><span class='match'>unit<\/span><\/li>\n  <li><span class='match'>unit<\/span>e<\/li>\n  <li><span class='match'>univer<\/span>sity<\/li>\n  <li><span class='match'>upon<\/span><\/li>\n  <li>v<span class='match'>isit<\/span><\/li>\n  <li>w<span class='match'>ater<\/span><\/li>\n  <li>w<span class='match'>oman<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->
 

### 14.3.5.1 Exercises

1. Describe, in words, what these expressions will match:

 - (.)\1\1 : A character repeated twice
 
 - "(.)(.)\\2\\1": Any two characters occuring once, and then repeated in reverse
 
 - (..)\1: Two characters repeated once
 
 - "(.).\\1.\\1": A string of 5 characters, where the 1st, 3rd and 5th characters are repeated and the 2nd and 4th can be anything.
 
 - "(.)(.)(.).*\\3\\2\\1": String of 6 or more characters, where the first 3 are repeated in reverse between any character (.) repeated 0 to inifnity times
 
2. Construct regular expressions to match words that:

 - Start and end with the same character: <!--html_preserve--><div id="htmlwidget-82d53555a566f5a5b438" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-82d53555a566f5a5b438">{"x":{"html":"<ul>\n  <li><span class='match'>america<\/span><\/li>\n  <li><span class='match'>area<\/span><\/li>\n  <li><span class='match'>dad<\/span><\/li>\n  <li><span class='match'>dead<\/span><\/li>\n  <li><span class='match'>depend<\/span><\/li>\n  <li><span class='match'>educate<\/span><\/li>\n  <li><span class='match'>else<\/span><\/li>\n  <li><span class='match'>encourage<\/span><\/li>\n  <li><span class='match'>engine<\/span><\/li>\n  <li><span class='match'>europe<\/span><\/li>\n  <li><span class='match'>evidence<\/span><\/li>\n  <li><span class='match'>example<\/span><\/li>\n  <li><span class='match'>excuse<\/span><\/li>\n  <li><span class='match'>exercise<\/span><\/li>\n  <li><span class='match'>expense<\/span><\/li>\n  <li><span class='match'>experience<\/span><\/li>\n  <li><span class='match'>eye<\/span><\/li>\n  <li><span class='match'>health<\/span><\/li>\n  <li><span class='match'>high<\/span><\/li>\n  <li><span class='match'>knock<\/span><\/li>\n  <li><span class='match'>level<\/span><\/li>\n  <li><span class='match'>local<\/span><\/li>\n  <li><span class='match'>nation<\/span><\/li>\n  <li><span class='match'>non<\/span><\/li>\n  <li><span class='match'>rather<\/span><\/li>\n  <li><span class='match'>refer<\/span><\/li>\n  <li><span class='match'>remember<\/span><\/li>\n  <li><span class='match'>serious<\/span><\/li>\n  <li><span class='match'>stairs<\/span><\/li>\n  <li><span class='match'>test<\/span><\/li>\n  <li><span class='match'>tonight<\/span><\/li>\n  <li><span class='match'>transport<\/span><\/li>\n  <li><span class='match'>treat<\/span><\/li>\n  <li><span class='match'>trust<\/span><\/li>\n  <li><span class='match'>window<\/span><\/li>\n  <li><span class='match'>yesterday<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

 - Contain a repeated pair of letters (e.g. “church” contains “ch” repeated twice.):
 <!--html_preserve--><div id="htmlwidget-8e333bdc460cf370396f" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-8e333bdc460cf370396f">{"x":{"html":"<ul>\n  <li><span class='match'>church<\/span><\/li>\n  <li><span class='match'>decide<\/span><\/li>\n  <li><span class='match'>photograph<\/span><\/li>\n  <li><span class='match'>require<\/span><\/li>\n  <li><span class='match'>sense<\/span><\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

 - Contain one letter repeated in at least three places (e.g. “eleven” contains three “e”s.)
 <!--html_preserve--><div id="htmlwidget-41fc630ca55347360120" style="width:960px;height:auto;" class="str_view html-widget"></div>
<script type="application/json" data-for="htmlwidget-41fc630ca55347360120">{"x":{"html":"<ul>\n  <li>a<span class='match'>pprop<\/span>riate<\/li>\n  <li><span class='match'>availa<\/span>ble<\/li>\n  <li>b<span class='match'>elieve<\/span><\/li>\n  <li>b<span class='match'>etwee<\/span>n<\/li>\n  <li>bu<span class='match'>siness<\/span><\/li>\n  <li>d<span class='match'>egree<\/span><\/li>\n  <li>diff<span class='match'>erence<\/span><\/li>\n  <li>di<span class='match'>scuss<\/span><\/li>\n  <li><span class='match'>eleve<\/span>n<\/li>\n  <li>e<span class='match'>nvironmen<\/span>t<\/li>\n  <li><span class='match'>evidence<\/span><\/li>\n  <li><span class='match'>exercise<\/span><\/li>\n  <li><span class='match'>expense<\/span><\/li>\n  <li><span class='match'>experience<\/span><\/li>\n  <li><span class='match'>indivi<\/span>dual<\/li>\n  <li>p<span class='match'>aragra<\/span>ph<\/li>\n  <li>r<span class='match'>eceive<\/span><\/li>\n  <li>r<span class='match'>emembe<\/span>r<\/li>\n  <li>r<span class='match'>eprese<\/span>nt<\/li>\n  <li>t<span class='match'>elephone<\/span><\/li>\n  <li>th<span class='match'>erefore<\/span><\/li>\n  <li>t<span class='match'>omorro<\/span>w<\/li>\n<\/ul>"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->
 
# 14.4 Tools
### 14.4.2 Exercises

1. For each of the following challenges, try solving it by using both a single regular expression, and a combination of multiple str_detect() calls.

 - Find all words that start or end with x: 

```r
words[str_detect(words, "^x|x$")]
```

```
## [1] "box" "sex" "six" "tax"
```

```r
i <- str_detect(words, "^x")
j <- str_detect(words, "x$")
head(list(words[i|j]))
```

```
## [[1]]
## [1] "box" "sex" "six" "tax"
```

 - Find all words that start with a vowel and end with a consonant.
 


```r
words[str_detect(words, "^[aeiou]|[^aeiou]$")]
```

```
##   [1] "a"           "able"        "about"       "absolute"    "accept"     
##   [6] "account"     "achieve"     "across"      "act"         "active"     
##  [11] "actual"      "add"         "address"     "admit"       "advertise"  
##  [16] "affect"      "afford"      "after"       "afternoon"   "again"      
##  [21] "against"     "age"         "agent"       "ago"         "agree"      
##  [26] "air"         "all"         "allow"       "almost"      "along"      
##  [31] "already"     "alright"     "also"        "although"    "always"     
##  [36] "america"     "amount"      "and"         "another"     "answer"     
##  [41] "any"         "apart"       "apparent"    "appear"      "apply"      
##  [46] "appoint"     "approach"    "appropriate" "area"        "argue"      
##  [51] "arm"         "around"      "arrange"     "art"         "as"         
##  [56] "ask"         "associate"   "assume"      "at"          "attend"     
##  [61] "authority"   "available"   "aware"       "away"        "awful"      
##  [66] "baby"        "back"        "bad"         "bag"         "ball"       
##  [71] "bank"        "bar"         "basis"       "bear"        "beat"       
##  [76] "beauty"      "bed"         "begin"       "behind"      "benefit"    
##  [81] "best"        "bet"         "between"     "big"         "bill"       
##  [86] "birth"       "bit"         "black"       "blood"       "blow"       
##  [91] "board"       "boat"        "body"        "book"        "both"       
##  [96] "bother"      "bottom"      "box"         "boy"         "break"      
## [101] "brief"       "brilliant"   "bring"       "britain"     "brother"    
## [106] "budget"      "build"       "bus"         "business"    "busy"       
## [111] "but"         "buy"         "by"          "call"        "can"        
## [116] "car"         "card"        "carry"       "cat"         "catch"      
## [121] "cent"        "certain"     "chair"       "chairman"    "chap"       
## [126] "character"   "cheap"       "check"       "child"       "Christ"     
## [131] "Christmas"   "church"      "city"        "claim"       "class"      
## [136] "clean"       "clear"       "client"      "clock"       "closes"     
## [141] "club"        "cold"        "collect"     "colour"      "comment"    
## [146] "commit"      "common"      "community"   "company"     "concern"    
## [151] "condition"   "confer"      "consider"    "consult"     "contact"    
## [156] "contract"    "control"     "cook"        "copy"        "corner"     
## [161] "correct"     "cost"        "could"       "council"     "count"      
## [166] "country"     "county"      "court"       "cover"       "cross"      
## [171] "cup"         "current"     "cut"         "dad"         "danger"     
## [176] "day"         "dead"        "deal"        "dear"        "decision"   
## [181] "deep"        "department"  "depend"      "design"      "detail"     
## [186] "develop"     "difficult"   "dinner"      "direct"      "discuss"    
## [191] "district"    "doctor"      "document"    "dog"         "door"       
## [196] "doubt"       "down"        "draw"        "dress"       "drink"      
## [201] "drop"        "dry"         "during"      "each"        "early"      
## [206] "east"        "easy"        "eat"         "economy"     "educate"    
## [211] "effect"      "egg"         "eight"       "either"      "elect"      
## [216] "electric"    "eleven"      "else"        "employ"      "encourage"  
## [221] "end"         "engine"      "english"     "enjoy"       "enough"     
## [226] "enter"       "environment" "equal"       "especial"    "europe"     
## [231] "even"        "evening"     "ever"        "every"       "evidence"   
## [236] "exact"       "example"     "except"      "excuse"      "exercise"   
## [241] "exist"       "expect"      "expense"     "experience"  "explain"    
## [246] "express"     "extra"       "eye"         "fact"        "fair"       
## [251] "fall"        "family"      "far"         "farm"        "fast"       
## [256] "father"      "favour"      "feed"        "feel"        "few"        
## [261] "field"       "fight"       "fill"        "film"        "final"      
## [266] "find"        "finish"      "first"       "fish"        "fit"        
## [271] "flat"        "floor"       "fly"         "follow"      "food"       
## [276] "foot"        "for"         "forget"      "form"        "forward"    
## [281] "four"        "friday"      "friend"      "from"        "front"      
## [286] "full"        "fun"         "function"    "fund"        "further"    
## [291] "garden"      "gas"         "general"     "germany"     "get"        
## [296] "girl"        "glass"       "god"         "good"        "govern"     
## [301] "grand"       "grant"       "great"       "green"       "ground"     
## [306] "group"       "grow"        "guess"       "guy"         "hair"       
## [311] "half"        "hall"        "hand"        "hang"        "happen"     
## [316] "happy"       "hard"        "head"        "health"      "hear"       
## [321] "heart"       "heat"        "heavy"       "hell"        "help"       
## [326] "high"        "history"     "hit"         "hold"        "holiday"    
## [331] "honest"      "hospital"    "hot"         "hour"        "how"        
## [336] "however"     "hundred"     "husband"     "idea"        "identify"   
## [341] "if"          "imagine"     "important"   "improve"     "in"         
## [346] "include"     "income"      "increase"    "indeed"      "individual" 
## [351] "industry"    "inform"      "inside"      "instead"     "insure"     
## [356] "interest"    "into"        "introduce"   "invest"      "involve"    
## [361] "issue"       "it"          "item"        "jesus"       "job"        
## [366] "join"        "jump"        "just"        "keep"        "key"        
## [371] "kid"         "kill"        "kind"        "king"        "kitchen"    
## [376] "knock"       "know"        "labour"      "lad"         "lady"       
## [381] "land"        "last"        "laugh"       "law"         "lay"        
## [386] "lead"        "learn"       "left"        "leg"         "less"       
## [391] "let"         "letter"      "level"       "light"       "likely"     
## [396] "limit"       "link"        "list"        "listen"      "load"       
## [401] "local"       "lock"        "london"      "long"        "look"       
## [406] "lord"        "lot"         "low"         "luck"        "lunch"      
## [411] "main"        "major"       "man"         "many"        "mark"       
## [416] "market"      "marry"       "match"       "matter"      "may"        
## [421] "mean"        "meaning"     "meet"        "member"      "mention"    
## [426] "might"       "milk"        "million"     "mind"        "minister"   
## [431] "minus"       "miss"        "mister"      "moment"      "monday"     
## [436] "money"       "month"       "morning"     "most"        "mother"     
## [441] "motion"      "mrs"         "much"        "music"       "must"       
## [446] "nation"      "near"        "necessary"   "need"        "never"      
## [451] "new"         "news"        "next"        "night"       "non"        
## [456] "normal"      "north"       "not"         "now"         "number"     
## [461] "obvious"     "occasion"    "odd"         "of"          "off"        
## [466] "offer"       "office"      "often"       "okay"        "old"        
## [471] "on"          "once"        "one"         "only"        "open"       
## [476] "operate"     "opportunity" "oppose"      "or"          "order"      
## [481] "organize"    "original"    "other"       "otherwise"   "ought"      
## [486] "out"         "over"        "own"         "pack"        "paint"      
## [491] "pair"        "paper"       "paragraph"   "pardon"      "parent"     
## [496] "park"        "part"        "particular"  "party"       "pass"       
## [501] "past"        "pay"         "pension"     "per"         "percent"    
## [506] "perfect"     "perhaps"     "period"      "person"      "photograph" 
## [511] "pick"        "plan"        "play"        "plus"        "point"      
## [516] "policy"      "politic"     "poor"        "position"    "post"       
## [521] "pound"       "power"       "present"     "press"       "pretty"     
## [526] "previous"    "print"       "problem"     "proceed"     "process"    
## [531] "product"     "project"     "proper"      "protect"     "public"     
## [536] "pull"        "push"        "put"         "quality"     "quarter"    
## [541] "question"    "quick"       "quid"        "quiet"       "rail"       
## [546] "rather"      "read"        "ready"       "real"        "really"     
## [551] "reason"      "recent"      "reckon"      "recommend"   "record"     
## [556] "red"         "refer"       "regard"      "region"      "relation"   
## [561] "remember"    "report"      "represent"   "research"    "respect"    
## [566] "rest"        "result"      "return"      "rid"         "right"      
## [571] "ring"        "road"        "roll"        "room"        "round"      
## [576] "run"         "saturday"    "say"         "school"      "scotland"   
## [581] "seat"        "second"      "secretary"   "section"     "seem"       
## [586] "self"        "sell"        "send"        "serious"     "set"        
## [591] "seven"       "sex"         "shall"       "sheet"       "shoot"      
## [596] "shop"        "short"       "should"      "show"        "shut"       
## [601] "sick"        "sign"        "similar"     "sing"        "sir"        
## [606] "sister"      "sit"         "six"         "sleep"       "slight"     
## [611] "slow"        "small"       "social"      "society"     "son"        
## [616] "soon"        "sorry"       "sort"        "sound"       "south"      
## [621] "speak"       "special"     "specific"    "speed"       "spell"      
## [626] "spend"       "staff"       "stairs"      "stand"       "standard"   
## [631] "start"       "station"     "stay"        "step"        "stick"      
## [636] "still"       "stop"        "story"       "straight"    "strategy"   
## [641] "street"      "strong"      "student"     "study"       "stuff"      
## [646] "stupid"      "subject"     "succeed"     "such"        "sudden"     
## [651] "suggest"     "suit"        "summer"      "sun"         "sunday"     
## [656] "supply"      "support"     "switch"      "system"      "talk"       
## [661] "tax"         "teach"       "team"        "television"  "tell"       
## [666] "ten"         "tend"        "term"        "test"        "than"       
## [671] "thank"       "then"        "they"        "thing"       "think"      
## [676] "thirteen"    "thirty"      "this"        "though"      "thousand"   
## [681] "through"     "throw"       "thursday"    "today"       "together"   
## [686] "tomorrow"    "tonight"     "top"         "total"       "touch"      
## [691] "toward"      "town"        "traffic"     "train"       "transport"  
## [696] "travel"      "treat"       "trust"       "try"         "tuesday"    
## [701] "turn"        "twenty"      "under"       "understand"  "union"      
## [706] "unit"        "unite"       "university"  "unless"      "until"      
## [711] "up"          "upon"        "use"         "usual"       "various"    
## [716] "very"        "view"        "visit"       "wait"        "walk"       
## [721] "wall"        "want"        "war"         "warm"        "wash"       
## [726] "watch"       "water"       "way"         "wear"        "wednesday"  
## [731] "week"        "weigh"       "well"        "west"        "what"       
## [736] "when"        "whether"     "which"       "why"         "will"       
## [741] "win"         "wind"        "window"      "wish"        "with"       
## [746] "within"      "without"     "woman"       "wonder"      "wood"       
## [751] "word"        "work"        "world"       "worry"       "worth"      
## [756] "would"       "wrong"       "year"        "yes"         "yesterday"  
## [761] "yet"         "young"
```

```r
i <- str_detect(words, "^[aeiou]")
j <- str_detect(words, "[^aeiou]$")
words[i&j] %>% 
      head()
```

```
## [1] "about"   "accept"  "account" "across"  "act"     "actual"
```

 - Are there any words that contain at least one of each different vowel?
 

```r
words[str_detect(words, "a") &
      str_detect(words, "e") &
      str_detect(words, "i") &
      str_detect(words, "o") &
      str_detect(words, "u")]
```

```
## character(0)
```

2. What word has the highest number of vowels? What word has the highest proportion of vowels? 

```r
## Highest number of vowels
max(str_count(words, "[aeiou]"))
```

```
## [1] 5
```

```r
## Get proportions - than extract the largest value
proportion <- max(str_count(words, "[aeiou]"))/str_length(words)
words[which(proportion == max(proportion))] ##??????
```

```
## [1] "a"
```

### 14.4.3.1 Exercises

*1. In the previous example, you might have noticed that the regular expression matched “flickered”, which is not a colour. Modify the regex to fix the problem.*


*2. From the Harvard sentences data, extract:*

 - The first word from each sentence: The, Glue, It, These, Rice, The, The, The, Four, Large

 - All words ending in ing: 

```r
pattern <- "\\b[A-Za-z]+ing\\b"
ing <- str_detect(sentences, pattern)
str_extract_all(sentences[ing], pattern, simplify = TRUE) %>% head(10)
```

```
##       [,1]     
##  [1,] "spring" 
##  [2,] "evening"
##  [3,] "morning"
##  [4,] "winding"
##  [5,] "living" 
##  [6,] "king"   
##  [7,] "Adding" 
##  [8,] "making" 
##  [9,] "raging" 
## [10,] "playing"
```
 
 - All plurals.

```r
pattern <- "\\b[A-Za-z]{3,}[^s]s\\b"
plural <- str_detect(sentences, pattern)
str_extract(sentences[plural], pattern) %>% head(10)
```

```
##  [1] "planks"    "bowls"     "lemons"    "hours"     "stockings"
##  [6] "helps"     "fires"     "man's"     "bonds"     "pants"
```

### 14.4.4.1 Exercises
*1. Find all words that come after a “number” like “one”, “two”, “three” etc. Pull out both the number and the word.*


```r
pattern <- "(one|two|three|four|five|six|seven|eight|nine|ten)\\s\\w+"
number <- str_detect(sentences, pattern = pattern)
str_extract(sentences[number], pattern) %>% head(10)
```

```
##  [1] "ten served"  "one over"    "seven books" "two met"     "two factors"
##  [6] "one and"     "three lists" "seven is"    "two when"    "one floor"
```

*2. Find all contractions. Separate out the pieces before and after the apostrophe.*
I couldn't figure out how to keep the apostrophe using str_split()

```r
pattern <- "[A-Za-z]*'[a-z]+"
x <- str_detect(sentences, pattern = pattern)
seperated <- str_extract(sentences[x], pattern)
unlist(str_split(seperated, pattern = "'"))
```

```
##  [1] "It"       "s"        "man"      "s"        "don"      "t"       
##  [7] "store"    "s"        "workmen"  "s"        "Let"      "s"       
## [13] "sun"      "s"        "child"    "s"        "king"     "s"       
## [19] "It"       "s"        "don"      "t"        "queen"    "s"       
## [25] "don"      "t"        "pirate"   "s"        "neighbor" "s"
```

```r
unlist(strsplit(seperated, "(?=')", perl = T))
```

```
##  [1] "It"       "'"        "s"        "man"      "'"        "s"       
##  [7] "don"      "'"        "t"        "store"    "'"        "s"       
## [13] "workmen"  "'"        "s"        "Let"      "'"        "s"       
## [19] "sun"      "'"        "s"        "child"    "'"        "s"       
## [25] "king"     "'"        "s"        "It"       "'"        "s"       
## [31] "don"      "'"        "t"        "queen"    "'"        "s"       
## [37] "don"      "'"        "t"        "pirate"   "'"        "s"       
## [43] "neighbor" "'"        "s"
```
### 14.4.5.1 Exercises

*1. Replace all forward slashes in a string with backslashes.*
This would only work if I used 4 backslashed 


```r
x <- "h/e/l/l/o"
x
```

```
## [1] "h/e/l/l/o"
```

```r
str_replace_all(x, "/", "\\")
```

```
## [1] "hello"
```

```r
str_replace_all(x, "/", "\\\\")
```

```
## [1] "h\\e\\l\\l\\o"
```

*2. Implement a simple version of str_to_lower() using replace_all().*

```r
x <- "abcd"
x
```

```
## [1] "abcd"
```

```r
str_replace_all(x, c("a" = "A", "b" = "B", "c" = "C", "d" = "D"))
```

```
## [1] "ABCD"
```

*3. Switch the first and last letters in words. Which of those strings are still words?*

```r
str_replace(words, 
            str_sub(words,1,1),str_sub(words, -1)) %>% 
      head(10)
```

```
##  [1] "a"        "eble"     "tbout"    "ebsolute" "tccept"   "tccount" 
##  [7] "echieve"  "scross"   "tct"      "ective"
```


### 14.4.6.1 Exercises

*1. Split up a string like "apples, pears, and bananas" into individual components.*

```r
x <- c("apples, pears, and bananas")
str_split(x, ", +(and +)?")[[1]]
```

```
## [1] "apples"  "pears"   "bananas"
```

*2. Why is it better to split up by boundary("word") than " "?*
Spliiting by boundary("word") will remove whitespace and any special characters and puncutation, where as " " will only remove a space.

*3. What does splitting with an empty string ("") do? Experiment, and then read the documentation.*

```r
"a|b|c|d" %>% 
  str_split("\\|") %>% 
  .[[1]]
```

```
## [1] "a" "b" "c" "d"
```

```r
"a|b|c|d" %>% 
  str_split("") %>% 
  .[[1]]
```

```
## [1] "a" "|" "b" "|" "c" "|" "d"
```

It seems to split the string by every character.

# 14.5 Other types of pattern

### 14.5.1 Exercises

*1. How would you find all strings containing \ with regex() vs. with fixed()?*


```r
x <- "a\\b"
str_subset(x, "\\\\")
```

```
## [1] "a\\b"
```

*2. What are the five most common words in sentences?*

```r
all_words <- unlist(str_extract_all(sentences, boundary("word")))

all_words %>% 
      table() %>% 
      sort(decreasing=T) %>% 
      head(5)
```

```
## .
## the The  of   a  to 
## 489 262 132 130 119
```


# 14.6 Other uses of regular expressions

# 14.7 stringi

### 14.7.1 Exercises

*1. Find the stringi functions that:*

 - Count the number of words: 
      - stri_count_boundaries(test, type="word")
      - stri_count_words()
 - Find duplicated strings: stri_duplicated()
       - stri_duplicated(), stri_duplicated_any()
 - Generate random text
       - stri_rand_shuffle(str)
       - stri_rand_lipsum(nparagraphs, start_lipsum = TRUE)

*2. How do you control the language that stri_sort() uses for sorting?*
stri_order(str, decreasing = FALSE, na_last = TRUE, opts_collator = NULL)
