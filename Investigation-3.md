``` r
# reading in the data
paste0("https://raw.githubusercontent.com/mathbeveridge/gameofthrones/master/data/got-s",1:8,"-edges.csv") -> edges
purrr::map(edges,read_csv) -> edges_tbl
```

    ## Parsed with column specification:
    ## cols(
    ##   Source = col_character(),
    ##   Target = col_character(),
    ##   Weight = col_double(),
    ##   Season = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   Source = col_character(),
    ##   Target = col_character(),
    ##   Weight = col_double(),
    ##   Season = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   Source = col_character(),
    ##   Target = col_character(),
    ##   Weight = col_double(),
    ##   Season = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   Source = col_character(),
    ##   Target = col_character(),
    ##   Weight = col_double(),
    ##   Season = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   Source = col_character(),
    ##   Target = col_character(),
    ##   Weight = col_double(),
    ##   Season = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   Source = col_character(),
    ##   Target = col_character(),
    ##   Weight = col_double(),
    ##   Season = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   Source = col_character(),
    ##   Target = col_character(),
    ##   Weight = col_double(),
    ##   Season = col_double()
    ## )
    ## Parsed with column specification:
    ## cols(
    ##   Source = col_character(),
    ##   Target = col_character(),
    ##   Weight = col_double(),
    ##   Season = col_double()
    ## )

I’ve never watched Game of Thrones, so I want to get an idea of
prominent characters based on degree centrality through each season.

## Season 1

``` r
# season 1
s1 <- graph_from_data_frame(edges_tbl[1])

# proportional to degree centrality
plot(s1, main = "Season 1: Proportional to Degree",
     vertex.size = igraph::degree(s1)/10,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.67)
```

![](Investigation-3_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
Tyrion, Ned, Robb, and Robert seem to have high degree centrality, as
they have a lot of links to other characters. I’ll keep an eye on these
names going forward as I will assume that they are vital characters
throughout the show.

## Season 2

``` r
# season 2
s2 <- graph_from_data_frame(edges_tbl[2])

# proportional to degree centrality
plot(s2, main = "Season 2: Proportional to Degree",
     vertex.size = igraph::degree(s2)/5,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.67)
```

![](Investigation-3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- --> I
see Tyrion and Robb again, in addition to some new names: Joffrey,
Tywin, Arya, and Cersei. Ned and Robert are still around, but don’t seem
to have as big of an influence as before. Possible that they died? Maybe
they fell off and simply aren’t as prevalent anymore.

## Season 3

``` r
# season 3
s3 <- graph_from_data_frame(edges_tbl[3])

# proportional to degree centrality
plot(s3, main = "Season 3: Proportional to Degree",
     vertex.size = igraph::degree(s3)/5,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.67)
```

![](Investigation-3_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
Tyrion and Tywin are remaining involved, but I noticed Robb, Catelyn,
and Jon start to emerge into the third season. Catelyn and Robb are
particularly close to each other (potential relationship?). Let’s look
out for those three in season 4.

## Season 4

``` r
# season 4
s4 <- graph_from_data_frame(edges_tbl[4])

# proportional to degree centrality
plot(s4, main = "Season 4: Proportional to Degree",
     vertex.size = igraph::degree(s4)/5,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.67)
```

![](Investigation-3_files/figure-gfm/unnamed-chunk-5-1.png)<!-- --> Jon
is still around, and Sam joins in with a seemingly integral role. Sansa,
Joffrey, Cersei, Jamie, Tywin, Tyrion, and Daenerys seem to have
important involvement. Robb practically disappears, as well as Catelyn,
which leads me to believe they either both died, or had diminished
involvement in this season for whatever reason.

## Season 5

``` r
# season 5
s5 <- graph_from_data_frame(edges_tbl[5])

# proportional to degree centrality
plot(s5, main = "Season 5: Proportional to Degree",
     vertex.size = igraph::degree(s5)/5,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.67)
```

![](Investigation-3_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
Littlefinger and Stannis are names I haven’t seen stick out in the
previous plots, so I’ll assume they were either introduced this season,
or were granted heavier involvement this season. Tyrion, Sansa, and
Cersei still stand out as they have been.

## Season 6

``` r
# season 6
s6 <- graph_from_data_frame(edges_tbl[6])

# proportional to degree centrality
plot(s6, main = "Season 6: Proportional to Degree",
     vertex.size = igraph::degree(s6)/5,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.67)
```

![](Investigation-3_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
Can’t find Littlefinger on here anymore; did he die? However, I still
do see Tyrion, Cersei, and Sansa. In particular, Sansa seems to have an
even larger role in this season compared to past seasons.

## Season 7

``` r
# season 7
s7 <- graph_from_data_frame(edges_tbl[7])

# proportional to degree centrality
plot(s7, main = "Season 7: Proportional to Degree",
     vertex.size = igraph::degree(s7)/5,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.67)
```

![](Investigation-3_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
Littlefinger pops back up again, so maybe I was quick to assume he died.
Perhaps he did die, but maybe he had some sort of hidden plot or
background that caused him to be brought back into the discussion. Jon
plays a bigger role in season 7, even outweighing Tyrion and Cersei (who
seemed to be prominent in each season). Davos and Theon are newer names
that I hadn’t noticed in the previous plots.

## Season 8

``` r
# season 8
s8 <- graph_from_data_frame(edges_tbl[8])

# proportional to degree centrality
plot(s8, main = "Season 8: Proportional to Degree",
     vertex.size = igraph::degree(s8)/5,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.67)
```

![](Investigation-3_files/figure-gfm/unnamed-chunk-9-1.png)<!-- --> Lots
of people are heavily involved, some of which are familiar faces from
the previous plots (e.g. Tyrion, Jaime, Daenerys).

So, I heard that at the end of GOT a lot of the remaining characters
die. I presume that there are a lot more people with higher degree
centralities in this plot because of some sort of way to tie together
all of the remaining prominent characters in a season finale. As a
result, you would expect an increase in nodal size for many of the
characters as their involvement likely increases, which in turn drives
up their degree centrality.
