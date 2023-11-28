Extinctions Unit
================
Caroline Wichterman, Megna Reddy

``` r
download.file("https://github.com/espm-157/extinction-template/releases/download/data2.0/extinction-data.zip", "extinction-data.zip")
unzip("extinction-data.zip")
```

``` r
base <- "https://apiv3.iucnredlist.org/api/v3"
endpoint <- "speciescount"
token <- "9bb4facb6d23f48efbf424bb05c0c1ef1cf6f468393bc745d42179ac4aca5fee"
url <- glue("{base}/{endpoint}?token={token}")

req <- GET(url)
x <- content(req)
x$speciescount
```

    [1] "150388"

``` r
species_endpoint <- "species"
page <- paste0("page/", 0:15)
all_pages <- glue("{base}/{species_endpoint}/{page}?token={token}")
```

``` r
if(!file.exists("all_species.rds")) {
  all_species <- map(all_pages, GET, .progress = TRUE)
  
  status <- map_int(all_species, status_code)
  stopifnot(all(status < 400))
  write_rds(all_species, "all_species.rds")
}
 all_species <- read_rds("all_species.rds")
```

``` r
all_resp <- map(all_species, content, encoding = "UTF-8")
```

``` r
sci_name <-
  map(all_resp, \(page) map_chr(page$result, "scientific_name")) |>
  list_c()

category <-
  map(all_resp, \(page) map_chr(page$result, "category")) |>
  list_c()

class <- 
  map(all_resp, \(page) map_chr(page$result, "class_name")) |>
  list_c()

all_species <- tibble(sci_name, category, class)
```

``` r
extinct_species <-
  all_species |>
  filter(category == "EX")
```

``` r
ext_sci_name <- extinct_species$sci_name
ex_urls <- glue("{base}/species/narrative/{ext_sci_name}?token={token}") |>
  URLencode()
```

``` r
if(!file.exists("ex_narrative.rds")) {
  ex_narrative <- map(ex_urls, GET,  .progress = TRUE)
  
  status <- map_int(ex_narrative, status_code)
  stopifnot(all(status < 400))
  
  write_rds(ex_narrative, "ex_narrative.rds")
}

ex_narrative <- read_rds("ex_narrative.rds")
```

``` r
narrative_contents <- map(ex_narrative, content)

narrative_population <- map(narrative_contents, 
                            \(x) map_chr(x$result[1], "population", .default = ""))

narrative_rationale <- map(narrative_contents, \(x) x$result[[1]]$rationale)
```

``` r
last_seen <- narrative_population |> 
  map_chr(str_extract, "\\d{4}") |> 
  as.integer()
```

``` r
extinction_dates <- 
  tibble(sci_name = ext_sci_name, last_seen) |> 
  distinct()

combined <- all_species |> left_join(extinction_dates)
```

    Joining with `by = join_by(sci_name)`

``` r
total_sp <- combined |> 
  filter(class %in% c("MAMMALIA", "AVES", "AMPHIBIA", "REPTILIA", "ACTINOPTERYGII")) |>
  count(class, name = "total")
total_sp
```

    # A tibble: 5 × 2
      class          total
      <chr>          <int>
    1 ACTINOPTERYGII 24223
    2 AMPHIBIA        7487
    3 AVES           11188
    4 MAMMALIA        6427
    5 REPTILIA       10283

``` r
final_tbl <- 
  combined |> 
  filter(category == "EX") |>
  filter(class %in% c("MAMMALIA", "AVES", "AMPHIBIA", "REPTILIA", "ACTINOPTERYGII")) |>
  mutate(last_seen = replace_na(last_seen, 2023),
         century =  str_extract(last_seen, "\\d{2}")) |>
  count(century, class) |> 
  left_join(total_sp) |>
  mutate(extinction_msy = n / total * 10000) # extinctions per million-species-years
```

    Joining with `by = join_by(class)`

``` r
 final_tbl 
```

    # A tibble: 20 × 5
       century class              n total extinction_msy
       <chr>   <chr>          <int> <int>          <dbl>
     1 14      MAMMALIA           1  6427          1.56 
     2 15      MAMMALIA           3  6427          4.67 
     3 16      AVES               1 11188          0.894
     4 17      AVES               1 11188          0.894
     5 17      MAMMALIA           1  6427          1.56 
     6 18      ACTINOPTERYGII     4 24223          1.65 
     7 18      AMPHIBIA           9  7487         12.0  
     8 18      AVES               4 11188          3.58 
     9 18      MAMMALIA          10  6427         15.6  
    10 18      REPTILIA          11 10283         10.7  
    11 19      ACTINOPTERYGII    51 24223         21.1  
    12 19      AMPHIBIA          21  7487         28.0  
    13 19      AVES               6 11188          5.36 
    14 19      MAMMALIA          26  6427         40.5  
    15 19      REPTILIA           8 10283          7.78 
    16 20      ACTINOPTERYGII    32 24223         13.2  
    17 20      AMPHIBIA           6  7487          8.01 
    18 20      AVES             147 11188        131.   
    19 20      MAMMALIA          53  6427         82.5  
    20 20      REPTILIA          13 10283         12.6  

## Extinctions Module

*Are we experiencing the sixth great extinction?*

What is the current pace of extinction? Is it accelerating? How does it
compare to background extinction rates?

## Background

-   [Section Intro Video](https://youtu.be/QsH6ytm89GI)
-   [Ceballos et al (2015)](http://doi.org/10.1126/sciadv.1400253)

Our focal task will be to reproduce the result from Ceballos and
colleagues showing the recent increase in extinction rates relative to
the background rate:

![](https://espm-157.carlboettiger.info/img/extinctions.jpg)

``` r
#ggplot(data = df, aes(x = total_sp, y = extinction_dates)) + 
  #geom_line()
```

``` r
##birds_total <- birds <- all_species %>% 
 ##filter(class == "AVES") %>% mutate(group = "Birds")

##mammal_total <- mammals <- all_species %>% 
 ##filter(class == "MAMMALIA") %>% mutate(group = "Mammals")

##vert_total <- vertebrate <- all_species %>% 
 ##filter(class %in% vertebrae) %>% mutate(group = "Vertebrates")

##other_total <- all_others <- all_species %>% 
 ##filter(phylum == "CHORDATA") %>% 
 ##filter(!class %in% c("MAMMALIA", "AVES")) %>% 
 ##mutate(group = "Other Vertebrates")
```

``` r
#ggplot(data = df, aes(total_sp, extinction_dates)) + geom_point()
```

``` r
##df %>% 
  ##ggplot(aes(x = century), group = group, color = group) +
 ## geom_line(aes(y = extinction_msy)) +
 ## labs(title = "Cumulative Vertebrate Species as EX or EW by the IUCN 2019",
  ##  y = 'Cumulative extinctions as % of UCN-evaluated species',
  ##  x = "Time interval") + 
##scale_x_discrete(labels=c("1500-1600", "1600-1700", "1700-1800", "1800-1900", "1900-2000")) +
##scale_y_continuous(breaks = seq(0,2, by=0.2)) +
##theme_classic() +
##theme(text = element_text(size=10))
```

## Computational Topics

-   Accessing data from a RESTful API
-   Error handling
-   JSON data format
-   Regular expressions
-   Working with missing values

## Additional references:

-   <http://www.hhmi.org/biointeractive/biodiversity-age-humans> (Video)
-   [Barnosky et al. (2011)](http://doi.org/10.1038/nature09678)
-   [Pimm et al (2014)](http://doi.org/10.1126/science.1246752)
-   [Sandom et al (2014)](http://dx.doi.org/10.1098/rspb.2013.3254)
