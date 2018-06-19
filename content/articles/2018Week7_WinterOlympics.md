---
title: The Winter Olympics: MakeOverMonday 2018 Week7
author: Aditya Mangal
date: 2018-2-15 17:51
Category: MakeOverMonday
Tags: R, Visualizations, MakeOverMonday, DataAnalysis, DataScience, Olympics
output: html_document
---


Goal of this Visualization task is to create an alternative visualization to the <a href="https://public.tableau.com/views/TheWinterOlympics/TheWinterOlympics?:embed=y&:showVizHome=no" target="_blank">Tableau's visualization for Winter Olympics data for different countries.</a>

In this blog post, I'm trying to generate a World Choropleth Map showing the total counts of medals for each country.

## Cleaning up workspace and loading required libraries


```r
rm(list = ls())
```


```r
library(tidyverse) #Data Wrangling
library(readxl) #Data Ingestion
library(ggplot2) #Data Visualization
library(leaflet)
library(rgeos)
library(rgdal)
library(stringr)
```
## Obtaining Data

Reading and viewing the dataset

```r
olympics = read_excel("/tmp/Winer Olympic Medals.xlsx")
```

```r
olympics
```

```
## # A tibble: 2,865 x 9
##     Year Sport   Event  Country Gender `Medal Rank` Medal `Name of Athlet…
##    <dbl> <chr>   <chr>  <chr>   <chr>         <dbl> <chr> <chr>           
##  1  1924 Bobsled Men's… Switze… Men               1 gold  Switzerland-1   
##  2  1924 Bobsled Men's… Britain Men               2 silv… Britain-1       
##  3  1924 Bobsled Men's… Belgium Men               3 bron… Belgium-1       
##  4  1924 Cross-… Men's… Norway  Men               1 gold  Thorleif Haug   
##  5  1924 Cross-… Men's… Norway  Men               2 silv… Johan GrÃ¸ttums…
##  6  1924 Cross-… Men's… Finland Men               3 bron… Tapani Niku     
##  7  1924 Cross-… Men's… Norway  Men               1 gold  Thorleif Haug   
##  8  1924 Cross-… Men's… Norway  Men               2 silv… Thoralf StrÃ¸ms…
##  9  1924 Cross-… Men's… Norway  Men               3 bron… Johan GrÃ¸ttums…
## 10  1924 Curling Men's… Britain Men               1 gold  Britain         
## # ... with 2,855 more rows, and 1 more variable: `Age of Athlete` <dbl>
```

Summarizing and getting stats to better understand the dataset


```r
olympics %>% 
  glimpse()
```

```
## Observations: 2,865
## Variables: 9
## $ Year                      <dbl> 1924, 1924, 1924, 1924, 1924, 1924, ...
## $ Sport                     <chr> "Bobsled", "Bobsled", "Bobsled", "Cr...
## $ Event                     <chr> "Men's Four/Five", "Men's Four/Five"...
## $ Country                   <chr> "Switzerland", "Britain", "Belgium",...
## $ Gender                    <chr> "Men", "Men", "Men", "Men", "Men", "...
## $ `Medal Rank`              <dbl> 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, ...
## $ Medal                     <chr> "gold", "silver", "bronze", "gold", ...
## $ `Name of Athlete or Team` <chr> "Switzerland-1", "Britain-1", "Belgi...
## $ `Age of Athlete`          <dbl> NA, NA, NA, 29, 24, 28, 29, 27, 24, ...
```

```r
olympics %>% 
  summary()
```

```
##       Year         Sport              Event             Country         
##  Min.   :1924   Length:2865        Length:2865        Length:2865       
##  1st Qu.:1972   Class :character   Class :character   Class :character  
##  Median :1992   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :1986                                                           
##  3rd Qu.:2006                                                           
##  Max.   :2014                                                           
##                                                                         
##     Gender            Medal Rank       Medal          
##  Length:2865        Min.   :1.000   Length:2865       
##  Class :character   1st Qu.:1.000   Class :character  
##  Mode  :character   Median :2.000   Mode  :character  
##                     Mean   :1.996                     
##                     3rd Qu.:3.000                     
##                     Max.   :3.000                     
##                                                       
##  Name of Athlete or Team Age of Athlete 
##  Length:2865             Min.   :14.00  
##  Class :character        1st Qu.:22.00  
##  Mode  :character        Median :25.00  
##                          Mean   :25.15  
##                          3rd Qu.:28.00  
##                          Max.   :42.00  
##                          NA's   :692
```

## Scrubbing data

As per the dataset requirement, East and West Germany are to be grouped with Germany and Soviet Union and the 1992 Unified Team needs to be combined with Russia


```r
olympics = olympics %>% 
  mutate(Country = recode(Country, "Soviet Union" = "Russia", "Unified Team" = "Russia",
                          "East Germany" = "Germany", "West Germany" = "Germany"))
```

Reading in the ISO-3166 country codes dataset in order to generate the choropleth


```r
countryCodes = read_csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv")
```

```r
countryCodes
```

```
## # A tibble: 249 x 11
##    name            `alpha-2` `alpha-3` `country-code` `iso_3166-2` region 
##    <chr>           <chr>     <chr>     <chr>          <chr>        <chr>  
##  1 Afghanistan     AF        AFG       004            ISO 3166-2:… Asia   
##  2 Åland Islands   AX        ALA       248            ISO 3166-2:… Europe 
##  3 Albania         AL        ALB       008            ISO 3166-2:… Europe 
##  4 Algeria         DZ        DZA       012            ISO 3166-2:… Africa 
##  5 American Samoa  AS        ASM       016            ISO 3166-2:… Oceania
##  6 Andorra         AD        AND       020            ISO 3166-2:… Europe 
##  7 Angola          AO        AGO       024            ISO 3166-2:… Africa 
##  8 Anguilla        AI        AIA       660            ISO 3166-2:… Americ…
##  9 Antarctica      AQ        ATA       010            ISO 3166-2:… <NA>   
## 10 Antigua and Ba… AG        ATG       028            ISO 3166-2:… Americ…
## # ... with 239 more rows, and 5 more variables: `sub-region` <chr>,
## #   `intermediate-region` <chr>, `region-code` <chr>,
## #   `sub-region-code` <chr>, `intermediate-region-code` <chr>
```

Joining the 2 datasets and verifying if any country name mismatch happening in the 2.


```r
olympics %>% 
  left_join(countryCodes, by=c("Country" = "name")) %>% 
  filter(is.na(`alpha-3`)) %>% 
  select(Country) %>% 
  unique()
```

```
## # A tibble: 8 x 1
##   Country       
##   <chr>         
## 1 Britain       
## 2 United States 
## 3 Czechoslovakia
## 4 Russia        
## 5 North Korea   
## 6 Yugoslavia    
## 7 South Korea   
## 8 Czech Republic
```

Looks like above 7 countries do not have a corresponding entry in the countryCodes dataset.
Lets try to find out the corresponding names for each of the 7 in the countryCodes dataset.


```r
countryCodes %>% 
  filter(str_detect(str_to_lower(name), "britain")) #United Kingdom of Great Britain and Northern Ireland
```

```
## # A tibble: 1 x 11
##   name              `alpha-2` `alpha-3` `country-code` `iso_3166-2` region
##   <chr>             <chr>     <chr>     <chr>          <chr>        <chr> 
## 1 United Kingdom o… GB        GBR       826            ISO 3166-2:… Europe
## # ... with 5 more variables: `sub-region` <chr>,
## #   `intermediate-region` <chr>, `region-code` <chr>,
## #   `sub-region-code` <chr>, `intermediate-region-code` <chr>
```

```r
countryCodes %>% 
  filter(str_detect(str_to_lower(name), "states")) #United States of America
```

```
## # A tibble: 3 x 11
##   name             `alpha-2` `alpha-3` `country-code` `iso_3166-2`  region
##   <chr>            <chr>     <chr>     <chr>          <chr>         <chr> 
## 1 Micronesia (Fed… FM        FSM       583            ISO 3166-2:FM Ocean…
## 2 United States o… US        USA       840            ISO 3166-2:US Ameri…
## 3 United States M… UM        UMI       581            ISO 3166-2:UM Ocean…
## # ... with 5 more variables: `sub-region` <chr>,
## #   `intermediate-region` <chr>, `region-code` <chr>,
## #   `sub-region-code` <chr>, `intermediate-region-code` <chr>
```

```r
countryCodes %>% 
  filter(str_detect(str_to_lower(name), "czech")) #Czech Republic
```

```
## # A tibble: 1 x 11
##   name    `alpha-2` `alpha-3` `country-code` `iso_3166-2`  region
##   <chr>   <chr>     <chr>     <chr>          <chr>         <chr> 
## 1 Czechia CZ        CZE       203            ISO 3166-2:CZ Europe
## # ... with 5 more variables: `sub-region` <chr>,
## #   `intermediate-region` <chr>, `region-code` <chr>,
## #   `sub-region-code` <chr>, `intermediate-region-code` <chr>
```

```r
countryCodes %>% 
  filter(str_detect(str_to_lower(name), "russia")) #Russian Federation
```

```
## # A tibble: 1 x 11
##   name              `alpha-2` `alpha-3` `country-code` `iso_3166-2` region
##   <chr>             <chr>     <chr>     <chr>          <chr>        <chr> 
## 1 Russian Federati… RU        RUS       643            ISO 3166-2:… Europe
## # ... with 5 more variables: `sub-region` <chr>,
## #   `intermediate-region` <chr>, `region-code` <chr>,
## #   `sub-region-code` <chr>, `intermediate-region-code` <chr>
```

```r
countryCodes %>% 
  filter(str_detect(str_to_lower(name), "korea")) #Korea (Democratic People's Republic of) = North Korea, Korea (Republic of) = South Korea
```

```
## # A tibble: 2 x 11
##   name              `alpha-2` `alpha-3` `country-code` `iso_3166-2` region
##   <chr>             <chr>     <chr>     <chr>          <chr>        <chr> 
## 1 Korea (Democrati… KP        PRK       408            ISO 3166-2:… Asia  
## 2 Korea (Republic … KR        KOR       410            ISO 3166-2:… Asia  
## # ... with 5 more variables: `sub-region` <chr>,
## #   `intermediate-region` <chr>, `region-code` <chr>,
## #   `sub-region-code` <chr>, `intermediate-region-code` <chr>
```

```r
countryCodes %>% 
  filter(str_detect(str_to_lower(name), "yugo")) #Macedonia (the former Yugoslav Republic of)
```

```
## # A tibble: 1 x 11
##   name              `alpha-2` `alpha-3` `country-code` `iso_3166-2` region
##   <chr>             <chr>     <chr>     <chr>          <chr>        <chr> 
## 1 Macedonia (the f… MK        MKD       807            ISO 3166-2:… Europe
## # ... with 5 more variables: `sub-region` <chr>,
## #   `intermediate-region` <chr>, `region-code` <chr>,
## #   `sub-region-code` <chr>, `intermediate-region-code` <chr>
```

Renaming mismatched countries in olympic dataset based on countryCodes dataset.


```r
olympics = olympics %>% 
  mutate(Country = recode(Country,
                          "Britain" = "United Kingdom of Great Britain and Northern Ireland",
                          "United States" = "United States of America",
                          "Czechoslovakia" = "Czech Republic",
                          "Russia" = "Russian Federation",
                          "North Korea" = "Korea (Democratic People's Republic of)",
                          "South Korea" = "Korea (Republic of)",
                          "Yugoslavia" = "Macedonia (the former Yugoslav Republic of)"))
```

Joining and viewing the 2 datasets


```r
olympics = olympics %>% 
  left_join(countryCodes, by=c("Country" = "name"))
olympics
```

```
## # A tibble: 2,865 x 19
##     Year Sport  Event  Country  Gender `Medal Rank` Medal `Name of Athlet…
##    <dbl> <chr>  <chr>  <chr>    <chr>         <dbl> <chr> <chr>           
##  1  1924 Bobsl… Men's… Switzer… Men               1 gold  Switzerland-1   
##  2  1924 Bobsl… Men's… United … Men               2 silv… Britain-1       
##  3  1924 Bobsl… Men's… Belgium  Men               3 bron… Belgium-1       
##  4  1924 Cross… Men's… Norway   Men               1 gold  Thorleif Haug   
##  5  1924 Cross… Men's… Norway   Men               2 silv… Johan GrÃ¸ttums…
##  6  1924 Cross… Men's… Finland  Men               3 bron… Tapani Niku     
##  7  1924 Cross… Men's… Norway   Men               1 gold  Thorleif Haug   
##  8  1924 Cross… Men's… Norway   Men               2 silv… Thoralf StrÃ¸ms…
##  9  1924 Cross… Men's… Norway   Men               3 bron… Johan GrÃ¸ttums…
## 10  1924 Curli… Men's… United … Men               1 gold  Britain         
## # ... with 2,855 more rows, and 11 more variables: `Age of Athlete` <dbl>,
## #   `alpha-2` <chr>, `alpha-3` <chr>, `country-code` <chr>,
## #   `iso_3166-2` <chr>, region <chr>, `sub-region` <chr>,
## #   `intermediate-region` <chr>, `region-code` <chr>,
## #   `sub-region-code` <chr>, `intermediate-region-code` <chr>
```

Aggregating per country to find the total number of medals for each country and its corresponding alpha-3 code.


```r
TotalMedalsPerCountry = olympics %>% 
  group_by(Country, `alpha-3`) %>% 
  summarise(TotalMedals = n()) %>% 
  rename(Code = `alpha-3`)
TotalMedalsPerCountry
```

```
## # A tibble: 39 x 3
## # Groups:   Country [39]
##    Country        Code  TotalMedals
##    <chr>          <chr>       <int>
##  1 Australia      AUS            12
##  2 Austria        AUT           218
##  3 Belarus        BLR            15
##  4 Belgium        BEL             5
##  5 Bulgaria       BGR             6
##  6 Canada         CAN           170
##  7 China          CHN            53
##  8 Croatia        HRV            11
##  9 Czech Republic <NA>           49
## 10 Denmark        DNK             1
## # ... with 29 more rows
```

Lets see the top countries based on total number of medals


```r
TotalMedalsPerCountry %>% 
  arrange(desc(TotalMedals))
```

```
## # A tibble: 39 x 3
## # Groups:   Country [39]
##    Country                  Code  TotalMedals
##    <chr>                    <chr>       <int>
##  1 Germany                  DEU           377
##  2 Russian Federation       RUS           341
##  3 Norway                   NOR           329
##  4 United States of America USA           282
##  5 Austria                  AUT           218
##  6 Canada                   CAN           170
##  7 Finland                  FIN           161
##  8 Sweden                   SWE           144
##  9 Switzerland              CHE           138
## 10 Italy                    ITA           114
## # ... with 29 more rows
```

Germany obtained the most number of medals (377) closely followed by Russia with (341)

## Exploring Data

Lets plot the above data on a map using leaflet.

Loading shape file data set from [World Borders Dataset](http://thematicmapping.org/downloads/world_borders.php). 


```r
shape = readOGR("~/Downloads/TM_WORLD_BORDERS_SIMPL-0.3/TM_WORLD_BORDERS_SIMPL-0.3.shp")
```

```
## OGR data source with driver: ESRI Shapefile 
## Source: "/Users/amangal/Downloads/TM_WORLD_BORDERS_SIMPL-0.3/TM_WORLD_BORDERS_SIMPL-0.3.shp", layer: "TM_WORLD_BORDERS_SIMPL-0.3"
## with 246 features
## It has 11 fields
## Integer64 fields read as strings:  POP2005
```

```r
names(shape)
```

```
##  [1] "FIPS"      "ISO2"      "ISO3"      "UN"        "NAME"     
##  [6] "AREA"      "POP2005"   "REGION"    "SUBREGION" "LON"      
## [11] "LAT"
```


```r
TotalMedalsPerCountry = TotalMedalsPerCountry %>% 
  left_join(tbl_df(shape@data), by = c("Code"="ISO3")) %>% 
  na.omit()

TotalMedalsPerCountry = TotalMedalsPerCountry %>% 
  mutate(label = str_c(sep = " - ", NAME, "TotalMedals", TotalMedals))
```

```r
bins = c(0, 10, 20, 30, 50, 100, 150, 200, 300, Inf)
pal = colorBin("RdYlGn", domain = TotalMedalsPerCountry$TotalMedals, bins = bins)

TotalMedalsPerCountry %>% 
  leaflet() %>% 
  addTiles() %>% 
  setView(53.019815, 1.369002, zoom = 1) %>% 
  addCircles(~LON, ~LAT, label = ~label, color = ~pal(TotalMedals), weight = 10)
```

![center](/figure/2018Week7_WinterOlympics/unnamed-chunk-3-1.png)


As can be clearly seen above, Norway and USA closely followed in the total medals ranking. 
Visualizing data on a map can provide a clear view of the overall data.

