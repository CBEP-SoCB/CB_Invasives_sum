Preparation of MIMIC Invasive Species Data for Analysis
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
3/12/2021

-   [Introduction](#introduction)
-   [Load Libraries](#load-libraries)
-   [Load Data](#load-data)
    -   [Establish Folder Reference](#establish-folder-reference)
-   [Import Fully QA/QC’d Data](#import-fully-qaqcd-data)
-   [Convert to Factors for Display
    Order](#convert-to-factors-for-display-order)
-   [Add Order Factors](#add-order-factors)
-   [Recent Data Only](#recent-data-only)
-   [Analyzing Sampling Effort](#analyzing-sampling-effort)
-   [Trend Data](#trend-data)
-   [Analysis Pricipals](#analysis-pricipals)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

# Introduction

This Notebook provides data preparation for analyzing the MIMIC invasive
species monitoring program from Casco Bay.

This notebook has not been carried forward into detailed analysis, as
the State of Casco Bay indicator used only simple graphical summaries.

The Marine Invader Monitoring and Information Collaborative (MIMIC) in
Casco Bay is a partnership between CBEP, the Wells National Estuarine
Research Reserve (Wells NERR), and the regional MIMIC program. The
Regional effort includes participants from several other New England
States.

Wells NERR trains community scientists to identify (currently) 23
species of invasives, including tunicates, bryozoans, algae and
crustaceans. Scientists visit sites monthly between May and October and
document abundance of these non-native species.

The program began with two sites in Casco Bay in 2008 and has expanded
in ensuing years to sample an additional mainland site and 12 sites
across four Islands (Peaks, Chebeague, Long, and Great Diamond).

# Load Libraries

``` r
library(tidyverse)
#> -- Attaching packages --------------------------------------- tidyverse 1.3.0 --
#> v ggplot2 3.3.3     v purrr   0.3.4
#> v tibble  3.0.5     v dplyr   1.0.3
#> v tidyr   1.1.2     v stringr 1.4.0
#> v readr   1.4.0     v forcats 0.5.0
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(readxl)

library(VGAM)
#> Loading required package: stats4
#> Loading required package: splines
#> 
#> Attaching package: 'VGAM'
#> The following object is masked from 'package:tidyr':
#> 
#>     fill
#library(readr)

library(GGally)
#> Registered S3 method overwritten by 'GGally':
#>   method from   
#>   +.gg   ggplot2
#library(zoo)
#library(lubridate)  # here, for the make_datetime() function

library(CBEPgraphics)
load_cbep_fonts()
theme_set(theme_cbep())
```

# Load Data

## Establish Folder Reference

``` r
sibfldnm <- 'Derived_Data'
parent   <- dirname(getwd())
sibling  <- file.path(parent,sibfldnm)
dir.create(file.path(getwd(), 'figures'), showWarnings = FALSE)
```

# Import Fully QA/QC’d Data

``` r
fn <- 'Abundance_Data.csv'
abundance_data <- read_csv(file.path(sibling, fn),
                           col_types = cols(
                             Date = col_datetime(format = ""),
                             Site = col_character(),
                             Type = col_character(),
                             City = col_character(),
                             State = col_character(),
                             Salinity = col_double(),
                             Temp = col_double(),
                             Month = col_character(),
                             Year = col_integer(),
                             Where = col_character(),
                             Species = col_character(),
                             Common = col_character(),
                             Abundance = col_character()
                           )) %>%
  mutate(Type  = factor(Type, levels = c('Dock', 'Tidepool')),
         Month = factor(Month, levels = month.abb),
         Abundance = ordered(Abundance, levels = c('Absent', 'Rare', 'Few', 
                                                   'Common', 'Abundant')))

fn <- 'Presence_Data.csv'
presence_data <- read_csv(file.path(sibling, fn),
                          col_types = cols(
                             Date = col_datetime(format = ""),
                             Site = col_character(),
                             Type = col_character(),
                             City = col_character(),
                             State = col_character(),
                             Salinity = col_double(),
                             Temp = col_double(),
                             Month = col_character(),
                             Year = col_integer(),
                             Where = col_character(),
                             Species = col_character(),
                             Common = col_character(),
                             Present = col_logical()
                           )) %>%
  mutate(Type  = factor(Type, levels = c('Dock', 'Tidepool')),
         Month = factor(Month, levels = month.abb))
```

# Convert to Factors for Display Order

``` r
abundance_data <- abundance_data %>%
 mutate(Site = factor(Site, levels = 
                         c(  "Spring Point Marina",
                             "SMCC Dock", 
                             "Siegel's Reef",
                             
                             "Peaks Dock",
                             "Peaks Tidepool",
                             
                             "Great Diamond Island Dock", 
                             "Great Diamond Island Tidepool",
                             
                             "Long Island Dock",
                             "Fowler's Tide Pool",
                             
                             "Chandlers Wharf Dock",
                             #"Chebeague Island Boat Yard",
                             "Chebeague Stone Pier", 
                             "Waldo Point"
                         )),
         Where = factor(Where, levels = c("Mainland", "Peaks","Great Diamond",
                                          "Long", "Chebeague") ))
```

``` r
presence_data <- presence_data %>%
  mutate(Site = factor(Site, levels = 
                         c(  "Spring Point Marina",
                             "SMCC Dock", 
                             "Siegel's Reef",
                             
                             "Peaks Dock",
                             "Peaks Tidepool",
                             
                             "Great Diamond Island Dock", 
                             "Great Diamond Island Tidepool",
                             
                             "Long Island Dock",
                             "Fowler's Tide Pool",
                             
                             "Chandlers Wharf Dock",
                             "Chebeague Stone Pier", 
                             "Waldo Point"
                         )),
         Where = factor(Where, levels = c("Mainland", "Peaks","Great Diamond",
                                          "Long", "Chebeague") ))
```

# Add Order Factors

We need to organize graphics by island in consistent structure. We will
use a bar chart, organized by Island and a common sequence within island
groups. To facilitate that, we need a factor that orders sites
consistently within island groups. While we are at it, we create
alternate labels for the plots.

``` r
orders <- tribble (
  ~Site,                            ~Order,      ~Label,
  "Spring Point Marina",               1,         "Spring Point Marina",  
  "SMCC Dock",                         2,         "SMCC Dock",
  "Siegel's Reef",                     3,         "Siegel's Reef",  
  
  "Peaks Dock",                        1,          "Peaks Dock",          
  "Peaks Tidepool",                    2,          "Peaks Tidepool", 
  
  "Great Diamond Island Dock",         1,          "Great Diamond Dock",    
  "Great Diamond Island Tidepool",     2,          "Great Diamond Tidepool",
  
  "Long Island Dock",                  1,          "Long Island Dock",  
  "Fowler's Tide Pool",                2,          "Fowler's Tidepool",   
  
  "Chandlers Wharf Dock",              1,          "Chandlers Wharf Dock",  
  "Chebeague Stone Pier",              2,          "Stone Pier",   
  "Waldo Point" ,                      3,          "Waldo Point")
```

``` r
abundance_data <- abundance_data %>%
  left_join(orders, by = 'Site')

presence_data <- presence_data %>%
  left_join(orders, by = 'Site')
```

# Recent Data Only

``` r
recent_presence_data <- presence_data %>% 
  filter(Year > 2015)
recent_abundance_data <-abundance_data %>% 
  filter(Year > 2015)
```

# Analyzing Sampling Effort

Lets look at distribution of EFFORT by sites and years.

``` r
site_visits <- presence_data %>%
  group_by(Site, Year, Month) %>%
  summarize(Sampled = n() > 0,
            .groups = 'drop')   # This includes NA values, which are still
                                # evidence of a site visit.

site_visits %>%
  group_by(Site, Year) %>%
  summarize(Visits = sum(Sampled)) %>%
  pivot_wider(names_from = Year, values_from = Visits) %>%
  select("Site", sort(colnames(.)))
#> `summarise()` has grouped output by 'Site'. You can override using the `.groups` argument.
#> # A tibble: 12 x 14
#> # Groups:   Site [12]
#>    Site  `2008` `2009` `2010` `2011` `2012` `2013` `2014` `2015` `2016` `2017`
#>    <chr>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>  <int>
#>  1 Chan~     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
#>  2 Cheb~     NA     NA     NA     NA     NA     NA      5      3      2      4
#>  3 Fowl~     NA     NA     NA     NA     NA     NA     NA     NA      2      3
#>  4 Grea~     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
#>  5 Grea~     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
#>  6 Long~     NA     NA     NA     NA     NA     NA     NA      1      1     NA
#>  7 Peak~     NA     NA     NA     NA     NA     NA      4      4      3      1
#>  8 Peak~     NA     NA     NA     NA     NA     NA      1      2      4      2
#>  9 Sieg~     NA      3      3      5      4      3      2      3      1      1
#> 10 SMCC~      2      4      3      5      3      2      2      4     NA     NA
#> 11 Spri~     NA     NA     NA     NA     NA     NA     NA     NA     NA      3
#> 12 Wald~     NA     NA     NA     NA     NA     NA     NA     NA     NA     NA
#> # ... with 3 more variables: `2018` <int>, `2019` <int>, `2020` <int>
```

So…

1.  Only the South Portland sites (Siegel’s Reef and SMCC Dock) have a
    long enough record for robust trend analysis. Peaks Dock and
    Chebeague Stone Pier have six years of record.

2.  Several Sites have almost never been sampled, and should probably be
    dropped from any site by site analysis, but could be included in
    Bay-wide species occurrences.

# Trend Data

For trend sites, we need sites with data from at least five of the last
ten years, and at least two years prior to the most recent five years.

``` r
trend_sites <- site_visits %>%
  group_by (Site, Year) %>%
  summarize(Sampled = any(Sampled > 0, na.rm = TRUE),
            .groups = 'drop') %>%
  group_by(Site) %>%
  summarize(Years = sum(Sampled),
            Recent = sum(Sampled & Year > 2015),
            .groups = 'keep') %>%
  filter(Years >= 10, Recent >= 3) %>%
  pull(Site)
  
trend_presence_data <- presence_data %>%
  filter (Site %in% trend_sites)
trend_abundance_data <- abundance_data %>%
  filter (Site %in% trend_sites)
```

# Analysis Pricipals

We have to be a bit careful here about interpreting these data, because
of varying annual effort. We need to make sure we are scaling analyses
by effort. We will consider a unit of effort to be the combination of a
site and a month of sampling.

We also need to consider different patterns of “common”.  
\* Species found at high abundances  
\* Species found in most years  
\* Species found at most sites.

Some species appear more abundant in recent years, but that may reflect
different distributions of effort among years and sites.

This suggests a couple of binomial models. but binomial models are going
to be structurally unstable, since many species are rare enough that we
will have structural zeros, and thus a significant Hauke-Donner effect.
