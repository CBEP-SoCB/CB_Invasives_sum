Review and Cleanup of MIMIC Invasive Species Data
================
Curtis C. Bohlen, Casco Bay Estuary Partnership
3/10/2021

-   [Introduction](#introduction)
-   [Load Libraries](#load-libraries)
-   [Load Data](#load-data)
    -   [Establish Folder Reference](#establish-folder-reference)
-   [Data on “Discontinued Sites”](#data-on-discontinued-sites)
    -   [Correct Town](#correct-town)
-   [Data Review](#data-review)
    -   [Inconsistent Data Entry](#inconsistent-data-entry)
        -   [Species Nomenclature](#species-nomenclature)
        -   [Abundance Levels](#abundance-levels)
        -   [Site Types](#site-types)
    -   [Correct Inconsistent Coding](#correct-inconsistent-coding)
        -   [Review Levels Again](#review-levels-again)
    -   [Cities and Sites](#cities-and-sites)
    -   [Select Only Casco Bay Data](#select-only-casco-bay-data)
    -   [Casco Bay Site List](#casco-bay-site-list)
    -   [Convert Sites to a Factor](#convert-sites-to-a-factor)
        -   [Add Additional Indicator](#add-additional-indicator)
    -   [Handling “Missing” Abundance
        Data](#handling-missing-abundance-data)
        -   [Distribution of Missing Abundance
            Data](#distribution-of-missing-abundance-data)
        -   [Checking Missing Data](#checking-missing-data)
    -   [No Species Reported](#no-species-reported)
    -   [Duplicate Records](#duplicate-records)
        -   [First Duplicate Field
            Record](#first-duplicate-field-record)
        -   [Second Duplicate Field
            Record](#second-duplicate-field-record)
        -   [Third Duplicate Field
            Record](#third-duplicate-field-record)
-   [Construct Presence / Absence
    Data](#construct-presence-absence-data)
-   [Data Reorganization](#data-reorganization)
    -   [Pivot to “Wide” Data Format](#pivot-to-wide-data-format)
    -   [Pivot Back to “Long” Data
        Format](#pivot-back-to-long-data-format)
-   [Add Common Names](#add-common-names)
-   [Export Revised Data](#export-revised-data)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

# Introduction

This Notebook provides graphic summaries of data from the MIMIC invasive
species monitoring program from Casco Bay

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
sibfldnm <- 'Original_Data'
parent   <- dirname(getwd())
sibling  <- file.path(parent,sibfldnm)
# dir.create(file.path(getwd(), 'figures'), showWarnings = FALSE)
```

We have data presented on two tabs, “Master Data Record” and
‘Removed-discontinued sites’. it’s not obvious why we would omit data
from a discontinued site. It depends on the underlying purpose.

``` r
fn <- 'MIMIC_MASTER_DATA_Wells_022521.xls'
fpath <- file.path(sibling, fn)

the_data <- read_excel(fpath, sheet = 'Master Data Record',
                       na = c('', 'na', 'NA')) %>%
  mutate(Month = as.numeric(format(Date, format = '%m')),
         Month = factor(Month, levels = 1:12, labels =  month.abb),
         Year = as.numeric(format(Date, format = '%Y'))) %>%
  filter(! is.na(Date)) %>%
  rename(Salinity = `Salinty (ppt)`,      # Note spelling error
         Temp     = `H20 temp C`)   %>%   # Note that's  H 2 zero,
  select(-State)
```

# Data on “Discontinued Sites”

These sites are present in the data, each with only one year of data,
and they were not included in State of Casco Bay for 2020.

``` r
other_data  <- read_excel(fpath, sheet = 'Removed-discontinued sites',
                       na = c('', 'na', 'NA')) %>%
  mutate(Month = as.numeric(format(Date, format = '%m')),
         Month = factor(Month, levels = 1:12, labels =  month.abb),
         Year = as.numeric(format(Date, format = '%Y'))) %>%  filter(! is.na(Date)) %>%
  rename(Salinity = `Salinty (ppt)`,   # Note spelling error
         Temp     = `H20 temp C`)      # Note that's  H 2 zero,

all_data <- bind_rows(the_data, other_data)
```

## Correct Town

We correct the misattribution of Cousins Island to Portland (it’s in
Yarmouth).

``` r
other_data <- other_data %>%
  mutate(City  = if_else(Site ==  "Cousin Island Dock", "Yarmouth", City)) 
#rm(other_data)
```

# Data Review

## Inconsistent Data Entry

### Species Nomenclature

``` r
sort(unique(the_data$Species))
#>  [1] "Ascidiella aspersa"       "Botrylloides violaceus"  
#>  [3] "Botryllus schlosseri"     "Bugula neritina"         
#>  [5] "Caprella mutica"          "Carcinus maenas"         
#>  [7] "Codium fragile"           "Colpomenia pereqrina"    
#>  [9] "Corella eumyota"          "Diadumene lineata"       
#> [11] "Didemnum vexillum"        "Diplosoma listerianum"   
#> [13] "Grateloupia turuturu"     "Hemigrapsus sanguineus"  
#> [15] "Membranipora membranacea" "membranipora sp."        
#> [17] "Membranipora sp."         "Ostrea edulis"           
#> [19] "palaemon elegans"         "Palaemon elegans"        
#> [21] "Styela clava"             "Tricellaria inopinata"
```

So we have a couple of things to note:  
1. Inconsistent coding of Membranipora  
2. Inconsistent capitalization of Palaemon 3. “Colpomenia pereqrina” is
misspelled , with a “Q” where it should have a “G”

### Abundance Levels

Show inconsistent capitalization.

``` r
sort(unique(the_data$Abundance))
#> [1] "Abundant" "common"   "Common"   "few"      "Few"      "rare"     "Rare"
```

Again, we have inconsistent capitalization

### Site Types

Also suffer from inconsistent capitalization…. Note that we have “Cobble
Shore” as a category here, but it was not used in Casco Bay, so will
drop out of the data once we select only Casco Bay locations.

``` r
sort(unique(the_data$Type))
#> [1] "Cobble Shore" "Dock"         "Tidepool"
```

## Correct Inconsistent Coding

``` r
the_data <- the_data %>%
  mutate(Species = if_else(grepl('Membranipora', Species, ignore.case = TRUE), 
                           'Membranipora sp.', 
                           Species)) %>%
  mutate(Species = if_else(grepl('Palaemon ', Species, ignore.case = TRUE), 
                           'Palaemon elegans', 
                           Species)) %>%
  mutate(Species = if_else(grepl('Colpomenia ', Species, ignore.case = TRUE), 
                           'Colpomenia peregrina', 
                           Species))
```

``` r
the_data <- the_data %>%
  mutate(Abundance = paste0(toupper(substr(Abundance, 1, 1)), 
                            tolower(substr(Abundance, 2, nchar(Abundance)))),
         Abundance = (na_if(Abundance, 'NANA')),
         Abundance = ordered(Abundance, levels = c('Rare', 'Few', 'Common', 'Abundant') )) %>%
  
 mutate(Type = paste0(toupper(substr(Type, 1, 1)), 
                            tolower(substr(Type, 2, nchar(Type)))),
        Type = (na_if(Type, 'NANA')),
        Type = (na_if(Type, 'Cobble shore')),  # Only used outside of Casco Bay
        Type = factor(Type) )
```

### Review Levels Again

``` r
sort(unique(the_data$Species))
#>  [1] "Ascidiella aspersa"     "Botrylloides violaceus" "Botryllus schlosseri"  
#>  [4] "Bugula neritina"        "Caprella mutica"        "Carcinus maenas"       
#>  [7] "Codium fragile"         "Colpomenia peregrina"   "Corella eumyota"       
#> [10] "Diadumene lineata"      "Didemnum vexillum"      "Diplosoma listerianum" 
#> [13] "Grateloupia turuturu"   "Hemigrapsus sanguineus" "Membranipora sp."      
#> [16] "Ostrea edulis"          "Palaemon elegans"       "Styela clava"          
#> [19] "Tricellaria inopinata"
sort(unique(the_data$Abundance))
#> [1] Rare     Few      Common   Abundant
#> Levels: Rare < Few < Common < Abundant
sort(unique(the_data$Type))
#> [1] Dock     Tidepool
#> Levels: Dock Tidepool
```

## Cities and Sites

We have data from outside of Casco Bay, which we need to eliminate.
While we are at it, we can correct some minor errors (spelling and
towns).

``` r
unique(the_data$City)
#> [1] "Wells"          "Biddeford"      "Kennebunk"      "York"          
#> [5] "South Portland" "Portland"       "Chebeague"      "Long Island"   
#> [9] "Kittery"
```

## Select Only Casco Bay Data

``` r
the_data <- the_data %>%
  filter(City %in% c('Chebeague', 'Long Island', 'Portland', 'South Portland'))
```

## Casco Bay Site List

``` r
xtabs(~ Site + Type, data = the_data)
#>                                Type
#> Site                            Dock Tidepool
#>   Chandlers Wharf Dock            42        0
#>   Chebeague Stone Pier           109        0
#>   Fowler's Tide Pool               0       72
#>   Great Diamond Island Dock       22        0
#>   Great Diamond Island Tidepool    0       26
#>   Long Island Dock                28        0
#>   Peaks Dock                      95        0
#>   Peaks Tidepool                   0       66
#>   Siegel's Reef                    0      108
#>   SMCC Dock                      150        0
#>   Spring Point Marina            102        0
#>   Waldo Point                      0       45
```

## Convert Sites to a Factor

Matt Liebman of EPA requested graphics ordered by distance of each
island from the Mainland (as he described it), and grouped or otherwise
coded by Mainland vs. Island and dock vs. tidepool. We create factors to
address that need

``` r
the_data <- the_data %>%
  mutate(Site = factor(Site, levels = 
                         c(  "Siegel's Reef",
                             "Spring Point Marina",
                             "SMCC Dock", 
                             
                             "Peaks Dock",
                             "Peaks Tidepool",
                             
                             "Great Diamond Island Dock", 
                             "Great Diamond Island Tidepool",
                             
                             "Long Island Dock",
                             "Fowler's Tide Pool",
                             
                             "Chandlers Wharf Dock",
                             #"Chebeague Island Boat Yard",  # Discontinued Site. Dropped...
                             "Chebeague Stone Pier", 
                             "Waldo Point"                 #,
                             
                             #"Cousin Island Dock"         # Discontinued Site. Dropped...
                         )))
```

### Add Additional Indicator

``` r
translator <- tribble(
  ~Site, ~Where,
  
  "Siegel's Reef",                  "Mainland",
  "Spring Point Marina",            "Mainland",
  "SMCC Dock",                      "Mainland",
  
  "Peaks Dock",                     "Peaks",
  "Peaks Tidepool",                 "Peaks",
 
  "Great Diamond Island Dock",      "Great Diamond",
  "Great Diamond Island Tidepool",  "Great Diamond",
  
  "Long Island Dock",               "Long",
  "Fowler's Tide Pool",             "Long",
  
  "Chandlers Wharf Dock",           "Chebeague",
  "Chebeague Island Boat Yard",     "Chebeague", # Discontinued Site. Dropped...
  "Chebeague Stone Pier",           "Chebeague",
  "Waldo Point",                    "Chebeague",
 
  "Cousin Island Dock",             "Cousins",   # Discontinued Site. Dropped... 
)


the_data <- the_data %>%
  left_join(translator , by = 'Site')
```

## Handling “Missing” Abundance Data

There is no reason why data should have been collected without related
abundance information, if all protocols were being followed. Here we
look more closely at those observations to figure out how best to handle
them.

In e-mail exchanges with Jeremy Miller, of Wells Estuarine Research
Reserve, he anticipates that most are errors introduced by volunteers
not recording data. Given the way the data sheets are constructed, and
volunteers are trained, most probably represent observations where
observers failed to record abundance. Accordingly, an observation is
evidence for presence of the species, but we do not know at what level.
We would have to make some sort of assumption about when observers would
be most likely to omit abundance information to use these observations
in proportional odds models.

Many observations have comments in the data that provide context or
confirm that no abundance data was provided, etc.

### Distribution of Missing Abundance Data

``` r
the_data %>% 
  filter(is.na(Abundance)) %>%
  group_by(Site, Year) %>%
  summarize(missed = any(is.na(Abundance)),
            count_missed  = sum(is.na(Abundance)))
#> `summarise()` has grouped output by 'Site'. You can override using the `.groups` argument.
#> # A tibble: 15 x 4
#> # Groups:   Site [8]
#>    Site                           Year missed count_missed
#>    <chr>                         <dbl> <lgl>         <int>
#>  1 Chebeague Stone Pier           2014 TRUE              2
#>  2 Chebeague Stone Pier           2015 TRUE              1
#>  3 Chebeague Stone Pier           2016 TRUE              5
#>  4 Chebeague Stone Pier           2018 TRUE              1
#>  5 Fowler's Tide Pool             2017 TRUE              1
#>  6 Fowler's Tide Pool             2018 TRUE              1
#>  7 Great Diamond Island Tidepool  2020 TRUE              2
#>  8 Long Island Dock               2018 TRUE              1
#>  9 Peaks Dock                     2020 TRUE              1
#> 10 Siegel's Reef                  2010 TRUE              7
#> 11 Siegel's Reef                  2012 TRUE              1
#> 12 Siegel's Reef                  2014 TRUE              1
#> 13 SMCC Dock                      2010 TRUE              8
#> 14 SMCC Dock                      2015 TRUE              1
#> 15 Spring Point Marina            2018 TRUE              9
```

### Checking Missing Data

#### 2010 Data

``` r
the_data %>%
  filter(Year == 2010) %>%
  select(-Date, -Type, -City, -Weather, -`Salinity`, 
         -Temp)
#> # A tibble: 15 x 7
#>    Site        Species           Abundance Comments          Month  Year Where  
#>    <chr>       <chr>             <ord>     <chr>             <fct> <dbl> <chr>  
#>  1 Siegel's R~ Botrylloides vio~ <NA>      No abundance rec~ Jul    2010 Mainla~
#>  2 Siegel's R~ Carcinus maenas   <NA>      No abundance rec~ Jul    2010 Mainla~
#>  3 SMCC Dock   Ascidiella asper~ <NA>      No abundance rec~ Jul    2010 Mainla~
#>  4 SMCC Dock   Hemigrapsus sang~ <NA>      No abundance rec~ Jul    2010 Mainla~
#>  5 Siegel's R~ Botrylloides vio~ <NA>      No abundance rec~ Aug    2010 Mainla~
#>  6 Siegel's R~ Botryllus schlos~ <NA>      No abundance rec~ Aug    2010 Mainla~
#>  7 Siegel's R~ Carcinus maenas   <NA>      No abundance rec~ Aug    2010 Mainla~
#>  8 SMCC Dock   Ascidiella asper~ <NA>      No abundance rec~ Aug    2010 Mainla~
#>  9 SMCC Dock   Botrylloides vio~ <NA>      No abundance rec~ Aug    2010 Mainla~
#> 10 SMCC Dock   Botryllus schlos~ <NA>      No abundance rec~ Aug    2010 Mainla~
#> 11 SMCC Dock   Carcinus maenas   <NA>      No abundance rec~ Aug    2010 Mainla~
#> 12 Siegel's R~ Botryllus schlos~ <NA>      No abundance rec~ Sep    2010 Mainla~
#> 13 Siegel's R~ Carcinus maenas   <NA>      No abundance rec~ Sep    2010 Mainla~
#> 14 SMCC Dock   Botrylloides vio~ <NA>      No abundance rec~ Sep    2010 Mainla~
#> 15 SMCC Dock   Botryllus schlos~ <NA>      No abundance rec~ Sep    2010 Mainla~
```

It appears that no data in 2010 was collected with abundance data. (At
that time, only the two “Mainland” stations were being monitored.)

#### Chebeage Stone Pier 2016 Missing Abundance Data

``` r
the_data %>% 
  filter(Site == "Chebeague Stone Pier", Year == 2016) %>%
  select(-Type, -Year, -Site, -City, -Weather, -Salinity, 
         -Temp, -Comments)
#> # A tibble: 15 x 5
#>    Date                Species                Abundance Month Where    
#>    <dttm>              <chr>                  <ord>     <fct> <chr>    
#>  1 2016-08-12 00:00:00 Ascidiella aspersa     Few       Aug   Chebeague
#>  2 2016-08-12 00:00:00 Botrylloides violaceus Abundant  Aug   Chebeague
#>  3 2016-08-12 00:00:00 Botryllus schlosseri   Few       Aug   Chebeague
#>  4 2016-08-12 00:00:00 Caprella mutica        Common    Aug   Chebeague
#>  5 2016-08-12 00:00:00 Didemnum vexillum      Abundant  Aug   Chebeague
#>  6 2016-08-12 00:00:00 Membranipora sp.       Common    Aug   Chebeague
#>  7 2016-08-19 00:00:00 Botrylloides violaceus Few       Aug   Chebeague
#>  8 2016-08-19 00:00:00 Botryllus schlosseri   Rare      Aug   Chebeague
#>  9 2016-08-19 00:00:00 Didemnum vexillum      Common    Aug   Chebeague
#> 10 2016-08-19 00:00:00 Membranipora sp.       Abundant  Aug   Chebeague
#> 11 2016-10-12 00:00:00 Botrylloides violaceus <NA>      Oct   Chebeague
#> 12 2016-10-12 00:00:00 Botryllus schlosseri   <NA>      Oct   Chebeague
#> 13 2016-10-12 00:00:00 Caprella mutica        <NA>      Oct   Chebeague
#> 14 2016-10-12 00:00:00 Didemnum vexillum      <NA>      Oct   Chebeague
#> 15 2016-10-12 00:00:00 Membranipora sp.       <NA>      Oct   Chebeague
```

So, the missing observations from 2016 are all are from the month of
October. This was one site visit in which full data was not collected.

#### Spring Point Marine 2018 Missing Abundance Data

``` r
the_data %>% 
  filter(Site == "Spring Point Marina", Year == 2018) %>%
  select(-Type, -Year, -Site, -City, -Weather, -Salinity, 
         -Temp, -Comments)
#> # A tibble: 43 x 5
#>    Date                Species                Abundance Month Where   
#>    <dttm>              <chr>                  <ord>     <fct> <chr>   
#>  1 2018-06-26 00:00:00 Ascidiella aspersa     Rare      Jun   Mainland
#>  2 2018-06-26 00:00:00 Styela clava           Rare      Jun   Mainland
#>  3 2018-06-26 00:00:00 Botrylloides violaceus Few       Jun   Mainland
#>  4 2018-06-26 00:00:00 Botryllus schlosseri   Few       Jun   Mainland
#>  5 2018-06-26 00:00:00 Carcinus maenas        Rare      Jun   Mainland
#>  6 2018-06-26 00:00:00 Tricellaria inopinata  Rare      Jun   Mainland
#>  7 2018-06-26 00:00:00 Caprella mutica        Few       Jun   Mainland
#>  8 2018-06-26 00:00:00 Membranipora sp.       Few       Jun   Mainland
#>  9 2018-07-18 00:00:00 Ascidiella aspersa     <NA>      Jul   Mainland
#> 10 2018-07-18 00:00:00 Styela clava           <NA>      Jul   Mainland
#> # ... with 33 more rows
```

The data without Abundances from 2018 is all from the month of July.
Again, it is likely this represents a data sheet that was not fully
filled in.

What that shows is that MOST sites in MOST years, lack of abundance
happened rarely. The exceptions are:

| Site                 | Year | Month   |
|----------------------|------|---------|
| Spring Point Marina  | 2018 | July    |
| Chebeague Stone Pier | 2016 | October |
| Siegel’s Reef        | 2010 | All     |
| SMCC Dock            | 2010 | All     |
| ———————–             | ———— | ————-   |

For each of those, we effectively have monthly visits that lacks all
abundance data.

#### Remaining Missing Abundance Data

Lets look at what is left after we remove those data sheets where data
was improperly recorded.

``` r
the_data %>%
  filter(! (Site == "Chebeague Stone Pier"  & Year ==  2016  & Month == 'October'),
         ! (Site == "Siegel's Reef" & Year ==  2010 ),
         ! (Site == "SMCC Dock"  & Year ==   2010),
         ! (Site == "Spring Point Marina" & Year == 2018)) %>%
  filter(is.na(Abundance)) %>%
  filter(Comments != "No abundance recorded") %>%
  filter(! is.na(Species)) %>%
  select(-Date, -Type, -City, -Weather, -Salinity, 
         -Temp, -Abundance) %>%
  arrange
#> # A tibble: 4 x 6
#>   Site          Species      Comments                        Month  Year Where  
#>   <chr>         <chr>        <chr>                           <fct> <dbl> <chr>  
#> 1 Siegel's Reef Membranipor~ The notes, show it to be absen~ Jul    2012 Mainla~
#> 2 Chebeague St~ Didemnum ve~ Tunicates on trap only - none ~ Jul    2018 Chebea~
#> 3 Great Diamon~ Botryllus s~ Not a dramatic tide (.8) with ~ Aug    2020 Great ~
#> 4 Great Diamon~ Membranipor~ Not a dramatic tide (.8) with ~ Aug    2020 Great ~
```

Based on the Comments, we believe we should retain each of these as
indicating an actual (if unquantified) observation of each species,
EXCEPT,

2012 Jul Siegel’s Reef Membranipora sp.

For that record,we accept the check mark, marked as “Absent”, as
described in the Accordingly, we delete that observation now.

``` r
the_data <- the_data %>%
  filter(! (Year == 2012 &
           Month == 'Jul' & 
           Site == "Siegel's Reef" &
           Species == "Membranipora sp."))
```

## No Species Reported

Some rows contain date and time information, but no species designation.

``` r
the_data %>%
  filter(is.na(Species)) %>%
  select(-Date, -Type, -City, -Weather, -Salinity, 
         -Temp, -Abundance)
#> # A tibble: 3 x 6
#>   Site         Species Comments                               Month  Year Where 
#>   <chr>        <chr>   <chr>                                  <fct> <dbl> <chr> 
#> 1 Chebeague S~ <NA>    Nothing to report. Native tunicates, ~ Jun    2015 Chebe~
#> 2 SMCC Dock    <NA>    No critters. Dock recently pulled      Jun    2015 Mainl~
#> 3 Peaks Dock   <NA>    Nothing found! Long blades of healthy~ May    2020 Peaks
```

These records are all markers of an actual site visit that resulted in
no observed species. Removing empty these rows would understate effort,
so could overestimate prevalence. We do not want to delete these
records.

## Duplicate Records

We use `pivot-wider()` (with `values_fn = length`) followed by
`pivot_longer()` and then filter to identify records collected on the
same date and site.

``` r
the_data %>%
  select(-Type, -City, -Weather, -Comments, -Salinity, -Temp) %>%
  mutate(Abundance = as.character(Abundance)) %>%
  pivot_wider(names_from = Species, 
              values_from = Abundance,
              values_fn = length,
              #values_fill = "Absent"
              ) %>% 
  pivot_longer( 6:24, names_to = "Species", values_to = "Count") %>%
  filter(Count > 1)
#> # A tibble: 9 x 7
#>   Date                Site            Month  Year Where   Species          Count
#>   <dttm>              <chr>           <fct> <dbl> <chr>   <chr>            <int>
#> 1 2013-07-12 00:00:00 Siegel's Reef   Jul    2013 Mainla~ Botrylloides vi~     2
#> 2 2013-07-12 00:00:00 Siegel's Reef   Jul    2013 Mainla~ Botryllus schlo~     2
#> 3 2013-07-12 00:00:00 Siegel's Reef   Jul    2013 Mainla~ Membranipora sp.     2
#> 4 2013-07-12 00:00:00 Siegel's Reef   Jul    2013 Mainla~ Styela clava         2
#> 5 2015-08-05 00:00:00 Chebeague Ston~ Aug    2015 Chebea~ Botrylloides vi~     2
#> 6 2015-08-05 00:00:00 Chebeague Ston~ Aug    2015 Chebea~ Caprella mutica      2
#> 7 2016-06-27 00:00:00 Peaks Tidepool  Jun    2016 Peaks   Botrylloides vi~     2
#> 8 2016-06-27 00:00:00 Peaks Tidepool  Jun    2016 Peaks   Membranipora sp.     2
#> 9 2016-06-27 00:00:00 Peaks Tidepool  Jun    2016 Peaks   Carcinus maenas      2
```

### First Duplicate Field Record

``` r
the_data %>%
  filter(Year    == 2013 &
     Month   == 'Jul' &
     Site    == "Siegel's Reef") %>%
  arrange(Species)
#> # A tibble: 9 x 13
#>   Date                Site  Type  City  Weather Salinity  Temp Species Abundance
#>   <dttm>              <chr> <fct> <chr> <chr>      <dbl> <dbl> <chr>   <ord>    
#> 1 2013-07-12 00:00:00 Sieg~ Tide~ Sout~ Cloudy~       25    19 Botryl~ Common   
#> 2 2013-07-12 00:00:00 Sieg~ Tide~ Sout~ Partly~       25    17 Botryl~ Abundant 
#> 3 2013-07-12 00:00:00 Sieg~ Tide~ Sout~ Cloudy~       25    19 Botryl~ Rare     
#> 4 2013-07-12 00:00:00 Sieg~ Tide~ Sout~ Partly~       25    17 Botryl~ Few      
#> 5 2013-07-12 00:00:00 Sieg~ Tide~ Sout~ Cloudy~       25    19 Carcin~ Rare     
#> 6 2013-07-12 00:00:00 Sieg~ Tide~ Sout~ Cloudy~       25    19 Membra~ Rare     
#> 7 2013-07-12 00:00:00 Sieg~ Tide~ Sout~ Partly~       25    17 Membra~ Few      
#> 8 2013-07-12 00:00:00 Sieg~ Tide~ Sout~ Cloudy~       25    19 Styela~ Rare     
#> 9 2013-07-12 00:00:00 Sieg~ Tide~ Sout~ Partly~       25    17 Styela~ Rare     
#> # ... with 4 more variables: Comments <chr>, Month <fct>, Year <dbl>,
#> #   Where <chr>
```

These show mostly PAIRS of duplicate observations, with differences in
wording of Weather and different temperatures. That suggests two
different observers. Different temperatures suggest different times of
day, or different thermometers, not well calibrated.

One observer (?) noted Green Crab, the other did not. perhaps a trainer
and a trainee. The abundance data differs in several places by one
abundance class.

We judge it is better to select the sample that observed the green crab,
as most of our analyses will be based on presence/absence.

#### Check Selection

``` r
the_data %>%
  filter(Year    == 2013 &
           Month   == 'Jul' &
           Site    == "Siegel's Reef" &
           Weather == 'Partly cloudy') %>%
  arrange(Species)
#> # A tibble: 4 x 13
#>   Date                Site  Type  City  Weather Salinity  Temp Species Abundance
#>   <dttm>              <chr> <fct> <chr> <chr>      <dbl> <dbl> <chr>   <ord>    
#> 1 2013-07-12 00:00:00 Sieg~ Tide~ Sout~ Partly~       25    17 Botryl~ Abundant 
#> 2 2013-07-12 00:00:00 Sieg~ Tide~ Sout~ Partly~       25    17 Botryl~ Few      
#> 3 2013-07-12 00:00:00 Sieg~ Tide~ Sout~ Partly~       25    17 Membra~ Few      
#> 4 2013-07-12 00:00:00 Sieg~ Tide~ Sout~ Partly~       25    17 Styela~ Rare     
#> # ... with 4 more variables: Comments <chr>, Month <fct>, Year <dbl>,
#> #   Where <chr>
```

#### Remove Duplicate Records

``` r
the_data <- the_data %>%
  filter( ! (Year    == 2013 &
               Month   == 'Jul' &
               Site    == "Siegel's Reef" &
               Weather == 'Partly cloudy'))
```

### Second Duplicate Field Record

``` r
the_data %>%
  filter(Year == 2015 &
     Month    == 'Aug' &
     Site     == 'Chebeague Stone Pier') %>%
  arrange(Species, Date)
#> # A tibble: 8 x 13
#>   Date                Site  Type  City  Weather Salinity  Temp Species Abundance
#>   <dttm>              <chr> <fct> <chr> <chr>      <dbl> <dbl> <chr>   <ord>    
#> 1 2015-08-05 00:00:00 Cheb~ Dock  Cheb~ sunny ~       34    19 Botryl~ Rare     
#> 2 2015-08-05 00:00:00 Cheb~ Dock  Cheb~ Sunny,~       34    19 Botryl~ Rare     
#> 3 2015-08-21 00:00:00 Cheb~ Dock  Cheb~ Foggy         35    20 Botryl~ Few      
#> 4 2015-08-21 00:00:00 Cheb~ Dock  Cheb~ Foggy         35    20 Botryl~ Few      
#> 5 2015-08-05 00:00:00 Cheb~ Dock  Cheb~ sunny ~       34    19 Caprel~ Rare     
#> 6 2015-08-05 00:00:00 Cheb~ Dock  Cheb~ Sunny,~       34    19 Caprel~ Rare     
#> 7 2015-08-21 00:00:00 Cheb~ Dock  Cheb~ Foggy         35    20 Caprel~ Rare     
#> 8 2015-08-21 00:00:00 Cheb~ Dock  Cheb~ Foggy         35    20 Membra~ Few      
#> # ... with 4 more variables: Comments <chr>, Month <fct>, Year <dbl>,
#> #   Where <chr>
```

Note a few things: 1. We have two sample collections in the same month.
We do not want to delete the wrong observations. 2. We have two pairs of
observations on 2015-08-05 that are exact duplicates, except for Weather
or Comments.

Different Weather descriptions suggests these were pulled from two
separate data sheets, filled out by different observers.

#### Check Selection

``` r
my_date <- as.Date('2015-08-05')

the_data %>%
  filter((Date    == my_date &
          Site == 'Chebeague Stone Pier' &
           Weather == 'Sunny, clear'))
#> # A tibble: 2 x 13
#>   Date                Site  Type  City  Weather Salinity  Temp Species Abundance
#>   <dttm>              <chr> <fct> <chr> <chr>      <dbl> <dbl> <chr>   <ord>    
#> 1 2015-08-05 00:00:00 Cheb~ Dock  Cheb~ Sunny,~       34    19 Botryl~ Rare     
#> 2 2015-08-05 00:00:00 Cheb~ Dock  Cheb~ Sunny,~       34    19 Caprel~ Rare     
#> # ... with 4 more variables: Comments <chr>, Month <fct>, Year <dbl>,
#> #   Where <chr>
```

#### Remove Duplicate Records

``` r
the_data <- the_data %>%
  filter(! (Date    == my_date &
              Site == 'Chebeague Stone Pier' &
              Weather == 'Sunny, clear'))
```

### Third Duplicate Field Record

``` r
the_data %>%
  filter(Year    == 2016 &
     Month   == 'Jun' &
     Site    == 'Peaks Tidepool') %>%
  arrange(Species, Date)
#> # A tibble: 6 x 13
#>   Date                Site  Type  City  Weather Salinity  Temp Species Abundance
#>   <dttm>              <chr> <fct> <chr> <chr>      <dbl> <dbl> <chr>   <ord>    
#> 1 2016-06-27 00:00:00 Peak~ Tide~ Port~ Clear,~       33    18 Botryl~ Rare     
#> 2 2016-06-27 00:00:00 Peak~ Tide~ Port~ Sunny ~       33    18 Botryl~ Rare     
#> 3 2016-06-27 00:00:00 Peak~ Tide~ Port~ Clear,~       33    18 Carcin~ Rare     
#> 4 2016-06-27 00:00:00 Peak~ Tide~ Port~ Sunny ~       33    18 Carcin~ Rare     
#> 5 2016-06-27 00:00:00 Peak~ Tide~ Port~ Clear ~       33    18 Membra~ Rare     
#> 6 2016-06-27 00:00:00 Peak~ Tide~ Port~ Sunny ~       33    18 Membra~ Few      
#> # ... with 4 more variables: Comments <chr>, Month <fct>, Year <dbl>,
#> #   Where <chr>
```

These show three PAIRS of duplicate observations, with differences in
wording of Weather and Comments. That suggest two different observers,
perhaps a trainer and a trainee. The abundance data is not QUITE
identical, with “Membranipora sp.” either “Rare” or “Few”.

#### Check Selection

``` r
the_data %>%
  filter(Year    == 2016 &
           Month   == 'Jun' &
           Site    == 'Peaks Tidepool' &
           Weather == 'Sunny and beautiful') %>%
  arrange(Species, Date)
#> # A tibble: 3 x 13
#>   Date                Site  Type  City  Weather Salinity  Temp Species Abundance
#>   <dttm>              <chr> <fct> <chr> <chr>      <dbl> <dbl> <chr>   <ord>    
#> 1 2016-06-27 00:00:00 Peak~ Tide~ Port~ Sunny ~       33    18 Botryl~ Rare     
#> 2 2016-06-27 00:00:00 Peak~ Tide~ Port~ Sunny ~       33    18 Carcin~ Rare     
#> 3 2016-06-27 00:00:00 Peak~ Tide~ Port~ Sunny ~       33    18 Membra~ Few      
#> # ... with 4 more variables: Comments <chr>, Month <fct>, Year <dbl>,
#> #   Where <chr>
```

#### Remove Duplicate Records

``` r
the_data <- the_data %>%
  filter( ! (Year    == 2016 &
               Month   == 'Jun' &
               Site    == 'Peaks Tidepool' &
               Weather == 'Sunny and beautiful'))
```

# Construct Presence / Absence Data

We now have a data set that contains one and only one row for every time
a species was observed. If the `Abundance` vector contain data, we have
an estimate of relative abundance, if not, all we know is that the
species was “present”.

We will proceed to analyze these data principally via presence /
absence, rather than as relative abundances.

We create a presence / absence data layer. Basically, any row that
remains in the data at this point indicated the species was “present”,
so all we need to do is create a “Present” vector with value “TRUE”
everywhere a species is listed, and FALSE anywhere else (only those
three dates and times when no invasive species were observed).

``` r
the_data <- the_data %>%
  mutate(Present = if_else(is.na(Species), FALSE, TRUE))
```

# Data Reorganization

The data we have now is incomplete, as it does not contain structural
zeros for species not observed. We need those structural zeros to allow
for modeling. The easiest way to build complete data is to pivot data to
wide form, replace NAs, and pivot back to long form. The wide form data
would be useful for ordination and other multivariate procedures, if we
chose to pursue them.

Note that this procedure retains any “real” NAs in the Abundance data,
while it fills in values not present in the draft long data with
“Absent”. This means we do not count data where species were recorded as
“present”, which is probematic. A decision needs t be made on each
specific analysis of the relative abundance data whether to replace
those NAs with “Absent” or not.

## Pivot to “Wide” Data Format

``` r
presence_data_wide <- the_data %>%
  select(-Abundance) %>%
  pivot_wider(-c(Weather, Comments), 
              names_from = Species, 
              values_from = Present,
              values_fill = FALSE
              )

abundance_data_wide <- the_data %>%
  select(-Present) %>%
  mutate(Abundance = ordered(Abundance, levels = c('Absent', 'Rare', 
                                                  'Few', 'Common', 
                                                  'Abundant'))) %>%
  pivot_wider(-c(Weather, Comments), 
              names_from = Species, 
              values_from = Abundance,
              values_fill = 'Absent'
              )
```

## Pivot Back to “Long” Data Format

``` r
presence_data <- presence_data_wide %>%
  pivot_longer(`Botrylloides violaceus`:`Tricellaria inopinata`, 
               names_to = 'Species',
               values_to = 'Present')

abundance_data <- abundance_data_wide %>%
  pivot_longer(`Botrylloides violaceus`:`Tricellaria inopinata`, 
               names_to = 'Species',
               values_to = 'Abundance')
```

# Add Common Names

It is convenient to have the common names included in these data, for

``` r
names_data <- read_excel(fpath, sheet = 'Species List', skip = 1,
                        col_names = c('Species', 'Common')) %>%
  filter(! is.na(Common))
```

``` r
presence_data <- presence_data %>%
  left_join(names_data, by = 'Species') %>%
  relocate(Common, .after = Species)

abundance_data <- abundance_data %>%
  left_join(names_data, by = 'Species') %>%
  relocate(Common, .after = Species)

rm(names_data)
```

# Export Revised Data

``` r
write_csv(abundance_data, 'Abundance_Data.csv')
write_csv(presence_data, 'Presence_Data.csv')
```
