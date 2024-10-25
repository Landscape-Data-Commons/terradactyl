# terradactyl <img src='man/figures/logo.png' align="right" height="120" />

``` r
# To install
remotes::install_github(repo = 'Landscape-Data-Commons/terradactyl')
library(terradactyl)
```

`terradactyl` can be used to gather core rangeland monitoring data into a standard format, calculate standardized indicators, custom indicators, and produce consistent data models. There are also a few quality control scripts available to help users ensure that species data are handled correctly.

The basic workflow of `terradactyl` is to first gather data into a standard format, also called a tall table. Then calculate indicators based on that format and grouping variables (if relevant). For some commonly calculated groups of indicators, such as for the AIM program or model inputs, there are also data model functions which take the tall table files and other data and produce data outputs specific to that user need. ([McCord et al., 2022](https://doi.org/10.1016/j.ecolind.2022.109511))


![image](https://user-images.githubusercontent.com/13965706/173185966-37bece42-c23a-46e0-881e-9b573ee3c1cd.png)
 
 
`terradactyl` relies on a few common organizing principles across functions. “PrimaryKey” refers to the unique plot-visit identifier and allows us to join tall tables across methods. In many instances, “PrimaryKey” is built using a combination of the Plot ID and the visit date. In all instances, “PrimaryKey” values must be established prior to using `terradactyl.` “LineKey” is optional and refers to individual transects (subsamples) collected at a plot. “DBKey” refers to the original database file, which allows us to track data provenance during QC. 'source' refers to a particular format type. Common sources are AIM, TerrADat, LMF, NRI. Data may be of source = 'AIM' but not belong to the AIM program, it just notes the original data format type. 

This vignette uses tall format data provided with terradactyl to demonstrate the calculation of indicators using the functions `pct_cover`, `mean_height`, `gap_cover`, `soil stability`, and  `species_count`, as well as convenience functions that provide filtered data from `pct_cover`.

### Cover Indicators based on Line-Point Intercept
#### Calculating Percent Cover 
The indicators calculated by `pct_cover` will be enough for most users and will allow you to calculate foliar cover by any code observed during the line-point intercept (LPI) method or on any arbitrarily defined grouping of species or cover. 

The other cover functions demonstrated here are convenience functions that call `pct_cover`, processing the data to filter down to only a few indicators. Below is a demonstration of `pct_cover`, which is then filtered to only return bare soil hits -- something that could also be accomplished with `pct_cover_bare_soil`, demonstrated later in this vignette.

To use the `pct_cover` indicator function you must specify a grouping variable. This is a column header with values defining the groups you are summarizing data over, such as "Duration" to get cover of annual and perennial plants. This parameter must be provided at the end of the function call, and notably must be input as if it were a variable rather than in a string. Here we use "code" in order to aggregate data for each different plant and surface code, such as ARTR2 (*Artemisia tridentata*, big sagebrush) or S (bare soil). 


```{r  pct_cover S hits demo}
# get demo data
lpi_tall <- terradactyl::tall_lpi_sample

cover_baresoil <-
# calculate percent cover for all first hit codes  
  pct_cover(lpi_tall = lpi_tall,
            tall = T,
            hit = "first",
            by_line = F,
            code) %>%
# subset to only cover of exposed soil
  dplyr::filter(indicator == "S")

head(cover_baresoil)
```

If cover by species group is of interest, first assign species attributes based on the “code” field in the lpi_tall table. You can do this manually in the R environment or by using `species_join`. Here, *Artemisia tridentata* (big sagebrush) and its subspecies *tridentata*, *vaseyana*, and *wyomingensis* are assigned to a single group, and cover of that group is calculated.

``` {r  custom indicator}
# define a grouping variable
lpi_tall$BigSagebrush <- "NotBigSage"
lpi_tall[lpi_tall$code %in% c("ARTRW8", "ARTRT", "ARTRV", "ARTR2"), 
         "BigSagebrush"] <- "BigSage"

# calculate cover by Artemisia and non-Artemisia species
cover_artemisia <-
  pct_cover(lpi_tall = lpi_tall,
            tall = F,
            hit = "first",
            by_line = F,
            BigSagebrush)

head(cover_artemisia)

```

If you want to join an external table of species attributes to the lpi_tall table, you can do this manually or with `species_join`. `species_join` will also assign growth habits to any unknown codes in your data and navigate different "SpeciesState" values if assigned in both the lpi_tall table (often via join with header) and in the source species list. "SpeciesState" is an optional field but must be present in both tables if present in one.  

To use `species_join` you must use the “data_code” parameter to specify the column name that contains the species codes you are joining on, as this may vary between input data types. LPI data (height and cover indicators) uses “code”, while species inventory data uses “Species”.

It is important to QC the species list, as any species that are present in the analyzed data but not the species list will not be tallied by any indicators.

```{r  lpi species join}
path_specieslist <- "C:/Users/jrbrehm/Documents/Data/tblStateSpecies.csv"

header <- terradactyl::tall_header_sample
header[1:6,1:4]

# join the header to a species file
lpi_species <- 
  species_join(data = merge(header, lpi_tall),
               species_file = path_specieslist,
               data_code = "code")

colnames(lpi_species)

```

Any of the columns present in the species list can be used to group data. In the next section, we will use "Duration", expecting two values: Annual or Perennial. To use a column for grouping, specify it without quotes at the end of the function call, as below. Attempting to calculate cover without specifying a grouping variable, attempting to specify a grouping variable as a string, or failing to specify all parameters will all cause the function to fail.


#### First vs Any
All cover indicators can be calculated for either the first hit (i.e. top canopy cover) or the any hit  (i.e. presence at any position from canopy to base). To specify, use the 'hit' parameter.

``` {r  pct_cover examples}
# calculate any hit cover of annual and perennial plants
cover_duration_anyhit <-
  pct_cover(lpi_tall = lpi_species,
            tall = F,
            hit = "any",
            by_line = F,
            Duration)
head(cover_duration_anyhit)

# Calculate first hit cover of annual and perennial plants
cover_duration_firsthit <-
  pct_cover(lpi_tall = lpi_species,
            tall = F,
            hit = "first",
            by_line = F,
            Duration)
head(cover_duration_firsthit) 

```

Note that any hit cover values for any plot can sum to greater than 100%, as it is possible for a single point to have multiple types of cover, such as annual grass growing underneath a shrub. Here only top canopy foliar cover is considered, excluding non-species codes (e.g., bare soil, litter), so rows do not sum to 100.

In contrast to any hit data, the sum of first hit cover is always less than or equal to 100% when all cover types are included. Here only foliar cover is considered, excluding non-species codes, so rows do not sum to 100. 

Any hit data is generally applicable and preferred for most studies, particularly those which prioritize species presence, as it captures data from across the entire canopy rather than solely focusing on the tallest species. First hit data is applicable for fewer uses, principally remote sensing and other studies focused on the top-down characteristics of the site.


#### Tall vs Wide
This data can be returned in either a tall format, with each row a cover type and a single column with numeric values, or in wide format with each row a plot and each column the percent cover of a given type. 

```{r}
# calculate first hit cover of annual and perennial plants, returned in tall format
cover_duration_firsthit_tall <-
  pct_cover(lpi_tall = lpi_species,
            tall = T,
            hit = "first",
            by_line = F,
            Duration) %>% 
  dplyr::arrange(PrimaryKey)
head(cover_duration_firsthit_tall)

# calculate first hit cover of annual and perennial plants, returned in wide format
cover_duration_firsthit_wide <-
  pct_cover(lpi_tall = lpi_species,
            tall = F,
            hit = "first",
            by_line = F,
            Duration) %>%
  dplyr::arrange(PrimaryKey)
head(cover_duration_firsthit_wide)
```

In the tall output, the “indicator” column contains the values “ANNUAL” and “PERENNIAL” each row specifying a different cover type at the associated study site. In tall output, there are multiple rows per value in “PrimaryKey”. In the wide output, “indicator” has been replaced with the columns “ANNUAL” and “PERENNIAL”. With wide output, each unique PrimaryKey has a single row.   

#### Other Cover Functions
`pct_cover` is a core function that will always produce percent cover. You will need to specify first hit or any hit, the grouping variable (e.g., “Duration”), and/or subset the results to get the indicator you need. There are several `pct_cover` subfunctions that calculate specific cover types and automate some of these specific implementations of `pct_cover.` In these functions, grouping variables are not necessary as the functions are specific to different grouping methods. Likewise, first vs any hit is not specified in `cover_between_plant` and `cover_baresoil` because these are by definition first hit indicators. `pct_cover_species` does not accept any grouping variables as input, because calculating cover by species implicitly requires that the grouping variable be the species code.

``` {r  pct_cover bare}
# calculate percent cover of all species
cover_species <- 
  pct_cover_species(lpi_tall = lpi_tall,
                    tall = T,
                    by_line = F,
                    hit = "any") %>%
  dplyr::arrange(PrimaryKey)
head(cover_species)

# many cover values are calculated with pct_cover_species
length(unique(cover_species$Species))

# calculate bare soil percent cover
cover_baresoil <- 
  pct_cover_bare_soil(lpi_tall = lpi_tall,
                      tall = F,
                      by_line = F)
head(cover_baresoil)

# calculate percent cover between plants                 
# L: Herbaceous litter. WL: Woody litter. S: Bare soil. R: Rock.
# see the Monitoring Manual (Herrick et al 2017) for code definitions
cover_between_plant <- 
  pct_cover_between_plant(lpi_tall = lpi_tall, 
                          tall = F,
                          by_line = F)
cover_between_plant[1:6,c("PrimaryKey", "L", "WL", "S", "R")]

```


### Calculating Height Indicators ###
Height indicators are calculated by `mean_height`, requiring LPI height data and a state species list. The state species list is used to assign attributes to each plant code, just as it is used in the LPI indicator calculation. 

``` {r  height}
height_tall <- terradactyl::tall_height_sample
header <- terradactyl::tall_header_sample

# join the tall height data, and species data
height_species <- 
  species_join(data = merge(height_tall, header),
               species_file = path_specieslist,
               data_code = "Species")

```

Height calculations use the 'method' parameter to specify either 'mean' or 'max'. If method is 'mean', the indicators will be based on the average height of all plants recorded in the LPI data. If 'method' is 'max', the output will be the average of the height of the tallest plants at each point. 

```{r}
# calculate mean of all heights
height_duration_mean <- 
  mean_height(height_species,
              method = "mean",
              omit_zero = T,
              by_line = F,
              tall = F,
              Duration)
head(height_duration_mean)

# calculate mean of maximum heights at each point
height_duration_meanmax <-
  mean_height(height_species,
             method = "max",
             omit_zero = T,
             by_line = F,
             tall = T)
head(height_duration_meanmax)

```


### Gap Indicators
Gap indicators are calculated by `gap_cover.` This function creates a named list of three data tables: percent, n, and length. These summarize the percent cover, number of gaps, and total length of gap aggregated for default gaps of size 20-25cm, 25-51cm, 51-101cm, 101-201cmm, and 201 cm+. Custom gap classes can be specified. 

Unlike the other functions demonstrated here, the gap_cover function returns a list of length 3. 'percent' contains the percent cover of each gap class, 'n' the number of gaps in each class, and 'length' the sum total of gap cover for each class. 

```{r  gap}
gap_tall <- terradactyl::tall_gap_sample

# calculate gap cover indicators
gap_stats <- 
  gap_cover(gap_tall = gap_tall,
            tall = F,
            breaks = c(20, 25, 51, 101, 201),
            type = "canopy",
            by_line = F)

# gap_cover returns three data frames, summarizing percent gap cover, number of gaps, and length of gaps, all calculated for each gap class
names(gap_stats)

```

Gaps can be summarized into any set of intervals by specifying the ‘breaks’ parameter as below. Canopy, basal, or perennial gaps are also specified separately through the “type” argument. 

```{r}
# calculate gap cover indicators with custom gap interval definitions, changing 201+ to 201-301 and 301+
gap_stats_custom_intervals <- 
  gap_cover(gap_tall = gap_tall, 
            tall = F,
            breaks = c(20,25,51,101,201,301), # Add a breakpoint at 301cm
            type = "canopy",
            by_line = F)
```


### Soil Stability Indicators
Mean soil stability is calculated using `soil_stability`, with only tall format soil stability data as input. The input parameters 'all', 'cover', and 'uncovered' determine whether or not soil stability data is aggregated for, respectively, all samples, all covered samples, and all uncovered samples. These are true/false parameters, all three defaulting to True. 

If 'all_cover_types' is true, the output will include mean stability grouped by each individual cover type. See the Monitoring Manual for code definitions (Herrick et al. 2017)

```{r  soil stab}
soil_stability_tall <- terradactyl::tall_soil_stability_sample

# calculate soil stability indicators, returning mean score of covered and uncovered samples
indicators_soil_stability <- 
  soil_stability(soil_stability_tall = soil_stability_tall,
                 all = F,
                 cover = T,
                 uncovered = T,
                 all_cover_types = F,
                 tall = F)
head(indicators_soil_stability)

indicators_soil_stability_alltypes <- 
  soil_stability(soil_stability_tall = soil_stability_tall,
                 all = F,
                 cover = T,
                 uncovered = T,
                 all_cover_types = T,
                 tall = F)
colnames(indicators_soil_stability_alltypes)

```


### Species Inventory Indicators
Species Inventory indicators are calculated by `species_count` Like the height indicators and some LPI indicators, these require a state species list in order to group data. The only indicator calculated here is a simple count of the number of species in each group.

Because we are using `species_join`, we must specify the “data_code” parameter as discussed above in the cover indicators section. Here we use species inventory data, where the species codes are stored in column “Species”.

``` {r  species inventory}
species_inventory_tall <- terradactyl::tall_species_inventory_sample
header <- terradactyl::tall_header_sample

# join the species inventory tall data and species data
species_inventory_join <-  
  species_join(merge(species_inventory_tall, header),
               species_file = path_specieslist, 
               data_code = "Species")

# calculate number of annual and perennial species
species_count_duration <- 
  species_count(species_inventory_tall = species_inventory_join,
  Duration)
head(species_count_duration)
```
Like with LPI data, any arbitrary variable can be used to group species inventory data. These can be specified in the species file, or directly in R as demonstrated below, again using the example of *Artemisia tridentata* subspecies. 

```{r}
# define a grouping variable
species_inventory_tall$BigSagebrush <- "NotBigSage"
species_inventory_tall[
  species_inventory_tall$code %in% c("ARTRW8", "ARTRT", "ARTRV", "ARTR2"),
  "BigSagebrush"] <- "BigSage"

# calculate number of big sage and non big sage species
species_count_artemisia <- 
  species_count(species_inventory_tall = species_inventory_tall,
                BigSagebrush)
head(species_count_artemisia)
```
### Conclusion

`terradactyl` contains many functions to calculate many indicators, something that may seem overwhelming at first. By demonstrating each of the core functions, we hope to make the learning curve easier for all users. Fortunately, a few functions like pct_cover or gap_cover are powerful and flexible, granting the user access to wide suites of indicator data with only a few lines of code. `terradactyl` also contains data model functions which bring multiple indicator calculation functions into a single wrapper to produce specific data formats. These data model functions have specific inputs and outputs. For example, `build_indicators` is used by the BLM AIM program to calculate standard indicators for that program and relies on a geodatabase and species list of a certain schema. For erosion modelling, `AERO` and `RHEM` produce outputs specific to those model requirements. 

### Works Cited
McCord et al. *A framework and toolset for standardizing agroecosystem indicators*. Ecological Indicators, Volume 144, 2022. [https://doi.org/10.1016/j.ecolind.2022.109511](https://doi.org/10.1016/j.ecolind.2022.109511)

Herrick et al. *Monitoring Manual for Grassland, Shrubland, and Savanna Ecosystems Volume 1: Core Methods, 2nd edition*. Jornada Experimental Range, Las Cruces New Mexico, 2017, page 30. [Available here.](http://www.landscapetoolbox.org/wp-content/uploads/2023/01/MMGSSE_20200211.pdf)
