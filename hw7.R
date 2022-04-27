library(tidyverse)
library(tidycensus)
library(sf)
library(terra)
library(dplyr)
library(ggplot2)

#Read raster files 
near_infrared = rast("sacramento_landsat_near_infrared.tif")
red = rast("sacramento_landsat_red.tif")


#Calculate NDVI
ndvi = (red - near_infrared) / (red + near_infrared)


#Plot NDVI
plot(ndvi)


#Retrieve information from the 2019 5-year ACS
#Create table for all variables in dataset
acs_vars = load_variables(2019, "acs5")


#Save data as csv file
write_csv(acs_vars, "acsvars.csv")


#Group wanted variables 
low_income = c(
  less_than_10k = "B25121_002",
  from_10k_to_20k = "B25121_017",
  from_20k_to_35k = "B25121_032"
)


#Retrieve census data
income = get_acs(geography = "tract", variables = low_income, state = "CA", 
          county = "Sacramento", year = 2019, survey="acs5")


#Sum of low income households
sac_low_income = 
  group_by(income, NAME) %>%
  summarize(total_low_income = sum(estimate))


#Find total number of households in all income brackets
total_income = 
  get_acs(geography = "tract", variables = "B25121_001", state = "CA", county = "Sacramento",
          year = 2019)


#Join the two datasets
join = left_join(sac_low_income, total_income, ON="NAME")


#Find the rate of households with low income by dividing the number of 
#low income households by the total number of households for each tract
join$low_income_rate = (join$total_low_income / join$estimate) * 100


#Read county tract shapefile
tract = read_sf("../plan390-hw7/tl_2010_06067_tract10/tl_2010_06067_tract10.shp")


#Rename GEOID column so it matches with the shapefile
join = rename(join, GEOID10 = GEOID)


#Join the data
tract = left_join(tract, join, by="GEOID10")


#Project data to California State Plane
tract = st_transform(tract,26943)


#Create map showing the proportion of households making less than
#$35,000 in each tract in Sacramento County
ggplot() +
  geom_sf(data=tract, aes(fill=low_income_rate)) +
  scale_fill_fermenter(palette="Greens")

  
