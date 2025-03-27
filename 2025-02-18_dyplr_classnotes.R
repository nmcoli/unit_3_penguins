#2025-02-18
# dyplr package (data manipulation)

# loads library
library(tidyverse)
# if you get notices about conflicts: 
# for example, the dplyr::filter() masks stats::filter() AND dplyr::lag() masks stats::lag()
# it overrights information and makes it to where one function form oen package is overwritten form another. 

# shows us which data packages are present. 
tidyverse_packages()

# in tidy data: each variable forms a column, each observation forms a row and each cell is a single measurement. 

# import --> tidy --> transform. 
# some older functions don't work with tibble. If this happens, use as.data.frame() to turn a tibble back into a data frame. 

#install.packages("palmerpenguins")
library(palmerpenguins)

head(penguins)
summary(penguins)
glimpse(penguins)
dim(penguins)

# working with dyply

gentoo = filter(penguins,species == "Gentoo")
summary(gentoo)
gentoo_ladies = filter(penguins, species=="Gentoo", sex == "female")
summary(gentoo_ladies)

# takes multiple commands and strings them together so when the run, the do so in sequence.
# %>% 

genttoo = penguins %>%
  filter(species == "Gentoo") %>%
  filter(sex ==  "female")
summary(gentoo)

female_mass = penguins %>%
  filter(sex == "female") %>%
  summarize(mean_mass_g = mean(body_mass_g))
female_mass

#base r version
female_mass = mean(penguins$body_mass_g[which(penguins$sex == "female")])
female_mass

species_mean_mass_g = penguins %>% 
  filter(!is.na(sex))%>%
  group_by(species,sex,island)%>%
  summarize(mean_mass_g = mean(body_mass_g, na.rm = TRUE),count=n()) %>% 
  print()
species_mean_mass_g

write.csv(file = "data/processed/mass_table.csv",x=species_mean_mass_g)

# temp = read_csv()

# mutate: 
penguins_for_america = penguins %>% 
  mutate(body_mass_lb = body_mass_g * 0.0022) # conversion: 0.0022 lb / grams
penguins_for_america

# distinct: 
islands = penguins %>% 
  distinct(island)
islands

species=penguins %>% 
  distinct(species)
species

# select:
penguins_brief = penguins %>%
  select(species,sex)%>%
  print()

penguins_brief = penguins %>%
  select(-body_mass_g)%>%
  print()

# sorting: 
penguins_sorted = penguins %>%
  arrange(body_mass_g,bill_depth_mm) %>%
  print()

penguins_sorted = penguins %>%
  arrange(desc(body_mass_g),(desc(bill_depth_mm))) %>%
  print()

# EXERRCISE 1.3
# go back and look at this
penguins_inches = penguins %>%
 filter(island == "Dream", species == "Adelie")%>%
 filter(!is.na(bill_length_mm))%>%
  mutate(bill_length_in = (bill_length_mm * 0.0393701))%>%
  summarize(mean_bill_length_in=mean(bill_length_in , na.rm=TRUE),
            sd_bill_length_in=sd(bill_length_in,na.rm=TRUE))%>%
  print()
