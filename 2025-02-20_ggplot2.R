#NC: 2025-02-20
# 3.2: Intro to ggplot

library(tidyverse)
library(palmerpenguins)

# a filter function has been "loaded" twice. one in dyplr (from tidyverse) and one in the default stats. The one that appears first is the one that has been most recently loaded. 

find("filter")
gentoo = penguins %>%
  dplyr::filter(species=="Gentoo") %>%
  dplyr::select(-body_mass_g)%>%
  print()
#from the filter package use :: and the function name

head(penguins)

penguins_no_nas = penguins %>%
  filter(!is.na(body_mass_g))

penguins_no_nas = penguins %>%
  filter(!is.na(sex))

ggplot(data=penguins_no_nas)+
  geom_point(aes(x=flipper_length_mm, y=body_mass_g, color=bill_length_mm,shape=sex))+
  geom_smooth(aes(x=flipper_length_mm, y = body_mass_g),method="lm")+
  xlab("Flipper Length (mm)")+
  ylab("Body Mass (g)")+
  ggtitle("Penguins are cute :)")+
  theme_bw()
# aes is a function that maps information from the data to your plot and determines how your plot looks. (for example if you put color = species it must go inside the aesthetics, where below color could go otuside becuase it was just pulling red, not species form the data)

# color can go in or outside off aes parentheses. 
# data=penguins_no_nas just makes sure that there's no na's and wont' throw a warning. 
# or do this for no na's in the sex variable, rather than the body mass variable...

# probably won't use method = "lm", it's a different type of smoothing

## EXERCISE 3.2.1 ##

penguins_ts = penguins %>% 
  group_by(species, year) %>%
  summarize(count = n())

penguins_ts

ggplot(data=penguins_ts)+
  geom_line(aes(x=year, y=count,color=species))+
  theme_classic()
# we used factors which is what have us that funky looking plot, by putting color=species in the aes function, it plots a line for each species. 

# Histograms!!

ggplot(data=penguins)+
  geom_histogram(aes(x=flipper_length_mm,
                     fill=species),
                 binwidth=2, 
                 position="identity",
                 alpha = 0.7)+
  scale_fill_manual(values=c("darkorange", "darkorchid","cyan4"))

# alpha value is the transparency value
# bind width tells me to plot a rectange polygon for in this case every 2 mm. 
# scale_fill_manual: using the fill scale and manually coloring it. 


#Boxplots: 

boxplot = ggplot(data=penguins)+
  geom_boxplot(aes(y=flipper_length_mm, x=species))+
  geom_jitter(aes(y=flipper_length_mm,x=species, color=species), width = 0.4)
# geom_jitter plots the indivdiual points slightly different in a random direction for visual purpose. 
# Bar charts: 

ggplot(data=penguins)+
  geom_bar(aes(x=island,  fill=species))+
  facet_wrap(~species,nrow=3)+
  coord_flip()

ggsave("figures/penguins_islands_species.png",device="png", units = "in", width = 5, height = 7, dpi = 300)


# facet_wrap(): give me a different panel for each species, don't forget the tilda, it means as a function of. 
# nrow: organizes the panels in rows and columns
# coord_flip(): flips coordinats, since we gave it x it swaps to y. 

# save figure as png to keep resultion. 

## EXERCISE 3.2.2
ggplot(data=penguins)+
  geom_point(aes(y=bill_depth_mm, x = bill_length_mm, color = sex))+
  facet_wrap(~species, scales = "free_y")+
  xlab("Bill Length (mm)")+
  ylab("Bill Depth (mm)")

  