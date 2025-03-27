#NC: 2025-02-25
# 3.3: t-tests & 3.4 Correlations



library(tidyverse)
library(palmerpenguins)
library(rstatix)

# hyptohesis testing: 
# null hyp: two variables are the same or similar. 
# alt hyp: not the same
# usually want alt to be what you want to test 
# p value: probability of getting your data, given that the literature dervied value is true. 

head(penguins)

# is the gentoo body mass, different form a literature value

# creates data set about the gentoo penguins
gentoo = penguins %>% 
  filter(species == "Gentoo")

# visualizes a histogram real quick
ggplot()+
  geom_histogram(aes(x=body_mass_g),data=gentoo)

# QQ Plot: looks at the normality
ggplot()+
  stat_qq(aes(sample=body_mass_g), data = gentoo)
dim(gentoo %>% filter (!is.na(body_mass_g)))

# encyclopedia of life (Smithsonian)
lit_body_mass_g = 5500 #EOL, lit derived gentoo penguin body mass

# base r function: gentoo body mass g compared to mu (the literrature dervied mean)
t.test(gentoo$body_mass_g, mu = lit_body_mass_g)   

# this actually saves it
my_t_test = t.test(gentoo$body_mass_g, mu = lit_body_mass_g)  
my_t_test


summary(my_t_test)
# r has a class of data called the h test (hyp test)
class(my_t_test)

my_t_test$p.value
my_t_test$statistic

# Anther way to run a t test using rstatix

#include variable name, ~ 1 means one sample t test and then provide the mu. 

# what does a pipe do? it takes whatever is before it (in this case gentoo and puts it into the next thing as the first parameter and in this case the first parameter in a t-test is data)

x = gentoo %>% 
  t_test(body_mass_g ~ 1, mu = lit_body_mass_g)

x$statistic

#Independent Sample t-test :)

# Example: Gentoo vs Adelie Body Mass

# shrink down the data first. (only interested in gentoo and adelie for now)
data_for_t_test = penguins %>% 
  filter(species %in% c("Gentoo","Adelie"),
         !is.na(body_mass_g)) %>% 
  select(species, body_mass_g) %>%
  droplevels()

head(data_for_t_test)
summary(data_for_t_test)

data_for_t_test %>%
  group_by(species)%>%
  summarize(avg = mean(body_mass_g), sd = sd(body_mass_g))

ggplot()+
  geom_histogram(aes(x=body_mass_g, fill=species), data = data_for_t_test) + 
  facet_wrap(~species)

ggplot()+
  stat_qq(aes(sample=body_mass_g, color=species), data = data_for_t_test) + 
  facet_wrap(~species, scales="free")

# this (below) checks to see if the variances for body mass of each species (gentoo and adelie) are different. Since p is > 0.159 we are good to proceed. 

# you do NOT want this to be significant different. 

data_for_t_test %>% 
  levene_test(body_mass_g ~ species)
#variances of the two samples are similar (not significantly different) --> move onto a student test

zoo = t.test(data_for_t_test$body_mass_g ~ data_for_t_test$species, var.equal = TRUE)
zoo

# with this we determine gentoo body masses are significanlty heaverr than adelie penguins

# Welches Two Sample t-test
summary(zoo)

# same as earlier but use r statix instead. 
data_for_t_test %>%
  t_test(body_mass_g ~ species)


# Paried Sample t-test: think of weights after thanksgiving dinner

## EXERCISE 3.1 ##

####################################

# 3.4: Correlations

# correlation bill depth vs bill length

ggplot()+
  geom_point(aes(x=bill_length_mm,y=bill_depth_mm), data = gentoo)

ggplot()+
   stat_qq(data=gentoo, aes (sample = bill_length_mm))

ggplot()+
   stat_qq(data=gentoo, aes (sample = bill_depth_mm))

ggplot()+
   geom_histogram(aes(x=bill_length_mm), data=gentoo)

ggplot()+
   geom_histogram(aes(x=bill_depth_mm), data=gentoo)

test =  cor(x=gentoo$bill_depth_mm, y = gentoo$bill_length_mm, use = "complete.obs")
# they're quite correlaed! yay. 
 
class(test)

hand_test = cor.test(x=gentoo$bill_depth_mm, y=gentoo$bill_length_mm, use= "compelte.obs")
class(hand_test)
summary(hand_test)

gentoo %>% 
  cor_test(bill_length_mm, bill_depth_mm)

head(gentoo)
gentoo[,3:6]
gentoo%>% select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)
cor(gentoo[,3:6], use="complete.obs")


library(GGally) 
gentoo %>% 
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)%>%
  ggpairs()

penguins %>% 
  select (body_mass_g,ends_with("mm"),species) %>%
  ggpairs(aes(color=species))
