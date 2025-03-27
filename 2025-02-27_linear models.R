#NC: 2025-02-25
# 3.5: Linear Models

# y = B1 + B2 * x + error

# Model assumptions are sometimes too strict and sometimes too loose... depends.

# need to understand your data and sometimes be experienced, be careful and thoughtful. 

# sometimes data doesn't perfeclty fit on a line, but it's pretty ideal. There is also an additional error term, residuals (measured error).

# Assumptions: Rules on what needs to be true to make a good linear model. Sometimes these can be too strict or too loose. You want to have a good instict and have a good understanding of your data. 

# 1) linear relationship
# 2) normality of the model residuals (difference between the observed data and the model predictions)
# 3) no or little multi-co linearity (depends on your goals though) --> hard to apportion the relative variance explained, when your ind. variables are co-linear.
# 4) no auto-correlation (samples are independent and not influenced by each other, ex: spatial correlation in chlorophyll sediments)
# 5) homoscedasticity (want to have homogeneity of variance in residuals across the independent variables)

#Can transform data to make it linear to allow us to put it in a model. As long as the parameters (betas) are linear. 

library(tidyverse)
library(palmerpenguins)
library(GGally)


head(penguins)
penguins %>% 
  select(bill_length_mm, bill_depth_mm) %>%
 GGally :: ggpairs() # comes from GGally

# neither of these are Gaussian (or normal), a bit lousy, plus the correlation is negative... say what??

# can call a package with two colons before you call the function. 

# function uses lm (y ~ x, data )
lm_1 = lm(bill_depth_mm ~ bill_length_mm, data = penguins)
lm_1

# the model is statistically significant (y intercept and slope)
# variation. 

# adjusted R squared is always a bit worse than the R-squared, it has been reduced based on how complex the model is.
# ** if you have to choose one choose the adjusted. 

# the adjusted R squared is saying that bill length only explains about 5% of variation in bill depth. ** Everything is significant but it isn't telling us much about bill depth. 

class(lm_1)
summary(lm_1) # provides basic stats on the residuals! (from -4 to 4)

# object of class lm is a list that contains various components such as the coefficients or residuals 
lm_1$coef
lm_1$coefficients[1]
lm_1$residuals
lm_1$residuals[1]

## Now We Plot It ##

# this is a quick model visualization.
ggplot(data = penguins, aes(x=bill_length_mm, y = bill_depth_mm)) + 
  geom_point()+
  geom_smooth(method = "lm")
# we want method = "lm", withouot it it looks odd. 

# using geom smooth to model of the fly is a bad practice. okay for quick visualization but don't want this to be permanent

# hints that as bill length gets bigger, bill depth gets smaller... hmmmn

# plot it again. 
plot(lm_1) #provides many plots in the console.

# residuals are pretty large for low values of peak depth and then overshoot and undershoot, thus we are not seeing homoscedasticity.

#it automatically tells us the row numbers that correspond to the most extreme outlines.

# QQ plot: sort of S shaped. 

# residuals vs leverage: how much leverage does a single point have in altering the slope. 


gentoo = penguins %>%
  filter(species == "Gentoo")

gentoo %>%
  select(bill_length_mm,bill_depth_mm)%>%
  ggpairs()

lm_2 = lm (bill_depth_mm ~ bill_length_mm, data = gentoo)
summary(lm_2)
# less extreme range in the residulas. 
# Adjusted R-Squared is attributing 0.41, much better than before. 

# no, plot
ggplot(data = gentoo, aes(x=bill_length_mm, y = bill_depth_mm)) +
  geom_point()+
  geom_smooth(method = "lm")
# no cluster that cant be unexplained, looks better and better explains the variation. 


ggplot(data = penguins)+
geom_point(aes(x=bill_length_mm, y = bill_depth_mm, color = species))+
geom_smooth(aes(x = bill_length_mm, y = bill_depth_mm), method = "lm", color = "black")+
geom_smooth(aes(x = bill_length_mm, y = bill_depth_mm, color = species), method = "lm")

# visualizing 4 models, all together, and each species. 
# slick way to create separate models for each species by grouping by color.

# observing Simpsons paradox, when adding all species you get the opposite from the true relationship. It occurs when trends that appear when a data set is separated into groups, reverse, when the data are aggregated.
  
## go back and find the error

## EXERCISE 3.5.1 ##
# build a linear model predicting Gentoo bill depth as a function of flipper length. plot the predictions. Which explanatory variable (bill length vs. flipper length) does a better job of predicting bill depth. 

lm_3 = lm (bill_depth_mm ~ flipper_length_mm, data = gentoo)
summary(lm_3)

ggplot(data = gentoo, aes(x=flipper_length_mm, y = bill_depth_mm)) + 
  geom_point()+
  geom_smooth(method = "lm")


#Do we want to understand what is driving y, or just predict a really good y. This will change how you desing your model.

# based on our understanding of the data 

# rm(gentoo) removes things from environment that were misspelled



##########################

## 3.5.2025 ##

##########################


## Multiple Regressions ##

# here is a filter right below
penguins_lm_3 = penguins %>%
  filter(!is.na(bill_depth_mm),
         !is.na(bill_length_mm),
         !is.na(species))

lm_3 = lm(bill_depth_mm ~ bill_length_mm + species, data = penguins_lm_3)

summary(lm_3)

coef(lm_3)
lm_3$coefficients

anova(lm_3)

# library(broom)
# tidy(lm_3)

# load the package and call the function (above), or look in the broom package and call the functio (below)


popper = broom::tidy(lm_3, conf.int = TRUE, conf.level = 0.95)
popper
# puts popper in a tibble / data frame. 
# set CI low and high and can change the level

popper$std.error


#install.packages("ggiraph")
library(ggiraph)

#install.packages("ggiraphExtra")
library(ggiraphExtra)


ggPredict(lm_3, se = TRUE, interactive = TRUE)
# all these species are parallel because they cannot interact. all will cross the y-acis at a different spot (bill length vs species)

# if one species should have a steeper or shallower slope, that's where interactions come in.


## Here is How We SHOULD Model ## 
#the line is the model prediction

# now we use model predictions by giving it data. 

# using the predict function in base R.

lm_3_predictions = predict(lm_3, interval = "confidence", level = 0.95)
head(lm_3_predictions)


penguins_lm3_predict = cbind(penguins_lm_3, lm_3_predictions)
head(penguins_lm3_predict)
# original data + bill depth prediction (fit, lower, and upper)

## Plot It!! ##

ggplot(data = penguins_lm3_predict,
       aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() + 
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill=species, color = NULL),alpha = 0.4)+
  geom_line(aes(y=fit),linewidth = 0.9) + theme_bw()
# the range of each of the lines is limited to the range of each species

# Note: it is dangerous to make a model outside of the boundaries of the data range you provided it.

## Next, make data predictions on new data (data outside of the original data) ##


# right here we are making data so that we can put it in our model (found the min and max range and then created a seq from the min to max going by .1)
min_bill = min(penguins$bill_length_mm, na.rm = TRUE)
max_bill = max(penguins$bill_length_mm, na.rm = TRUE)

newdata_bill_length_mm = seq(from=min_bill, to = max_bill, by = 0.1)
head(newdata_bill_length_mm)
tail(newdata_bill_length_mm)

# expand.grid: new data set species 
unique(penguins$species)

# this is using base R
newdata = expand.grid(bill_length_mm = newdata_bill_length_mm,
                      species = unique(penguins$species))
# important to use the same variable names that lm_3 would recognize, and expands it across species. 

head(newdata)
tail(newdata)
summary(newdata)

# we did all the above to make fake x variables we can feed into our model. 

new_predictions = predict(lm_3, interval = "confidence", newdata=newdata) #first new data is the name of the parameter, second new data is what we created, otherwise it will use the data associated with lm_3
head(new_predictions)

newdata_predict_lm_3 = cbind(newdata,new_predictions)
newdata_predict_lm_3
head(newdata_predict_lm_3)

ggplot() + 
  geom_point(data = penguins_lm_3, 
             aes (x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_ribbon(data = newdata_predict_lm_3, 
              aes (x = bill_length_mm, ymin = lwr, ymax = upr, fill = species, color = NULL),
              alpha = 0.5) + 
  geom_line(data = newdata_predict_lm_3, aes(y = fit, x = bill_length_mm, color = species))

# now, do it the tidyverse way

# start with a new table name of results. start with the model, pipe from the model to a function in the broom package. Here we use the augment function instead of the predict function. 
lm_3_predict = lm_3 %>%
  broom::augment(data = penguins_lm_3, se_fit = TRUE, interval = "confidence")
# adding se_fit = True, and interval, gives us the CI around our ribbon. 

head(lm_3_predict)
glimpse(lm_3_predict)

# now plot it..!
ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = species), data = lm_3_predict)+
  geom_point()+
  geom_ribbon(aes(ymin= .lower, ymax = .upper, fill = species, color = NULL), alpha = 0.5) +
geom_line(aes(y = .fitted))+theme_bw()

# now use the expand function in the tidy package. # similar to base R expand.grid. take all the bill lengths and all the species and make sure all possible combinations are accounted for. (now 32.1 is paired with adelie, chinstrap, and gentoo)

penguins %>%
  arrange(bill_length_mm) %>% 
  print()

newdata = penguins_lm_3 %>%
  tidyr::expand(bill_length_mm, species)
head(newdata)
penguins %>% arrange(bill_length_mm)

lm_3_predict = lm_3 %>% 
  broom :: augment (newdata = newdata, se_fit = TRUE, interval = "confidence")
head(lm_3_predict)

# predict the y for all the x's that we created in our expanded data frame (tibble). Standard error = TRUE and the interval as confidence.


# plot it...

ggplot() +
  geom_point(data = penguins_lm_3, aes (x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_ribbon(data = lm_3_predict, 
              aes(ymin = .lower, ymax = .upper, x = bill_length_mm, fill = species),
              alpha = 0.5) + 
  geom_line(data = lm_3_predict,
            aes(y = .fitted, x = bill_length_mm,color = species))+
  theme_bw()
# main takeaway - can visualize more easily by color coding the categories. 

## somewhere right before this, something is named the same thing. Make sure to run in order. 

# predictions that we made with a simple two variable multiple regression model, one was continuous, one categorical.


#################################

## New Information After Spring Break 3.18.25##

##################################


## Now try an Interaction term

# the lines were all parallel because we did not have a model that allowed the slope of the line to change with each species.. 

# any time you add more information, the model should become better. 

# look at R-Squared AND the adjusted R Squared and see how they increase or decrease when you alter the model. 

# can look at AIC function in the console. Better model is the model with a SMALLER AIC value two decimals and it must be using the same data. 
# AIC(lm_3, lm_4)

# depth ~ length + species + length*species  --> the last part is the interaction term

lm_4 = lm(bill_depth_mm ~ bill_length_mm * species, data = penguins_lm_3)
# now we get an interaction between bill length and species (allows different slopes). 

# interaction between bill length and species. slope for bill length, then corrects for chinstrap and gentoo species. interaction between bill length for chinstrap and gentoo. 

# not the best model becuase by adding another layer, it attrbiutes less value (Lower adjusted R squared. )

summary(lm_4)
AIC(lm_3,lm_4)
# lm_3 is a lower number

step(lm_4)
best_model = step(lm_4)
summary(best_model)
# shows you the most complex model and then drops terms (AIC may get smaller and in this case that means the AIC value for the slightly less compelex model)

dumb_model = lm(bill_depth_mm ~ bill_length_mm : species, data = penguins_lm_3)

# if you're interested in the interaction of predictors, you also want to be examining those predictions individually. Don't examine an interaction without also examining them all individually. 

library(broom)

lm_4_predict = lm_4 %>% 
augment(se.fit = TRUE, interval = "confidence")

ggplot() +
  geom_point(data = penguins_lm_3, aes (x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_ribbon(data = lm_4_predict, 
              aes(ymin = .lower, ymax = .upper, x = bill_length_mm, fill = species),
              alpha = 0.5) + 
  geom_line(data = lm_4_predict,
            aes(y = .fitted, x = bill_length_mm,color = species))+
  theme_bw()


## Multiple Continuous Predictions :O ##

# look at bill depth as a function of bill length, flipper length, and body mass.

# for now only doing gentoo, so filter gentoo data here.
gentoo = penguins %>%
  filter(species == "Gentoo")
head(gentoo)

# bill depth as a function of bill length (significant)
lm_gentoo_1 = lm(bill_depth_mm ~ bill_length_mm, data = gentoo)
summary (lm_gentoo_1)

# slightly more complex. 
# bill depth as a function of bill length + flipper length (a second continuous term) (significant)
lm_gentoo_2 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm, data = gentoo)
summary(lm_gentoo_2)

# slightly more complex. 
# bill depth as a function of bill length + flipper length + body mass (two more continous terms) significant.

lm_gentoo_3 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm + body_mass_g, data = gentoo)
summary (lm_gentoo_3)

# do these to check which model is the best (in this case the best model is the most complex)
AIC(lm_gentoo_1, lm_gentoo_2, lm_gentoo_3) #option 1
step(lm_gentoo_3) #option 2

# you're answering 1 of two questions #
# do you want to find the best possible prediction of y or what drives y? 


# Now, conduct the variance inflation factor test (VIF). This is in the car library. 

library(car)

# feed the VIF function the most complex model. 

vif(lm_gentoo_3)

library(dplyr)

# if one of the numbers is greater than 10 it's very co-linear and is not adding a lot of independent information significance. 5-10 range... sorta okay. Less than 2... Good. 


# generating new prediction and hold 1 variable constant at a time. 

median_bill_length_mm = median (gentoo$bill_length_mm, na.rm = TRUE)
median_flipper_length_mm = median (gentoo$flipper_length_mm, na.rm = TRUE)

# select body mass and make bill length and flipper length constant. Set bill length and flipper length to median values. 
newdata = gentoo%>%
  select(body_mass_g) %>%
  mutate(bill_length_mm = median_bill_length_mm,
          flipper_length_mm = median_flipper_length_mm)
newdata

lm_gentoo_3_predict = lm_gentoo_3 %>%
  broom::augment(newdata = newdata, se_fit = TRUE, interval = "confidence")
head(lm_gentoo_3_predict)

ggplot()+
  geom_point(aes(x=body_mass_g, y = bill_depth_mm), data = gentoo) + 
  geom_ribbon(aes(ymin = .lower, ymax = .upper, x = body_mass_g),
              data = lm_gentoo_3_predict,
              alpha = 0.5) +
  geom_line(aes(y = .fitted, x = body_mass_g), data = lm_gentoo_3_predict) + 
  annotate ("text", x = 4250, y = 17, label = paste0("flipper length (mm) = ", median_flipper_length_mm)) + 
  annotate ("text", x = 4250, y = 16.5, label = paste0("bill length (mm) = ", median_flipper_length_mm))

## ANOVA ##

penguin_lm = lm(body_mass_g ~ species + sex, data = penguins)
summary(penguin_lm)

anova(penguin_lm)

penguin_anova = aov(body_mass_g ~ species + sex, data = penguins)
summary(penguin_anova)
class(penguin_anova)

body_mass_means = penguins %>% 
  group_by(sex, species) %>%
  summarize(mean_body_mass_g =  mean(body_mass_g, na.rm = TRUE))

TukeyHSD(penguin_anova)


## Exercise 5.4 ##
# another anova for adelie penguin and islands
# run a tukey if there is significance.