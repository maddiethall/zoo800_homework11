#############################
######## Homework 11 ########
####### Maddie Thall ########
#############################

library(ggplot2)

# OBJECTIVE 1

set.seed(123)

n_total = 100
Habitat = rep(c("Primary", "Secondary"), each = n_total / 2)
meanDailyTemp_C = runif(n_total, min = 20, max = 35)

beta_0 = 4 # intercept for Primary forest
beta_1 = -0.12 # slope for Primary forest
beta_2 = 2 # intercept difference for Secondary forest 
beta_3 = -0.05 # slope difference for Secondary forest

error = rlnorm(n_total, meanlog = 0, sdlog = 0.5)

meanDailyDistance_km = beta_0 + 
  beta_1 * meanDailyTemp_C + 
  beta_2 * (Habitat == "Secondary") + 
  beta_3 * meanDailyTemp_C * (Habitat == "Secondary") + 
  error

monkey_df = data.frame(
  Habitat,
  meanDailyTemp_C,
  meanDailyDistance_km
)

#### Scenario:
# Primate habitats are often fragmented due to anthropogenic pressures
# such as deforestation and land-use change. Researchers are interested
# in understanding how habitat type (primary vs. secondary forest) and
# temperature influence the daily travel distance of primates,
# and whether the effect of temperature on travel distance differs
# between habitat types due to differing food distributions and tree cover.

## Two-level factor: habitat ("Primary" and "Secondary")
## Continuous predictor: meanDailyTemp_C (mean daily temperature in Celsius)
## Response variable: meanDailyDistance_km (mean daily travel distance in kilometers)

#### Question: How does temperature and habitat affect travel distance, 
#### and do monkeys in Primary and Secondary forests respond differently
#### to changes in mean daily temperature in terms of their daily travel distance?


########### OBJECTIVE 2 ########################################################

# Scenario given to me by Sophia Mummert:
# The chacma baboons at NVBP faces ecological pressure in the winter season
# in nutrition available in their Afrotemperate habitat. With this seasonal
# flux in nutrient availability, they face changes to activity budgets, seeing
# a decrease in activities such as resting. This is due to an
# increased amount of time spent on foraging, as their preferred foods are not
# available. 

# Does the chacma baboon's face a seasonal difference in time spent resting
# in response to the time spent foraging?

# Two-level factor (Season): Summer vs. Winter 
# Continuous predictor: Mean time spent foraging in hours (weekly)
# Response Variable: Mean time spent resting in hours (weekly)

baboon_df = read.csv("baboon_sim_data (2).csv")

mod1 = lm(time_spent_resting ~ time_spent_foraging * group, data = baboon_df)
summary(mod1)
# significant interaction effect between season and foraging time (p < 0.001)
# significant effect of season on resting time (p = 0.03)
# significant effect of foraging time on resting time (p < 0.001)
#### parameter estimates:
#### beta_0 = 8; beta_1 = -0.06; beta_2 = 2.14; beta_3 = -0.09

mod2 = lm(time_spent_resting ~ time_spent_foraging + group, data = baboon_df)
summary(mod2)
# removing the interaction effect from the model decreases the fit

ggplot(baboon_df, aes(x = time_spent_foraging, y = time_spent_resting, color = group)) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Seasonal Effect of Time Spent Resting vs Foraging",
    x = "Mean Weekly Foraging Time (hours)",
    y = "Mean Weekly Resting Time (hours)"
  ) +
  theme_minimal()
# different slopes for summer and winter groups, both negative correlation
# steeper slope for winter group, indicating a more severe effect of foraging time on resting time
# compared to the summer group


#### RESULTS EXPLAINATION ####
# These data show a significant effect of seasonality on the relationship between 
# time spent resting and foraging. In both winter and summer, there is a significant 
# negative correlation between time spent foraging and time spent resting, 
# meaning that as foraging time increases, resting time decreases. 
# However, the effect is more pronounced in the winter season, as indicated by the steeper 
# slope in the winter group. This suggests that during winter, when food resources are scarcer, 
# baboons need to allocate more time to foraging, which in turn reduces their resting time 
# more significantly compared to the summer season. The significant interaction effect 
# between season and foraging time indicates that the relationship between foraging and resting 
# time is influenced by seasonal changes, likely due to the varying availability of food resources.

#### Comparison to the truth:
## true parameters: beta_0 = 6; beta_1 = -0.05; beta_2 = 3; beta_3 = -0.1
# My estimates are fairly close to the true parameters,
# and Sophia confirmed that my interpretation of the results is correct!
