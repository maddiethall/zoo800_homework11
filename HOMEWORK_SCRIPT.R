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

# write.csv(monkey_df, "monkey_travel_data.csv", row.names = FALSE)

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


# OBJECTIVE 2

# Scenario given to me by Sophia Mummert:
# The chacma baboons at NVBP faces ecological pressure in the winter season
# in nutrition available in their Afrotemperate habitat. With this seasonal
# flux in nutrient availability, they face changes to activity budgets, seeing
# a decrease in activities such as sociality or resting. This is due to an
# increased amount of time spent on foraging.

# x: time spent foraging
# y: time spent in resting
# group: season (winter vs summer)

# Does the chacma baboon's time spent resting face similar 
# decrease in response to increased foraging
# during the summmer and winter?


