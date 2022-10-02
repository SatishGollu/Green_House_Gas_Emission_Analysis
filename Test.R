#install.packages("ggplot2") # once per machine
library("ggplot2")          # in each relevant script

# Load the `dplyr` and `tidyr` libraries for data manipulation
library("dplyr")
library("tidyr")

View(midwest)
# Plot the `midwest` data set, with college education rate on the x-axis and
# percentage of adult poverty on the y-axis
ggplot(data = midwest) +
  geom_point(mapping = aes(x = percollege, y = percadultpoverty,color = state)) 


# A bar chart of the total population of each state
# The `state` is mapped to the x-axis, and the `poptotal` is mapped
# to the y-axis
ggplot(data = midwest) +
  geom_col(mapping = aes(x = state, y = poptotal))

ggplot(data = midwest) +
  geom_smooth(mapping = aes(x = state, y = poptotal))

# A plot with both points and a smoothed line
ggplot(data = midwest) +
  geom_point(mapping = aes(x = percollege, y = percadultpoverty)) +
  geom_smooth(mapping = aes(x = percollege, y = percadultpoverty))

# A plot with both points and a smoothed line, sharing aesthetic mappings
ggplot(data = midwest, mapping = aes(x = percollege, y = percadultpoverty)) +
  geom_point() + # uses the default x and y mappings
  geom_smooth() + # uses the default x and y mappings
  geom_point(mapping = aes(y = percchildbelowpovert)) # uses own y mapping

# Change the color of each point based on the state it is in
ggplot(data = midwest) +
  geom_point(
    mapping = aes(x = percollege, y = percadultpoverty, color = state)
  )

# Set a consistent color ("red") for all points -- not driven by data
ggplot(data = midwest) +
  geom_point(
    mapping = aes(x = percollege, y = percadultpoverty),
    color = "red",
    alpha = 0.7
  )

# Wrangle the data using `tidyr` and `dplyr` -- a common step!
# Select the columns for racial population totals, then
# `gather()` those column values into `race` and `population` columns
state_race_long <- midwest %>%
  select(state, popwhite, popblack, popamerindian, popasian, popother) %>%
  gather(key = race, value = population, -state) # all columns except `state`

# Create a stacked bar chart of the number of people in each state
# Fill the bars using different colors to show racial composition
ggplot(state_race_long) +
  geom_col(mapping = aes(x = state, y = population, fill = race))


# Create a percentage (filled) column of the population (by race) in each state
ggplot(state_race_long) +
  geom_col(
    mapping = aes(x = state, y = population, fill = race), position = "fill"
  )

# Create a grouped (dodged) column of the number of people (by race) in each state
ggplot(state_race_long) +
  geom_col(
    mapping = aes(x = state, y = population, fill = race), position = "dodge"
  )

# Plot the `midwest` data set, with college education rate on the x-axis and
# percentage of adult poverty on the y-axis. Color by state.
ggplot(data = midwest) +
  geom_point(mapping = aes(x = percollege, y = percadultpoverty, color = state))


# Plot the `midwest` data set, with college education rate and
# percentage of adult poverty. Explicitly set the scales.
ggplot(data = midwest) +
  geom_point(mapping = aes(x = percollege, y = percadultpoverty, color = state)) +
  scale_x_continuous() + # explicitly set a continuous scale for the x-axis
  scale_y_continuous() + # explicitly set a continuous scale for the y-axis
  scale_color_discrete() # explicitly set a discrete scale for the color aesthetic


# Create a better label for the `inmetro` column
labeled <- midwest %>%
  mutate(location = if_else(inmetro == 0, "Rural", "Urban"))

# Subset data by state
wisconsin_data <- labeled %>% filter(state == "WI")
michigan_data <- labeled %>% filter(state == "MI")

# Define continuous scales based on the entire data set:
# range() produces a (min, max) vector to use as the limits
x_scale <- scale_x_continuous(limits = range(labeled$percollege))
y_scale <- scale_y_continuous(limits = range(labeled$percadultpoverty))

# Define a discrete color scale using the unique set of locations (urban/rural)
color_scale <- scale_color_discrete(limits = unique(labeled$location))

# Plot the Wisconsin data, explicitly setting the scales
ggplot(data = wisconsin_data) +
  geom_point(
    mapping = aes(x = percollege, y = percadultpoverty, color = location)
  ) +
  x_scale +
  y_scale +
  color_scale

# Plot the Michigan data using the same scales
ggplot(data = michigan_data) +
  geom_point(
    mapping = aes(x = percollege, y = percadultpoverty, color = location)
  ) +
  x_scale +
  y_scale +
  color_scale


# Change the color of each point based on the state it is in
ggplot(data = midwest) +
  geom_point(
    mapping = aes(x = percollege, y = percadultpoverty, color = state)
  ) +
  scale_color_brewer(palette = "Set3") # use the "Set3" color palette

# Create a horizontal bar chart of the most populous counties
# Thoughtful use of `tidyr` and `dplyr` is required for wrangling

# Filter down to top 10 most populous counties
top_10 <- midwest %>%
  top_n(10, wt = poptotal) %>%
  unite(county_state, county, state, sep = ", ") %>% # combine state + county
  arrange(poptotal) %>% # sort the data by population
  mutate(location = factor(county_state, county_state)) # set the row order

# Render a horizontal bar chart of population
ggplot(top_10) +
  geom_col(mapping = aes(x = location, y = poptotal)) +
  coord_flip() # switch the orientation of the x- and y-axes


# Create a better label for the `inmetro` column
labeled <- midwest %>%
  mutate(location = if_else(inmetro == 0, "Rural", "Urban"))

# Create the same chart as Figure 16.9, faceted by state
ggplot(data = labeled) +
  geom_point(
    mapping = aes(x = percollege, y = percadultpoverty, color = location),
    alpha = .7
  ) +
  facet_wrap(~state) # pass the `state` column as a *fomula* to `facet_wrap()`



state# Adding better labels to the plot in Figure 16.10
ggplot(data = labeled) +
  geom_point(
    mapping = aes(x = percollege, y = percadultpoverty, color = location),
    alpha = .6
  ) +
  
  # Add title and axis labels
  labs(
    title = "Percent College Educated versus Poverty Rates", # plot title
    x = "Percentage of College Educated Adults", # x-axis label
    y = "Percentage of Adults Living in Poverty", # y-axis label
    color = "Urbanity" # legend label for the "color" property
  )

