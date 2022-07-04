library(gt)
library(dplyr)
library(palmerpenguins)
set.seed(3)
penguins_smaple <- 
  penguins |> 
  slice_sample(n = 20)
weight_freq <- 
  penguins_smaple |> 
  pull(body_mass_g) |> 
  cut(breaks = seq(2000, 
                   7000, 
                   by = 1000),
      dig.lab = 4) |> 
  table()
tibble::tibble(
  class = names(weight_freq),
  frequency = weight_freq) |> 
  arrange(desc(class)) |> 
  gt::gt() |> 
  gt::gtsave("images/gt_penguins_weight_frequency_table.png")

quantile(penguins_smaple$body_mass_g)
summary(penguins_smaple$body_mass_g)
boxplot(penguins_smaple$body_mass_g)
