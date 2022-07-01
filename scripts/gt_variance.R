library(dplyr)
library(gt)
library(palmerpenguins)
library(ggplot2)
df <- 
  penguins |> 
  filter(species == "Adelie") |> 
  select(body_mass_g) |> 
  filter(!is.na(body_mass_g)) |> 
  slice_head(n = 5)

df <- 
  df |> 
  mutate(deviation = body_mass_g - mean(df$body_mass_g, na.rm = TRUE),
         `deviation^2` = deviation^2)

out <- 
  df |> 
  gt() |> 
  summary_rows(fns = list(total = "sum",
                          mean = "mean"), 
               columns = c(body_mass_g, deviation, `deviation^2`))


gt::gtsave(out, filename = "images/gt_variance.png")


penguins |> 
#  filter(!is.na(body_mass_g)) |> 
  group_by(species) |> 
  summarise(mean = mean(body_mass_g, na.rm = TRUE),
            var = var(body_mass_g, na.rm = TRUE),
            sd = sd(body_mass_g, na.rm = TRUE)) |> 
  ungroup() |> 
  gt::gt() |> 
  gt::gtsave("images/gt_penguins_summary.png")

# penguins |> 
#   filter(species == "Adelie") |> 
#   ggplot(aes(body_mass_g, fill = species)) +
#   geom_histogram(bins = 20) +
#   geom_vline(xintercept = 3701) +
#   geom_vline(xintercept = 3701 + 459) +
#   geom_vline(xintercept = 3701 - 459)

penguins |> 
  mutate(species = recode(species,
                          `Adelie` = "アデリーペンギン",
                          `Gentoo` = "ジェンツーペンギン",
                          `Chinstrap` = "ヒゲペンギン")) |> 
  ggplot(aes(body_mass_g, fill = species)) +
  geom_histogram(position = "identity") +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  guides(fill = "none") +
  facet_wrap(~ species, scales = "free_x", ncol = 1) +
  labs(x = "体重 (g)",
       y = "頻度",
       title = "3種のペンギンの体重のヒストグラム")

ggsave("images/penguins_body_mass_histogram.png", last_plot(),
       width = 5,
       height = 8,
       dpi = 320)
 

# df |> 
#   ggplot(aes(body_mass_g)) +
#   geom_density() +
#   geom_vline(xintercept = mean(df$body_mass_g)) +
#   geom_vline(xintercept = mean(df$body_mass_g) + sqrt(var(df$body_mass_g))) +
#   geom_vline(xintercept = mean(df$body_mass_g) - sqrt(var(df$body_mass_g)))
