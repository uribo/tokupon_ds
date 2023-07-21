library(reprex)
# library(styler)
options(
  reprex.highlight.hl_style  = "base16/github",
  reprex.highlight.font      = "Courier",
  reprex.highlight.font_size = 36
)
reprex({
  source("data-raw/zoo.R")
  df_zoo$body_length_cm
},wd = getwd(), venue = "rtf")

reprex({
  x <- c(1, 10, 5, 3, 7)
  (1 + 10 + 5 + 3 + 7) / length(x)
  # mean()関数を用いて平均値を計算します。
  mean(x)
}, venue = "rtf")
reprex({
  x <- c(1, 10, 5, 3, 7)
  # xの数値は大きさの順番になっていないので並び替える
  sort(x)
  sort(x)[3]
  median(x)
  
  # データの個数が偶数の場合の中央値の求め方
  x <- c(1, 2, 4, 6)
  # 真ん中の両隣の値の平均値を中央値とする
  median(x)
}, venue = "rtf")
reprex({
  library(palmerpenguins)
  quantile(penguins$flipper_length_mm, na.rm = TRUE)
}, venue = "rtf")
reprex({
  x <- c(5, 1, 3, 5, 10, 5, 3, 7)
  # 最頻値を求めます
  names(which(table(x) == max(table(x))))
}, venue = "rtf")

reprex({
  x <- c(5, 1, 3, 5, 10, 5, 3, 7)
  range(x)
  min(x)
  max(x)
}, venue = "rtf")

reprex({
  set.seed(12)
  x <- 
    sort(round(rnorm(n = 40, mean = 48, sd = 16), digits = 0))
  x[32:40] <- x[32:40] + rpois(n = 9, lambda = 10)
  
  
  # クラス中の40人のテストの点数（点数順）
  x
  mean(x) # クラスの平均点
  median(x) # クラスの点数の中央値
  
  x[1:20]
  
  x[21:40]
}, venue = "rtf")

reprex({
  library(palmerpenguins)
  library(dplyr, warn.conflicts = FALSE)
  df <- 
    penguins |> 
    filter(species == "Adelie") |> 
    select(body_mass_g) |> 
    filter(!is.na(body_mass_g)) |> 
    slice_head(n = 5)
  # df
  df <- 
    df |> 
    # 各値について偏差 deviation（平均よりもいくら大きいか小さいか）を求める
    mutate(deviation = body_mass_g - mean(df$body_mass_g, na.rm = TRUE))
  # df
  
  df <- 
    df |> 
    mutate(deviation2 = deviation^2)
  
  df
  
  sum(df$deviation2) / nrow(df)
  
}, venue = "rtf")
reprex({
  library(dplyr, warn.conflicts = FALSE)
  source("data-raw/zoo.R")
  df_zoo$taxon
}, wd = getwd(), venue = "rtf")

reprex({
  library(palmerpenguins)
  weight_freq <- 
    table(cut(penguins$body_mass_g, 
              breaks = seq(2000, 
                           7000, 
                           by = 1000),
              dig.lab = 4))
  tibble::tibble(
    class = names(weight_freq),
    frequency = weight_freq)
}, venue = "rtf")
reprex({
  library(ggplot2)
  library(palmerpenguins)
  knitr::opts_knit$set(upload.fun = identity)
  penguins |> 
    ggplot(aes(body_mass_g)) +
    # ヒストグラムでは柱の階級をビン bin と呼びます
    geom_histogram(bins = 5) +
    ylab("Frequency") +
    xlab("Body mass (g)") +
    labs(title = "ペンギンの体重のヒストグラム")
}, venue = "rtf")
reprex({
  library(ggplot2)
  library(dplyr, warn.conflicts = FALSE)
  source("data-raw/zoo.R")
  source("scripts/color_palette.R")
  knitr::opts_knit$set(upload.fun = identity)
  df_zoo |> 
    filter(!is.na(body_length_cm)) |> 
    group_by(taxon) |> 
    mutate(body_length_median = median(body_length_cm)) |> 
    ungroup() |> 
    mutate(taxon = forcats::fct_reorder(taxon, body_length_median)) |> 
    ggplot(aes(taxon, body_length_cm, color = taxon)) +
    geom_boxplot() +
    coord_flip() +
    scale_colour_tokupon() +
    guides(color = "none") +
    labs(title = "動物データの分類群ごとの体長の箱ヒゲ図")
}, wd = getwd(), venue = "rtf")

reprex({
  library(palmerpenguins)
  library(dplyr, warn.conflicts = FALSE)
  # ペンギンデータから2件分を取り出して共分散を求めます
  df <- 
    penguins |> 
    slice_head(n = 2) |> 
    select(flipper_length_mm, bill_length_mm)
  df
}, venue = "rtf")
reprex({
  library(palmerpenguins)
  library(dplyr, warn.conflicts = FALSE)
  df <- 
    penguins |> 
    slice_head(n = 2) |> 
    select(flipper_length_mm, bill_length_mm)
  df <- 
    df |> 
    mutate(across(everything(),.fns = mean, .names = "{.col}_mean")) |> 
    rowwise() |> 
    mutate(flipper_length_deviation = flipper_length_mm - flipper_length_mm_mean,
           bill_length_deviation = bill_length_mm - bill_length_mm_mean) |> 
    mutate(deviation_cross = flipper_length_deviation * bill_length_deviation) |> 
    ungroup()
  
  # df |>
  #   select(c(1:2, 5:7)) |>
  #   gt::gt() |>
  #   gt::summary_rows(fns = list(mean = "mean"),
  #              columns = c(flipper_length_mm, bill_length_mm)) |>
  #   gt::summary_rows(fns = list(total = "sum"),
  #              columns = c(flipper_length_deviation, bill_length_deviation, deviation_cross)) |>
  #   gt::gtsave("out.png")
  
  df
}, venue = "rtf")

reprex({
  library(palmerpenguins)
  library(dplyr, warn.conflicts = FALSE)
  df_mm <- 
    penguins |> 
    select(flipper_length_mm, bill_length_mm) |> 
    purrr::set_names(c("flipper_length", "bill_length"))
  cov(df_mm$flipper_length, df_mm$bill_length, use = "complete.obs")
  df_cm <- 
    df_mm |> 
    transmute(across(everything(), .fns = ~ .x / 10))
  cov(df_cm$flipper_length, df_cm$bill_length,  use = "complete.obs")
}, venue = "rtf")

reprex({
  library(palmerpenguins)
  cor(penguins$flipper_length_mm, penguins$bill_length_mm, use = "complete.obs")
  
  df_cm <- 
    penguins |> 
    dplyr::select(flipper_length_mm, bill_length_mm) |> 
    purrr::set_names(c("flipper_length", "bill_length")) |> 
    dplyr::transmute(across(everything(), .fns = ~ .x / 10))
  cor(df_cm$flipper_length, df_cm$bill_length,  use = "complete.obs")
}, venue = "rtf")
reprex({
  library(ggplot2)
  library(dplyr)
  source("data-raw/zoo.R")
  source("scripts/color_palette.R")
  knitr::opts_knit$set(upload.fun = identity)
  df_zoo |>
    filter(!is.na(body_length_cm)) |> 
    ggplot(aes(name, body_length_cm, fill = taxon)) +
    geom_bar(stat = "identity") +
    scale_fill_tokupon() +
    xlab(NULL) +
    ylab("体長 (cm)") +
    labs(title = "とくしま動物園で飼育される動物の標準的な体長")
  
  df_zoo |>
    filter(!is.na(body_length_cm)) |> 
    ggplot(aes(forcats::fct_reorder(name, body_length_cm), body_length_cm, fill = taxon)) +
    geom_bar(stat = "identity") +
    scale_fill_tokupon() +
    coord_flip() +
    xlab(NULL) +
    ylab("体長 (cm)") +
    labs(title = "とくしま動物園で飼育される動物の標準的な体長")
}, wd = getwd(), venue = "rtf")
reprex({
  library(ggplot2)
  library(dplyr)
  source("data-raw/zoo.R")
  source("scripts/color_palette.R")
  knitr::opts_knit$set(upload.fun = identity)
  df_zoo |> 
    count(taxon) |> 
    mutate(prop = n / sum(n) * 100) |> 
    ggplot(aes(x = "", y = prop, fill = taxon)) +
    geom_bar(stat = "identity", width = 1) +
    scale_fill_tokupon() +
    coord_polar("y")
}, wd = getwd(), venue = "rtf")

tibble::tribble(
  ~相関係数, ~相関の強さ,
  "\u00b10.7以上", "とても強い",
  "\u00b10.4~0.7", "やや強い",
  "\u00b10.2~0.4", "弱い",
  "\u00b10.2以下", "ほとんどなし") |> 
  gt::gt() |> 
  gt::gtsave("out.png")

anscombe |> 
  gt::gt() |> 
  gt::gtsave("out.png")

# penguins |> 
#   head() |> 
#   gt::gt() |> 
#   gt::gtsave("out.png")
# df_zoo |> 
#   head() |> 
#   gt::gt() |> 
#   gt::gtsave("out.png")

# count(df_zoo, taxon, sort = TRUE, name = "frequency") |> 
#   gt::gt() |> 
#   gt::gtsave("out.png")

set.seed(123)
tibble::tibble(
  `サイコロの目` = sample(seq.int(6), size = 100, replace = TRUE)
) |> 
  count(`サイコロの目`, name = "frequency") |> 
  gt::gt() |>
  gt::gtsave("out.png")


# さまざまな分散
tibble::tibble(
  id = rep(seq.int(1, 4), each = 5),
  x = c(c(0, 0, 0, 0, 0),
        c(1, 2, 3, 2, 1),
        c(1, 100, 5, 8, 1),
        c(1, 6, 40, 56, 1))) |> 
  group_by(id) |> 
  mutate(mean = mean(x),
         variance = paste("variance = ", var(x))) |> 
  ungroup() |> 
  ggplot(aes(x = "", y = x, color = variance)) +
  geom_point(alpha = 0.75) +
  coord_flip() +
  scale_colour_tokupon() +
  guides(color = "none") +
  facet_wrap(~ variance, ncol = 4, scales = "free_x")
ggsave("out.png", last_plot(), width = 12, height = 2)

