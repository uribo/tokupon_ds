if (!file.exists("data-raw/tokushima_zoo_animals22.csv")) {
  library(dplyr)
  library(rvest)
  url <-
    "https://www.city.tokushima.tokushima.jp/zoo/gallery/ichiran.html"
  x <-
    read_html(url)
  # 2016-04-01
  x |>
    html_element(css = '#main > div.h1bg > div.update.clearfix > p') |>
    html_text()
  animals <-
    x |>
    html_elements(css = '#main > div.main_inner > div > p > span') |>
    html_text() %>%
    ensurer::ensure(length(.) == 22)
  
  # リスザルはリスザル属の総称
  # ポニーは肩までの高さが147cm以下の馬の総称
  df_zoo <-
    tibble::tibble(
      taxon = c("食肉類", "鳥類", "食肉類", "鳥類", "霊長類",
                "霊長類", "霊長類", "食肉類", "齧歯類", "食肉類",
                "鳥類", "偶蹄類", "食肉類", "食肉類", "鳥類",
                "食肉類", "霊長類", "鳥類", "鯨偶蹄類", "奇蹄類",
                "齧歯類", "鯨偶蹄類"),
      name = animals,
      body_length_cm = c(63.5, 100, 64, 110, 85,
                         66, 80, 168, 134, 250,
                         130, 175, 31, NA_real_, 1.2,
                         250, 35, 69, NA_real_, NA_real_,
                         40, NA_real_),
      weight_kg = c(6, 3.5, 5.4, 6.5, 60,
                    10, 20, 80, 66, 225,
                    # ホッキョクグマは平均体重のうち最大値を採用
                    9, 220, 0.9, 30.3, 15,
                    410, 1.1, 6, 140, NA_real_,
                    1.5, NA_real_)) %>%
    assertr::verify(dim(.) == c(22, 4))
  df_zoo |> 
    readr::write_csv("data-raw/tokushima_zoo_animals22.csv")
} else {
  df_zoo <-
    readr::read_csv("data-raw/tokushima_zoo_animals22.csv", 
                    col_types = "ccdd")
}
