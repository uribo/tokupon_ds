if (file.exists("data-raw/令和2年度_学校保健統計調査_都道府県別_身長と体重_5歳.csv") == FALSE) {
  download.file("https://www.e-stat.go.jp/stat-search/file-download?statInfId=000032108465&fileKind=0",
                destfile = "data-raw/r2_hoken_tokei_05.xls")
  df_hoken_toukei <- 
    readxl::read_xls("data-raw/r2_hoken_tokei_05.xls", 
                     sheet = 1,
                     range = "B8:J55",
                     col_names = c("区分",
                                   paste("男",
                                         c(paste("身長",
                                                 c("平均値", "標準偏差"),
                                                 sep = "_"), 
                                           paste("体重",
                                                 c("平均値", "標準偏差"),
                                                 sep = "_")),
                                         sep = "_"),
                                   paste("女",
                                         c(paste("身長",
                                                 c("平均値", "標準偏差"),
                                                 sep = "_"), 
                                           paste("体重",
                                                 c("平均値", "標準偏差"),
                                                 sep = "_")),
                                         sep = "_"))) |> 
    dplyr::mutate(区分 = stringr::str_squish(区分) |> 
                    stringr::str_remove_all("[:space:]")) |> 
    dplyr::filter(区分 != "全国") |>  
    dplyr::arrange(dplyr::desc(男_身長_平均値))
  
  df_hoken_toukei |> 
    readr::write_csv("data-raw/令和2年度_学校保健統計調査_都道府県別_身長と体重_5歳.csv")
} else {
  df_hoken_toukei <-
    readr::read_csv("data-raw/令和2年度_学校保健統計調査_都道府県別_身長と体重_5歳.csv",
                    col_types = "cdddddddd")
}
