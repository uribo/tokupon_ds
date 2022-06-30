library(magrittr)
df_zoo_name <- 
  tibble::tibble(
    name = c("レッサーパンダ", "ホオジロカンムリヅル", 
             "コツメカワウソ", "カナダガン", "チンパンジー", 
             "シシオザル", "マントヒヒ", "ピューマ", "カピバラ", 
             "ライオン", "アフリカハゲコウ", "シロオリックス", 
             "ミーアキャット", "シンリンオオカミ", "アンデスコンドル", 
             "ホッキョクグマ", "リスザル", "フンボルトペンギン", 
             "ラマ", "ポニー", "モルモット", "ヒツジ"),
    scientific_name = c("Ailurus fulgens", "Balearica regulorum", 
                        "Aonyx cinerea", "Branta canadensis", "Pan troglodytes",
                        "Macaca silenus", "Papio hamadryas", "Puma concolor", 
                        "Hydrochoerus hydrochaeris", "Panthera leo",
                        "Leptoptilos crumeniferus", "Oryx dammah", "Suricata suricatta", 
                        "Canis lupus", "Vultur gryphus",
                        "Ursus maritimus", NA_character_, "Spheniscus humboldti",
                        "Lama glama", NA_character_,
                        "Cavia porcellus", "Ovis aries")) |> 
  dplyr::mutate(link = paste0("https://ja.wikipedia.org/wiki/",
                              dplyr::recode(name,
                              `アンデスコンドル` = "コンドル",
                              `ラマ` = "リャマ"))) %>% 
  dplyr::filter(!is.na(scientific_name)) %>% 
  assertr::verify(dim(.) == c(20, 3))

if (file.exists("data-raw/tokushima_zoo_animals_conservation_status.rds") == FALSE) {
  library(rredlist)
  animal_scname <- 
    df_zoo_name |> 
    purrr::pluck("scientific_name")
  
  if (file.exists("data-raw/redlist_search_raw.rds") == FALSE) {
    result_red_list_search <-
      animal_scname |> 
      purrr::map_dfr(
        ~ tibble::as_tibble(purrr::pluck(rredlist::rl_search(name = .x), 
                                         "result")))
    result_red_list_search |> 
      readr::write_rds("data-raw/redlist_search_raw.rds")    
  } else {
    result_red_list_search <-
      readr::read_rds("data-raw/redlist_search_raw.rds")
  }
  if (file.exists("data-raw/redlist_occ_country_raw.rds") == FALSE) {
    result_red_list_occ <- 
      animal_scname |> 
      purrr::set_names(animal_scname) |>
      purrr::map_dfr(~ tibble::as_tibble(purrr::pluck(rredlist::rl_occ_country(name = .x), 
                                                      "result")),
                     .id = "scientific_name")
    result_red_list_occ |> 
      readr::write_rds("data-raw/redlist_occ_country_raw.rds")
  } else {
    result_red_list_occ <- 
      readr::read_rds("data-raw/redlist_occ_country_raw.rds")
  }
  result_red_list_occ <- 
    result_red_list_occ |> 
    dplyr::select(!c(distribution_code)) |> 
    tidyr::nest(occ = c(code, country, presence, origin))
  
  df_red_list_information <-
    result_red_list_search |>
    dplyr::select(
      taxonid,
      scientific_name,
      main_common_name,
      category,
      population_trend,
      tidyselect::ends_with("_system"),
      tidyselect::starts_with("elevation_"),
      tidyselect::starts_with("depth_")) |>
    tidyr::nest(ecosystem = tidyselect::ends_with("_system"),
                limitation = c(tidyselect::ends_with("upper"), tidyselect::ends_with("lower"))) |>
    dplyr::left_join(result_red_list_occ, by = "scientific_name")
  df_zoo_conservation <- 
    df_zoo_name |> 
    dplyr::select(!link) |> 
    dplyr::left_join(df_red_list_information, by = "scientific_name") %>%
    assertr::verify(dim(.) == c(20, 9))
  df_zoo_conservation |> 
    readr::write_rds("data-raw/tokushima_zoo_animals_conservation_status.rds")
} else {
  df_zoo_conservation <-
    readr::read_rds("data-raw/tokushima_zoo_animals_conservation_status.rds")
}
