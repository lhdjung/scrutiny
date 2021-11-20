

# This function used to be inserted into the definition of functions such as
# `grim_map()` using code like below:

#if (inherits(data, "scr_split_by_parens")) {
#  data <- transform_split_parens_object(data, "x", "n")
#}



transform_split_parens_object <- function(data) {

  class_d <- class(data)

  end1 <- class_d[stringr::str_detect(class_d, "scr_end1_")]
  end1 <- stringr::str_remove(end1, "scr_end1_")

  end2 <- class_d[stringr::str_detect(class_d, "scr_end2_")]
  end2 <- stringr::str_remove(end2, "scr_end2_")

  uscore_end1 <- paste0("_", end1)
  uscore_end2 <- paste0("_", end2)

  cols_1 <- data %>%
    dplyr::select(contains(uscore_end1)) %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = ".origin",
                        values_to = end1)

  cols_1 <- cols_1 %>%
    dplyr::mutate(key = 1:nrow(cols_1))

  cols_2 <- data %>%
    dplyr::select(contains(uscore_end2)) %>%
    tidyr::pivot_longer(cols = everything(),
                        names_to = ".origin_2",
                        values_to = end2)

  cols_2 <- cols_2 %>%
    dplyr::mutate(key = 1:nrow(cols_2))


  dplyr::left_join(cols_1, cols_2, by = "key") %>%
    dplyr::select(-.data$key, -.data$.origin_2) %>%
    dplyr::mutate(.origin = stringr::str_remove(.data$.origin, uscore_end1)) %>%
    dplyr::arrange(.data$.origin)
}



