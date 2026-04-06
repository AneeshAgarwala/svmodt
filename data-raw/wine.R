wine <- read.csv("D:/SVMODT/project-svodt/data-raw/wine.data",
  header = FALSE,
  col.names = c(
    "class",
    "alcohol",
    "malic_acid",
    "ash",
    "alcalinity_of_ash",
    "magnesium",
    "total_phenols",
    "flavanoids",
    "nonflavanoid_phenols",
    "proanthocyanins",
    "color_intensity",
    "hue",
    "od280_od315_of_diluted_wines",
    "proline"
  )
) |>
  mutate(class = as.factor(class))

usethis::use_data(wine, overwrite = TRUE)
