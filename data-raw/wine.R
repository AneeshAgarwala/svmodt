wine <- read.table("data-raw/wine.data", sep = ",", col.names = c(
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
  "od280_od315",
  "proline"
))

usethis::use_data(wine, overwrite = TRUE)
