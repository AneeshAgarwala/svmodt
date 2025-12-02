echocardiogram <- read.csv("D:/SVMODT/project-svodt/data-raw/echocardiogram.data", header=FALSE,
                           col.names = c(
                             "survival",
                             "still_alive",
                             "age_at_heart_attack",
                             "pericardial_effusion",
                             "fractional_shortening",
                             "epss",
                             "lvdd",
                             "wall_motion_score",
                             "wall_motion_index",
                             "mult",
                             "name",
                             "group",
                             "alive_at_1"
                             ), na.strings = "?") |>
  dplyr::select(-name, -group, -alive_at_1, -survival) |>
  na.omit(still_alive) |>
  mutate(still_alive = as.factor(still_alive))

usethis::use_data(echocardiogram, overwrite = TRUE)

