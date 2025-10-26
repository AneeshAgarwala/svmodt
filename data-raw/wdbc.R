wdbc <- read.csv("D:/SVMODT/project-svodt/data-raw/wdbc.csv", header=TRUE)
wdbc <- wdbc[,2:32]
wdbc$diagnosis <- as.factor(wdbc$diagnosis)

usethis::use_data(wdbc, overwrite = TRUE)
