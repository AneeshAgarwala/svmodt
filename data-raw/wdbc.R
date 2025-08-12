#' Wisconsin Diagnostic Breast Cancer
#'
#' Created by Dr. William H. Wolberg, W. Nick Street, and Olvi L. Mangasarian at the University of Wisconsin–Madison
#' to classify breast tumor samples as benign or malignant based on quantitative image analysis of fine needle aspirate \(FNA \) images of breast masses.
#'
#' @format ## `wdbc`
#' A data frame with 569 rows and 32 columns
#'
#' @source <https://archive.ics.uci.edu/dataset/17/breast+cancer+wisconsin+diagnostic>

wdbc <- read.csv("D:/SVMODT/project-svodt/data-raw/wdbc.csv", header=TRUE)


usethis::use_data(wdbc, overwrite = TRUE)
