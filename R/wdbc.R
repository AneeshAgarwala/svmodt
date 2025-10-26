#' Wisconsin Diagnostic Breast Cancer Dataset
#'
#' The WDBC dataset contains quantitative measurements from digitized images of
#' fine needle aspirates (FNA) of breast masses. It is commonly used for
#' classification tasks to distinguish between benign and malignant tumors.
#'
#' @format A data frame with 569 rows and 32 columns:
#' \describe{
#'   \item{radius_mean}{Mean of radius}
#'   \item{radius_se}{Standard error of radius}
#'   \item{radius_worst}{Worst (largest) radius}
#'   \item{texture_mean}{Mean of texture}
#'   \item{texture_se}{Standard error of texture}
#'   \item{texture_worst}{Worst texture}
#'   \item{perimeter_mean}{Mean of perimeter}
#'   \item{perimeter_se}{Standard error of perimeter}
#'   \item{perimeter_worst}{Worst perimeter}
#'   \item{area_mean}{Mean area}
#'   \item{area_se}{Standard error of area}
#'   \item{area_worst}{Worst area}
#'   \item{smoothness_mean}{Mean smoothness}
#'   \item{smoothness_se}{Standard error of smoothness}
#'   \item{smoothness_worst}{Worst smoothness}
#'   \item{compactness_mean}{Mean compactness}
#'   \item{compactness_se}{Standard error of compactness}
#'   \item{compactness_worst}{Worst compactness}
#'   \item{concavity_mean}{Mean concavity}
#'   \item{concavity_se}{Standard error of concavity}
#'   \item{concavity_worst}{Worst concavity}
#'   \item{concave.points_mean}{Mean concave points}
#'   \item{concave.points_se}{Standard error of concave points}
#'   \item{concave.points_worst}{Worst concave points}
#'   \item{symmetry_mean}{Mean symmetry}
#'   \item{symmetry_se}{Standard error of symmetry}
#'   \item{symmetry_worst}{Worst symmetry}
#'   \item{fractal_dimension_mean}{Mean fractal dimension}
#'   \item{fractal_dimension_se}{Standard error of fractal dimension}
#'   \item{fractal_dimension_worst}{Worst fractal dimension}
#'   \item{diagnosis}{Factor with levels 'B' and 'M'}
#' }
#'
#' @source
#' Dr. William H. Wolberg, W. Nick Street, and Olvi L. Mangasarian, University of Wisconsin–Madison.
#' Original dataset available at: <https://archive.ics.uci.edu/dataset/17/breast+cancer+wisconsin+diagnostic>
"wdbc"
