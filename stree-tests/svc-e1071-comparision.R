library(reticulate)
library(rsample)
library(dplyr)
library(purrr)
library(tibble)
library(e1071)
devtools::load_all()

# Prepare data
ctg3 <- ctg |> dplyr::select(-CLASS)
ctg10 <- ctg |> dplyr::select(-NSP)
scaled_dermatology <- standard_scaler(dermatology)
scaled_ctg10 <- standard_scaler(ctg10)
scaled_australian_credit <- standard_scaler(australian_credit)
scaled_fertility <- standard_scaler(fertility)
scaled_wdbc <- standard_scaler(wdbc)
scaled_iris <- iris |>
  dplyr::mutate(Species = as.factor(Species)) |>
  standard_scaler()

set.seed(23)

split_data <- initial_split(scaled_ctg10, prop = 0.8, strata = CLASS)
train_data <- training(split_data)
test_data <- testing(split_data)

x_train <- train_data[, 1:21]
y_train <- train_data$CLASS
x_test <- test_data[, 1:21]
y_test <- test_data$CLASS

# Python STREE
stree <- import("stree")
sklearn_svm <- import("sklearn.svm")

svc_args <- list(
  C = 1,
  tol = 0.0001,
  kernel = "linear"
)

svc_model <- do.call(stree$Stree, svc_args)
svc_model$fit(x_train, y_train)
py_pred <- svc_model$predict(x_test)
DiagrammeR::grViz(svc_model$graph())


# R STREE
r_stree_model <- stree_split(data = train_data, response = "CLASS", kernel = "linear", verbose = TRUE)
r_stree_preds <- stree_predict(r_stree_model, test_data)
print_stree(r_stree_model)


# SVMODT TREE
r_svmodt_model <- svm_split(data = train_data, response = "class", impurity_measure = "entropy", verbose = TRUE)
r_svmodt_preds <- svm_predict_tree(tree = r_svmodt_model, newdata = test_data)
print_svm_tree(tree = r_svmodt_model, show_feature_info = FALSE)



# Prediction Accuracy
mean(r_stree_preds == y_test)
mean(py_pred == y_test)
mean(r_svmodt_preds == y_test)


table(r_stree_preds, py_pred)



