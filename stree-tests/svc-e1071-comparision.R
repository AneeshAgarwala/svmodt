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

split_data <- initial_split(scaled_ctg10, prop = 0.8, strata = CLASS)
train_data <- training(split_data)
test_data <- testing(split_data)

x_train <- train_data[, 1:21]
y_train <- train_data$CLASS
x_test <- test_data[, 1:21]
y_test <- test_data$CLASS

# Python SVM
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

# R E1071 - FIX: Train on train_data only, not entire dataset
r_model <- svm(CLASS ~ .,
               data = train_data,
               cost = 1,
               tolerance = 0.0001,
               kernel = "linear",
               scale = FALSE)

r_stree_model <- stree_split(data = train_data, response = "CLASS", kernel = "linear", verbose = TRUE)

r_stree_preds <- stree_predict(r_stree_model, test_data)

mean(r_stree_preds == y_test)


r_pred <- predict(r_model, test_data)

# Compare predictions
table(py_pred == r_pred)

# Check accuracies
py_accuracy <- mean(py_pred == y_test)
r_accuracy <- mean(r_pred == y_test)


