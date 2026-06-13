# svmodt 0.1.0

## Initial release

* First CRAN release of **svmodt**.
* Added `svm_split()` for constructing oblique decision trees using Support Vector Machine (SVM) based splits.
* Added S3 methods for `svmodt_node` objects:

  * `print()`
  * `predict()`
  * `plot()`
* Added `trace_path()` to inspect the decision path followed by an observation through a fitted tree.
* Included example datasets:

  * `wdbc` (Wisconsin Diagnostic Breast Cancer Dataset)
  * `wine` (Wine Dataset)
* Added package documentation, examples, and unit tests.
