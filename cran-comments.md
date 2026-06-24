## Resubmission
 
This is a resubmission. The following issues raised by the CRAN reviewer have been addressed:
 
* Added `\value` tags to `predict.svmodt_node.Rd` and `print.svmodt_node.Rd`, documenting
  the structure and meaning of each function's return value.
* Removed `\examples{}` blocks from unexported functions `fit_svm_with_weights()` and
  `trace_prediction_path()` (both `@keywords internal`). The example in
  `predict.svmodt_node.Rd` was also corrected to use only exported functions.
* Replaced all `\dontrun{}` wrappers with `\donttest{}` in examples that do not require
  missing software or API keys (`predict.svmodt_node`, `print.svmodt_node`,
  `trace_path.svmodt_node`, and `plot.svmodt_node`).
* Regarding the request for references in the DESCRIPTION: the theoretical method
  implemented in this package is described in a paper that is currently under preparation
  and does not yet have a DOI or stable URL. A reference will be added in a future
  submission once the paper is published.

---

## R CMD check results

0 errors | 0 warnings | 1 note

The NOTE is:
 
* Possibly misspelled words in DESCRIPTION:
  * SVMODT
  * hyperplanes
These are intentional domain-specific terms. SVMODT is the acronym for the method
implemented by this package (Support Vector Machine Oblique Decision Trees). "hyperplanes"
is standard machine learning terminology.
 
This is the second submission of the package.
