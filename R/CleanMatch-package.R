#' @keywords internal
"_PACKAGE"

## data.table is used through data.table::data.table() only; this tells data.table that the
## package uses its [ syntax although it does not import the whole package
.datatable.aware <- TRUE

## column names used inside data.table expressions
utils::globalVariables("lf")
