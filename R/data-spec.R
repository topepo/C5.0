# Direct data transfer to the C code
#
# C5.0 was written as a command-line program that reads a `.names` file
# (attribute definitions), a `.data` file (one comma-separated line per case),
# and a `.costs` file. Versions up to 0.2.0 formatted R data into those text
# formats and the C code parsed them back. That round trip needs escaping,
# loses whitespace in values, gives special meaning to values such as "?" and
# "N/A", and reserves some column names.
#
# Now R passes plain vectors to `C_c50_fit()` and `C_c50_predict()`
# (src/top.c), and src/rdata.c fills the C data structures directly:
#
# * `c50_data_spec()` describes the attributes once, at fit time. The spec is
#   stored in the fitted object and reused for prediction.
# * `c50_data_columns()` converts a data frame to the list of columns that the
#   C code expects: doubles for numeric columns and integer codes (1..k,
#   matching `spec$levels`) for factor and character columns.
# * `c50_cost_matrix()` checks the cost matrix and puts its rows and columns
#   in the order of the class levels.

c50_data_spec <- function(
  x,
  y,
  label = "outcome",
  has_weights = FALSE,
  call = rlang::caller_env()
) {
  if (!is.character(label) || length(label) != 1 || is.na(label)) {
    cli_abort(
      "{.arg label} must be a single string, not {.obj_type_friendly {label}}.",
      call = call
    )
  }
  if (ncol(x) == 0) {
    cli_abort("{.arg x} must have at least one column.", call = call)
  }
  check_class_levels(y, call = call)
  check_column_names(x, label, has_weights, call = call)

  is_discrete <- vapply(x, is_discrete_column, logical(1))
  is_continuous <- vapply(x, is_continuous_column, logical(1))
  bad <- !is_discrete & !is_continuous
  if (any(bad)) {
    bad_cols <- names(x)[bad]
    bad_types <- vapply(x[bad], function(col) class(col)[1], character(1))
    bad_desc <- paste0("`", bad_cols, "` (", bad_types, ")")
    cli_abort(
      c(
        "Predictors must be numeric, factor, or character columns.",
        "x" = "Unsupported column{?s}: {.and {bad_desc}}.",
        "i" = "Convert logical and date columns to numbers or factors first."
      ),
      call = call
    )
  }

  lvls <- lapply(x, column_levels)
  bad_lvls <- vapply(lvls, anyNA, logical(1))
  if (any(bad_lvls)) {
    cli_abort(
      c(
        "Factor levels must not be {.code NA}.",
        "x" = "Column{?s} with a missing level: {.var {names(x)[bad_lvls]}}."
      ),
      call = call
    )
  }

  list(
    names = names(x),
    discrete = unname(is_discrete),
    ordered = unname(vapply(x, is.ordered, logical(1))),
    levels = unname(lvls),
    class_levels = levels(y),
    class_ordered = is.ordered(y),
    target = label,
    has_weights = has_weights
  )
}

c50_data_columns <- function(
  x,
  spec,
  new_data = FALSE,
  call = rlang::caller_env()
) {
  if (!is.data.frame(x) || inherits(x, "tbl_df")) {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
  }
  missing_cols <- setdiff(spec$names, names(x))
  if (length(missing_cols) > 0) {
    cli_abort(
      "{.arg newdata} is missing column{?s} {.var {missing_cols}}.",
      call = call
    )
  }
  x <- x[spec$names]

  unseen <- list()
  cols <- vector("list", length(spec$names))
  for (j in seq_along(spec$names)) {
    col <- x[[j]]
    if (spec$discrete[j]) {
      values <- as.character(col)
      codes <- match(values, spec$levels[[j]])
      new_values <- !is.na(values) & is.na(codes)
      if (any(new_values)) {
        unseen[[spec$names[j]]] <- unique(values[new_values])
      }
      cols[[j]] <- as.integer(codes)
    } else {
      if (new_data) {
        col <- coerce_new_numeric(col)
      }
      if (!is.numeric(col)) {
        cli_abort(
          "Column {.var {spec$names[j]}} was numeric in the training set
           but is {.obj_type_friendly {col}} in {.arg newdata}.",
          call = call
        )
      }
      check_finite(col, spec$names[j], call = call)
      cols[[j]] <- as.double(col)
    }
  }
  if (length(unseen) > 0) {
    if (!new_data) {
      cli_abort(
        "Internal error: the training data have values that are not in the
         attribute spec.",
        .internal = TRUE,
        call = call
      )
    }
    warn_unseen_levels(unseen, call = call)
  }
  cols
}

c50_cost_matrix <- function(costs, lvl, call = rlang::caller_env()) {
  if (!is.numeric(costs)) {
    cli_abort(
      "{.arg costs} must be a numeric matrix, not {.obj_type_friendly {costs}}.",
      call = call
    )
  }
  if (any(!is.finite(costs)) || any(costs < 0)) {
    cli_abort(
      c(
        "{.arg costs} must contain only finite, non-negative values.",
        "i" = "Missing, infinite, and negative costs are not allowed."
      ),
      call = call
    )
  }
  # With two classes, C5.0 reweights the training cases by the costs, and a
  # zero cost gives a class a weight of zero. The C code then builds a tree
  # with no usable leaves and crashes when it predicts, so zero costs are only
  # allowed with three or more classes, where they are used only to choose the
  # class with the lowest expected cost.
  off_diag <- costs[row(costs) != col(costs)]
  if (nrow(costs) == 2 && any(off_diag == 0)) {
    cli_abort(
      c(
        "With two classes, the off-diagonal elements of {.arg costs} must be
         greater than zero.",
        "i" = "Use a small positive value instead of zero."
      ),
      call = call
    )
  }
  if (length(off_diag) > 0 && all(off_diag == 0)) {
    cli_abort(
      "At least one off-diagonal element of {.arg costs} must be greater
       than zero.",
      call = call
    )
  }
  row_ok <- setequal(rownames(costs), lvl) && !anyDuplicated(rownames(costs))
  col_ok <- setequal(colnames(costs), lvl) && !anyDuplicated(colnames(costs))
  if (!row_ok || !col_ok) {
    cli_abort(
      c(
        "The row and column names of {.arg costs} must be the levels of
         {.arg y}.",
        "i" = "Levels of {.arg y}: {.val {lvl}}.",
        "x" = "Row names: {.val {rownames(costs)}}.",
        "x" = "Column names: {.val {colnames(costs)}}."
      ),
      call = call
    )
  }
  out <- costs[match(lvl, rownames(costs)), match(lvl, colnames(costs)),
    drop = FALSE
  ]
  storage.mode(out) <- "double"
  out
}

# Build the text of a `.names` file from the attribute spec. The C code no
# longer uses it, but fitted objects keep it as `names`, the same as in
# versions up to 0.2.0, for code that parses it to get the predictor types
# and levels. Like `Cubist::makeNamesFile()` did, the attribute names are
# escaped but the values are not. Unlike before, there are no comment lines
# with the R version and a time stamp.
c50_names_string <- function(spec) {
  types <- rep("continuous.", length(spec$names))
  for (j in which(spec$discrete)) {
    prefix <- ""
    if (spec$ordered[j]) {
      prefix <- "[ordered]"
    }
    types[j] <- paste0(prefix, paste(spec$levels[[j]], collapse = ","), ".")
  }
  att_lines <- paste0(c50_escapes(spec$names), ": ", types)
  if (spec$has_weights) {
    att_lines <- c(att_lines, "case weight: continuous.")
  }
  class_prefix <- ""
  if (spec$class_ordered) {
    class_prefix <- "[ordered] "
  }
  paste0(
    "\n",
    spec$target,
    ".\n\n",
    spec$target,
    ": ",
    class_prefix,
    paste(spec$class_levels, collapse = ","),
    ".\n",
    paste(att_lines, collapse = "\n"),
    "\n"
  )
}

# ------------------------------------------------------------------------------

# Prefix every character other than letters, digits, and whitespace with a
# backslash, as the C5.0 text formats require
c50_escapes <- function(x) {
  gsub("([^[:alnum:][:space:]])", "\\\\\\1", x, useBytes = TRUE)
}

is_discrete_column <- function(col) {
  is.factor(col) || is.character(col)
}

# Dates and times are not numeric here: `is.numeric()` is FALSE for them
is_continuous_column <- function(col) {
  is.numeric(col)
}

column_levels <- function(col) {
  if (is.factor(col)) {
    return(levels(col))
  }
  if (is.character(col)) {
    # radix sorting is locale independent, so the level order (and thus the
    # model) is the same on every platform
    return(sort(unique(col[!is.na(col)]), method = "radix"))
  }
  NULL
}

# Prepare a column of `newdata` for a predictor that was numeric in the
# training set. Up to version 0.2.0, all of the data were written to text, so
# any column class was accepted. A column that has only missing values (e.g.
# `data.frame(x = NA)`, which is logical) is still treated as missing. Other
# classes are left alone, so the caller gives an error.
coerce_new_numeric <- function(col) {
  if (all(is.na(col))) {
    return(rep(NA_real_, length(col)))
  }
  col
}

# The C code stops (and returns no model) when it sees an infinite value, so
# catch these here. Missing values (NA, NaN) are allowed.
check_finite <- function(x, what, call = rlang::caller_env()) {
  if (any(is.infinite(x))) {
    cli_abort(
      c(
        "{.var {what}} has infinite values.",
        "i" = "Use {.code NA} for missing values instead."
      ),
      call = call
    )
  }
  invisible(NULL)
}

check_class_levels <- function(y, call = rlang::caller_env()) {
  lvl <- levels(y)
  if (anyNA(lvl)) {
    cli_abort("The levels of {.arg y} must not be {.code NA}.", call = call)
  }
  if (length(lvl) < 1) {
    cli_abort(
      "{.arg y} must have at least one level.",
      call = call
    )
  }
  invisible(NULL)
}

check_column_names <- function(x, label, has_weights, call = rlang::caller_env()) {
  nms <- names(x)
  if (is.null(nms) || anyNA(nms) || any(nms == "")) {
    cli_abort("All columns of {.arg x} must have non-empty names.", call = call)
  }
  dups <- unique(nms[duplicated(nms)])
  if (length(dups) > 0) {
    cli_abort(
      "Column names of {.arg x} must be unique. Duplicated: {.val {dups}}.",
      call = call
    )
  }
  if (label %in% nms) {
    cli_abort(
      c(
        "The outcome label {.val {label}} is also a column name in {.arg x}.",
        "i" = "Use {.code C5.0Control(label = )} to choose a different label."
      ),
      call = call
    )
  }
  if (has_weights && "case weight" %in% nms) {
    cli_abort(
      "{.val case weight} cannot be a column name in {.arg x} when
       {.arg weights} are used.",
      call = call
    )
  }
  invisible(NULL)
}

warn_unseen_levels <- function(unseen, call = rlang::caller_env()) {
  details <- vapply(
    names(unseen),
    function(nm) {
      vals <- paste0("\"", unseen[[nm]], "\"", collapse = ", ")
      paste0(nm, ": ", vals)
    },
    character(1)
  )
  # The values are data, not cli markup
  details <- gsub("{", "{{", details, fixed = TRUE)
  details <- gsub("}", "}}", details, fixed = TRUE)
  names(details) <- rep("*", length(details))
  cli_warn(
    c(
      "{.arg newdata} has values that were not in the training set.",
      "i" = "They are treated as missing.",
      details
    ),
    call = call
  )
}
