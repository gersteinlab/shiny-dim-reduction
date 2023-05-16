# The purpose of this file is to list coding conventions used in this project.
# Each file begins with comments like these, which summarize the file's purpose.

# order of imports:
# --check install.R
# --import third-party libraries
#     install.R handles all edge cases
#     so use library(), not require()
# --source local code files

# -------------------------
# SECTION HEADER (ALL CAPS)
# -------------------------

# settings: end in newline, no trailing whitespace, spaces only
# most code-describing comments are terse and minimally formatted
# --comments use double hyphens to denote bullet points
# --wrapping is done by using tabs (2 spaces)
# when quoting an expression, double quotes are used on the outermost quote
#   and single quotes are used internally (ex: for JavaScript snippets)
# to avoid overloading, we use "<-" for assignment and "=" for parameters
# when declaring lists, names are quoted
# parameters are assigned according to the form "x = y, a = b"
# bucket_case is used for function names
# brackets are done according to K&R
# redundant return statements are avoided
# if wrapping an expression is necessary, end lines on an operator (like +)
# using "T", "F" as abbreviations for "TRUE", "FALSE" is strictly avoided
# functions with comments should obey roxygen2 conventions
# the function below demonstrates the above principles

#' Manipulate two objects
#'
#' @param x An object.
#' @param y An object.
#' @returns A list.
example_func <- function(a = 1, b = 2)
{
  list("A test" = a, "B test" = b, "C test" = "test", "D test" = TRUE && TRUE && TRUE &&
         TRUE && (FALSE || TRUE || FALSE || TRUE || FALSE || TRUE || FALSE || TRUE))
}
