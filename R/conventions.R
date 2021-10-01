# The purpose of this file is to list coding conventions used in this project.
# Each file begins with comments of this form, which summarize the purpose of the file.

# --------------
# SECTION HEADER
# --------------

# settings: end in newline, no trailing whitespace, spaces only
# aside from the opening comment, most comments are terse and minimally formatted
# --comments use double hyphens to denote bullet points
# --wrapping is done by using tabs (2 spaces)
# when quoting an expression, double quotes are used on the outermost quote
#   and single quotes are used internally (ex: for JavaScript snippets)
# to avoid overloading, we use "<-" for assignment and "=" for parameters
# when declaring lists, names are quoted
# parameters are assigned according to the form "x = y"
# bucket_case is used for function names
# brackets are done according to K&R
# redundant return statements are avoided
# if wrapping an expression is necessary, end lines on an operator
# using "T", "F" as abbreviations for "TRUE", "FALSE" is strictly avoided

# this function demonstrates the above principles
example_func <- function(a = 1, b = 2)
{
  list("A test" = a, "B test" = b, "C test" = "test", "D test" = TRUE && TRUE && TRUE &&
         TRUE && (FALSE || TRUE || FALSE || TRUE || FALSE || TRUE || FALSE || TRUE))
}
