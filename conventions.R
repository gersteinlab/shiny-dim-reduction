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
# booleans refer to TRUE or FALSE, not including NA.
# when partial matching is not needed, use "[[.]]" over "$" for accession

# functions with comments should obey roxygen2 conventions
# --if the returned value will not be used, do not specify @returns
# --use [type] to specify the types of parameters.
# the function below demonstrates the above principles

#' Manipulate two objects
#'
#' @param x [object]
#' @param y [object]
#' @returns [list] list("A" = a, "B" = b, "C" = "test", "D" = TRUE)
example_func <- function(a = 1, b = 2)
{
  list("A" = a, "B" = b, "C" = "test", "D" = TRUE && TRUE &&
         TRUE && (FALSE || TRUE || FALSE || TRUE))
}

# common mistakes:
# --use "any(x)" instead of "sum(x) > 0"

# to test how many times a function is called in the codebase:
# grep --include=\*.R -rnw '.' -e "function_name"

# USEFUL GREP FLAGS:
# --include: Specify a glob pattern for files to search.
# -r: Recursive without following symbolic links.
# -n: Shows line numbers.
# -w: Searches for words ('b' will not match 'abc')
# -e: Specifies a search pattern.
# -i: Be case-insensitive for letters.
# -F: Treat patterns as literal strings instead of regexes.

# SEE COMMIT STATS
# git log --author="justinchang1124" --pretty=tformat: --numstat > commit_stats.txt
# perl -ane'$i += $F[0]; $d += $F[1]; END{ print "added: $i removed: $d\n"}' < commit_stats.txt

# -------------------
# COUNT LINES OF CODE
# -------------------

msg_code_com <- function(src, n_code, n_com)
{
  cat(sprintf("%27s >%5d code +%5d com = %5d\n", src,
              n_code, n_com, n_code + n_com))
}

total_code <- 0
total_comment <- 0
for (file in list.files(path = ".", recursive = TRUE))
{
  if (grepl("[.][R]$", file))
  {
    all_file_lines <- readLines(file)
    is_blank <- grepl("^\\s*$", all_file_lines)
    is_comment <- grepl("^\\s*#.*$", all_file_lines)
    num_comment <- sum(is_comment)
    num_code <- sum(!is_blank) - num_comment
    total_comment <- total_comment + num_comment
    total_code <- total_code + num_code
    msg_code_com(file, num_code, num_comment)
  }
}
msg_code_com("TOTAL COUNT", total_code, total_comment)
