# The purpose of this file is to provide functions for bcrypt authentication.

if (!exists("sdr_config"))
  source("app/install.R")

library(bcrypt)

#' whether x is a 'credentials' object
#'
#' @param x An object.
#' @returns TRUE or FALSE.
are_credentials <- function(x)
{
  is.character(x) &&
    is.character(names(x)) && is_unique(names(x)) && none_na(names(x))
}

# for changing what the credentials are
assign_global("credentials", list())
set_credentials <- function(x)
{
  stopifnot(are_credentials(x))
  assign_global("credentials", x)
}

# straightforward password hashing
my_hash <- function(password)
{
  stopifnot(is_str(password))
  bcrypt::hashpw(password, gensalt(12))
}

# checks if credentials[[username]] == password
my_auth <- function(username, password)
{
  stopifnot(exists("credentials"))
  is_str(username) && is_str(password) &&
    (username %in% names(credentials)) &&
    bcrypt::checkpw(password, credentials[[username]])
}
