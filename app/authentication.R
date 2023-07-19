# The purpose of this file is to provide functions for
# bcrypt authentication. It is a minimum viable example
# and should not be used for protecting user data.

if (!exists("sdr_config"))
  source("app/install.R")

library(bcrypt)

#' whether x is a 'credentials' object
#' note: minimum password length of 6 from NIST 2023.
#'
#' @param x An object.
#' @returns TRUE or FALSE.
are_credentials <- function(x)
{
  is.character(x) &&
    are_list_names(names(x)) &&
    all(nchar(x) >= 6)
}

#' sets the global credentials object to x
#'
#' @param x A credentials object.
assign_global("credentials", list())
set_credentials <- function(x)
{
  stopifnot(are_credentials(x))
  assign_global("credentials", x)
}

#' hashes the provided password
#'
#' @param x A string.
#' @returns A string.
my_hash <- function(password)
{
  stopifnot(is_str(password))
  bcrypt::hashpw(password, gensalt(12))
}

#' checks if a username / password authenticates
#'
#' @param x A string.
#' @returns A string.
my_auth <- function(username, password)
{
  stopifnot(exists("credentials"))
  # aka: credentials[[username]] == hashpw(password)
  is_str(username) && is_str(password) &&
    (username %in% names(credentials)) &&
    bcrypt::checkpw(password, credentials[[username]])
}
