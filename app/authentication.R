# The purpose of this file is to provide functions for
# bcrypt authentication. It is a minimal working example (MWE)
# and should not be used for protecting user data.

if (!exists("sdr_config"))
  source("app/install.R")

library(bcrypt)

#' whether x is a 'credentials' object
#' note: minimum password length of 6 from NIST 2023.
#'
#' @param x [object]
#' @returns [boolean]
are_credentials <- function(x)
{
  is.character(x) &&
    has_safe_names(x) &&
    all(nchar(x) >= 6)
}

#' sets the global credentials to x
#'
#' @param x [credentials]
assign_global("credentials", list())
set_credentials <- function(x)
{
  stopifnot(are_credentials(x))
  assign_global("credentials", x)
}

#' hashes the provided password
#'
#' @param password [string]
#' @returns [string]
mwe_hash <- function(password)
{
  stopifnot(is_str(password))
  bcrypt::hashpw(password, gensalt(12))
}

#' checks if a username / password authenticates
#'
#' @param username [string]
#' @param password [string]
#' @returns [boolean]
mwe_auth <- function(username, password)
{
  stopifnot(exists("credentials"))
  # aka: identical(credentials[username], hashpw(password))
  is_str(username) && is_str(password) &&
    (username %in% names(credentials)) &&
    bcrypt::checkpw(password, credentials[username])
}
