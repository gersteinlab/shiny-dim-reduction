# The purpose of this file is to provide functions for bcrypt authentication.

if (!exists("sdr_config"))
  source("app/install.R")

library(bcrypt)

# straightforward password hashing
my_hash <- function(password)
{
  stopifnot(is_str(password))
  bcrypt::hashpw(password, gensalt(12))
}

# checks if user_credentials[[username]] == password
my_auth <- function(username, password, user_credentials)
{
  is_str(username) && is_str(password) &&
    (username %in% names(user_credentials)) &&
    bcrypt::checkpw(password, user_credentials[[username]])
}
