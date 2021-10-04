# The purpose of this file is to provide functions for basic bcrypt authentication

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}

require("bcrypt")

# straightforward password hashing
my_hash <- function(password)
{
  bcrypt::hashpw(password, gensalt(12))
}

# checks if user_credentials[[username]] == password
my_auth <- function(username, password, user_credentials)
{
  length(username) == 1 && length(password) == 1 && is.character(username) &&
    is.character(password) && (username %in% names(user_credentials)) &&
    bcrypt::checkpw(password, user_credentials[[username]])
}
