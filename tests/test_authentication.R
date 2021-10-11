# This file tests authentication.R.

# -----
# SETUP
# -----

if (!exists("ran_install"))
{
  if (file.exists("install.R"))
    source("install.R")
  else
    stop("Could not confirm installation. Please source install.R manually.")
}

source_sdr("authentication.R")

# -----
# TESTS
# -----

auth_test <- function()
{
  print_clean("Functions Tested: my_hash, my_auth")
  username <- "Justin"
  password <- "123456"
  user_credentials <- list()
  user_credentials[[username]] <- my_hash(password)
  print_clean("Hashed password:")
  print(user_credentials[[username]])
  print_clean("Did authentication succeed for correct username / password?")
  print(my_auth(username, password, user_credentials))
  print_clean("Did authentication succeed for incorrect username?")
  print(my_auth("Joel", password, user_credentials))
  print_clean("Did authentication succeed for incorrect password?")
  print(my_auth(username, "234567", user_credentials))
  print_clean("Did authentication succeed for incorrect username / password?")
  print(my_auth("Joel", "234567", user_credentials))
  print_clean("What happens when NULL / NULL / NULL are the inputs?")
  print(my_auth(NULL, NULL, NULL))
  print_clean("What happens when credentials are absent?")
  print(my_auth(username, password, NULL))
}

# -------
# RUN ALL
# -------
auth_test()
