# This file tests authentication.R.
# source("tests/test_authentication.R")

# -----
# SETUP
# -----

source("app/install.R")
if (!require(testthat))
  stop("Missing package: testthat")

source("app/authentication.R")

# -----
# TESTS
# -----

start_time <- Sys.time()

test_that("my_hash(), my_auth() work", {
  user_credentials <- unlist(list(
    "Justin" = my_hash("123456"),
    "Joel" = my_hash("abcdef")
  ))

  are_credentials(character()) %>% expect_true()
  are_credentials(user_credentials) %>% expect_true()
  is_str(user_credentials["Justin"]) %>% expect_true()
  is_str(user_credentials["Joel"]) %>% expect_true()
  nchar_hash1 <- nchar(user_credentials["Justin"])
  nchar_hash2 <- nchar(user_credentials["Joel"])
  (nchar_hash1 >= 16) %>% expect_true()
  (nchar_hash2 >= 16) %>% expect_true()

  set_credentials(user_credentials)

  # correct usernames / passwords
  my_auth("Justin", "123456") %>% expect_true()
  my_auth("Joel", "abcdef") %>% expect_true()

  # incorrect username
  my_auth("Justino", "123456") %>% expect_false()
  # incorrect password
  my_auth("Justin", "1234567") %>% expect_false()
  # null username
  my_auth(NULL, "123456") %>% expect_false()
  # null password
  my_auth("Justin", NULL) %>% expect_false()

  # check what happens when credentials are removed
  set_credentials(character())
  my_auth("Justin", "123456") %>% expect_false()
  my_auth("Joel", "abcdef") %>% expect_false()
})


testing_time <- time_diff(start_time)
message_f("TESTING TIME (seconds): %.1f", testing_time)
