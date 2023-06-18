
test_that("managing api keys works", {
  has_key <- bls_has_key()
  original_key <- NULL

  if(has_key){
    original_key <- bls_get_key()
    bls_unset_key()
  }

  new_key <- paste(sample(c(0:9, letters, LETTERS), 32, TRUE), collapse="")

  #no initial key
  expect_false(bls_has_key())

  # Set a session key
  expect_true(bls_set_key(new_key))

  expect_true(bls_has_key())

  # Get session key
  expect_equal(bls_get_key(), new_key)

  # Reset to original key
  if(has_key) {
    bls_set_key(original_key)
  } else {
    bls_unset_key()
  }

  expect_equal(bls_get_key(), original_key)
  expect_equal(bls_has_key(), has_key)
})
