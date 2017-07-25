context("Model extractors")

mod1 <- lm(width ~ sex * length, data = mosaicData::KidsFeet)
mod4 <- rpart::rpart(sex ~ length + domhand, data = mosaicData::KidsFeet)

test_that("explanatory and response variables are extracted", {

  expect_true( all(c("sex", "length") %in% explanatory_vars(mod1)))
  expect_equal("width", response_var(mod1))

  expect_true( all(c("length", "domhand") %in% explanatory_vars(mod4)))
  expect_equal("sex", response_var(mod4))
})

test_that("Training data is extracted", {
  expect_is(data_from_model(mod1), "data.frame")
  expect_true(all(c("width", "sex", "length") %in% names(data_from_model(mod1))))
  expect_is(data_from_model(mod4), "data.frame")
})
