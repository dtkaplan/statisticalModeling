context("Model extractors")

mod1 <- lm(width ~ sex * length, data = mosaicData::KidsFeet)
mod2 <- glm(sex == "B" ~ width + length + domhand, data = mosaicData::KidsFeet, family = "binomial")
mod3 <- mosaic::gwm(sex ~ domhand + biggerfoot, data = mosaicData::KidsFeet)
mod4 <- rpart::rpart(sex ~ length + domhand, data = mosaicData::KidsFeet)

test_that("explanatory and response variables are extracted", {

  expect_true( all(c("sex", "length") %in% explanatory_vars(mod1)))
  expect_equal("width", response_var(mod1))

  expect_true( all(c("width", "length", "domhand") %in% explanatory_vars(mod2)))
  expect_equal("sex == \"B\"", response_var(mod2))

  expect_true( all(c("biggerfoot", "domhand") %in% explanatory_vars(mod3)))
  expect_equal("sex", response_var(mod3))

  expect_true( all(c("length", "domhand") %in% explanatory_vars(mod4)))
  expect_equal("sex", response_var(mod4))
})

test_that("Training data is extracted", {
  expect_is(data_from_model(mod1), "data.frame")
  expect_true(all(c("width", "sex", "length") %in% names(data_from_model(mod1))))
  expect_is(data_from_model(mod2), "data.frame")
  expect_is(data_from_model(mod3), "data.frame")
  expect_is(data_from_model(mod4), "data.frame")
})
