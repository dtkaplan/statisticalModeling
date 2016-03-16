context("Effect size calculations")

mod1 <- glm(married == "Married" ~ sector + sex + educ, data = mosaicData::CPS85, family = "binomial")

test_that("effect sizes match predict", {

  one <- predict(mod1, newdata = data.frame(sector = "prof", sex = "M", educ = 15), type = "link")
  two <- predict(mod1, newdata = data.frame(sector = "prof", sex = "F", educ = 15), type = "link")
  three <- effect_size(mod1, ~ sex, at = list(sector = "prof", educ = 15, sex = "F"), type = "link")
  expect_equal(as.numeric(three), as.numeric(one - two))
})

test_that("glm effect sizes are on response (by default)", {
  one <- predict(mod1, newdata = data.frame(sector = "prof", sex = "M", educ = 15), type = "response")
  two <- predict(mod1, newdata = data.frame(sector = "prof", sex = "F", educ = 15), type = "response")
  three <- effect_size(mod1, ~ sex, at = list(sector = "prof", educ = 15, sex = "F"))
  expect_equal(as.numeric(three), as.numeric(one - two))
})

test_that("effect sizes are properly named", {
  three <- effect_size(mod1, ~ sex, at = list(sector = "prof", educ = 15, sex = "F"))
  expect_equal(names(three), "married..from..F..to..M")
})
