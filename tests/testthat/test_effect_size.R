context("Effect size calculations")

mod1 <- glm(married == "Married" ~ sector + sex + educ, data = mosaicData::CPS85, family = "binomial")
mod2 <- rpart::rpart(married ~ age + sex + sector, data = mosaicData::CPS85)

test_that("effect sizes match predict", {
  one <- predict(mod1, newdata = data.frame(sector = "prof", sex = "M", educ = 15), type = "link")
  two <- predict(mod1, newdata = data.frame(sector = "prof", sex = "F", educ = 15), type = "link")
  three <- effect_size(mod1, ~ sex, sector = "prof", educ = 15, sex = "F", type = "link")
  expect_equal(as.numeric(three$change), as.numeric(one - two))
})

test_that("glm effect sizes are on response (by default)", {
  one <- predict(mod1, newdata = data.frame(sector = "prof", sex = "M", educ = 15), type = "response")
  two <- predict(mod1, newdata = data.frame(sector = "prof", sex = "F", educ = 15), type = "response")
  three <- effect_size(mod1, ~ sex, sector = "prof", educ = 15, sex = "F")
  expect_equal(as.numeric(three$change), as.numeric(one - two))
})

test_that("effect sizes are properly named", {
  three <- effect_size(mod1, ~ sex, sector = "prof", educ = 15, sex = "F")
  expect_equal(names(three)[1], "change")
  four <- effect_size(mod1, ~ educ, sector = "prof", educ = 15, sex = "F")
  expect_equal(names(four)[1], "slope")
})

test_that("effect sizes work for rpart", {
  one <- effect_size(mod2, ~ sex, sector = "prof", sex = "F")
  expect_true(all(c("change.Married", "change.Single") %in% names(one)))
})

test_that("effect sizes are calculated for all combinations of variables specified as arguments", {
  one <- effect_size(mod1, ~ sex, sector = c("prof", "clerical"), educ = 10:11)
  expect_equal(nrow(one), 2 ^ 2) # two variables at two levels
  two <- effect_size(mod1, ~ sex, at = list(sector = c("prof", "clerical")),
                     educ = 10:11)
  expect_equal(nrow(two), 2 ^ 2)
})

test_that("effect sizes work with a <data> argument.", {
  one <- effect_size(mod1, ~ sex, data = head(mosaicData::CPS85, 5))
  expect_equal(nrow(one), 5)
})

test_that("default values for inputs are made if no explicit values in at, data or ...", {
  one <- effect_size(mod1, ~ sex, sector = c("prof", "clerical"))
})
