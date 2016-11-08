context("Can get data from model objects.")

mod1 <- glm(married == "Married" ~ sector + sex + educ, data = mosaicData::CPS85, family = "binomial")
mod2 <- rpart::rpart(married ~ sector + sex + educ, data = mosaicData::CPS85)
  
test_that("data from rpart", {
  expect_equal(statisticalModeling:::data_from_model.rpart(mod2), mosaicData::CPS85)
  one <- evaluate_model(mod2, nlevels = 8)
  expect_true(length(unique(one$sector)) == 8)
  two <- cv_pred_error(mod2, output = "error_rate")
  expect_true(abs(mean(two$error_rate) - 0.36) < .02)
})

test_that("data from glm", {
  
  one <- predict(mod1, newdata = data.frame(sector = "prof", sex = "M", educ = 15), type = "link")
  two <- predict(mod1, newdata = data.frame(sector = "prof", sex = "F", educ = 15), type = "link")
  three <- effect_size(mod1, ~ sex, at = list(sector = "prof", educ = 15, sex = "F"), type = "link")
  expect_equal(as.numeric(three$change), as.numeric(one - two))
})

test_that("can deal with odd scoping situations", {
  data(CPS85, package = "mosaicData")
  f1 <- function() {
    g <- function() {
      model <- lm(wage ~ age + sector, data = CPS85)
      effect_size(model, ~ age)
    }
    g()
  }
  f2 <- function() {
    data(CPS85, package = "mosaicData", envir = environment())
    g <- function() {
      model <- lm(wage ~ age + sector, data = CPS85)
      effect_size(model, ~ age)
    }
    g()
  }
  f3 <- function() {
    g <- function() {
      rpart::rpart(wage ~ age + sector, data = CPS85)
    }
    g()
  }
  f4 <- function() {
    data(CPS85, package = "mosaicData", envir = environment())
    g <- function() {
      rpart::rpart(wage ~ age + sector, data = CPS85)
    }
    g()
  }
  expect_true(f1()$slope > 0)
  expect_true(f2()$slope > 0)
  expect_equal(statisticalModeling:::data_from_model(f3()), CPS85)
  expect_equal(statisticalModeling:::data_from_model(f4()), CPS85)
}
)