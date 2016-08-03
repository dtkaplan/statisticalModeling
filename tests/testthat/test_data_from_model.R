context("Can get data from model objects.")

mod1 <- glm(married == "Married" ~ sector + sex + educ, data = mosaicData::CPS85, family = "binomial")
mod2 <- rpart::rpart(married ~ sector + sex + educ, data = mosaicData::CPS85)
  
test_that("data from glm", {
  
  one <- predict(mod1, newdata = data.frame(sector = "prof", sex = "M", educ = 15), type = "link")
  two <- predict(mod1, newdata = data.frame(sector = "prof", sex = "F", educ = 15), type = "link")
  three <- effect_size(mod1, ~ sex, at = list(sector = "prof", educ = 15, sex = "F"), type = "link")
  expect_equal(as.numeric(three), as.numeric(one - two))
})