context("Test the automatic generation of 'typical' levels of explanatory variables")

mod1 <- glm(married == "Married" ~ sector + sex + educ, data = mosaicData::CPS85, family = "binomial")
mod2 <- rpart::rpart(married ~ age + sex + sector, data = mosaicData::CPS85)

test_that("correct number of input cases are created", {
  # two levels of each of three variables in all combinations
  one <- evaluate_model(mod1, nlevels = 2)
  expect_equal(nrow(one), 2 ^ 3)   
  # 3 different levels for educ, one each for sex and sector
  two <- evaluate_model(mod1, sex = "F", educ = 12:14, sector = "clerical")
  expect_equal(nrow(two), 3)
  # data argument works
  three <- evaluate_model(mod1, data = head(mosaicData::CPS85, 10))
  expect_equal(nrow(three), 10)
  # at= is a synonym for data= but uses all combinations of the levels
  four <- evaluate_model(mod1, at = head(mosaicData::CPS85, 5))
  expect_equal(nrow(four), 5 ^ 3) # three variables at five levels
  # inline overrides <at>
  five <- evaluate_model(mod1, at = list(sex = c("M", "F"), sector = c("const", "prof")), sex = "F", educ = 12, sector = "clerical")
  expect_equal(nrow(five), 1)  
  
})
  