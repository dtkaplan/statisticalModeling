# Calculating the GPA
gpa_mod_2 <- lm(gradepoint ~ sid + dept + level, data = College_grades)

# The GPA for two students
evaluate_model(gpa_mod_2, at = list(sid = c("S32115", "S32262")))

# Use effect_size() to find the difference
effect_size(gpa_mod_2, ~sid ,  at = list(sid = "S32115"), to = "S32262")

effect_size(gpa_mod_2, ~sid , 
            at = list(sid = "S32115"), 
            to = list(sid = "S32262"))


set.seed(101)
require(rpart)
Runners <- mosaic::sample(na.omit(Runners), size = 5000)
Runners$start_position <- 
  with(Runners, cut(gun - net, 
                    breaks = c(-Inf, 1, 3, Inf),
                    labels = c("eager", "calm", "mellow")))
null_model <- rpart(start_position ~ I(1 + 0 * age),   
                    data = Runners, cp = 0.001)
model_1 <- rpart(start_position ~ age, 
                    data = Runners, cp = 0.001)
model_2 <- rpart(start_position ~ age + sex, 
                    data = Runners, cp = 0.001)
cv_pred_error(null_model, model_1, model_2, output = "error_rate")

require(statisticalModeling)
require(mosaicData)
require(dplyr)
require(ggplot2)
Birth_weight <- 
  mosaicData::Gestation %>%
  select(baby_wt = wt, 
         income = inc, 
         mother_age = age, smoke, gestation, mother_wt = wt.1 ) %>%
  filter(smoke < 2, gestation > 225) %>% 
  na.omit() %>%
  mutate(smoke = ifelse(smoke == 1, "smoker", "nonsmoker"),
         income = paste0("level_", income))
model_1 <- lm(baby_wt ~ gestation + smoke + mother_age + mother_wt, data = Birth_weight )
model_2 <- lm(baby_wt ~ gestation * smoke + mother_age + mother_wt, data = Birth_weight )
effect_size(model_1, ~ smoke)
res <- cv_pred_error(model_1, model_2)
fmodel(model_1, ~ gestation + smoke) + geom_point(data = Birth_weight[1:1000,], alpha = .1, aes(color = smoke))
effect_size(model_2, ~ smoke, at = list(gestation = c(250, 270, 280)))



Tadpoles <- read.csv("http://tiny.cc/mosaic/tadpole-speeds.csv", stringsAsFactors = TRUE)
mod <- lm(vmax ~ length + group * poly(rtemp, 2),  data = Tadpoles)
effect_size(mod, ~ group)
effect_size(mod, ~ rtemp)
fmodel(mod, ~ rtemp + group + length)

apply(sapply(explan_vars, FUN = function(x) grepl(x, names(data))), 1, FUN = any )

# Package functions work with model formula containing transforms

data(CPS85, package = "mosaicData")
mod_0 <- lm(wage ~ exper + sex, data = CPS85)
mod_1 <- lm(wage ~ poly(exper, 2) + sex, data = CPS85)  
mod_2 <- lm(log(wage) ~ poly(exper, 2) + sex, data = CPS85)

evaluate_model(mod_0)
evaluate_model(mod_1)
evaluate_model(mod_2)

mod_3 <- glm(sex == "F" ~ exper * sector * union, data = CPS85, family = "binomial")
mod_4 <- glm(sex == "F" ~ poly(exper,2) * sector * union, data = CPS85, family = "binomial")
require(splines)
mod_5 <- glm(sex == "F" ~ ns(exper,1) * sector * union, data = CPS85, family = "binomial")

