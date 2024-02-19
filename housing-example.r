library(tidyverse)
library(Hmisc)

set.seed(20240216)

# Build a simple data set based on housing prices. 
num_rows <- 1000

d <- tibble(
  square_footage = round(rnorm(num_rows,mean=1500,sd=800),0),
  age = pmax(rnorm(num_rows,mean=30,sd=20),0)
)

d$square_footage <- d$square_footage - min(d$square_footage) + 650

d %>% 
  slice_sample(n=10)

describe(d)

# goal: new, 1000sq ~ 400,000
# every 5 years of age is -10K
d <- d %>% 
  mutate(
    raw_price = 400000 + 50*(square_footage - 1000) + (-2000) * age,
    price = raw_price + rnorm(num_rows,mean=0,sd=30000)
  )

summary(lm(price ~ square_footage + age, data=d))
arm::display(lm(price ~ square_footage + age, data=d))
