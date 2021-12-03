require(arules)
require(arulesViz)
require(dplyr)

setwd("R:/Graduate School/IST707 - Data Analysis/Week 3")
data <- read.csv("bankdata_csv_all.csv") %>% as_tibble()

data$children <- as.factor(data$children)
data <- data %>% select(-id) %>% mutate_if(is.character, funs(as.factor)) %>% 
  mutate_if(is.numeric, funs(discretize))

a_data <- apriori(data, parameter = list(supp = .001, conf = .7, maxlen=4), appearance = list(default="lhs", rhs="pep=YES"))
neg_data <- apriori(data, parameter = list(supp = .001, conf = .7, maxlen=4), appearance = list(default="lhs", rhs="pep=NO"))
a_data <- sort(a_data, decreasing = T, by = "lift")
neg_data <- sort(neg_data, decreasing = T, by ="lift")
inspect(a_data)
inspect(neg_data)

