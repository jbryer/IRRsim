library(IRRsim)
library(tidyverse)
library(tidytext)
library(nnet)
# library(BayesLogit) # devtools::install_github('cran/BayesLogit')

data(asap2)
data(asap2_lr)

train.percent <- 0.7

# Verify that the two data frames align up row-to-row
table(asap2$essay_id == asap2_lr$essay_id)

asap2$agreed1 <- asap2$rater1_domain1 == asap2$rater2_domain1
asap2$agreed2 <- asap2$rater1_domain2 == asap2$rater2_domain2

asap2_lr$domain1_score <- asap2$domain1_score
asap2_lr$domain2_score <- asap2$domain2_score

table(asap2$domain1_score, asap2$agreed1, useNA = 'ifany') %>%
	print %>% prop.table(1) * 100
table(asap2$domain2_score, asap2$agreed2, useNA = 'ifany') %>%
	print %>% prop.table(1) * 100

table(asap2$domain1_score, asap2$domain2_score, useNA = 'ifany')

asap2 <- asap2 %>% filter(domain1_score %in% c(2:4) & domain2_score %in% c(2:4))
asap2_lr <- asap2_lr %>% filter(domain1_score %in% c(2:4) & domain2_score %in% c(2:4))

asap2$domain1_score <- factor(asap2$domain1_score, ordered = TRUE)
asap2_lr$domain1_score <- factor(asap2_lr$domain1_score, ordered = TRUE)
asap2$domain2_score <- factor(asap2$domain2_score, ordered = TRUE)
asap2_lr$domain2_score <- factor(asap2_lr$domain2_score, ordered = TRUE)

# Verify that the two data frames align up row-to-row
table(asap2$essay_id == asap2_lr$essay_id)
asap2_lr$essay_id <- NULL

if(file.exists('data-raw/mlr.rda')) {
	load('data-raw/mlr.rda')
} else {
	set.seed(2112)
	train.rows <- sample(c(TRUE, FALSE), replace = TRUE, size = nrow(asap2),
						 prob = c(nrow(asap2) * train.percent, nrow(asap2) * (1 - train.percent)))
	# table(asap2$agreed1, train.rows) %>% print %>% prop.table(1) * 100
	# table(asap2_lr$domain1_score, asap2$agreed1)

	mlr.out.full1 <- multinom(domain1_score ~ .,
							 data = asap2_lr[train.rows,],
							 MaxNWts = 100000)
	mlr.out.certain1 <- multinom(domain1_score ~ .,
							 data = asap2_lr[train.rows & asap2$agreed1,],
							 MaxNWts = 100000)

	predict.full1 <- predict(mlr.out.full1, newdata = asap2_lr[!train.rows,])
	predict.certain.agreed1 <- predict(mlr.out.certain1, newdata = asap2_lr[!train.rows & asap2$agreed1,])
	predict.certain.full1 <- predict(mlr.out.certain1, newdata = asap2_lr[!train.rows,])

	mlr.out.full2 <- multinom(domain2_score ~ .,
							  data = asap2_lr[train.rows,],
							  MaxNWts = 100000)
	mlr.out.certain2 <- multinom(domain2_score ~ .,
								 data = asap2_lr[train.rows & asap2$agreed2,],
								 MaxNWts = 100000)

	predict.full2 <- predict(mlr.out.full2, newdata = asap2_lr[!train.rows,])
	predict.certain.agreed2 <- predict(mlr.out.certain2, newdata = asap2_lr[!train.rows & asap2$agreed2,])
	predict.certain.full2 <- predict(mlr.out.certain2, newdata = asap2_lr[!train.rows,])

	save(train.rows,
		 mlr.out.certain1, mlr.out.full1,
		 predict.full1, predict.certain.agreed1, predict.certain.full1,
		 mlr.out.certain2, mlr.out.full2,
		 predict.full2, predict.certain.agreed2, predict.certain.full2,
		 file = 'data-raw/mlr.rda')
}

# Domain 1
table(asap2$agreed1) %>% prop.table
# Full
table(asap2_lr[!train.rows,]$domain1_score, predict.full1, useNA = 'ifany')
sum(as.character(asap2_lr[!train.rows,]$domain1_score) == as.character(predict.full1)) / length(predict.full1)
# Certain with agreed scores
table(asap2_lr[!train.rows & asap2$agreed1,]$domain1_score, predict.certain.agreed1, useNA = 'ifany')
sum(as.character(asap2_lr[!train.rows & asap2$agreed1,]$domain1_score) == as.character(predict.certain.agreed1)) / length(predict.certain.agreed1)
# Certain with all validation scores
table(asap2_lr[!train.rows,]$domain1_score, predict.certain.full1, useNA = 'ifany')
sum(as.character(asap2_lr[!train.rows,]$domain1_score) == as.character(predict.certain.full1)) / length(predict.certain.full1)

# Domain 2
table(asap2$agreed2, useNA = 'ifany') %>% prop.table
# Full
table(asap2_lr[!train.rows,]$domain2_score, predict.full2, useNA = 'ifany')
sum(as.character(asap2_lr[!train.rows,]$domain2_score) == as.character(predict.full2)) / length(predict.full2)
# Certain with agreed scores
table(asap2_lr[!train.rows & asap2$agreed2,]$domain2_score, predict.certain.agreed2, useNA = 'ifany')
sum(as.character(asap2_lr[!train.rows & asap2$agreed2,]$domain2_score) == as.character(predict.certain.agreed2)) / length(predict.certain.agreed2)
# Certain with all validation scores
table(asap2_lr[!train.rows,]$domain2_score, predict.certain.full2, useNA = 'ifany')
sum(as.character(asap2_lr[!train.rows,]$domain2_score) == as.character(predict.certain.full2)) / length(predict.certain.full2)
