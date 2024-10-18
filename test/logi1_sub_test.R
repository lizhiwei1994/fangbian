set.seed(123)
test_data <- data.frame(
  is_bapwv = sample(c(0, 1), 100, replace = TRUE),  # 二分类变量
  sbp = rnorm(100),  # 连续变量
  covar1 = rnorm(100),
  covar2 = rnorm(100),
  covar3 = rnorm(100),
  type = sample(c("A", "B"), 100, replace = TRUE),
  sex =  sample(c("f", "m"), 100, replace = TRUE)
)

cuo =  data.frame(
  is_bapwv = sample(c(0, 1), 100, replace = TRUE),
  sbp = rnorm(100),  # 连续变量
  covar1 = rnorm(100),
  covar3 = rnorm(100),
  covar2 = sample(c("A", "B"), 100, replace = TRUE),
  sex =  sample(c("f", "m"), 100, replace = TRUE)
)
a = logi1_sub(data = test_data,
              y = 'is_bapwv',
              x = 'sbp', covar = c('covar1', 'covar2', 'covar3'),
              by = c('type', 'sex'),
              log_info = T)


