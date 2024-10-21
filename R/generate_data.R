#' 生成示例数据集的函数
#'
#' 该函数生成一个包含200行的示例数据集，包含数值型、因子型和字符型变量。
#' 该函数只能在R包内部使用
#' @return 返回一个data.frame，包含示例数据
#' @keywords internal
generate_data <- function() {
  set.seed(123)
  data <- data.frame(
    y_num = sample(c(0, 1), 200, replace = TRUE),
    y_factor = factor(sample(c(0, 1), 200, replace = TRUE)),
    y_char = as.character(sample(c(0, 1), 200, replace = TRUE)),
    x1_num = runif(200, min = 0.01, max = 100),
    x2_factor = factor(sample(c("group_1", "group_2", "group_3"), 200, replace = TRUE)),
    x3_char = sample(c("type_A", "type_B", "type_C"), 200, replace = TRUE),
    covar1_num = rnorm(200, mean = 50, sd = 10),
    covar2_num = rnorm(200, mean = 100, sd = 20),
    covar3_char = sample(c("level_1", "level_2", "level_3", "level_4"), 200, replace = TRUE),
    covar4_factor = factor(sample(c("category_A", "category_B", "category_C", "category_D"), 200, replace = TRUE))
  )
  return(data)
}
