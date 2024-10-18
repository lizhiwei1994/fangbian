#' 亚组Logistics模型拟合与结果提取
#'
#' logi1_sub 用于在分组的基础上拟合逻辑回归模型，
#' 帮助用户分析感兴趣的自变量与结局变量之间的关系，
#' 并根据分组变量逐层提取结果。
#' 函数能够对每个分组的不同水平分别拟合模型，
#' 并提取回归系数、相对风险（RR）及其置信区间等信息。
#' 此外，函数还会计算各分组变量与自变量之间的交互作用 P 值，
#' 以评估自变量与分组变量之间的潜在交互效果。
#'
#' @param data 数据集
#' @param y 结局变量的名字，字符型
#' @param x 感兴趣的自变量x的名字，字符型
#' @param covar 协变量的名字，字符型，可以有多个，也可以没有
#' @param by 分组变量的名字，字符型，可以有多个
#' @param log_info 逻辑型，是否显示日志，默认为TRUE
#'
#' @return 一个包含原始模型结果、交互作用P值结果、提取结果的列表:
#'
#' raw: 一个嵌套的列表，包含所有分组变量及其水平的原始模型结果
#' - 每个分组变量对应一个列表，例如 results_raw$age，包含该分组变量各个水平的原始模型结果
#' - 每个水平中的列表包含通过 summary() 函数获得的模型摘要，包括系数估计、标准误、z 值和 P 值等
#'
#' PforInteract: 一个列表，包含每个分组变量的交互作用 P 值结果
#' - 每个分组变量的名称作为列表的键，例如 results_pforinteract$age
#' - 每个键对应的值是通过 anova() 计算的交互作用 P 值，用于判断分组变量与感兴趣自变量之间是否存在显著交互作用
#'
#' extracted_results: 一个 tibble，存储提取后的主要结果，方便进一步分析或呈现
#' - y: 结局变量的名称
#' - x: 感兴趣的自变量名称
#' - subset: 分组变量的名称
#' - level: 分组变量的具体水平
#' - sample: 当前分组水平中的总样本数量
#' - cases: 当前分组水平中结局事件为 1 的样本数量
#' - PforInteraction: 交互作用 P 值，对应于该分组变量是否有显著交互
#' - rr: 相对风险 (RR) 值，即通过对自变量进行逻辑回归模型拟合后计算得到的指数化系数
#' - lo 和 hi: 相对风险的 95% 置信区间下限和上限，表示估计值的不确定性范围

#' @import dplyr
#' @import broom
#' @import purrr
#' @import tibble
#' @import logger
#' @import glue
#' @export
#'
#' @examples
#' set.seed(123)
#' test_data <- data.frame(
#'   is_bapwv = sample(c(0, 1), 100, replace = TRUE),  # 二分类变量
#'   sbp = rnorm(100),  # 连续变量
#'   covar1 = rnorm(100),
#'   covar2 = rnorm(100),
#'   covar3 = rnorm(100),
#'   type = sample(c("A", "B"), 100, replace = TRUE),
#'   sex =  sample(c("f", "m"), 100, replace = TRUE)
#' )
#' a = logi1_sub(data = test_data,
#' y = 'is_bapwv',
#' x = 'sbp', covar = c('covar1', 'covar2', 'covar3'),
#' by = c('type', 'sex'),
#' log_info = T)
#' print(a)
logi1_sub <- function(data, y, x, covar = NULL, by, log_info = TRUE) {
  # 加载必要的包
  # library(dplyr)
  # library(broom)
  # library(purrr)
  # library(tibble)
  # library(logger)
  # library(glue)

  # 输入参数检查
  if (!all(data[[y]] %in% c(0, 1), na.rm = TRUE)) {
    stop(paste("结局变量", y, "必须使用01编码，其中1表示发生了感兴趣的事件。"))
  }
  if (!is.numeric(data[[x]])) {
    stop(paste("自变量", x, "必须是连续型变量。"))
  }
  # 检查分组变量的类型
  invalid_vars <- by[!sapply(data[by], function(var) is.factor(var) || is.character(var))]
  if (length(invalid_vars) > 0) {
    stop(paste("分组变量by必须是因子型或字符型，但是你提供的变量", paste(invalid_vars, collapse = ", "), "不符合要求。"))
  }

  # 设置日志记录
  log_layout(layout_glue_generator(format = "[{format(time, '%Y-%m-%d %H:%M:%S')}] {level}: {msg}"))

  # 初始化结果存储列表
  results_raw <- list()
  results_pforinteract <- list()
  extracted_results <- tibble()

  if (log_info) log_info("开始按分组变量进行模型拟合")

  for (i in seq_along(by)) {
    group_var <- by[i]
    if (log_info) log_info(sprintf("用户设置了%d个分组变量，当前是第%d个分组变量，分组变量名称为：%s", length(by), i, group_var))
    if (log_info) log_info(sprintf("开始处理分组变量：%s", group_var))
    group_levels <- unique(data[[group_var]])
    results_raw[[group_var]] <- list()

    for (level in group_levels) {
      if (log_info) log_info(sprintf("处理分组水平：%s", level))
      # 按照分组变量的具体水平筛选数据
      data_subset <- data %>% filter(.data[[group_var]] == level)

      # 创建Logistics回归模型公式
      if (is.null(covar)) {
        formula <- as.formula(paste(y, "~", x))
      } else {
        formula <- as.formula(paste(y, "~", x, "+", paste(covar, collapse = "+")))
      }
      if (log_info) log_info(sprintf("拟合模型公式：%s", deparse(formula)))
      model <- glm(formula, data = data_subset, family = binomial())

      # 存储模型的summary结果
      results_raw[[group_var]][[level]] <- {summary_model <- summary(model); summary_model$call <- formula; summary_model}
      if (log_info) log_info("模型拟合完成并存储结果")

      # 提取并存储结果到tibble格式
      model_coefs <- tidy(model) %>% filter(term == x)
      rr <- exp(model_coefs$estimate)
      lo <- exp(model_coefs$estimate - 1.96 * model_coefs$std.error)
      hi <- exp(model_coefs$estimate + 1.96 * model_coefs$std.error)
      cases <- sum(data_subset[[y]] == 1, na.rm = TRUE)
      sample_size <- nrow(data_subset)

      extracted_results <- bind_rows(extracted_results, tibble(
        y = y,
        x = x,
        subset = group_var,
        level = level,
        sample = sample_size,
        cases = cases,
        rr = rr,
        lo = lo,
        hi = hi
      ))
      if (log_info) log_info(sprintf("提取并存储结果：RR = %.6f", rr))
    }

    # 计算交互作用的P值
    if (log_info) log_info(sprintf("开始计算交互作用P值，分组变量：%s", group_var))
    if (is.null(covar)) {
      formula1 <- as.formula(paste(y, "~", x, "+", group_var))
      formula2 <- as.formula(paste(y, "~", x, "*", group_var))
    } else {
      formula1 <- as.formula(paste(y, "~", x, "+", group_var, "+", paste(covar, collapse = "+")))
      formula2 <- as.formula(paste(y, "~", x, "*", group_var, "+", paste(covar, collapse = "+")))
    }
    model1 <- glm(formula1, data = data, family = binomial())
    model2 <- glm(formula2, data = data, family = binomial())
    anova_result <- anova(model1, model2, test = "Chisq")
    results_pforinteract[[group_var]] <- anova_result

    # 将交互作用P值添加到提取的结果中
    p_value <- anova_result$`Pr(>Chi)`[2]
    extracted_results <- extracted_results %>%
      mutate(PforInteraction = ifelse(subset == group_var, p_value, PforInteraction))
    if (log_info) log_info(sprintf("交互作用P值计算结果：P值 = %.6f", p_value))
  }

  if (log_info) log_info("所有模型拟合与结果提取完成")

  return(list(
    raw = results_raw,
    PforInteract = results_pforinteract,
    extracted_results = extracted_results
  ))
}
