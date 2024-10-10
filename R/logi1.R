#' Fitting Logistic Regression Model
#'
#' This function fits a logistic regression model and extracts key results.
#'
#' @param data A data frame or tibble.
#' @param y Outcome variable (binary, length 1).
#' @param x Predictor variable (continuous, length 1).
#' @param covars Covariates (character vector, optional).
#' @param q_grp Number of groups for dividing the continuous predictor variable.
#' @param q_val Thresholds for grouping the continuous variable (optional).
#' @param q_ref Reference group for the predictor variable.
#' @param digit_grp Number of digits for displaying the grouped ranges.
#' @return A list containing model summaries and other information.
#' @export
logi1 <- function(data, y, x, covars, q_grp = NULL, q_val = NULL, q_ref = 1, digit_grp = 2) {
  # 加载所需的包
  # library(dplyr)
  # library(tidyr)
  # library(broom)
  # library(purrr)
  # library(logger)
  # library(glue)

  # 设置日志记录
  log_layout(layout_glue_generator(format = "[{format(time, '%Y-%m-%d %H:%M:%S')}] {level}: {msg}"))

  # 输入检查
  log_info("开始输入检查...")
  stopifnot(is.data.frame(data) || inherits(data, 'tbl_df'))  # 检查data是否为数据框或tibble
  stopifnot(is.character(y) && length(y) == 1)  # 检查y是否为长度为1的字符型变量
  stopifnot(is.character(x) && length(x) == 1)  # 检查x是否为长度为1的字符型变量
  stopifnot(is.character(covars) && length(covars) >= 0)  # 检查covars是否为字符型且长度大于等于0

  # 检查q_grp和q_val的设置
  if (!is.null(q_grp) && !is.null(q_val)) {
    stop("q_grp和q_val不能同时设置")
  }
  if (!is.null(q_grp)) {
    stopifnot(q_grp > 0 && length(q_grp) == 1)  # 检查q_grp是否为正整数且长度为1
  }
  if (!is.null(q_val)) {
    stopifnot(is.numeric(q_val))  # 检查q_val是否为数值型
  }
  log_info("输入检查通过！")

  # 创建公式
  log_info("创建公式...")
  covar_formula <- if(length(covars) == 0) "" else paste(covars, collapse = ' + ')
  form_base <- as.formula(paste(y, "~", x, ifelse(covar_formula == "", "", paste("+", covar_formula))))
  log_info("基础公式 (model_1): {deparse(form_base)}")

  # 拟合模型1（原始模型）
  log_info("拟合模型1（原始模型）...")
  model1 <- glm(form_base, data = data, family = binomial())
  model1$call$formula <- form_base
  model1_summary <- summary(model1)
  log_info("模型1拟合完成！")

  # 拟合模型2（x标准化）
  log_info("拟合模型2（x标准化）...")
  data <- data %>% mutate(!!paste0(x, '_scaled') := scale(.data[[x]], center = TRUE, scale = TRUE)[, 1])  # 标准化x变量并单独存储
  form_scaled <- as.formula(paste(y, "~", paste0(x, '_scaled'), ifelse(covar_formula == "", "", paste("+", covar_formula))))
  log_info("标准化公式 (model_2): {deparse(form_scaled)}")
  model2 <- glm(form_scaled, data = data, family = binomial())
  model2$call$formula <- form_scaled
  model2_summary <- summary(model2)
  log_info("模型2拟合完成！")

  # 为模型3和模型4准备分组后的x变量
  log_info("准备分组后的x变量...")
  if (!is.null(q_grp)) {
    breaks <- quantile(data[[x]], probs = seq(0, 1, length.out = q_grp + 1), na.rm = TRUE)
    log_info("分组方式为q_grp，用户指定分组数量，将变量{x}按照分位数分组后，划分为{q_grp}组")
  } else if (!is.null(q_val)) {
    breaks <- c(-Inf, q_val, Inf)
    log_info("分组方式为q_val，用户指定分组切点，将使用用户指定切点，将变量{x}划分为{length(q_val) + 1}组")
  }

  if (!is.null(breaks)) {
    x_grouped <- cut(data[[x]], breaks = breaks, include.lowest = TRUE)  # 根据分位数或指定的阈值进行分组
    original_levels <- levels(x_grouped)  # 保存原始的分组水平顺序
    levels(x_grouped) <- paste0("grp_", seq_along(levels(x_grouped)))  # 修改分组后的名称为grp_1, grp_2, ...
    x_grouped <- relevel(x_grouped, ref = paste0("grp_", q_ref))  # 设置参考组
  }
  data <- data %>% mutate(!!paste0(x, '_grouped') := x_grouped)
  log_info("分组完成！")

  # 创建分组信息数据框
  group_info <- data.frame(
    group_var = x,
    group_name = paste0("grp_", seq_along(original_levels)),
    group_range = sapply(seq_along(original_levels), function(i) {
      if (i == 1) {
        sprintf(paste0("x <= %.", digit_grp, "f"), breaks[i + 1])
      } else if (i == length(original_levels)) {
        sprintf(paste0("x > %.", digit_grp, "f"), breaks[i])
      } else {
        sprintf(paste0("%.", digit_grp, "f < x <= %.", digit_grp, "f"), breaks[i], breaks[i + 1])
      }
    }),
    group_ref = ifelse(paste0("grp_", seq_along(original_levels)) == paste0("grp_", q_ref), "Ref", NA)
  )

  # 展示分组信息数据框
  log_info("分组信息数据框: {capture.output(print(group_info))}")

  # 拟合模型3（分类变量）
  log_info("拟合模型3（分类变量）...")
  form_grouped <- as.formula(paste(y, "~", paste0(x, '_grouped'), ifelse(covar_formula == "", "", paste("+", covar_formula))))
  log_info("分类变量公式 (model_3): {deparse(form_grouped)}")
  model3 <- glm(form_grouped, data = data, family = binomial())
  model3$call$formula <- form_grouped
  model3_summary <- summary(model3)
  log_info("模型3拟合完成！")

  # 拟合模型4（分类变量转为数值型）
  log_info("拟合模型4（分类变量转为数值型）...")
  data <- data %>% mutate(!!paste0(x, '_grouped_numeric') := as.numeric(x_grouped))  # 将分组后的变量转换为数值型
  form_grouped_numeric <- as.formula(paste(y, "~", paste0(x, '_grouped_numeric'), ifelse(covar_formula == "", "", paste("+", covar_formula))))
  log_info("数值型分类变量公式 (model_4): {deparse(form_grouped_numeric)}")
  model4 <- glm(form_grouped_numeric, data = data, family = binomial())
  model4$call$formula <- form_grouped_numeric
  model4_summary <- summary(model4)
  log_info("模型4拟合完成！")

  # 从模型中提取结果
  log_info("从模型中提取结果...")
  extract_results <- function(model) {
    log_info("提取模型结果...")
    tidy(model) %>%
      filter(term != "(Intercept)" & grepl(x, term)) %>%  # 仅提取与x相关的变量，过滤掉截距项
      transmute(
        term,
        rr = exp(estimate),  # 计算相对风险（RR）
        lo = exp(estimate - 1.96 * std.error),  # 计算RR的95%置信区间下限
        hi = exp(estimate + 1.96 * std.error),  # 计算RR的95%置信区间上限
        rr2 = sprintf("%.2f", rr),  # rr保留2位小数
        lo2 = sprintf("%.2f", lo),  # lo保留2位小数
        hi2 = sprintf("%.2f", hi),  # hi保留2位小数
        ci = as.character(glue("{rr2} ({lo2}, {hi2})")),  # 拼接置信区间
        p = p.value,  # p值
        p2 = case_when(
          p <= 0.001 ~ "< 0.001",
          p <= 0.05 ~ "< 0.05",
          TRUE ~ sprintf("%.2f", p)
        )
      )
  }

  model1_results <- extract_results(model1)
  model2_results <- extract_results(model2)
  model3_results <- extract_results(model3)

  # 从模型4中提取趋势的p值
  log_info("从模型4中提取趋势的p值...")
  p_trend1 <- tryCatch({
    tidy(model4) %>% filter(term == paste0(x, '_grouped_numeric')) %>% pull(p.value)
  }, error = function(e) {
    log_warn("未找到x_grouped_numeric项，无法提取p值。")
    NA
  })
  model3_results <- model3_results %>%
    mutate(
      PforTrend = p_trend1,
      PforTrend2 = case_when(
        PforTrend <= 0.001 ~ "< 0.001",
        PforTrend <= 0.05 ~ "< 0.05",
        TRUE ~ sprintf("%.2f", PforTrend)
      )
    )

  # 创建输出列表
  log_info("创建输出列表...")
  output <- list(
    raw = list(
      raw_model1 = model1_summary,
      raw_model2 = model2_summary,
      raw_model3 = model3_summary,
      raw_model4 = model4_summary
    ),
    model1_continuous = model1_results,
    model2_per_SD = model2_results,
    model3_classified = model3_results,
    group = group_info
  )

  log_info("函数执行完成，返回结果！")
  return(output)
}

