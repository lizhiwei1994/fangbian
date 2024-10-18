#' 拟合逻辑回归以及提取模型结果
#'
#' 该函数将拟合3种Logistics回归，并提取模型结果。
#' 第一种是将变量x(连续型)直接纳入模型；
#' 第二种是将变量x标化后(mean = 0, sd = 1)纳入模型；
#' 第三种是将变量x按照用户指定的分组方式(默认为分位数分组)转化为分类变量后纳入模型；
#'
#' @param data 数据集（必须是数据框或tibble）
#' @param y 结局变量名称（字符，长度为1，必须是二分类）
#' @param x 感兴趣的自变量名称（字符型，长度为1，x对应的数据必须是连续型）
#' @param covars 协变量名称（字符型，长度大于等于0，可以为空）
#' @param q_grp 分组数，将x对应的连续型数据划分为几组（正整数，长度为1），不能与q_val同时设置
#' @param q_val 分组的界值，将x划分为不同的组别（数值型，长度任意），不能与q_grp同时设置
#' @param q_ref 参考组，指定x转换为分类变量后，第几组为参考组（正整数，长度为1，默认值为1）
#' @param digit_grp 数值保留小数位数，用于分组范围的展示（正整数，默认值为2）
#' @param digit_rr rr值及95%CI保留的小数位数（正整数，默认值为2）
#' @param log_info 逻辑型，是否显示日志，默认为TRUE
#' @return 返回一个列表，列表中有4个元素，具体如下：
#'
#' **元素1: raw (type = list)**
#'
#'  - raw_model1: 模型1（原始连续型变量x）原始结果 (type = summary.glm)
#'
#'  - raw_model2: 模型2（变量x标化后）原始结果 (type = summary.glm)
#'
#'  - raw_model3: 模型3（变量x分类后）原始结果 (type = summary.glm)
#'
#'  - raw_model4: 模型4（变量x分类后，再转化为数值，仅用于提取趋势性检验P值）原始结果 (type = summary.glm)
#'
#' **元素2: model1_continuous (type = data.frame)**
#'
#' - 模型1提取后的结果
#'
#' **元素3: model2_per_SD (type = data.frame)**
#'
#' - 模型2提取后的结果
#'
#' **元素4: model3_classified  (type = data.frame)**
#'
#' - 模型3提取后的结果（相对于前2个模型，多了一个趋势性检验P值）
#'
#' **元素5: group (type = data.frame)**
#'
#' - 模型3中变量x的分组标准
#'
#' model1_continuous, model2_per_SD, model3_classified中变量定义如下：
#'
#' - term: 连续型变量x的名称
#'
#' - rr: RR值 (原始)
#'
#' - lo: RR值的95%CI下限 (原始)
#'
#' - hi: RR值的95%CI上限 (原始)
#'
#' - rr2: RR值 (保留2位小数)
#'
#' - lo2: RR值的95%CI下限 (保留2位小数)
#'
#' - hi2: RR值的95%CI上限 (保留2位小数)
#'
#' - ci: RR值和95%CI的组合：rr (lo, hi)
#'
#' - p: RR值对应的P值 (原始)
#'
#' - p2: 如果p>0.05则显示保留2位小数的p值，如果p<0.001则显示"<0.001"，，如果p<0.05则显示"<0.05"
#'
#' - PforTrend: 趋势性检验P值（只有model3_classified才有这个变量）
#'
#' - PforTrend2: 计算方法同p2
#'
#' group中的变量定义如下：
#'
#' - group_var: 变量x的名称
#'
#' - group_name: 变量x分组后的名称
#'
#' - group_range: 每组的取值范围
#'
#' - group_ref: 哪一组是参考组 (Ref)
#'
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import broom
#' @import purrr
#' @import logger
#' @import glue
#'
#' @examples
#' # 示例数据生成
#' set.seed(123)
#' test_data <- data.frame(
#'   is_bapwv = sample(c(0, 1), 100, replace = TRUE),  # 二分类变量
#'   sbp = rnorm(100),  # 连续变量
#'   covar1 = rnorm(100),
#'   covar2 = sample(c("A", "B"), 100, replace = TRUE)
#' )
#'
#' # 调用 logi1 函数拟合逻辑回归模型
#' output <- logi1(
#'   data = test_data,
#'   y = "is_bapwv",
#'   x = "sbp",
#'   covars = c("covar1", "covar2"),
#'   q_grp = 4
#' )
#'
#' # 输出结果
#' print(output)
logi1 <- function(data, y, x, covars, q_grp = NULL, q_val = NULL,
                  q_ref = 1, digit_grp = 2, digit_rr = 2,
                  log_info = TRUE) {
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
  if (log_info) log_info("开始输入检查...")
  stopifnot(is.data.frame(data) || inherits(data, 'tbl_df'))  # 检查data是否为数据框或tibble
  stopifnot(is.character(y) && length(y) == 1)  # 检查y是否为长度为1的字符型变量
  stopifnot(is.character(x) && length(x) == 1)  # 检查x是否为长度为1的字符型变量
  stopifnot(is.character(covars) && length(covars) >= 0)  # 检查covars是否为字符型且长度大于等于0

  # 检查x的类型
  if (!is.numeric(data[[x]]) && !is.factor(data[[x]]) && !is.character(data[[x]])) {
    stop("x必须是数值型、因子型或字符型变量")
  }

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
  if (log_info) log_info("输入检查通过！")

  # 创建公式
  if (log_info) log_info("创建公式...")
  covar_formula <- if(length(covars) == 0) "" else paste(covars, collapse = ' + ')
  form_base <- as.formula(paste(y, "~", x, ifelse(covar_formula == "", "", paste("+", covar_formula))))
  if (log_info) log_info("基础公式 (model_1): {deparse(form_base)}")

  # 创建提取结果的自定义函数
  extract_results <- function(model) {
    if (log_info) log_info("提取模型结果...")
    tidy(model) %>%
      filter(term != "(Intercept)" & grepl(x, term)) %>%  # 仅提取与x相关的变量，过滤掉截距项
      transmute(
        y = y,
        term,
        rr = exp(estimate),  # 计算相对风险（RR）
        lo = exp(estimate - 1.96 * std.error),  # 计算RR的95%置信区间下限
        hi = exp(estimate + 1.96 * std.error),  # 计算RR的95%置信区间上限
        rr2 = sprintf(glue('%.{digit_rr}f'), rr),  # rr保留2位小数
        lo2 = sprintf(glue('%.{digit_rr}f'), lo),  # lo保留2位小数
        hi2 = sprintf(glue('%.{digit_rr}f'), hi),  # hi保留2位小数
        ci = as.character(glue("{rr2} ({lo2}, {hi2})")),  # 拼接置信区间
        p = p.value,  # p值
        p2 = case_when(
          p <= 0.001 ~ "< 0.001",
          p <= 0.05 ~ "< 0.05",
          TRUE ~ sprintf("%.2f", p)
        )
      )
  }

  # 针对x为因子或字符型的情况
  if (is.factor(data[[x]]) || is.character(data[[x]])) {
    if (log_info) log_info("x为因子或字符型，直接拟合模型...")
    model1 <- glm(form_base, data = data, family = binomial())
    model1$call$formula <- form_base
    model1_summary <- summary(model1)
    model1_results <- extract_results(model1)
    output <- list(
      raw = list(raw_model1 = model1_summary),
      model = model1_results
    )
    return(output)
  }

  if (is.numeric(data[[x]])) {
    if (log_info) log_info("x是数值型，将分别拟合三种模型")
  }


  model1 <- glm(form_base, data = data, family = binomial())
  model1$call$formula <- form_base
  model1_summary <- summary(model1)
  if (log_info) log_info("模型1拟合完成！")

  # 拟合模型2（x标准化）
  if (log_info) log_info("拟合模型2（x标准化）...")
  data <- data %>% mutate(!!paste0(x, '_scaled') := scale(.data[[x]], center = TRUE, scale = TRUE)[, 1])  # 标准化x变量并单独存储
  form_scaled <- as.formula(paste(y, "~", paste0(x, '_scaled'), ifelse(covar_formula == "", "", paste("+", covar_formula))))
  if (log_info) log_info("标准化公式 (model_2): {deparse(form_scaled)}")
  model2 <- glm(form_scaled, data = data, family = binomial())
  model2$call$formula <- form_scaled
  model2_summary <- summary(model2)
  if (log_info) log_info("模型2拟合完成！")

  # 其余部分保持不变
  # 为模型3和模型4准备分组后的x变量
  if (log_info) log_info("准备分组后的x变量...")
  if (!is.null(q_grp)) {
    breaks <- quantile(data[[x]], probs = seq(0, 1, length.out = q_grp + 1), na.rm = TRUE)
    if (log_info) log_info("分组方式为q_grp，用户指定分组数量，将变量{x}按照分位数分组后，划分为{q_grp}组")
  } else if (!is.null(q_val)) {
    breaks <- c(-Inf, q_val, Inf)
    if (log_info) log_info("分组方式为q_val，用户指定分组切点，将使用用户指定切点，将变量{x}划分为{length(q_val) + 1}组")
  }

  if (!is.null(breaks)) {
    x_grouped <- cut(data[[x]], breaks = breaks, include.lowest = TRUE)  # 根据分位数或指定的阈值进行分组
    original_levels <- levels(x_grouped)  # 保存原始的分组水平顺序
    levels(x_grouped) <- paste0("grp_", seq_along(levels(x_grouped)))  # 修改分组后的名称为grp_1, grp_2, ...
    x_grouped <- relevel(x_grouped, ref = paste0("grp_", q_ref))  # 设置参考组
  }
  data <- data %>% mutate(!!paste0(x, '_grouped') := x_grouped)
  if (log_info) log_info("分组完成！")

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
  if (log_info) log_info("分组信息数据框: {capture.output(print(group_info))}")

  # 拟合模型3（分类变量）
  if (log_info) log_info("拟合模型3（分类变量）...")
  form_grouped <- as.formula(paste(y, "~", paste0(x, '_grouped'), ifelse(covar_formula == "", "", paste("+", covar_formula))))
  if (log_info) log_info("分类变量公式 (model_3): {deparse(form_grouped)}")
  model3 <- glm(form_grouped, data = data, family = binomial())
  model3$call$formula <- form_grouped
  model3_summary <- summary(model3)
  if (log_info) log_info("模型3拟合完成！")

  # 拟合模型4（分类变量转为数值型）
  if (log_info) log_info("拟合模型4（分类变量转为数值型）...")
  data <- data %>% mutate(!!paste0(x, '_grouped_numeric') := as.numeric(x_grouped))  # 将分组后的变量转换为数值型
  form_grouped_numeric <- as.formula(paste(y, "~", paste0(x, '_grouped_numeric'), ifelse(covar_formula == "", "", paste("+", covar_formula))))
  if (log_info) log_info("数值型分类变量公式 (model_4): {deparse(form_grouped_numeric)}")
  model4 <- glm(form_grouped_numeric, data = data, family = binomial())
  model4$call$formula <- form_grouped_numeric
  model4_summary <- summary(model4)
  if (log_info) log_info("模型4拟合完成！")

  # 从模型中提取结果
  if (log_info) log_info("从模型中提取结果...")


  model1_results <- extract_results(model1)
  model2_results <- extract_results(model2)
  model3_results <- extract_results(model3)

  # 从模型4中提取趋势的p值
  if (log_info) log_info("从模型4中提取趋势的p值...")
  p_trend1 <- tryCatch({
    tidy(model4) %>% filter(term == paste0(x, '_grouped_numeric')) %>% pull(p.value)
  }, error = function(e) {
    if (log_info) log_warn("未找到x_grouped_numeric项，无法提取p值。")
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
  if (log_info) log_info("创建输出列表...")
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

  if (log_info) log_info("函数执行完成，返回结果！")
  return(output)
}
