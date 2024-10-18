logi2 <- function(data, y, x, covars,
                  q_ref = 1, digit_rr = 2, log_info = TRUE){
  # 加载所需的包
  library(dplyr)
  library(tidyr)
  library(broom)
  library(purrr)
  library(logger)
  library(glue)

  # 设置日志记录
  log_layout(layout_glue_generator(format = "[{format(time, '%Y-%m-%d %H:%M:%S')}] {level}: {msg}"))

  # 输入检查
  if (log_info) log_info("开始输入检查...")
  stopifnot(is.data.frame(data) || inherits(data, 'tbl_df'))  # 检查data是否为数据框或tibble
  stopifnot(is.character(y) && length(y) == 1)  # 检查y是否为长度为1的字符型变量
  stopifnot(is.character(x) && length(x) == 1)  # 检查x是否为长度为1的字符型变量
  stopifnot(is.character(covars) && length(covars) >= 0)  # 检查covars是否为字符型且长度大于等于0


  if (!is.character(data[[x]]) && !is.factor(data[[x]])) { # 检查 data[[x]] 是否为字符或因子向量
    stop(paste("自变量", x, "必须是因子或者字符型"))
  }


  if (!all(data[[y]] %in% c(0, 1), na.rm = TRUE)) { # 检查y是否为01编码
    stop(paste("结局变量", y, "必须使用01编码，其中1表示发生了感兴趣的事件。"))
  }

}
