#' 示例数据集
#'
#' 这是一个包含200行的示例数据集，包含多个变量，包括数值型、因子型和字符型变量。
#'
#' @format 一个包含200行的data.frame，变量如下：
#' \describe{
#'   \item{y_num}{数值型，0或1}
#'   \item{y_factor}{因子型，0或1}
#'   \item{y_char}{字符型，0或1}
#'   \item{x1_num}{数值型，值大于0}
#'   \item{x2_factor}{因子型，包含 "group_1", "group_2", "group_3" 三类}
#'   \item{x3_char}{字符型，包含 "type_A", "type_B", "type_C" 三类}
#'   \item{covar1_num}{数值型协变量}
#'   \item{covar2_num}{数值型协变量}
#'   \item{covar3_char}{字符型协变量，包含 "level_1", "level_2", "level_3", "level_4" 四类}
#'   \item{covar4_factor}{因子型协变量，包含 "category_A", "category_B", "category_C", "category_D" 四类}
#' }
#' @source 该数据集为包开发过程中生成的示例数据
#' @examples
#' data(mydata)
#' head(mydata)
"mydata"
