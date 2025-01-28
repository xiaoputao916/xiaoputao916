# 加载必要的包
if (!require("igraph")) install.packages("igraph")
if (!require("dplyr")) install.packages("dplyr")
if (!require("readxl")) install.packages("readxl")
if (!require("tidyr")) install.packages("tidyr")

library(igraph)
library(dplyr)
library(readxl)
library(tidyr)

# 设置文件路径
file_path <- "C:\\Users\\s224035518\\OneDrive - Deakin University\\part1\\raw data\\world_goal_24years.xlsx"

# 读取数据
data <- read_excel(file_path)

# 定义滑动窗口大小
window_size <- 10
num_windows <- nrow(data) - window_size + 1

# 获取目标列
goal_columns <- grep("^goal", colnames(data), value = TRUE)

# 初始化结果存储
synergy_results <- data.frame()
tradeoff_results <- data.frame()

# 高斯平滑函数,平滑度为3
gaussian_smooth <- function(x, sigma = 2) {
  weights <- dnorm(seq(-3 * sigma, 3 * sigma, length.out = 7), mean = 0, sd = sigma)
  weights <- weights / sum(weights)
  stats::filter(x, weights, sides = 2)
}

# 滑动窗口分析
for (i in 1:num_windows) {
  # 提取当前窗口的数据
  data_window <- data[i:(i + window_size - 1), ]
  numeric_data <- data_window[, goal_columns]
  
  # 初始化相关性矩阵
  correlation_matrix <- matrix(0, ncol = ncol(numeric_data), nrow = ncol(numeric_data))
  colnames(correlation_matrix) <- colnames(numeric_data)
  rownames(correlation_matrix) <- colnames(numeric_data)
  
  # 计算相关性矩阵并过滤不显著的相关性
  for (x in 1:ncol(numeric_data)) {
    for (y in x:ncol(numeric_data)) {
      if (x != y) {
        test <- cor.test(numeric_data[[x]], numeric_data[[y]], method = "spearman", use = "pairwise.complete.obs")
        if (test$p.value <= 0.05) {
          correlation_matrix[x, y] <- test$estimate
          correlation_matrix[y, x] <- test$estimate
        }
      }
    }
  }
  
  # 构建协同和权衡网络
  synergy_matrix <- ifelse(correlation_matrix > 0, correlation_matrix, 0)
  tradeoff_matrix <- ifelse(correlation_matrix < 0, -correlation_matrix, 0)
  
  # 计算协同网络的 Hub Score
  synergy_graph <- graph.adjacency(synergy_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)
  synergy_hubs <- hub_score(synergy_graph, weights = E(synergy_graph)$weight)$vector
  
  # 存储协同结果
  synergy_results <- rbind(
    synergy_results,
    data.frame(
      window = paste(min(data_window$year), max(data_window$year), sep = "-"),
      year = mean(data_window$year),
      t(as.data.frame(synergy_hubs, col.names = goal_columns))
    )
  )
  
  # 计算权衡网络的 Hub Score
  tradeoff_graph <- graph.adjacency(tradeoff_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)
  tradeoff_hubs <- hub_score(tradeoff_graph, weights = E(tradeoff_graph)$weight)$vector
  
  # 存储权衡结果
  tradeoff_results <- rbind(
    tradeoff_results,
    data.frame(
      window = paste(min(data_window$year), max(data_window$year), sep = "-"),
      year = mean(data_window$year),
      t(as.data.frame(tradeoff_hubs, col.names = goal_columns))
    )
  )
}

# 对表格进行格式整理
synergy_results <- synergy_results %>%
  mutate(across(starts_with("goal"), ~ gaussian_smooth(.x, sigma = 2)))

tradeoff_results <- tradeoff_results %>%
  mutate(across(starts_with("goal"), ~ gaussian_smooth(.x, sigma = 2)))

# 保存结果为 CSV
write.csv(synergy_results, "synergy_results_time_based_gaussian.csv", row.names = FALSE)
write.csv(tradeoff_results, "tradeoff_results_time_based_gaussian.csv", row.names = FALSE)
