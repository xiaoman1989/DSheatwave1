# 加载必要的包
library(ggplot2)
library(dplyr)
library(mgcv)  # 用于GAM
library(readxl)  # 用于读取Excel文件
setwd("D:/地湖所工作/2025 热浪小论文XMGS02/Fig_S1")
# 1. 创建输出文件夹
output_dir <- "Fig_S1_260113"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  cat(paste("创建文件夹:", output_dir, "\n"))
} else {
  cat(paste("文件夹已存在:", output_dir, "\n"))
}

# 2. 读取Excel文件
# 请确保文件路径正确
data <- read_excel("fig_S1.xlsx")

# 检查数据结构
str(data)
head(data)

# 3. 转换必要的列为因子
data$Treatment <- as.factor(data$Treatment)
data$Tank <- as.factor(data$Tank)

# 4. 创建混合处理的新变量
data$Mixing_status <- ifelse(grepl("Mixing", data$Treatment), "Mixing", "Non-Mixing")
data$Mixing_status <- as.factor(data$Mixing_status)

theme_pub <- theme_minimal(base_size = 16) +
  theme(
    # 去掉网格线
    panel.grid = element_blank(),
    
    # 加外框（四条边）
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 1
    ),
    
    # 坐标轴
    axis.title = element_text(size = 18),
    axis.text  = element_text(size = 16, color = "black"),
    
    # 坐标轴刻度线
    axis.ticks = element_line(color = "black", linewidth = 0.8),
    
    # 图例
    legend.title = element_blank(),
    legend.text  = element_text(size = 16),
    
    # 分面标签
    strip.text = element_text(size = 16),
    
    # 背景
    plot.background  = element_blank(),
    panel.background = element_blank()
  )

# 5. 创建并保存第一张图：biomass ~ stability 散点图（按处理着色）
plot1 <- ggplot(data, aes(x = stability, y = total_biomass, color = Treatment)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, bs = "tp"),
    se = TRUE,
    alpha = 0.2
  ) +
  labs(
    x = "Stability",
    y = "Total biomass"
  ) +
  scale_color_brewer(palette = "Set2") +
  theme_pub


# 保存第一张图
ggsave(file.path(output_dir, "1_biomass_vs_stability_by_treatment.png"), 
       plot1, width = 10, height = 6, dpi = 300)
cat("已保存: 1_biomass_vs_stability_by_treatment.png\n")

# 6. 创建并保存第二张图：biomass ~ stability（按混合状态着色）
plot1_mixing <- ggplot(data, aes(x = stability, y = total_biomass, color = Mixing_status)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, bs = "tp"),
    se = TRUE,
    alpha = 0.2
  ) +
  labs(
    x = "Stability",
    y = "Total biomass"
  ) +
  scale_color_manual(
    values = c("Mixing" = "#4B9CD3",
               "Non-Mixing"   = "#E24A33")
  ) +
  theme_pub

ggsave(file.path(output_dir, "2_biomass_vs_stability_by_mixing.png"), 
       plot1_mixing, width = 10, height = 6, dpi = 300)
cat("已保存: 2_biomass_vs_stability_by_mixing.png\n")

# 7. 创建并保存第三张图：TDP_bottom ~ stability
# 注意：有些行的TDP_bottom是NA，需要过滤
plot2_bottom <- ggplot(
  data %>% filter(!is.na(TDP_bottom)),
  aes(x = stability, y = TDP_bottom, color = Mixing_status)
) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, bs = "tp"),
    se = TRUE,
    alpha = 0.2
  ) +
  labs(
    x = "Stability",
    y = "Bottom TDP"
  ) +
  scale_color_manual(
    values = c("Mixing" = "#4B9CD3",
               "Non-Mixing"   = "#E24A33")
  ) +
  theme_pub


ggsave(file.path(output_dir, "3_TDPbottom_vs_stability_by_mixing.png"), 
       plot2_bottom, width = 10, height = 6, dpi = 300)
cat("已保存: 3_TDPbottom_vs_stability_by_mixing.png\n")

# 8. 创建并保存第四张图：TDP_surface ~ stability
plot2_surface <- ggplot(
  data %>% filter(!is.na(TDP_surface)),
  aes(x = stability, y = TDP_surface, color = Mixing_status)
) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, bs = "tp"),
    se = TRUE,
    alpha = 0.2
  ) +
  labs(
    x = "Stability",
    y = "Surface TDP"
  ) +
  scale_color_manual(
    values = c("Mixing" = "#4B9CD3",
               "Non-Mixing"   = "#E24A33")
  ) +
  theme_pub

ggsave(file.path(output_dir, "4_TDPsurface_vs_stability_by_mixing.png"), 
       plot2_surface, width = 10, height = 6, dpi = 300)
cat("已保存: 4_TDPsurface_vs_stability_by_mixing.png\n")

# 9. 创建并保存分面图（可选）
plot1_facet <- ggplot(data, aes(x = stability, y = total_biomass, color = Treatment)) +
  geom_point(size = 2) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, bs = "tp"),
    se = FALSE
  ) +
  facet_wrap(~ Treatment, scales = "free") +
  labs(
    x = "Stability",
    y = "Total biomass"
  ) +
  theme_pub +
  theme(legend.position = "none")


ggsave(file.path(output_dir, "5_biomass_vs_stability_faceted.png"), 
       plot1_facet, width = 12, height = 8, dpi = 300)
cat("已保存: 5_biomass_vs_stability_faceted.png\n")

# 10. 创建并保存LOESS版本（作为GAM的替代）
plot_loess <- ggplot(data, aes(x = stability, y = total_biomass, color = Mixing_status)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", 
              se = TRUE, 
              alpha = 0.2,
              span = 0.8) +  # span控制平滑度
  labs(title = "Biomass vs. Stability by Mixing Status (LOESS)",
       x = "Stability",
       y = "Total Biomass",
       color = "Mixing Status") +
  theme_minimal() +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  scale_color_manual(values = c("Mixing" = "#4B9CD3", "Non-Mixing" = "#E24A33"))

ggsave(file.path(output_dir, "6_biomass_vs_stability_loess.png"), 
       plot_loess, width = 10, height = 6, dpi = 300)
cat("已保存: 6_biomass_vs_stability_loess.png\n")

# 11. 创建汇总统计表并保存到CSV文件
# 计算各处理组的统计量
summary_stats <- data %>%
  group_by(Treatment, Mixing_status) %>%
  summarise(
    n = n(),
    mean_stability = mean(stability, na.rm = TRUE),
    sd_stability = sd(stability, na.rm = TRUE),
    mean_biomass = mean(total_biomass, na.rm = TRUE),
    sd_biomass = sd(total_biomass, na.rm = TRUE),
    mean_TDP_surface = mean(TDP_surface, na.rm = TRUE),
    sd_TDP_surface = sd(TDP_surface, na.rm = TRUE),
    .groups = 'drop'
  )

# 保存统计表
write.csv(summary_stats, file.path(output_dir, "summary_statistics.csv"), row.names = FALSE)
cat("已保存: summary_statistics.csv\n")

# 12. 创建相关性分析表
correlation_table <- data %>%
  summarise(
    cor_biomass_stability = cor(total_biomass, stability, use = "complete.obs"),
    cor_TDPbottom_stability = cor(TDP_bottom, stability, use = "complete.obs"),
    cor_TDPsurface_stability = cor(TDP_surface, stability, use = "complete.obs"),
    cor_biomass_TDPsurface = cor(total_biomass, TDP_surface, use = "complete.obs")
  )

# 按混合状态分组的相关性
correlation_by_mixing <- data %>%
  group_by(Mixing_status) %>%
  summarise(
    cor_biomass_stability = cor(total_biomass, stability, use = "complete.obs"),
    cor_TDPbottom_stability = cor(TDP_bottom, stability, use = "complete.obs"),
    cor_TDPsurface_stability = cor(TDP_surface, stability, use = "complete.obs"),
    n = n()
  )

# 保存相关性表
write.csv(correlation_table, file.path(output_dir, "correlation_overall.csv"), row.names = FALSE)
write.csv(correlation_by_mixing, file.path(output_dir, "correlation_by_mixing.csv"), row.names = FALSE)
cat("已保存: correlation_overall.csv\n")
cat("已保存: correlation_by_mixing.csv\n")

# 13. 创建运行日志
log_file <- file.path(output_dir, "analysis_log.txt")
sink(log_file)
cat("=== 数据分析运行日志 ===\n")
cat(paste("运行时间:", Sys.time(), "\n"))
cat(paste("数据行数:", nrow(data), "\n"))
cat(paste("数据列数:", ncol(data), "\n"))
cat("\n=== 变量信息 ===\n")
str(data)
cat("\n=== 处理组分布 ===\n")
print(table(data$Treatment))
cat("\n=== 混合状态分布 ===\n")
print(table(data$Mixing_status))
cat("\n=== 保存的文件列表 ===\n")
cat("1_biomass_vs_stability_by_treatment.png\n")
cat("2_biomass_vs_stability_by_mixing.png\n")
cat("3_TDPbottom_vs_stability_by_mixing.png\n")
cat("4_TDPsurface_vs_stability_by_mixing.png\n")
cat("5_biomass_vs_stability_faceted.png\n")
cat("6_biomass_vs_stability_loess.png\n")
cat("summary_statistics.csv\n")
cat("correlation_overall.csv\n")
cat("correlation_by_mixing.csv\n")
sink()

cat("已保存: analysis_log.txt\n")
cat("\n=== 分析完成 ===\n")
cat(paste("所有输出已保存到文件夹:", output_dir, "\n"))
cat(paste("路径:", file.path(getwd(), output_dir), "\n"))

