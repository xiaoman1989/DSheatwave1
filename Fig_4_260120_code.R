## ======================= 数据读取与清洗 =======================
rm(list = ls())
library(tidyr)
library(ggplot2)
library(readxl)
library(readr)
library(dplyr)
library(ggprism)
library(lubridate)
library(patchwork)
library(openxlsx)
library(scales)
library(stringr)
library(magick)

setwd("D:/地湖所工作/2025 热浪小论文XMGS02/Fig4_cell_conc_suri")

rawdata <- read_excel(
  "Cell_all.xlsx",
  col_types = c("skip","text","skip","text","numeric",
                "text","text","text","text")
)

rawdata <- na.omit(rawdata)
rawdata$Date <- as.character(rawdata$Date)

## 只保留 2024-04-15 到 2024-04-23
base_date <- as.Date("2024-04-15")
rawdata$Day <- as.numeric(as.Date(rawdata$Date) - base_date)
rawdata <- subset(rawdata, Day >= 0 & Day <= 8)

## ======================= 混合计算方式（修正版） =======================
# 1. 计算总细胞浓度（直接对所有物种求和）
total_cells_daily <- rawdata %>%
  group_by(Day, Tank, Treatment) %>%
  summarise(
    total_cells = sum(Cell_conc_10_4_cell_L, na.rm = TRUE),
    .groups = "drop"
  )

# 2. 计算门水平数据（先按门求平均）
phylum_summary <- rawdata %>%
  group_by(Day, Tank, Latin_phylum, Treatment) %>%
  summarise(
    cells_mean = mean(Cell_conc_10_4_cell_L, na.rm = TRUE),  # 门内物种平均
    cells_sum = sum(Cell_conc_10_4_cell_L, na.rm = TRUE),    # 门内物种总和（用于计算比例）
    species_count = n(),                                     # 门内物种数量
    .groups = "drop"
  )

# 3. 合并数据并计算比例
phylum_data <- phylum_summary %>%
  left_join(total_cells_daily, by = c("Day", "Tank", "Treatment")) %>%
  mutate(
    proportion = cells_sum / total_cells,                     # 使用门内物种总和计算比例
    proportion_percent = proportion * 100                     # 转换为百分比
  )

# 4. 验证比例总和（应该等于1.0）
prop_check <- phylum_data %>%
  group_by(Tank, Day) %>%
  summarise(
    total_prop = sum(proportion),
    total_prop_percent = sum(proportion_percent),
    .groups = "drop"
  )

cat("\n=== 比例总和验证 ===\n")
print(prop_check)
cat("\n比例总和平均值:", mean(prop_check$total_prop), "\n")
cat("比例总和标准差:", sd(prop_check$total_prop), "\n")

# 如果比例总和不完全等于1，可以重新归一化（可选）
if(max(abs(prop_check$total_prop - 1)) > 0.001) {
  cat("\n注意：比例总和不完全等于1，进行归一化调整...\n")
  
  phylum_data <- phylum_data %>%
    group_by(Tank, Day) %>%
    mutate(
      proportion = proportion / sum(proportion),            # 重新归一化
      proportion_percent = proportion * 100                 # 更新百分比
    ) %>%
    ungroup()
  
  # 再次验证
  prop_check2 <- phylum_data %>%
    group_by(Tank, Day) %>%
    summarise(
      total_prop = sum(proportion),
      total_prop_percent = sum(proportion_percent),
      .groups = "drop"
    )
  
  cat("\n归一化后比例总和验证:\n")
  print(prop_check2)
  cat("\n归一化后比例总和平均值:", mean(prop_check2$total_prop), "\n")
}

## ======================= 统计总细胞浓度变化 =======================
# 使用真实的总细胞浓度进行统计
total_cell_stats <- total_cells_daily %>%
  group_by(Tank, Treatment, Day) %>%
  summarise(
    total_cells_mean = mean(total_cells, na.rm = TRUE),
    total_cells_sd = sd(total_cells, na.rm = TRUE),
    .groups = "drop"
  )

# 2. 计算每个处理组的Day 0和Day 8的变化
cell_change_stats <- total_cell_stats %>%
  filter(Day %in% c(0, 8)) %>%
  pivot_wider(
    id_cols = c(Tank, Treatment),
    names_from = Day,
    values_from = c(total_cells_mean, total_cells_sd),
    names_sep = "_"
  ) %>%
  mutate(
    change_absolute = total_cells_mean_8 - total_cells_mean_0,
    change_percent = (total_cells_mean_8 - total_cells_mean_0) / total_cells_mean_0 * 100,
    Day0_value = sprintf("%.1f ± %.1f", total_cells_mean_0, total_cells_sd_0),
    Day8_value = sprintf("%.1f ± %.1f", total_cells_mean_8, total_cells_sd_8),
    Absolute_change = sprintf("%.1f", change_absolute),
    Percent_change = sprintf("%.1f%%", change_percent)
  ) %>%
  select(Tank, Treatment, Day0_value, Day8_value, Absolute_change, Percent_change)

# 3. 按处理组分组统计
treatment_labels <- c(
  "57" = "C",
  "63" = "N",
  "54" = "M",
  "59" = "M",
  "62" = "H + N",
  "67" = "H + N",
  "68" = "M + N",
  "70" = "M + N",
  "64" = "H + M + N",
  "65" = "H + M + N"
)

cell_change_stats <- cell_change_stats %>%
  mutate(
    Treatment_Group = treatment_labels[as.character(Tank)]
  )

# 处理组汇总统计
treatment_summary <- cell_change_stats %>%
  group_by(Treatment_Group) %>%
  summarise(
    n_tanks = n(),
    Day0_avg = mean(as.numeric(str_extract(Day0_value, "[0-9.]+"))),
    Day0_sd = mean(as.numeric(str_extract(Day0_value, "(?<=± )[0-9.]+"))),
    Day8_avg = mean(as.numeric(str_extract(Day8_value, "[0-9.]+"))),
    Day8_sd = mean(as.numeric(str_extract(Day8_value, "(?<=± )[0-9.]+"))),
    Change_avg = mean(as.numeric(str_extract(Absolute_change, "[0-9.-]+"))),
    Percent_avg = mean(as.numeric(str_extract(Percent_change, "[0-9.-]+")))
  ) %>%
  mutate(
    Day0 = sprintf("%.1f ± %.1f", Day0_avg, Day0_sd),
    Day8 = sprintf("%.1f ± %.1f", Day8_avg, Day8_sd),
    Absolute_change = sprintf("%.1f", Change_avg),
    Percent_change = sprintf("%.1f%%", Percent_avg)
  ) %>%
  select(Treatment_Group, n_tanks, Day0, Day8, Absolute_change, Percent_change)

## ======================= 门类比例详细统计 =======================
# 1. 计算每个处理组各门类的Day 0和Day 8的比例变化
phylum_proportion_stats <- phylum_data %>%
  filter(Day %in% c(0, 8)) %>%
  group_by(Tank, Treatment, Latin_phylum, Day) %>%
  summarise(
    proportion_mean = mean(proportion_percent, na.rm = TRUE),  # 百分比形式
    cells_mean = mean(cells_mean, na.rm = TRUE),               # 细胞浓度平均值
    species_count = mean(species_count, na.rm = TRUE),         # 物种数量
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(Tank, Treatment, Latin_phylum, species_count),
    names_from = Day,
    values_from = c(proportion_mean, cells_mean),
    names_sep = "_"
  ) %>%
  mutate(
    proportion_change = proportion_mean_8 - proportion_mean_0,
    proportion_change_percent = (proportion_mean_8 - proportion_mean_0) / proportion_mean_0 * 100,
    cells_change = cells_mean_8 - cells_mean_0,
    cells_change_percent = (cells_mean_8 - cells_mean_0) / cells_mean_0 * 100,
    Day0_proportion = sprintf("%.1f%%", proportion_mean_0),
    Day8_proportion = sprintf("%.1f%%", proportion_mean_8),
    Proportion_change = sprintf("%+.1f%%", proportion_change),
    Proportion_change_pct = sprintf("%+.1f%%", proportion_change_percent),
    Day0_cells = sprintf("%.1f", cells_mean_0),
    Day8_cells = sprintf("%.1f", cells_mean_8),
    Cells_change = sprintf("%+.1f", cells_change),
    Cells_change_pct = sprintf("%+.1f%%", cells_change_percent)
  ) %>%
  select(
    Tank, Treatment, Latin_phylum, species_count,
    Day0_proportion, Day8_proportion, Proportion_change, Proportion_change_pct,
    Day0_cells, Day8_cells, Cells_change, Cells_change_pct
  )

# 2. 添加处理组标签
phylum_proportion_stats <- phylum_proportion_stats %>%
  mutate(
    Treatment_Group = treatment_labels[as.character(Tank)]
  )

# 3. 按处理组汇总门类比例变化
phylum_treatment_summary <- phylum_proportion_stats %>%
  group_by(Treatment_Group, Latin_phylum) %>%
  summarise(
    n_tanks = n(),
    avg_species_count = mean(species_count, na.rm = TRUE),
    avg_Day0_prop = mean(as.numeric(str_extract(Day0_proportion, "[0-9.]+")), na.rm = TRUE),
    avg_Day8_prop = mean(as.numeric(str_extract(Day8_proportion, "[0-9.]+")), na.rm = TRUE),
    avg_prop_change = mean(as.numeric(str_extract(Proportion_change, "[0-9.-]+")), na.rm = TRUE),
    avg_Day0_cells = mean(as.numeric(Day0_cells), na.rm = TRUE),
    avg_Day8_cells = mean(as.numeric(Day8_cells), na.rm = TRUE),
    avg_cells_change = mean(as.numeric(Cells_change), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Day0_proportion = sprintf("%.1f%%", avg_Day0_prop),
    Day8_proportion = sprintf("%.1f%%", avg_Day8_prop),
    Proportion_change = sprintf("%+.1f%%", avg_prop_change),
    Day0_cells = sprintf("%.1f", avg_Day0_cells),
    Day8_cells = sprintf("%.1f", avg_Day8_cells),
    Cells_change = sprintf("%+.1f", avg_cells_change)
  ) %>%
  select(
    Treatment_Group, Latin_phylum, n_tanks, avg_species_count,
    Day0_proportion, Day8_proportion, Proportion_change,
    Day0_cells, Day8_cells, Cells_change
  )

# 4. 计算各门类在Day 8的主导地位（按比例排序）
phylum_dominance_day8 <- phylum_proportion_stats %>%
  group_by(Treatment_Group, Latin_phylum) %>%
  summarise(
    avg_Day8_prop = mean(as.numeric(str_extract(Day8_proportion, "[0-9.]+")), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Treatment_Group) %>%
  arrange(desc(avg_Day8_prop)) %>%
  mutate(
    rank = row_number(),
    dominance_level = case_when(
      avg_Day8_prop >= 50 ~ "主导门类(≥50%)",
      avg_Day8_prop >= 20 ~ "主要门类(20-50%)",
      avg_Day8_prop >= 5 ~ "次要门类(5-20%)",
      TRUE ~ "稀有门类(<5%)"
    )
  ) %>%
  select(Treatment_Group, Latin_phylum, rank, avg_Day8_prop, dominance_level)

# 5. 计算各处理组门类多样性变化
phylum_diversity_stats <- phylum_data %>%
  group_by(Tank, Treatment, Day) %>%
  summarise(
    num_phyla = n_distinct(Latin_phylum),                    # 门类数量
    shannon_index = -sum(proportion * log(proportion + 1e-10)), # Shannon多样性指数
    simpson_index = 1 - sum(proportion^2),                    # Simpson多样性指数
    total_species = sum(species_count),                      # 总物种数
    .groups = "drop"
  ) %>%
  filter(Day %in% c(0, 8)) %>%
  pivot_wider(
    id_cols = c(Tank, Treatment),
    names_from = Day,
    values_from = c(num_phyla, shannon_index, simpson_index, total_species),
    names_sep = "_"
  ) %>%
  mutate(
    Treatment_Group = treatment_labels[as.character(Tank)],
    phyla_change = num_phyla_8 - num_phyla_0,
    shannon_change = shannon_index_8 - shannon_index_0,
    simpson_change = simpson_index_8 - simpson_index_0,
    species_change = total_species_8 - total_species_0
  )

## ======================= 保存统计结果到Excel =======================
output_folder <- "Phylum_cells_260120"
if (!dir.exists(output_folder)) dir.create(output_folder)

# 创建Excel工作簿
wb <- createWorkbook()

# 1. 总细胞浓度变化统计
addWorksheet(wb, "Total_Cell_Statistics")
writeData(wb, "Total_Cell_Statistics", 
          cell_change_stats %>% select(Tank, Treatment, Treatment_Group, 
                                       Day0_value, Day8_value, Absolute_change, Percent_change),
          startRow = 1, startCol = 1)
setColWidths(wb, "Total_Cell_Statistics", cols = 1:7, widths = "auto")

# 2. 处理组汇总统计
addWorksheet(wb, "Treatment_Group_Summary")
writeData(wb, "Treatment_Group_Summary", treatment_summary,
          startRow = 1, startCol = 1)
setColWidths(wb, "Treatment_Group_Summary", cols = 1:6, widths = "auto")

# 3. 各时间点详细数据
addWorksheet(wb, "Daily_Concentration")
daily_data <- total_cell_stats %>%
  mutate(
    Concentration = sprintf("%.1f ± %.1f", total_cells_mean, total_cells_sd)
  ) %>%
  select(Tank, Treatment, Day, Concentration) %>%
  pivot_wider(
    names_from = Day,
    values_from = Concentration
  )
writeData(wb, "Daily_Concentration", daily_data,
          startRow = 1, startCol = 1)
setColWidths(wb, "Daily_Concentration", cols = 1:11, widths = "auto")

# 4. 门类比例详细统计（每个tank）
addWorksheet(wb, "Phylum_Proportion_Detailed")
writeData(wb, "Phylum_Proportion_Detailed", phylum_proportion_stats,
          startRow = 1, startCol = 1)
setColWidths(wb, "Phylum_Proportion_Detailed", cols = 1:14, widths = "auto")

# 5. 处理组门类比例汇总
addWorksheet(wb, "Phylum_Treatment_Summary")
writeData(wb, "Phylum_Treatment_Summary", phylum_treatment_summary,
          startRow = 1, startCol = 1)
setColWidths(wb, "Phylum_Treatment_Summary", cols = 1:10, widths = "auto")

# 6. Day 8门类主导地位分析
addWorksheet(wb, "Phylum_Dominance_Day8")
writeData(wb, "Phylum_Dominance_Day8", phylum_dominance_day8,
          startRow = 1, startCol = 1)
setColWidths(wb, "Phylum_Dominance_Day8", cols = 1:5, widths = "auto")

# 7. 门类多样性变化统计
addWorksheet(wb, "Diversity_Statistics")
writeData(wb, "Diversity_Statistics", phylum_diversity_stats,
          startRow = 1, startCol = 1)
setColWidths(wb, "Diversity_Statistics", cols = 1:16, widths = "auto")

# 8. 原始门类数据（用于验证）
addWorksheet(wb, "Phylum_Raw_Data")
writeData(wb, "Phylum_Raw_Data", phylum_data,
          startRow = 1, startCol = 1)
setColWidths(wb, "Phylum_Raw_Data", cols = 1:ncol(phylum_data), widths = "auto")

# 保存Excel文件
excel_file <- file.path(output_folder, "Total_Cell_Concentration_Statistics.xlsx")
saveWorkbook(wb, excel_file, overwrite = TRUE)

## ======================= 打印关键统计信息 =======================
cat("\n=== 总细胞浓度变化统计 ===\n\n")

cat("1. 各处理组Day 0到Day 8的变化：\n")
for(i in 1:nrow(treatment_summary)) {
  cat(sprintf("  %s (n=%d): Day0=%s → Day8=%s, 绝对变化=%s, 相对变化=%s\n",
              treatment_summary$Treatment_Group[i],
              treatment_summary$n_tanks[i],
              treatment_summary$Day0[i],
              treatment_summary$Day8[i],
              treatment_summary$Absolute_change[i],
              treatment_summary$Percent_change[i]))
}

cat("\n2. Day 8各处理组总细胞浓度排序（从高到低）：\n")
day8_order <- treatment_summary %>%
  mutate(Day8_value = as.numeric(str_extract(Day8, "[0-9.]+"))) %>%
  arrange(desc(Day8_value))
for(i in 1:nrow(day8_order)) {
  cat(sprintf("  %d. %s: %s\n", i, day8_order$Treatment_Group[i], day8_order$Day8[i]))
}

cat("\n=== 门类比例变化统计 ===\n\n")

cat("3. 各处理组Day 8门类组成（按比例排序）：\n")
for(treatment in unique(phylum_dominance_day8$Treatment_Group)) {
  cat(sprintf("\n  %s处理组：\n", treatment))
  dominance_data <- phylum_dominance_day8 %>%
    filter(Treatment_Group == treatment) %>%
    arrange(desc(avg_Day8_prop))
  
  for(j in 1:nrow(dominance_data)) {
    cat(sprintf("    %d. %s: %.1f%% (%s)\n", 
                j, 
                dominance_data$Latin_phylum[j],
                dominance_data$avg_Day8_prop[j],
                dominance_data$dominance_level[j]))
  }
}

cat("\n4. 门类多样性变化：\n")
diversity_summary <- phylum_diversity_stats %>%
  group_by(Treatment_Group) %>%
  summarise(
    avg_phyla_change = mean(phyla_change, na.rm = TRUE),
    avg_shannon_change = mean(shannon_change, na.rm = TRUE),
    avg_simpson_change = mean(simpson_change, na.rm = TRUE),
    avg_species_change = mean(species_change, na.rm = TRUE),
    .groups = "drop"
  )

for(i in 1:nrow(diversity_summary)) {
  cat(sprintf("  %s: 门类数变化=%+.1f, Shannon指数变化=%+.2f, Simpson指数变化=%+.3f, 物种数变化=%+.1f\n",
              diversity_summary$Treatment_Group[i],
              diversity_summary$avg_phyla_change[i],
              diversity_summary$avg_shannon_change[i],
              diversity_summary$avg_simpson_change[i],
              diversity_summary$avg_species_change[i]))
}

## ======================= 颜色 =======================
phylum_colors <- c(
  "Cyanophyta"        = "#3498db",
  "Cryptophyta"        = "#E31A1C",
  "Chrysophyta"        = "#FAFA33",
  "XanThophyta"        = "#FDBF6F",
  "Pyrrophyta"         = "#800080",
  "Euglenophyta"       = "grey",
  "Bacillariophyta"  = "#9c640c",
  "Chlorophyta"        = "#33A02C",
  "Dinophyta"          = "pink"
)

## ======================= 子图顺序和处理组标签 =======================
tank_order <- c(57, 63, 54, 59, 62, 67, 68, 70, 64, 65)

# 添加处理组标签到数据
phylum_data <- phylum_data %>%
  mutate(
    Tank = factor(Tank, levels = tank_order),
    Treatment_Label = treatment_labels[as.character(Tank)]
  )

## ======================= 作图 =======================

max_cells <- max(phylum_data$total_cells)

# ===== 1. 构建子图编号数据 =====
panel_labels <- data.frame(
  Tank = factor(tank_order, levels = tank_order),
  panel_tag = paste0("(", LETTERS[1:10], ")")
)

# 计算每个分面左上角的位置
panel_pos <- phylum_data %>%
  group_by(Tank) %>%
  summarise(
    x = min(Day),
    y = max(total_cells),
    .groups = "drop"
  ) %>%
  left_join(panel_labels, by = "Tank")

# ===== 2. 主图 =====
p <- ggplot(phylum_data) +
  geom_col(
    aes(
      x = factor(Day),
      y = proportion * max_cells,
      fill = Latin_phylum
    ),
    width = 0.6,
    alpha = 0.8
  ) +
  geom_line(
    aes(
      x = factor(Day),
      y = total_cells,
      group = 1
    ),
    colour = "black",
    linewidth = 1
  ) +
  
  ## ===== 子图编号：固定左上角（不进图内）=====
geom_text(
  data = panel_labels,
  aes(
    x = -Inf,
    y = Inf,
    label = panel_tag
  ),
  inherit.aes = FALSE,
  hjust = 0,   # 向右一点
  vjust = -0.4,    # 向下一点
  size  = 5
) +
  
  scale_y_continuous(
    name = expression(
      "Total cell concentration (" * 10^4 * " cells L"^{-1} * ")"
    ),
    labels = label_comma(),
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(
      ~ .x / max_cells,
      name = "Phylum proportion (%)",
      labels = function(x) paste0(round(x * 100, 0))
    )
  ) +
  scale_x_discrete("Time (d)") +
  scale_fill_manual(values = phylum_colors) +
  
  facet_wrap(
    ~Tank,
    ncol = 2,
    nrow = 5,
    labeller = as_labeller(function(x) treatment_labels[x])
  ) +
  
  ## ⚠️ 关键：允许文字画在面板边缘
  coord_cartesian(clip = "off") +
  
  theme_prism(base_size = 14, base_family = "Arial") +
  theme(
    text             = element_text(face = "plain"),
    strip.text       = element_text(size = 14),
    strip.background = element_blank(),
    axis.text        = element_text(),
    axis.title       = element_text(),
    legend.text      = element_text(),
    legend.position  = "bottom",
    panel.border     = element_rect(
      colour = "black",
      fill   = NA,
      linewidth = 0.5
    ),
    panel.spacing.y  = unit(1.3, "lines"),
    plot.margin      = margin(10, 10, 10, 10)
  ) +
  guides(
    fill = guide_legend(
      nrow = 1,
      byrow = TRUE
    )
  ) +
  labs(fill = NULL)


## ======================= 输出图形 =======================

## ---------- PDF ----------
pdf_file <- file.path(output_folder, "Fig4_phylum_cells_8.pdf")
ggsave(
  pdf_file,
  plot  = p,
  width = 10,
  height = 12,
  device = cairo_pdf
)

## ---------- PDF → JPG ----------
img <- image_read_pdf(pdf_file, density = 400)
image_write(
  img,
  path = file.path(output_folder, "Fig4_phylum_cells_8.jpg"),
  format = "jpg"
)


## ======================= 验证混合方法 =======================
cat("\n=== 混合方法验证 ===\n")
cat("藻门比例计算：先按门求平均值\n")
cat("总细胞浓度计算：直接对所有物种浓度求和\n")

# 显示示例数据验证
example_tank_day <- phylum_data %>%
  filter(Tank == 57, Day == 0) %>%
  select(Tank, Day, Latin_phylum, cells_mean, total_cells, proportion_percent) %>%
  head()

cat("\n示例（Tank 57, Day 0）：\n")
print(example_tank_day)

cat("\n=== 处理完成 ===\n")
cat(sprintf("1. 统计结果已保存到: %s\n", excel_file))
cat(sprintf("2. 图形已保存到: %s\n", pdf_file))
cat(sprintf("3. 图形(JPG)已保存到: %s\n", file.path(output_folder, "Fig4_phylum_cells_8.jpg")))
cat("\n注意：使用混合计算方法：\n")
cat("  - 藻门比例 = 门内物种平均浓度 / 总细胞浓度\n")
cat("  - 总细胞浓度 = 所有物种浓度直接求和\n")
cat("\nExcel文件中新增的工作表：\n")
cat("  - Phylum_Proportion_Detailed: 每个tank的门类比例详细统计\n")
cat("  - Phylum_Treatment_Summary: 处理组门类比例汇总\n")
cat("  - Phylum_Dominance_Day8: Day 8门类主导地位分析\n")
cat("  - Diversity_Statistics: 门类多样性变化统计\n")
cat("  - Phylum_Raw_Data: 原始门类数据\n")