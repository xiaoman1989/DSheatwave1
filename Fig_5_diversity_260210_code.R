# ============================================================
# 🌿 Diversity & Total Cell Concentration
# Binary comparison + significance brackets
# PDF → JPG workflow (journal-safe)
# 添加：生成所有p值统计表格
# ============================================================

rm(list = ls())

# ===================== Packages =====================
packages <- c(
  "readxl","dplyr","tidyr","purrr","vegan",
  "ggplot2","ggpubr","rstatix","stringr",
  "patchwork","openxlsx","magick"
)
missing <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(missing)) install.packages(missing)
lapply(packages, library, character.only = TRUE)

# ===================== Paths =====================
setwd("D:/地湖所工作/2025 热浪小论文XMGS02/Fig5_diversity")

raw <- read_excel("Cell_all.xlsx")

output_folder <- "diversity_260210"
if(!dir.exists(output_folder)) dir.create(output_folder)

# ===================== Data preprocessing =====================
selected_tanks <- c(57, 63, 54, 59, 68, 70, 62, 67, 64, 65)

data <- raw %>%
  rename(
    Latin_species = any_of(c("Latin_species","Latin species")),
    Cell_conc     = any_of(c("Cell_conc_10_4_cell_L","Cell_conc")),
    Treatment     = any_of("Treatment"),
    Tank          = any_of("Tank"),
    Date          = any_of("Date"),
    Number        = any_of("Number")
  ) %>%
  filter(Tank %in% selected_tanks) %>%
  mutate(
    Cell_conc = as.numeric(Cell_conc),
    Cell_conc = ifelse(is.na(Cell_conc), 0, Cell_conc),
    Date = as.Date(Date),
    Day  = as.numeric(Date - as.Date("2024-04-13")),
    Heating   = ifelse(str_detect(Treatment, regex("Heatwave", ignore_case = TRUE)),
                       "Heatwave","Non-heatwave"),
    Nutrients = ifelse(str_detect(Treatment, regex("Nutrients", ignore_case = TRUE)),
                       "Nutrients","Non-nutrients"),
    Mixing    = ifelse(str_detect(Treatment, regex("Mixing", ignore_case = TRUE)),
                       "Mixing","Non-mixing")
  ) %>%
  mutate(
    Heating   = factor(Heating,   levels = c("Non-heatwave","Heatwave")),
    Nutrients = factor(Nutrients, levels = c("Non-nutrients","Nutrients")),
    Mixing    = factor(Mixing,    levels = c("Non-mixing","Mixing"))
  )

# ===================== Diversity indices =====================
diversity_data <- data %>%
  group_by(Number, Tank, Treatment, Date, Day,
           Heating, Nutrients, Mixing) %>%
  summarise(
    Total_Cell_conc = sum(Cell_conc, na.rm = TRUE),
    species_table = list(
      tibble(Latin_species, Abundance = Cell_conc) %>%
        group_by(Latin_species) %>%
        summarise(Abundance = sum(Abundance), .groups = "drop") %>%
        filter(Abundance > 0)
    ),
    .groups = "drop"
  ) %>%
  mutate(
    Species_Richness = map_int(species_table, nrow),
    Shannon = map_dbl(species_table,
                      ~ ifelse(nrow(.x) > 0,
                               diversity(.x$Abundance,"shannon"), NA)),
    Simpson = map_dbl(species_table,
                      ~ ifelse(nrow(.x) > 0,
                               diversity(.x$Abundance,"simpson"), NA)),
    Pielou  = map2_dbl(Shannon, Species_Richness,
                       ~ ifelse(.y > 1, .x/log(.y), NA)),
    # 添加Margalef指数计算
    Margalef = map2_dbl(Species_Richness, Total_Cell_conc,
                        ~ ifelse(.y > 0, (.x - 1) / log(.y), NA))
  ) %>%
  select(-species_table)

write.xlsx(diversity_data,
           file.path(output_folder,"diversity_per_sample_10tanks.xlsx"))

# ===================== Y-axis label mapping =====================
y_label_map <- c(
  "Total_Cell_conc"  = expression("Total cell concentration ("*10^4*" cells "*L^-1*")"),
  "Species_Richness" = "Species Richness",
  "Shannon"          = "Shannon-Wiener diversity index",
  "Simpson"          = "Simpson's diversity index",
  "Pielou"           = "Pielou's evenness index",
  "Margalef"         = "Margalef's richness index"
)

# ===================== 新增：p值计算函数 =====================
calculate_pvalues <- function(df, response_vars, factors, y_label_map) {
  results <- list()
  
  for (resp in response_vars) {
    for (factor in factors) {
      # 准备数据
      tmp <- df %>% 
        select(all_of(c(resp, factor))) %>% 
        drop_na()
      
      # 正态性检验
      normal <- shapiro.test(tmp[[resp]])$p.value > 0.05
      
      # 分组名称
      group1 <- levels(df[[factor]])[1]
      group2 <- levels(df[[factor]])[2]
      
      # 计算描述性统计
      group_stats <- tmp %>%
        group_by(!!sym(factor)) %>%
        summarise(
          n = n(),
          mean = mean(!!sym(resp), na.rm = TRUE),
          sd = sd(!!sym(resp), na.rm = TRUE),
          se = sd / sqrt(n),
          .groups = "drop"
        )
      
      # 提取组1的统计量
      stats_group1 <- group_stats %>% 
        filter(!!sym(factor) == group1)
      stats_group2 <- group_stats %>% 
        filter(!!sym(factor) == group2)
      
      # 进行统计检验
      if (normal) {
        test_result <- t.test(as.formula(paste(resp, "~", factor)), data = tmp)
        test_type <- "t-test"
        statistic <- test_result$statistic
        df_val <- test_result$parameter
      } else {
        test_result <- wilcox.test(as.formula(paste(resp, "~", factor)), data = tmp)
        test_type <- "Wilcoxon"
        statistic <- test_result$statistic
        df_val <- NA
      }
      
      # 处理y轴标签（转换为字符）
      response_label <- y_label_map[resp]
      if (is.expression(response_label)) {
        response_label <- as.character(response_label)
      }
      
      # 构建结果行
      result_row <- data.frame(
        Response_Variable = resp,
        Response_Label = response_label,  # 修改这里：使用字符而非expression
        Factor = factor,
        Group1 = group1,
        Group2 = group2,
        Test_Type = test_type,
        Statistic = round(statistic, 4),
        DF = ifelse(is.na(df_val), NA, round(df_val, 2)),
        p_value = test_result$p.value,
        Significance = case_when(
          test_result$p.value < 0.001 ~ "***",
          test_result$p.value < 0.01  ~ "**",
          test_result$p.value < 0.05  ~ "*",
          test_result$p.value < 0.1   ~ ".",
          TRUE ~ "ns"
        ),
        Group1_n = stats_group1$n,
        Group1_mean = round(stats_group1$mean, 4),
        Group1_sd = round(stats_group1$sd, 4),
        Group1_se = round(stats_group1$se, 4),
        Group2_n = stats_group2$n,
        Group2_mean = round(stats_group2$mean, 4),
        Group2_sd = round(stats_group2$sd, 4),
        Group2_se = round(stats_group2$se, 4),
        Normal_Distribution = normal,
        stringsAsFactors = FALSE
      )
      
      results[[paste(resp, factor, sep = "_")]] <- result_row
    }
  }
  
  # 合并所有结果
  pvalue_table <- do.call(rbind, results)
  rownames(pvalue_table) <- NULL
  
  return(pvalue_table)
}

# ===================== 新增：所有组合的p值统计表格 =====================
response_vars <- c(
  "Total_Cell_conc",
  "Species_Richness",
  "Shannon",
  "Simpson",
  "Pielou",
  "Margalef"
)

factors <- c("Heating", "Nutrients", "Mixing")

# 计算所有p值（传入y_label_map）
pvalue_results <- calculate_pvalues(diversity_data, response_vars, factors, y_label_map)

# 创建带有格式的工作簿
wb <- createWorkbook()

# 1. 主要p值结果表格
addWorksheet(wb, "Pvalue_Statistics")
writeData(wb, "Pvalue_Statistics", pvalue_results, startRow = 1, startCol = 1)

# 设置列宽
setColWidths(wb, "Pvalue_Statistics", cols = 1:ncol(pvalue_results), widths = "auto")

# 2. 汇总表格（按显著性分组）
summary_table <- pvalue_results %>%
  group_by(Response_Variable, Factor) %>%
  summarise(
    Significance = first(Significance),
    p_value = first(p_value),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Factor,
    values_from = c(Significance, p_value),
    names_sep = "_"
  ) %>%
  select(
    Response_Variable,
    Heating_Significance = Significance_Heating,
    Heating_p_value = p_value_Heating,
    Nutrients_Significance = Significance_Nutrients,
    Nutrients_p_value = p_value_Nutrients,
    Mixing_Significance = Significance_Mixing,
    Mixing_p_value = p_value_Mixing
  )

addWorksheet(wb, "Summary_by_Variable")
writeData(wb, "Summary_by_Variable", summary_table, startRow = 1, startCol = 1)
setColWidths(wb, "Summary_by_Variable", cols = 1:ncol(summary_table), widths = "auto")

# 3. 按因子汇总
factor_summary <- pvalue_results %>%
  group_by(Factor) %>%
  summarise(
    Total_Comparisons = n(),
    Significant_001 = sum(p_value < 0.001),
    Significant_01 = sum(p_value < 0.01),
    Significant_05 = sum(p_value < 0.05),
    Non_Significant = sum(p_value >= 0.05),
    Mean_p_value = mean(p_value, na.rm = TRUE),
    .groups = "drop"
  )

addWorksheet(wb, "Summary_by_Factor")
writeData(wb, "Summary_by_Factor", factor_summary, startRow = 1, startCol = 1)
setColWidths(wb, "Summary_by_Factor", cols = 1:ncol(factor_summary), widths = "auto")

# 保存Excel文件
excel_file <- file.path(output_folder, "statistical_analysis_results.xlsx")
saveWorkbook(wb, excel_file, overwrite = TRUE)

# ===================== Plot function (保持不变) =====================
plot_with_signif <- function(df, response, factor, y_label, show_y = TRUE){
  
  tmp <- df %>% select(all_of(c(response,factor))) %>% drop_na()
  
  normal <- shapiro.test(tmp[[response]])$p.value > 0.05
  test <- if(normal){
    t.test(as.formula(paste(response,"~",factor)), data = tmp)
  } else {
    wilcox.test(as.formula(paste(response,"~",factor)), data = tmp)
  }
  
  stat.test <- tibble(
    group1 = levels(df[[factor]])[1],
    group2 = levels(df[[factor]])[2],
    y.position = max(df[[response]], na.rm = TRUE) * 1.05,
    label = case_when(
      test$p.value < 0.001 ~ "***",
      test$p.value < 0.01  ~ "**",
      test$p.value < 0.05  ~ "*",
      test$p.value < 0.1   ~ ".",
      TRUE ~ "ns"
    )
  )
  
  p <- ggplot(df, aes_string(x = factor, y = response, fill = factor)) +
    geom_boxplot(width = 0.6, outlier.shape = NA, alpha = 0.7) +
    geom_jitter(width = 0.15, size = 1.5, alpha = 0.7) +
    stat_pvalue_manual(
      stat.test,
      xmin = "group1", xmax = "group2",
      label = "label",
      tip.length = 0.01,
      size = 8
    ) +
    scale_fill_manual(values = c("#CC0000","#70A5D9")) +
    theme_minimal(base_size = 20) +
    labs(x = NULL, y = if(show_y) y_label else NULL) +
    theme(
      legend.position = "none",
      text = element_text(family = "Arial"),
      axis.title.y = if(show_y) element_text(size = 22) else element_blank(),
      axis.text.y  = if(show_y) element_text(size = 22) else element_blank(),
      axis.text.x  = element_text(size = 22),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA)
    )
  
  return(p)
}

# ===================== Plot & export =====================
for(resp in response_vars){
  
  ylab <- y_label_map[resp]
  
  p <- plot_with_signif(diversity_data, resp, "Heating",   ylab, TRUE) +
    plot_with_signif(diversity_data, resp, "Nutrients", ylab, FALSE) +
    plot_with_signif(diversity_data, resp, "Mixing",    ylab, FALSE)
  
  pdf_file <- file.path(output_folder,
                        paste0(resp,"_3factors_10tanks.pdf"))
  jpg_file <- file.path(output_folder,
                        paste0(resp,"_3factors_10tanks.jpg"))
  
  # ---- PDF ----
  ggsave(pdf_file, p, width = 15, height = 6, device = cairo_pdf)
  
  # ---- PDF → JPG ----
  img <- image_read_pdf(pdf_file, density = 300)
  image_write(img, jpg_file, format = "jpg")
}

# ===================== Margalef指数统计摘要 =====================
margalef_summary <- diversity_data %>%
  group_by(Heating, Nutrients, Mixing) %>%
  summarise(
    n = n(),
    mean = mean(Margalef, na.rm = TRUE),
    sd = sd(Margalef, na.rm = TRUE),
    se = sd / sqrt(n),
    min = min(Margalef, na.rm = TRUE),
    max = max(Margalef, na.rm = TRUE),
    .groups = "drop"
  )

write.xlsx(margalef_summary,
           file.path(output_folder, "margalef_summary.xlsx"))

# ===================== Done =====================
cat("✅ 分析完成\n",
    "输出文件夹：", output_folder, "\n",
    "PDF 与 JPG 均已生成\n",
    "统计结果已保存至：", excel_file, "\n",
    "包含以下工作表：\n",
    "1. Pvalue_Statistics - 详细的p值统计结果\n",
    "2. Summary_by_Variable - 按响应变量汇总\n",
    "3. Summary_by_Factor - 按因子汇总\n")