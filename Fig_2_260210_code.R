# ==========================================================
# 加载包
# ==========================================================
library(readxl)
library(openxlsx)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(stringr)
library(rLakeAnalyzer)
library(patchwork)
library(cowplot)
library(magick)   # ← 用于 PDF → JPG
# ==========================================================
# 设置工作目录
# ==========================================================
setwd("D:/地湖所工作/2025 热浪小论文XMGS02/Fig2_Temp_suri")

# ==========================================================
# 创建新目录保存图片
# ==========================================================
figure_dir <- "Figure_2_260210"
if (!dir.exists(figure_dir)) {
  dir.create(figure_dir)
  cat("已创建目录:", figure_dir, "\n")
}

# ==========================================================
# 读取原始数据
# ==========================================================
rawdata <- read_excel("2024.3~5实时水温.xlsx")

# ==========================================================
# 数据处理
# ==========================================================
tempdata <- rawdata %>%
  filter(as.Date(TimeStamp) >= as.Date("2024-04-11") &
           as.Date(TimeStamp) <= as.Date("2024-04-30")) %>%
  select(TimeStamp, TankADD, TempVal1:TempVal10) %>%
  rename(
    time = TimeStamp,
    tank = TankADD,
    temp_10cm = TempVal1,
    temp_20cm = TempVal2,
    temp_30cm = TempVal3,
    temp_40cm = TempVal4,
    temp_50cm = TempVal5,
    temp_60cm = TempVal6,
    temp_70cm = TempVal7,
    temp_80cm = TempVal8,
    temp_90cm = TempVal9,
    temp_100cm = TempVal10
  )

tempdata_clean <- tempdata %>%
  mutate(across(starts_with("temp_"), ~ ifelse(. <= 0, NA, .))) %>%
  mutate(time = with_tz(time, tzone = "Asia/Shanghai"))

# ==========================================================
# 每天最接近 10:00
# ==========================================================
temp_10am <- tempdata_clean %>%
  mutate(
    date = as.Date(time),
    diff_10am = abs(as.numeric(
      difftime(time, as.POSIXct(paste(date, "10:00:00")), units = "mins")
    ))
  ) %>%
  group_by(tank, date) %>%
  slice_min(diff_10am, n = 1, with_ties = FALSE) %>%
  ungroup()

# ==========================================================
# 实验设置
# ==========================================================
reference_date <- as.Date("2024-04-15")

selected_tanks <- c(70, 62, 65, 63)

selected_dates_temp <- as.Date(c(
  "2024-04-15", "2024-04-17", "2024-04-19",
  "2024-04-21", "2024-04-23"
))

selected_dates_stability <- as.Date(c(
  "2024-04-15", "2024-04-17", "2024-04-19",
  "2024-04-21", "2024-04-23",
  "2024-04-26", "2024-04-30"
))

# ==========================================================
# 数据筛选
# ==========================================================
filtered_data_temp <- temp_10am %>%
  filter(tank %in% selected_tanks,
         date %in% selected_dates_temp) %>%
  mutate(Day = as.integer(date - reference_date))

filtered_data_stability <- temp_10am %>%
  filter(tank %in% selected_tanks,
         date %in% selected_dates_stability) %>%
  mutate(Day = as.integer(date - reference_date))

# ==========================================================
# 施密特稳定性
# ==========================================================
# 构建 bathymetry 数据
radius <- 2.3 / 2
area <- pi * radius^2
bthD <- seq(0.1, 1.4, by = 0.1)
bthA <- rep(area, length(bthD))

# 提取温度列及深度信息（从 filtered_data_stability 中提取）
temp_cols <- grep("^temp_\\d+cm$", names(filtered_data_stability), value = TRUE)
depths <- as.numeric(gsub("temp_|cm", "", temp_cols)) / 100  # 转换为米

# 删除全是 NA 的列
temp_cols <- temp_cols[colSums(!is.na(filtered_data_stability[temp_cols])) > 0]
depths <- depths[match(temp_cols, 
                       grep("^temp_\\d+cm$", names(filtered_data_stability), value = TRUE))]

# 删除全是 NA 的行（保持原始数据不变，在计算时处理）
filtered_data_clean <- filtered_data_stability %>%
  filter(rowSums(!is.na(across(all_of(temp_cols)))) >= 3)  # 至少3个温度点

# 计算 Schmidt Stability
schmidt_results <- filtered_data_clean %>%
  rowwise() %>%
  mutate(
    stability = {
      wtr <- c_across(all_of(temp_cols))
      valid <- !is.na(wtr)
      if (sum(valid) >= 3) {
        tryCatch({
          schmidt.stability(
            wtr = wtr[valid],
            depths = depths[valid],
            bthA = bthA,
            bthD = bthD
          )
        }, error = function(e) NA)
      } else {
        NA
      }
    }
  ) %>%
  ungroup()
# ==========================================================
# 处理组信息
# ==========================================================
treatment_info <- data.frame(
  tank = c(70, 62, 65, 63),
  treatment_label = c("Mixing + Nutrients", "Heatwave + Nutrients", "Heatwave + Mixing + Nutrients", "Nutrients")
)

final_stability_data <- schmidt_results %>%
  left_join(treatment_info, by = "tank")

# ==========================================================
# 图例映射
# ==========================================================
treatment_colors <- c(
  "Nutrients" = "black",
  "Heatwave + Nutrients" = "#CC0000",
  "Heatwave + Mixing + Nutrients" = "#CC0000",
  "Mixing + Nutrients" = "black"
)

treatment_linetypes <- c(
  "Nutrients" = "solid",
  "Heatwave + Nutrients" = "solid",
  "Heatwave + Mixing + Nutrients" = "dashed",
  "Mixing + Nutrients" = "dashed"
)

treatment_labels <- c(
  "Nutrients" = "N",
  "Heatwave + Nutrients" = "H + N",
  "Heatwave + Mixing + Nutrients" = "H + M + N",
  "Mixing + Nutrients" = "M + N"
)

# ==========================================================
# 温度剖面
# ==========================================================
temp_long <- filtered_data_temp %>%
  pivot_longer(starts_with("temp_"),
               names_to = "depth",
               values_to = "temperature") %>%
  mutate(depth_cm = as.numeric(str_extract(depth, "\\d+"))) %>%
  left_join(treatment_info, by = "tank") %>%
  filter(!is.na(temperature))

# ==========================================================
# 打印 Day 8 的具体温度数据（用于 Fig.2a）
# ==========================================================
temp_day8_table <- temp_long %>%
  filter(Day == 8) %>%
  arrange(treatment_label, depth_cm) %>%
  select(
    treatment = treatment_label,
    depth_cm,
    temperature
  )

print(temp_day8_table)

# ==========================================================
# (a) Day 8 温度剖面
# ==========================================================
p_day8 <- ggplot(
  temp_long %>% filter(Day == 8),
  aes(temperature, depth_cm,
      color = treatment_label, linetype = treatment_label)
) +
  geom_path(size = 1.2) +
  geom_point(size = 2.5) +
  scale_y_reverse(breaks = seq(0, 100, 10)) +
  scale_x_continuous(limits = c(18, 30), breaks = seq(18, 30, 2)) +
  scale_color_manual(
    name = NULL,
    values = treatment_colors,
    labels = treatment_labels,
    guide  = guide_legend(
      keywidth = 2.5)
  ) +
  scale_linetype_manual(
    name = NULL,
    values = treatment_linetypes,
    labels = treatment_labels,
    guide  = guide_legend(
      keywidth  = 2.5,
      keyheight = 0.8)
  ) +
  labs(x = "Temperature (°C)", y = "Depth (cm)") +
  theme_minimal(base_size = 20) +
  theme(
    text = element_text(family = "Arial"),
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = "none"
  )

# ==========================================================
# 打印施密特稳定度（Fig.2b 实际用到的数据）
# ==========================================================
stability_table <- final_stability_data %>%
  arrange(treatment_label, Day) %>%
  select(
    tank,
    treatment = treatment_label,
    date,
    Day,
    stability
  )

print(stability_table)

# ==========================================================
# (b) 施密特稳定性 - 只显示0-8天
# ==========================================================
stability_plot <- ggplot(
  final_stability_data,
  aes(Day, stability,
      color = treatment_label, linetype = treatment_label)
) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_x_continuous(limits = c(0, 8), breaks = seq(0, 8, 2)) +  # ← 只显示0-8天
  scale_color_manual(values = treatment_colors,
                     labels = treatment_labels,
                     guide  = guide_legend(
                       keywidth = 2.5)) +
  scale_linetype_manual(values = treatment_linetypes,
                        labels = treatment_labels,
                        guide  = guide_legend(
                          keywidth  = 2.5,
                          keyheight = 0.8
                        )) +
  labs(x = "Day", y = "Schmidt Stability (J/m²)") +
  theme_minimal(base_size = 20) +
  theme(
    text = element_text(family = "Arial"),
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = "none",
    plot.tag = element_text(size = 20, face = "bold")
  )

# ==========================================================
# 共享图例 + 合并
# ==========================================================
shared_legend <- cowplot::get_legend(
  p_day8 +
    theme(
      legend.position = "bottom",
      legend.text  = element_text(size = 20),
      legend.title = element_blank(),     # ← 再保险一次
      legend.key.width  = unit(2., "cm"),
      legend.key.height = unit(0.8, "cm")
    )
)

combined_plot <- (p_day8 | stability_plot) / shared_legend +
  plot_layout(heights = c(10, 1))

# ==========================================================
# ① 先导出 PDF（母版，字体正确）到新目录
# ==========================================================
pdf_path <- file.path(figure_dir, "Fig2_Temperature_Stability.pdf")
ggsave(
  pdf_path,
  combined_plot,
  width  = 14,
  height = 7,
  device = cairo_pdf
)

# ==========================================================
# ② PDF → JPG（600 dpi）保存到新目录
# ==========================================================
img <- image_read_pdf(
  pdf_path,
  density = 600
)

jpg_path <- file.path(figure_dir, "Fig2_Temperature_Stability.jpg")
image_write(
  img,
  jpg_path,
  format = "jpg"
)

cat("=== Fig.2 已成功生成到目录:", figure_dir, "===\n")
cat("PDF文件:", pdf_path, "\n")
cat("JPG文件:", jpg_path, "\n")

# ==========================================================
# 导出处理后的温度数据为 Excel 到新目录
# ==========================================================

# 1️⃣ 整理最终要导出的温度数据
export_temp_data <- temp_long %>%
  arrange(treatment_label, Day, depth_cm) %>%
  rename(
    treatment = treatment_label
  ) %>%
  select(
    tank,
    treatment,
    date,
    Day,
    depth_cm,
    temperature
  )

# 2️⃣ 写出为 Excel 到新目录
excel_path <- file.path(figure_dir, "Processed_Temperature_Data.xlsx")
write.xlsx(
  export_temp_data,
  file = excel_path,
  overwrite = TRUE
)

cat("=== 温度处理数据已成功导出：", excel_path, "===\n")