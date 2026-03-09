# =====================================================
# 🌿 TP & TDP dynamics (Surface / Bottom) – Scheme A
# =====================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(purrr)
library(cowplot)
library(magick)

# =====================================================
# 1. 读取数据
# =====================================================

setwd("D:/地湖所工作/2025 热浪小论文XMGS02/Fig3environtal_var_suri")
data <- read_excel("all_data_suri.xlsx")

# =====================================================
# 2. Treatment 缩写、颜色、线型（保持原规则）
# =====================================================

treat_labels <- c(
  "Control"                   = "C",
  "Nutrients"                 = "N",
  "Mixing"                    = "M",
  "Mixing+Nutrients"          = "M+N",
  "Heatwave+Nutrients"        = "H+N",
  "Heatwave+Mixing+Nutrients" = "H+M+N"
)

treat_colors <- c(
  "Control"                   = "#000000",
  "Nutrients"                 = "#70A5D9",
  "Mixing"                    = "#000000",
  "Mixing+Nutrients"          = "#70A5D9",
  "Heatwave+Nutrients"        = "#CC0000",
  "Heatwave+Mixing+Nutrients" = "#CC0000"
)

treat_linetypes <- c(
  "Control"                   = "solid",
  "Nutrients"                 = "solid",
  "Mixing"                    = "dashed",
  "Mixing+Nutrients"          = "dashed",
  "Heatwave+Nutrients"        = "solid",
  "Heatwave+Mixing+Nutrients" = "dashed"
)

# =====================================================
# 3. 固定 Treatment 顺序（⚠️确保图例 C 在 M 上）
# =====================================================

data$Treatment <- factor(
  data$Treatment,
  levels = c(
    "Control",
    "Nutrients",
    "Heatwave+Nutrients",
    "Mixing",
    "Mixing+Nutrients",
    "Heatwave+Mixing+Nutrients"
  )
)

# =====================================================
# 4. 按照新顺序：Surface TP → Bottom TP → Surface TDP → Bottom TDP
# =====================================================

vars <- c(
  "TP_surface",  # (A) Surface TP
  "TP_bottom",   # (B) Bottom TP
  "TDP_surface", # (C) Surface TDP
  "TDP_bottom"   # (D) Bottom TDP
)

data_long <- data %>%
  pivot_longer(
    cols = all_of(vars),
    names_to = "Parameter",
    values_to = "Value"
  ) %>%
  # 过滤底层只保留 2,4,8 天
  filter(
    (Parameter %in% c("TP_bottom", "TDP_bottom") & Day %in% c(2,4,8)) |
      (Parameter %in% c("TP_surface", "TDP_surface"))
  )

# 计算Y轴范围
tp_range <- data_long %>%
  filter(Parameter %in% c("TP_surface", "TP_bottom")) %>%
  summarise(
    ymin = min(Value, na.rm = TRUE),
    ymax = max(Value, na.rm = TRUE)
  ) %>%
  mutate(
    ymin = ymin * 0.95,
    ymax = ymax * 1.05
  )

tdp_range <- data_long %>%
  filter(Parameter %in% c("TDP_surface", "TDP_bottom")) %>%
  summarise(
    ymin = min(Value, na.rm = TRUE),
    ymax = max(Value, na.rm = TRUE)
  ) %>%
  mutate(
    ymin = ymin * 0.95,
    ymax = ymax * 1.05
  )

# =====================================================
# 5. Y 轴标签（保持原样）
# =====================================================

y_labels <- list(
  "TP_surface"  = expression("Surface " * TP * " (mg L"^{-1}*")"),
  "TP_bottom"   = expression("Bottom " * TP * " (mg L"^{-1}*")"),
  "TDP_surface" = expression("Surface " * TDP * " (mg L"^{-1}*")"),
  "TDP_bottom"  = expression("Bottom " * TDP * " (mg L"^{-1}*")")
)

# =====================================================
# 6. 单 panel 作图函数
# =====================================================

make_plot <- function(param) {
  
  # 判断是 TP 还是 TDP
  if (param %in% c("TP_surface", "TP_bottom")) {
    y_limits <- c(tp_range$ymin, tp_range$ymax)
  } else {
    y_limits <- c(tdp_range$ymin, tdp_range$ymax)
  }
  
  ggplot(
    filter(data_long, Parameter == param),
    aes(
      x = Day,
      y = Value,
      color = Treatment,
      linetype = Treatment,
      group = interaction(Tank, Treatment)
    )
  ) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(
      values = treat_colors,
      labels = treat_labels,
      breaks = levels(data$Treatment)
    ) +
    scale_linetype_manual(
      values = treat_linetypes,
      labels = treat_labels,
      breaks = levels(data$Treatment)
    ) +
    scale_y_continuous(limits = y_limits) +
    labs(x = NULL, y = y_labels[[param]]) +
    theme_bw(base_size = 16) +
    theme(
      legend.position   = "none",
      panel.grid.minor  = element_blank(),
      axis.title.y      = element_text(size = 16),
      axis.text         = element_text(size = 14),
      plot.margin       = margin(6, 6, 6, 6)
    )
}

# =====================================================
# 7. 生成 4 个 panel（按照新顺序）
# =====================================================

plots <- map(vars, make_plot)

combined_plot <- wrap_plots(
  plots,
  ncol = 2  # 两列布局，顺序为：A | B
) +         #               C | D
  plot_annotation(
    tag_levels = "A",  # 改为大写字母
    tag_prefix = "(",  # 前缀保持括号
    tag_suffix = ")",  # 后缀保持括号
    theme = theme(
      plot.tag = element_text(size = 17, face = "bold")
    )
  )

# =====================================================
# 8. 公共图例（C / M 为黑线并上下排列）
# =====================================================

legend_plot <- ggplot(
  data_long,
  aes(
    x = Day,
    y = Value,
    color = Treatment,
    linetype = Treatment
  )
) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(
    values = treat_colors,
    labels = treat_labels,
    breaks = levels(data$Treatment)
  ) +
  scale_linetype_manual(
    values = treat_linetypes,
    labels = treat_labels,
    breaks = levels(data$Treatment)
  ) +
  theme_void(base_size = 16) +
  theme(
    legend.position   = "bottom",
    legend.text       = element_text(size = 16),
    legend.key.width  = unit(2.2, "cm"),
    legend.title      = element_blank()
  ) +
  guides(
    color    = guide_legend(ncol = 3, byrow = TRUE),
    linetype = guide_legend(ncol = 3, byrow = TRUE)
  )

legend <- cowplot::get_legend(legend_plot)

# =====================================================
# 9. 拼接主图 + 图例
# =====================================================

final_plot <- plot_grid(
  combined_plot,
  legend,
  ncol = 1,
  rel_heights = c(1, 0.12)
)

# =====================================================
# 10. 输出（PDF + JPG）
# =====================================================

output_folder <- "TP_TDP_surface_bottom_260120"
if (!dir.exists(output_folder)) dir.create(output_folder)

pdf_file <- file.path(output_folder, "TP_TDP_surface_bottom_schemeA.pdf")
ggsave(
  pdf_file,
  final_plot,
  width = 12,
  height = 10,
  device = cairo_pdf
)

img <- image_read_pdf(pdf_file, density = 400)
image_write(
  img,
  path = file.path(output_folder, "TP_TDP_surface_bottom_schemeA.jpg"),
  format = "jpg"
)

cat("图形已生成，按照以下顺序排列：\n")
cat("(A) Surface TP\n")
cat("(B) Bottom TP\n")
cat("(C) Surface TDP\n")
cat("(D) Bottom TDP\n")