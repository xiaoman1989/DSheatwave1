## 0. 环境准备 ==============================================================
rm(list = ls())
pacman::p_load(tidyverse, readxl, lubridate, patchwork, stringr, writexl, grDevices)

## 1. 读数据 ================================================================
setwd("D:/地湖所工作/2025 热浪小论文XMGS02/Fig6_dominance")
raw0 <- read_excel(
  "Cell_all.xlsx",
  col_types = c("skip","text","skip","skip","numeric",
                "text","text","text","text")
)
colnames(raw0) <- c("Latin_species","Cell_conc","Date","Tank","Treatment","Latin_phylum")

## 2. 日期处理 ===============================================================
raw <- raw0 %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date >= "2024-04-13") %>%
  mutate(Day = as.numeric(Date - as.Date("2024-04-15")))

## 3. 清洗物种名、提取属名 ===================================================
raw <- raw %>%
  mutate(Latin_species = str_replace_all(Latin_species, "\\u00A0|\\u202F", " "),
         Genus = word(Latin_species, 1, sep = fixed(" ")))

## 4. 计算门、属、种 三级相对丰度 & 优势度 ================================
phylum_abundance <- raw %>%
  group_by(Day, Tank, Treatment) %>%
  mutate(N = sum(Cell_conc, na.rm = TRUE)) %>%
  group_by(Day, Tank, Treatment, Latin_phylum) %>%
  summarise(ni = sum(Cell_conc, na.rm = TRUE),
            N = first(N),
            .groups = "drop") %>%
  mutate(Density_Ratio = ni / N)

dominance_genus <- raw %>%
  group_by(Day, Tank, Treatment) %>%
  mutate(N = sum(Cell_conc, na.rm = TRUE)) %>%
  group_by(Day, Tank, Treatment, Genus) %>%
  summarise(ni = sum(Cell_conc, na.rm = TRUE),
            N = first(N),
            .groups = "drop") %>%
  mutate(fi = 1, Dominance = (ni / N) * fi)

dominance_species <- raw %>%
  group_by(Day, Tank, Treatment) %>%
  mutate(N = sum(Cell_conc, na.rm = TRUE)) %>%
  group_by(Day, Tank, Treatment, Latin_species) %>%
  summarise(ni = sum(Cell_conc, na.rm = TRUE),
            N = first(N),
            .groups = "drop") %>%
  mutate(fi = 1, Dominance = (ni / N) * fi)

## 5. Tank-Treatment 对应表 & 处理组缩写 =====================================
tank_treatment <- tibble(
  Tank = c(57, 63, 54, 59, 62, 67, 68, 70, 64, 65),
  Treatment = c("Control","Nutrients",
                "Mixing","Mixing",
                "Heatwave + Nutrients","Heatwave + Nutrients",
                "Mixing + Nutrients","Mixing + Nutrients",
                "Heatwave + Mixing + Nutrients",
                "Heatwave + Mixing + Nutrients")
)

# 定义处理组缩写对应关系
treatment_abbr <- tribble(
  ~Treatment, ~Abbreviation,
  "Control", "C",
  "Nutrients", "N",
  "Mixing", "M",
  "Heatwave + Nutrients", "H + N",
  "Mixing + Nutrients", "M + N",
  "Heatwave + Mixing + Nutrients", "H + M + N"
)

# 合并缩写到tank_treatment
tank_treatment <- tank_treatment %>%
  left_join(treatment_abbr, by = "Treatment")

tank_order <- tank_treatment$Tank

# 获取唯一的处理组顺序（去除重复）
unique_treatment_order <- unique(tank_treatment$Treatment)

## 6. 定义门类颜色映射 ======================================================
phylum_colors <- c(
  "Cyanophyceae"        = "#3498db",  # 蓝色
  "Chlorophyta"         = "#33A02C",  # 绿色
  "Cryptophyta"        = "#E31A1C",     # 红色
  "Bacillariophyceae"   = "#9c640c"   # 棕色
)

## 7. 新版终极绘图函数（处理组缩写 & 虚线分隔 & 双格式输出）==================
plot_heat <- function(df, y_var, title = "", threshold = 0.02, min_tank_count = 3, 
                      plot_label = ""){
  if(!"Dominance" %in% names(df)){
    stop("plot_heat(): 输入数据必须包含 'Dominance' 列。")
  }
  
  ## ---- 1. 筛物种 ----
  filter_sp <- df %>%
    group_by(!!sym(y_var)) %>%
    summarise(
      max_dom = max(Dominance, na.rm = TRUE),
      tank_n = n_distinct(Tank[Dominance > threshold], na.rm = TRUE)
    ) %>%
    filter(max_dom > threshold, tank_n >= min_tank_count) %>%
    pull(!!sym(y_var))
  
  df_filt <- df %>%
    filter(!!sym(y_var) %in% filter_sp)
  
  ## ---- 2. 保留 Day 0/2/4/6/8 ----
  plot_dat <- df_filt %>%
    filter(Day %in% c(0,2,4,6,8)) %>%
    filter(Dominance > threshold)
  
  plot_dat$Tank <- factor(as.character(plot_dat$Tank), levels = as.character(tank_order))
  
  plot_dat <- plot_dat %>%
    mutate(Tank_Day = paste0(Tank, "_", Day))
  
  tank_day_order <- expand_grid(Tank = tank_order, Day = c(0,2,4,6,8)) %>%
    mutate(Tank_Day = paste0(Tank, "_", Day)) %>%
    pull(Tank_Day)
  
  plot_dat$Tank_Day <- factor(plot_dat$Tank_Day, levels = tank_day_order)
  
  ## ---- 3. Phylum ----
  plot_dat <- plot_dat %>%
    left_join(distinct(raw, !!sym(y_var), Latin_phylum), by = y_var)
  
  y_order <- plot_dat %>%
    arrange(Latin_phylum, !!sym(y_var)) %>%
    pull(!!sym(y_var)) %>%
    unique()
  
  plot_dat[[y_var]] <- factor(plot_dat[[y_var]], levels = y_order)
  
  ## ---- 4. Phylum 分割线 ----
  phylum_order <- plot_dat %>%
    distinct(!!sym(y_var), Latin_phylum) %>%
    arrange(!!sym(y_var)) %>%
    mutate(row_num = row_number())
  
  phylum_splits <- phylum_order %>%
    group_by(Latin_phylum) %>%
    summarise(min_row = min(row_num),
              max_row = max(row_num),
              .groups = "drop") %>%
    mutate(label_pos = (min_row + max_row) / 2)
  
  ## ---- 5. Tank / Treatment 标签 ----
  tank_day_map <- expand_grid(Tank = tank_order, Day = c(0,2,4,6,8)) %>%
    mutate(Tank_Day = paste0(Tank, "_", Day)) %>%
    left_join(tank_treatment, by = "Tank")
  
  levels_vec <- levels(plot_dat$Tank_Day)
  tank_day_map <- tank_day_map %>%
    mutate(pos = match(Tank_Day, levels_vec))
  
  ## ---- 6. 计算线条位置 ----
  # 每个Tank有5个时间点（0,2,4,6,8）
  # 不同处理组之间：实线
  # 同一处理组的不同Tank之间：虚线
  
  # 获取每个处理组的Tank信息
  treatment_groups <- tank_treatment %>%
    group_by(Treatment, Abbreviation) %>%
    summarise(
      tanks = list(Tank),
      tank_count = n(),
      .groups = "drop"
    )
  
  # 计算每个Tank的起始和结束位置
  tank_positions <- tibble(Tank = tank_order) %>%
    mutate(
      tank_index = match(Tank, tank_order),
      start_pos = (tank_index - 1) * 5 + 0.5,  # 每个Tank有5个时间点
      end_pos = tank_index * 5 + 0.5
    ) %>%
    left_join(tank_treatment, by = "Tank")
  
  # 计算虚线位置：同一处理组的不同Tank之间
  dashed_positions <- c()
  for(i in 1:nrow(treatment_groups)) {
    group_tanks <- treatment_groups$tanks[[i]]
    if(length(group_tanks) > 1) {
      # 对同一处理组的多个Tank，在它们之间加虚线
      for(j in 1:(length(group_tanks)-1)) {
        tank1 <- group_tanks[j]
        tank2 <- group_tanks[j+1]
        pos1 <- tank_positions$end_pos[tank_positions$Tank == tank1]
        dashed_positions <- c(dashed_positions, pos1)
      }
    }
  }
  
  # 计算实线位置：不同处理组之间
  solid_positions <- c()
  for(i in 1:(nrow(tank_positions)-1)) {
    current_treatment <- tank_positions$Treatment[i]
    next_treatment <- tank_positions$Treatment[i+1]
    if(current_treatment != next_treatment) {
      solid_positions <- c(solid_positions, tank_positions$end_pos[i])
    }
  }
  
  ## ---- 7. 计算每个Tank中心的标签位置 ----
  tank_labels <- tank_positions %>%
    mutate(
      label_pos = (start_pos + end_pos) / 2,  # Tank中心位置
      label_y = -1.5  # 标签在Y轴的位置
    )
  
  ## ---- 8. legend 最大值 ----
  max_dom <- max(plot_dat$Dominance, na.rm = TRUE)
  
  ## ---- 9. 计算Y轴标签颜色 ----
  # 获取每个门类的颜色
  phylum_color_map <- plot_dat %>%
    distinct(!!sym(y_var), Latin_phylum) %>%
    arrange(!!sym(y_var)) %>%
    mutate(
      color = case_when(
        Latin_phylum == "Cyanophyceae" ~ "#3498db",
        Latin_phylum == "Chlorophyta" ~ "#33A02C",
        Latin_phylum == "Cryptophyta" ~ "#E31A1C",
        Latin_phylum == "Bacillariophyceae" ~ "#9c640c",
        TRUE ~ "black"  # 默认颜色
      )
    )
  
  # 创建Y轴标签颜色向量（与标签顺序一致）
  y_labels <- levels(plot_dat[[y_var]])
  y_colors <- phylum_color_map$color[match(y_labels, phylum_color_map[[y_var]])]
  
  ## ---- 10. 绘图 ----
  max_x <- length(unique(plot_dat$Tank_Day))
  y_max <- length(levels(plot_dat[[y_var]]))
  
  # 定义更细腻的颜色梯度
  color_breaks <- seq(threshold, max_dom, length.out = 6)
  color_values <- scales::rescale(color_breaks)
  
  # 计算图例高度（基于Y轴物种数量）
  legend_height <- y_max * 0.5  # 每个物种大约0.6单位高度
  
  # 创建颜色渐变函数（这里添加）
  color_palette <- colorRampPalette(c("#8FBC8F", "#DC143C", "#414986"))
  
  p <- ggplot(plot_dat, aes(x = Tank_Day, y = !!sym(y_var), fill = Dominance)) +
    geom_tile(color = "white", linewidth = 0.3) +
    
    # 修改后的配色
    scale_fill_gradientn(
      colours = color_palette(10),  # 使用10个颜色的渐变
      # values = scales::rescale(seq(0, 1, length.out = 10)),  # 可选：明确指定位置
      limits = c(threshold, max_dom),
      breaks = color_breaks,
      labels = scales::number_format(accuracy = 0.01),
      name = "Y",
      guide = guide_colorbar(
        barwidth = unit(1.2, "cm"),
        barheight = unit(legend_height, "cm"),
        title.position = "top",
        title.hjust = 0.25,
        title.vjust = 0.5,
        title.theme = element_text(
          size = 16,
          #face = "bold",
          margin = margin(b = 8)
        ),
        label.theme = element_text(
          size = 16,
          face = "bold",
          angle = 0,
          hjust = 0.5,
          vjust = 0.5,
          margin = margin(l = 2, r = 2)
        ),
        ticks.colour = "black",
        ticks.linewidth = 1,
        frame.colour = "black",
        frame.linewidth = 1,
        direction = "vertical"
      )
    ) +
    # ... 其他图形代码保持不变 ...
    scale_x_discrete(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0)) +
    # Phylum 分割线（实线，加粗）
    geom_hline(data = phylum_splits[-nrow(phylum_splits),], 
               aes(yintercept = max_row + 0.5), 
               color = "black", linewidth = 0.8) +
    # 不同处理组之间的竖线（实线，加粗）
    geom_vline(xintercept = solid_positions, 
               colour = "black", linewidth = 0.8) +
    # 相同处理组之间的竖线（虚线，加粗）
    geom_vline(xintercept = dashed_positions, 
               colour = "black", linewidth = 0.6, linetype = "dashed") +
    # 外框（加粗）
    annotate("rect", 
             xmin = 0.5, xmax = max_x + 0.5, 
             ymin = 0.5, ymax = y_max + 0.5, 
             colour = "black", fill = NA, linewidth = 1.2) +
    # Day 标签（加大字号）
    geom_text(
      data = tank_day_map,
      aes(x = pos, y = 0, label = Day),
      inherit.aes = FALSE, size = 6, #fontface = "bold"
    ) +
    # ★ 每个Tank的处理组缩写标签（在Tank中心位置）
    geom_text(
      data = tank_labels,
      aes(x = label_pos, y = -1.5, label = Abbreviation),
      inherit.aes = FALSE, size = 6, #fontface = "bold"
    ) +
    coord_fixed(ratio = 0.9, clip = "off", 
                xlim = c(0.5, max_x + 0.5), 
                ylim = c(0.5, y_max + 0.5)) +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.x = element_blank(),
      # Y轴标签：使用计算的颜色，加粗斜体
      axis.text.y = element_text(
        face = "bold.italic", 
        size = 16,
        colour = y_colors  # 应用颜色
      ),
      panel.grid = element_blank(),
      # 调整边距，为图例留出更多空间
      plot.margin = margin(1, 3, 5, 1, "cm"),  # 增加右侧边距
      
    )
  
  # 返回绘图数据和绘图对象
  return(list(plot = p, 
              plot_data = plot_dat,
              y_order = y_order,
              y_var = y_var))
}

## 8. 输出路径 ===============================================================
output_folder <- "dominance_260120"
if(!dir.exists(output_folder)) dir.create(output_folder)

## 9. 出图并保存（PDF和JPG双格式）=============================================
genus_result <- plot_heat(dominance_genus, "Genus", plot_label = "(a)")
species_result <- plot_heat(dominance_species, "Latin_species", plot_label = "(b)")

p_genus <- genus_result$plot
p_species <- species_result$plot

# 保存PDF格式
ggsave(file.path(output_folder, "heatmap_genus_fi1.pdf"), p_genus, 
       width = 18, height = 12)  # 增加宽度以容纳图例
ggsave(file.path(output_folder, "heatmap_species_fi1.pdf"), p_species, 
       width = 18, height = 12)

# 保存JPG格式（高分辨率）
ggsave(file.path(output_folder, "heatmap_genus_fi1.jpg"), p_genus, 
       width = 18, height = 12, dpi = 300)
ggsave(file.path(output_folder, "heatmap_species_fi1.jpg"), p_species, 
       width = 18, height = 12, dpi = 300)

## 10. 组合图（可选）==========================================================
# 如果需要将两个图组合在一起
combined_plot <- p_genus / p_species + 
  plot_layout(heights = c(1, 1))

ggsave(file.path(output_folder, "combined_heatmaps.pdf"), combined_plot, 
       width = 18, height = 22)  # 增加宽度
ggsave(file.path(output_folder, "combined_heatmaps.jpg"), combined_plot, 
       width = 18, height = 22, dpi = 300)

## 11. 生成与热图完全对应的优势度数值宽表格 ===================================
# 获取热图中的物种顺序
genus_order <- genus_result$y_order
species_order <- species_result$y_order

# 处理组和Tank顺序
tank_order <- tank_treatment$Tank

# Day顺序
day_order <- c(0, 2, 4, 6, 8)

# 检查数据
cat("\n=== 数据检查 ===\n")
cat("属水平物种数:", length(genus_order), "\n")
cat("种水平物种数:", length(species_order), "\n")
cat("Tank数量:", length(tank_order), "\n")
cat("Day数量:", length(day_order), "\n")

# 1. 生成属水平的宽表格（与热图完全对应）
genus_wide_table <- dominance_genus %>%
  # 筛选热图中出现的属
  filter(Genus %in% genus_order) %>%
  # 确保属的顺序与热图一致
  mutate(Genus = factor(Genus, levels = genus_order)) %>%
  # 筛选热图中的Day
  filter(Day %in% day_order) %>%
  # 筛选热图中的Tank
  filter(Tank %in% tank_order) %>%
  # 确保Tank顺序与热图一致
  mutate(Tank = factor(Tank, levels = tank_order)) %>%
  # 按照与热图相同的顺序排序
  arrange(Tank, Day) %>%
  # 转换为宽格式
  select(Tank, Treatment, Day, Genus, Dominance) %>%
  pivot_wider(
    names_from = c(Tank, Day),
    values_from = Dominance,
    names_sep = "_Day",
    values_fill = 0
  ) %>%
  # 按照热图中的属顺序排序
  arrange(Genus) %>%
  # 添加门类信息
  left_join(
    raw %>% distinct(Genus, Latin_phylum) %>% rename(Phylum = Latin_phylum),
    by = "Genus"
  ) %>%
  # 将Phylum列移到前面
  select(Phylum, Genus, everything())

# 2. 生成种水平的宽表格（与热图完全对应）
species_wide_table <- dominance_species %>%
  # 筛选热图中出现的种
  filter(Latin_species %in% species_order) %>%
  # 确保种的顺序与热图一致
  mutate(Latin_species = factor(Latin_species, levels = species_order)) %>%
  # 筛选热图中的Day
  filter(Day %in% day_order) %>%
  # 筛选热图中的Tank
  filter(Tank %in% tank_order) %>%
  # 确保Tank顺序与热图一致
  mutate(Tank = factor(Tank, levels = tank_order)) %>%
  # 按照与热图相同的顺序排序
  arrange(Tank, Day) %>%
  # 转换为宽格式
  select(Tank, Treatment, Day, Latin_species, Dominance) %>%
  pivot_wider(
    names_from = c(Tank, Day),
    values_from = Dominance,
    names_sep = "_Day",
    values_fill = 0
  ) %>%
  # 按照热图中的种顺序排序
  arrange(Latin_species) %>%
  # 添加门类和属信息
  left_join(
    raw %>% distinct(Latin_species, Latin_phylum, Genus) %>% 
      rename(Phylum = Latin_phylum),
    by = "Latin_species"
  ) %>%
  # 将Phylum和Genus列移到前面
  select(Phylum, Genus, Latin_species, everything())

# 3. 创建Tank-Day对应表，显示每个列对应的处理组
tank_day_info <- expand_grid(
  Tank = tank_order,
  Day = day_order
) %>%
  left_join(tank_treatment, by = "Tank") %>%
  mutate(
    Column_Name = paste0(Tank, "_Day", Day),
    Treatment_Abbr = Abbreviation
  ) %>%
  select(Column_Name, Tank, Day, Treatment, Treatment_Abbr)

# 4. 保存为Excel文件
dominance_tables <- list(
  # 属优势度表格
  Genus_Dominance = genus_wide_table,
  # 种优势度表格
  Species_Dominance = species_wide_table,
  # 处理组信息
  Treatment_Info = tank_treatment,
  # Tank-Day列对应信息
  Tank_Day_Columns = tank_day_info,
  # 属顺序说明
  Genus_Order_Info = data.frame(
    Order = 1:length(genus_order),
    Genus = genus_order
  ),
  # 种顺序说明
  Species_Order_Info = data.frame(
    Order = 1:length(species_order),
    Species = species_order
  )
)

# 保存Excel文件
write_xlsx(dominance_tables, file.path(output_folder, "dominance_wide_tables_fi1.xlsx"))

## 12. 同时保存CSV格式以便于其他软件使用 =====================================
# 属水平表格
write_csv(genus_wide_table, file.path(output_folder, "genus_dominance_wide_fi1.csv"))
# 种水平表格
write_csv(species_wide_table, file.path(output_folder, "species_dominance_wide_fi1.csv"))
# Tank-Day信息表格
write_csv(tank_day_info, file.path(output_folder, "tank_day_info_fi1.csv"))

## 13. 输出原代码中的宽表（用于回归分析）======================================
phylum_wide <- phylum_abundance %>%
  mutate(Latin_phylum = str_c(Latin_phylum, "_Ratio")) %>%
  pivot_wider(names_from = Latin_phylum, 
              values_from = Density_Ratio, 
              values_fill = 0)

# 使用热图中出现的属
genus_wide_original <- dominance_genus %>%
  filter(Genus %in% genus_order) %>%
  mutate(Genus = str_c(Genus, "_Ratio")) %>%
  pivot_wider(names_from = Genus, 
              values_from = Dominance, 
              values_fill = 0)

# 使用热图中出现的种
species_wide_original <- dominance_species %>%
  filter(Latin_species %in% species_order) %>%
  mutate(Latin_species = str_c(Latin_species, "_Ratio")) %>%
  pivot_wider(names_from = Latin_species, 
              values_from = Dominance, 
              values_fill = 0)

regress_tab <- list(phylum_wide, genus_wide_original, species_wide_original) %>%
  reduce(left_join, by = c("Day","Tank","Treatment")) %>%
  mutate(across(where(is.numeric), ~replace_na(.,0)))

write_xlsx(regress_tab, file.path(output_folder, "ratio_cells_for_model_fi1.xlsx"))

## 14. 打印关键信息 ===========================================================
cat("\n=== 关键信息汇总 ===\n")
cat("属水平热图包含物种数:", length(genus_order), "\n")
cat("种水平热图包含物种数:", length(species_order), "\n")
cat("属顺序:", paste(genus_order, collapse = ", "), "\n")
cat("种顺序:", paste(species_order, collapse = ", "), "\n")
cat("Tank顺序:", paste(tank_order, collapse = ", "), "\n")
cat("Day顺序:", paste(day_order, collapse = ", "), "\n")
cat("唯一处理组顺序:", paste(unique_treatment_order, collapse = ", "), "\n")
cat("\n=== 文件保存信息 ===\n")
cat("宽表格已保存至:", output_folder, "\n")
cat("生成的文件:\n")
cat("1. dominance_wide_tables_fi1.xlsx - Excel格式宽表格\n")
cat("2. genus_dominance_wide_fi1.csv - CSV格式属水平表格\n")
cat("3. species_dominance_wide_fi1.csv - CSV格式种水平表格\n")
cat("4. tank_day_info_fi1.csv - Tank-Day对应信息\n")
cat("5. ratio_cells_for_model_fi1.xlsx - 用于回归分析的表格\n")
cat("6. heatmap_genus_fi1.pdf/jpg - 属水平热图\n")
cat("7. heatmap_species_fi1.pdf/jpg - 种水平热图\n")