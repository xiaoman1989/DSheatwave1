# ==========================================================
# 🌿 最优模型筛选与可视化 (X-Y变量) + Predicted R² + 生态学优先
# ==========================================================

# 加载包 ---------------------------------------------------
library(readxl)
library(leaps)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(MuMIn)
library(tidyr)
library(broom)
library(scales)
library(ggrepel)

# ==========================================================
# 设置路径与输出文件夹
# ==========================================================
setwd("D:/地湖所工作/2025 热浪小论文XMGS02/Fig7_Multivariant_analysis/R2withbottom")
out_dir <- "Figure_7_250210"
if(!dir.exists(out_dir)) dir.create(out_dir)

# ==========================================================
# 数据与变量定义
# ==========================================================
X_vars <- c(
  "NH4_bottom", "PO4_bottom",
  "TDN_bottom", "TDP_bottom",
  "NH4_surface", "PO4_surface",
  "TDN_surface", "TDP_surface", 
  "NTU_surface", "Temp_surface", "DO_surface", "stability"
)

# 定义生态学关键因子（优先选择的变量）
ecological_key_vars <- c("Temp_surface", "stability")

Y_vars <- c("total_biomass", "total_cells", "Shannon", "Pielou", "Simpson", "Margalef", "Species_Number", "Fv_Fm",
            "Chlorophyta_bio", "Chlorophyta_cells", "Chlorophyta_Ratio",
            "Cyanophyceae_bio", "Cyanophyceae_cells", "Cyanophyceae_Ratio",
            "filamentous_ratio","filamentous_cells", "bloom_ratio", "bloom_cells", "Pseudanabaena_sp_cells",
            "Pseudanabaena_sp_Ratio", "Closterium_sp_bio", "Closterium_sp_cells", "Closterium_sp_Ratio",
            "Chlorella_sp_bio", "Chlorella_sp_cells", "Chlorella_sp_Ratio", 
            "Scenedesmus_sp_Ratio", "Kirchneriella_sp_bio", "Kirchneriella_sp_Ratio","Cryptomonas_sp_bio"
)

# 创建响应变量标签映射（用于图表展示）
response_labels <- c(
  "total_biomass" = "Total biomass",
  "total_cells" = "Total cell concentration",
  "Shannon" = "Shannon",
  "Pielou" = "Pielou",
  "Simpson" = "Simpson",
  "Margalef" = "Margalef",
  "Species_Number" = "Species Number",
  "Fv_Fm" = "Fv/Fm",
  "Chlorophyta_bio" = "Chlorophyta biomass",
  "Chlorophyta_cells" = "Chlorophyta cells",
  "Chlorophyta_Ratio" = "Chlorophyta Ratio",
  "Cyanophyceae_bio" = "Cyanophycea biomass",
  "Cyanophyceae_cells" = "Cyanophyceae cells",
  "Cyanophyceae_Ratio" = "Cyanophyceae Ratio",
  "filamentous_ratio" = "filamentous ratio",
  "filamentous_cells" = "filamentous cells",
  "bloom_ratio" = "bloom_ratio",
  "bloom_cells" = "bloom_cells",
  "Pseudanabaena_sp_cells" = "Pseudanabaena_sp_cells",
  "Pseudanabaena_sp_Ratio" = "Pseudanabaena_sp_Ratio",
  "Closterium_sp_bio" = "Closterium_sp_bio",
  "Closterium_sp_cells" = "Closterium_sp_cells",
  "Closterium_sp_Ratio" = "Closterium_sp_Ratio",
  "Chlorella_sp_bio" = "Chlorella_sp_bio",
  "Chlorella_sp_cells" = "Chlorella_sp_cells",
  "Chlorella_sp_Ratio" = "Chlorella_sp_Ratio",
  "Scenedesmus_sp_Ratio" = "Scenedesmus_sp_Ratio",
  "Kirchneriella_sp_bio" = "Kirchneriella_sp_bio",
  "Kirchneriella_sp_Ratio" = "Kirchneriella_sp_Ratio",
  "Cryptomonas_sp_bio" = "Cryptomonas_sp_bio"
)

data <- read_excel("data_all.xlsx")
data <- na.omit(data[, c(Y_vars, X_vars)])

# ==========================================================
# 🌿 Predicted R² 计算函数定义
# ==========================================================
pred_r2 <- function(model) {
  y <- model$model[[1]]
  h <- lm.influence(model)$hat
  press <- sum((residuals(model) / (1 - h))^2)  # PRESS
  tss <- sum((y - mean(y))^2)
  pred_r2_value <- 1 - press / tss
  return(pred_r2_value)
}

# ==========================================================
# 🌿 生态学关键因子评分函数
# ==========================================================
ecological_score <- function(variables, key_vars = ecological_key_vars) {
  # 计算模型包含的关键生态因子数量
  score <- sum(key_vars %in% variables)
  return(score)
}

# ==========================================================
# 🌿 完整最优模型筛选流程（统计+生态学）
# ==========================================================

results <- list()
filtering_details <- list()  # 记录筛选过程的详细信息
model_comparison_details <- list()  # 记录模型比较详情

for (y in Y_vars) {
  cat("正在处理响应变量:", y, "\n")
  
  formula <- as.formula(paste(y, "~", paste(X_vars, collapse = "+")))
  
  regfit <- regsubsets(formula, data = data, nbest = 1, nvmax = length(X_vars), method = "exhaustive")
  summary_fit <- summary(regfit)
  
  aicc_values <- rep(NA, length(summary_fit$adjr2))
  pred_r2_values <- rep(NA, length(summary_fit$adjr2))
  model_objects <- list()  # 存储所有模型对象
  
  # ---- 计算所有模型的 AICc 和 PredR2 ----
  for (i in 1:length(summary_fit$adjr2)) {
    vars_i <- names(coef(regfit, i))[-1]
    if (length(vars_i) == 0) {
      formula_i <- as.formula(paste(y, "~ 1"))
    } else {
      formula_i <- as.formula(paste(y, "~", paste(vars_i, collapse = "+")))
    }
    
    lm_i <- lm(formula_i, data = data, na.action = na.omit)
    model_objects[[i]] <- lm_i
    aicc_values[i] <- AICc(lm_i)
    pred_r2_values[i] <- pred_r2(lm_i)
  }
  
  delta_aicc <- aicc_values - min(aicc_values, na.rm = TRUE)
  overfit_diff <- summary_fit$adjr2 - pred_r2_values
  
  # ---- 记录所有模型信息 ----
  all_models_info <- data.frame(
    Response = y,
    Model_Size = 1:length(summary_fit$adjr2),
    AdjR2 = round(summary_fit$adjr2, 3),
    PredR2 = round(pred_r2_values, 3),
    Delta_AICc = round(delta_aicc, 2),
    Overfit_Diff = round(overfit_diff, 3)
  )
  
  # ---- 严格筛选流程 ----
  candidate_idx <- 1:length(summary_fit$adjr2)
  
  # 筛选步骤1: ΔAICc < 2
  step1_idx <- candidate_idx[delta_aicc[candidate_idx] < 2 & !is.na(delta_aicc[candidate_idx])]
  if (length(step1_idx) == 0) {
    filtering_details[[y]] <- all_models_info %>%
      mutate(Step1_ΔAICc = "Fail", Step2_PredR2 = "Fail", Step3_Overfit = "Fail", 
             Final_Selection = "No", Selection_Reason = "No model with ΔAICc < 2")
    next
  }
  
  # 筛选步骤2: PredR2 > 0
  step2_idx <- step1_idx[pred_r2_values[step1_idx] > 0 & !is.na(pred_r2_values[step1_idx])]
  if (length(step2_idx) == 0) {
    filtering_details[[y]] <- all_models_info %>%
      mutate(Step1_ΔAICc = ifelse(Model_Size %in% step1_idx, "Pass", "Fail"),
             Step2_PredR2 = "Fail", Step3_Overfit = "Fail", 
             Final_Selection = "No", Selection_Reason = "No model with PredR² > 0")
    next
  }
  
  # 筛选步骤3: 过拟合 ≤ 0.2
  step3_idx <- step2_idx[overfit_diff[step2_idx] <= 0.2 & !is.na(overfit_diff[step2_idx])]
  if (length(step3_idx) == 0) {
    filtering_details[[y]] <- all_models_info %>%
      mutate(Step1_ΔAICc = ifelse(Model_Size %in% step1_idx, "Pass", "Fail"),
             Step2_PredR2 = ifelse(Model_Size %in% step2_idx, "Pass", "Fail"),
             Step3_Overfit = "Fail", Final_Selection = "No",
             Selection_Reason = "No model with overfitting ≤ 0.2")
    next
  }
  
  # ---- ANOVA 模型比较与生态学优先选择 ----
  candidate_models <- step3_idx
  best_model_idx <- NULL
  selection_reason <- ""
  
  if (length(candidate_models) == 1) {
    # 只有一个候选模型
    best_model_idx <- candidate_models[1]
    selection_reason <- "Only one candidate model"
  } else {
    # 多个候选模型，进行ANOVA比较
    anova_results <- list()
    significant_diff <- FALSE
    
    for (i in 1:(length(candidate_models)-1)) {
      for (j in (i+1):length(candidate_models)) {
        model1 <- model_objects[[candidate_models[i]]]
        model2 <- model_objects[[candidate_models[j]]]
        
        # 确保模型是嵌套关系才能进行ANOVA
        vars1 <- names(coef(model1))[-1]
        vars2 <- names(coef(model2))[-1]
        
        # 检查是否是嵌套模型
        is_nested <- all(vars1 %in% vars2) || all(vars2 %in% vars1)
        
        if (is_nested && length(vars1) > 0 && length(vars2) > 0) {
          anova_test <- tryCatch({
            anova(model1, model2)
          }, error = function(e) {
            return(NULL)
          })
          
          if (!is.null(anova_test)) {
            p_value <- anova_test$`Pr(>F)`[2]
            if (!is.na(p_value) && p_value < 0.05) {
              significant_diff <- TRUE
            }
            anova_results[[paste(i, j, sep="_")]] <- data.frame(
              Model1 = candidate_models[i],
              Model2 = candidate_models[j],
              P_value = p_value
            )
          }
        }
      }
    }
    
    if (length(anova_results) > 0) {
      anova_df <- do.call(rbind, anova_results)
      
      if (significant_diff) {
        # 有显著差异，选择AdjR²最大的模型
        best_model_idx <- candidate_models[which.max(summary_fit$adjr2[candidate_models])]
        selection_reason <- "ANOVA showed significant differences, selected highest AdjR²"
      } else {
        # 无显著差异，应用生态学优先原则
        ecological_scores <- sapply(candidate_models, function(idx) {
          vars <- names(coef(regfit, idx))[-1]
          if (is.null(vars)) return(0)
          return(ecological_score(vars))
        })
        
        # 选择生态学得分最高的模型，如果得分相同则选AdjR²最高的
        max_score <- max(ecological_scores)
        best_candidates <- candidate_models[ecological_scores == max_score]
        if (length(best_candidates) == 1) {
          best_model_idx <- best_candidates
        } else {
          best_model_idx <- best_candidates[which.max(summary_fit$adjr2[best_candidates])]
        }
        selection_reason <- paste("ANOVA showed no significant differences, selected model with highest ecological score (", max_score, " key variables)")
      }
    } else {
      # 无法进行ANOVA比较，使用生态学优先原则
      ecological_scores <- sapply(candidate_models, function(idx) {
        vars <- names(coef(regfit, idx))[-1]
        if (is.null(vars)) return(0)
        return(ecological_score(vars))
      })
      
      max_score <- max(ecological_scores)
      best_candidates <- candidate_models[ecological_scores == max_score]
      if (length(best_candidates) == 1) {
        best_model_idx <- best_candidates
      } else {
        best_model_idx <- best_candidates[which.max(summary_fit$adjr2[best_candidates])]
      }
      selection_reason <- paste("Models not comparable by ANOVA, selected model with highest ecological score (", max_score, " key variables)")
    }
  }
  
  # 记录模型比较详情
  if (length(candidate_models) > 1) {
    comparison_info <- data.frame(
      Response = y,
      Candidate_Models = paste(candidate_models, collapse = ", "),
      ANOVA_Result = ifelse(exists("significant_diff"), 
                            ifelse(significant_diff, "Significant differences", "No significant differences"),
                            "Not comparable"),
      Ecological_Selection = ifelse(exists("ecological_scores"), 
                                    paste("Max score:", max(ecological_scores)),
                                    "Not applied"),
      Final_Selected_Model = best_model_idx,
      Selection_Reason = selection_reason
    )
    model_comparison_details[[y]] <- comparison_info
  }
  
  # 记录筛选详情
  filtering_details[[y]] <- all_models_info %>%
    mutate(
      Step1_ΔAICc = ifelse(Model_Size %in% step1_idx, "Pass", "Fail"),
      Step2_PredR2 = ifelse(Model_Size %in% step2_idx, "Pass", "Fail"),
      Step3_Overfit = ifelse(Model_Size %in% step3_idx, "Pass", "Fail"),
      Final_Selection = ifelse(Model_Size == best_model_idx, "Yes", "No"),
      Selection_Reason = ifelse(Model_Size == best_model_idx, selection_reason, "")
    )
  
  # ---- 记录最终模型信息 ----
  best_vars <- names(coef(regfit, best_model_idx))[-1]
  if (is.null(best_vars)) best_vars <- character(0)
  ecological_score_final <- ecological_score(best_vars)
  
  results[[y]] <- data.frame(
    Response = y,
    Best_R2 = round(summary_fit$rsq[best_model_idx], 3),
    Best_AdjR2 = round(summary_fit$adjr2[best_model_idx], 3),
    Best_PredR2 = round(pred_r2_values[best_model_idx], 3),
    Best_AICc = round(aicc_values[best_model_idx], 2),
    Delta_AICc = round(delta_aicc[best_model_idx], 2),
    Overfit = round(overfit_diff[best_model_idx], 3),
    Ecological_Score = ecological_score_final,
    Best_Variables = ifelse(length(best_vars) > 0, paste(best_vars, collapse = ", "), "None"),
    Model_Size = length(best_vars),
    Selection_Reason = selection_reason
  )
}

# ==========================================================
# 🌿 输出结果
# ==========================================================

# 保存筛选过程详情
if (length(filtering_details) > 0) {
  filtering_details_df <- do.call(rbind, filtering_details)
  write.xlsx(filtering_details_df, 
             paste0(out_dir, "/Model_Filtering_Process_Details.xlsx"),
             overwrite = TRUE)
}

# 保存模型比较详情
if (length(model_comparison_details) > 0) {
  model_comparison_df <- do.call(rbind, model_comparison_details)
  write.xlsx(model_comparison_df,
             paste0(out_dir, "/Model_Comparison_Details.xlsx"),
             overwrite = TRUE)
}

# 保存最终筛选结果（只包含通过所有条件的模型）
if (length(results) > 0) {
  model_summary <- do.call(rbind, results)
  write.xlsx(model_summary,
             paste0(out_dir, "/Best_Model_Summary_Ecological_Priority.xlsx"),
             overwrite = TRUE)
  
  # 筛选统计
  cat("\n=== 完整筛选统计 ===\n")
  cat("总响应变量数量:", length(Y_vars), "\n")
  cat("通过所有筛选条件的模型数量:", nrow(model_summary), "\n")
  cat("筛选比例:", round(nrow(model_summary) / length(Y_vars) * 100, 1), "%\n")
  
  # 生态学关键因子统计
  cat("\n=== 生态学关键因子统计 ===\n")
  all_vars <- unlist(strsplit(model_summary$Best_Variables, ", "))
  if (length(all_vars) > 0) {
    key_var_counts <- table(all_vars)
    key_var_counts <- key_var_counts[names(key_var_counts) %in% ecological_key_vars]
    if (length(key_var_counts) > 0) {
      key_var_counts <- sort(key_var_counts, decreasing = TRUE)
      print(key_var_counts)
    } else {
      cat("没有生态学关键因子被选中\n")
    }
  } else {
    cat("没有变量被选中\n")
  }
  
  # ==========================================================
  # 🌿 仅绘制 AdjR² > 0.3 的响应变量
  # ==========================================================
  selected_models <- model_summary %>% filter(Best_AdjR2 > 0.3)
  
  if (nrow(selected_models) > 0) {
    cat("找到", nrow(selected_models), "个 AdjR² > 0.3 的模型用于绘图\n")
    
    # 创建绘图文件夹
    plot_dir <- paste0(out_dir, "/Plots")
    if(!dir.exists(plot_dir)) dir.create(plot_dir)
    
    # 绘制 R² 黑白棋图
    for (y in selected_models$Response) {
      formula <- as.formula(paste(y, "~", paste(X_vars, collapse = "+")))
      regfit <- regsubsets(formula, data = data, nbest = 1, nvmax = length(X_vars), method = "exhaustive")
      summary_fit <- summary(regfit)
      
      png(paste0(plot_dir, "/Regsubsets_R2_", y, ".png"), width = 1200, height = 900, res = 150)
      par(mar = c(10, 6, 6, 2))
      plot(regfit,
           scale = "adjr2",
           main = " ",
           ylab = expression("Adjusted R"^2),
           nvmax = length(X_vars)
      )
      dev.off()
    }
    
    # ==========================================================
    # 🌿 标准化回归系数计算
    # ==========================================================
    data_scaled <- data %>% mutate(across(all_of(c(X_vars, Y_vars)), scale))
    beta_results <- list()
    
    for (y in selected_models$Response) {
      best_vars_str <- selected_models$Best_Variables[selected_models$Response == y]
      if (best_vars_str == "None" || is.na(best_vars_str) || best_vars_str == "") {
        cat("跳过响应变量", y, "- 没有自变量\n")
        next
      }
      
      best_vars <- unlist(strsplit(best_vars_str, ", "))
      best_vars <- trimws(best_vars)
      
      # 检查是否有自变量
      if (length(best_vars) == 0 || (length(best_vars) == 1 && best_vars == "None")) {
        cat("跳过响应变量", y, "- 没有自变量\n")
        next
      }
      
      formula <- as.formula(paste(y, "~", paste(best_vars, collapse = "+")))
      model <- lm(formula, data = data_scaled, na.action = na.omit)
      
      # 安全提取系数
      coef_summary <- summary(model)$coefficients
      if (nrow(coef_summary) > 1) {  # 确保有自变量系数
        coefs <- coef_summary[-1, 1]  # 移除截距项
        names(coefs) <- rownames(coef_summary)[-1]
        
        beta_results[[y]] <- data.frame(Response = y, Variable = names(coefs), Beta = coefs)
      } else {
        cat("警告: 响应变量", y, "的模型没有有效的自变量系数\n")
      }
    }
    
    # 只有当有结果时才继续
    if (length(beta_results) > 0) {
      beta_df <- do.call(rbind, beta_results)
      
      # 创建宽格式数据
      beta_wide <- beta_df %>%
        tidyr::pivot_wider(names_from = Variable, values_from = Beta, values_fill = 0)
      write.xlsx(beta_wide, paste0(out_dir, "/Beta_Coefficients_Selected.xlsx"), overwrite = TRUE)
      
      # ==========================================================
      # 🌿 标准化系数总体热图（使用响应变量标签）
      # ==========================================================
      # 确保所有变量都在因子水平中
      all_vars_in_models <- unique(beta_df$Variable)
      beta_long <- beta_df %>%
        mutate(
          Variable = factor(Variable, levels = all_vars_in_models),
          Response = factor(Response, levels = selected_models$Response),
          # 添加响应变量标签列
          Response_Label = factor(response_labels[as.character(Response)], 
                                  levels = response_labels[selected_models$Response])
        )
      
      p <- ggplot(beta_long, aes(x = Variable, y = Response_Label, fill = Beta)) +
        geom_tile(color = "grey80") +
        scale_fill_gradient2(
          low = "#4575b4", mid = "white", high = "#d73027",
          midpoint = 0, name = "β value"
        ) +
        theme_minimal(base_size = 13) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title = element_blank(),
              panel.grid = element_blank()) +
        ggtitle("Standardized Regression Coefficients (β) Heatmap\nAdjR² > 0.3 & Ecologically Prioritized Models")
      
      ggsave(paste0(out_dir, "/Beta_Heatmap_Selected.png"), p, width = 12, height = 8, dpi = 300)
      
      # ==========================================================
      # 🌿 各响应变量 β 条形图（使用响应变量标签）
      # ==========================================================
      beta_summary_list <- list()
      bar_dir <- paste0(out_dir, "/Beta_Barplots")
      if(!dir.exists(bar_dir)) dir.create(bar_dir)
      
      for (y in selected_models$Response) {
        best_vars_str <- selected_models$Best_Variables[selected_models$Response == y]
        if (best_vars_str == "None" || is.na(best_vars_str) || best_vars_str == "") next
        
        best_vars <- unlist(strsplit(best_vars_str, ", "))
        best_vars <- trimws(best_vars)
        if (length(best_vars) == 0) next
        
        formula <- as.formula(paste(y, "~", paste(best_vars, collapse = "+")))
        model <- lm(formula, data = data_scaled, na.action = na.omit)
        
        # 使用broom::tidy安全提取系数
        coef_df <- tryCatch({
          broom::tidy(model) %>%
            filter(term != "(Intercept)")  # 移除截距项
        }, error = function(e) {
          return(data.frame())
        })
        
        # 检查是否有系数
        if (nrow(coef_df) == 0) {
          cat("跳过响应变量", y, "- 没有有效的系数\n")
          next
        }
        
        model_summary_y <- summary(model)
        
        coef_df <- coef_df %>%
          mutate(
            Response = y,
            # 添加响应变量标签
            Response_Label = response_labels[y],
            Adjusted_R2 = round(model_summary_y$adj.r.squared, 3),
            Significance = case_when(
              p.value < 0.001 ~ "***",
              p.value < 0.01 ~ "**",
              p.value < 0.05 ~ "*",
              TRUE ~ ""
            )
          ) %>%
          rename(Variable = term, Beta = estimate, P_value = p.value) %>%
          select(Response, Response_Label, Variable, Beta, P_value, Significance, Adjusted_R2)
        
        beta_summary_list[[y]] <- coef_df
        
        # 创建条形图（使用响应变量标签作为标题）
        p <- ggplot(coef_df, aes(x = Beta, y = reorder(Variable, Beta), fill = Beta)) +
          geom_col(width = 0.6) +
          geom_text(aes(label = Significance),
                    hjust = ifelse(coef_df$Beta > 0, -0.2, 1.2),
                    vjust = 0.5,
                    color = "black",
                    size = 5,
                    fontface = "bold") +
          scale_x_continuous(
            limits = c(min(coef_df$Beta) - 0.2, max(coef_df$Beta) + 0.2),
            expand = expansion(mult = 0.1)
          ) +
          scale_fill_gradient2(low = "#4575b4", mid = "white", high = "#d73027", midpoint = 0) +
          labs(
            title = paste0(coef_df$Response_Label[1], " | Adjusted R² = ", coef_df$Adjusted_R2[1]),
            x = "Estimate (Standardized)",
            y = "Predictors"
          ) +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                legend.position = "right")
        
        ggsave(paste0(bar_dir, "/Beta_", y, ".jpg"), p, width = 7, height = 5, dpi = 300)
      }
      
      # ==========================================================
      # 🌿 汇总标准化结果表
      # ==========================================================
      if (length(beta_summary_list) > 0) {
        beta_summary_df <- do.call(rbind, beta_summary_list) %>%
          left_join(selected_models[, c("Response", "Best_AdjR2", "Ecological_Score", "Selection_Reason")], by = "Response") %>%
          arrange(desc(Best_AdjR2))
        
        write.xlsx(beta_summary_df, paste0(out_dir, "/Beta_Summary_Detail_Selected.xlsx"), overwrite = TRUE)
        
        # ==========================================================
        # 🌿 生态学关键因子重要性统计
        # ==========================================================
        ecological_importance <- beta_summary_df %>%
          filter(Variable %in% ecological_key_vars) %>%
          group_by(Variable) %>%
          summarise(
            Frequency = n(),
            Mean_Beta = mean(Beta, na.rm = TRUE),
            Positive_Effects = sum(Beta > 0),
            Negative_Effects = sum(Beta < 0),
            Significant_Effects = sum(P_value < 0.05)
          ) %>%
          arrange(desc(Frequency))
        
        write.xlsx(ecological_importance, paste0(out_dir, "/Ecological_Key_Variables_Importance.xlsx"), overwrite = TRUE)
      }
    } else {
      cat("没有可用的标准化系数结果用于绘图\n")
    }
    
  } else {
    cat("没有找到 AdjR² > 0.3 的模型用于绘图\n")
  }
  
} else {
  cat("没有模型通过所有筛选条件\n")
}

cat("\n=== 分析完成 ===\n")
cat("输出文件夹:", out_dir, "\n")
cat("结果文件已保存\n")