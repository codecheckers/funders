#############################################################################
#                                                                           #
#           Open Science checklist for Grant proposals study analysis       #
#                      https://github.com/ayudewi/funders                   #
#                                                                           #
#############################################################################

# Important! To reproduce this code, set your working directory first!

# setwd("...") # to set working directory

# Two datasets for this analysis:
# (1) db_master_grants_fullset.xlsx
# (2) checklist.xlsx

# Libraries
library(readxl)
library(pROC)
library(dplyr)
library(ggplot2)
library(tools)
library(openxlsx)
library(tidyr)
library(gtable)
library(binom)
library(gt)
library(lattice)
library(gtsummary)
library(webshot2)
library(stringr)

# Data preparation --------------------------------------------------------

# Loading dataset 1
df <- read_excel("db_master_grants_fullset.xlsx", sheet = "master")
View(df)

# Cleaning dataset 1
names(df) <- gsub(" ", "_", names(df))
df$reproducibility_binary <- ifelse(df$reproducibility == "yes", 1, 0)
df$field <- str_to_title(df$field)

df <- df |>
        mutate(
                nm = tolower(`funder_name`),
                funder_clean = case_when(
                        str_detect(nm, "\\beuropean union\\b") |
                                str_detect(nm, "\\beu\\b") |
                                str_detect(nm, "eu\\s*horizon") |
                                str_detect(nm, "horizon\\s*(europe|2020)") ~ "European Union",
                        TRUE ~ `funder_name`
                )
        ) |>
        select(-nm)

df <- df %>%
        mutate(field = case_when(
                field %in% c("Physic", "Physics") ~ "Physics",
                TRUE ~ field
        ))

View(df)

df <- df |>
        rename(actual_open_data = open_data,
               actual_open_code = open_code,
               actual_reproducibility = reproducibility)


# Proposals characteristics -----------------------------------------------

# Bar graph year
year_summary <- df |>
        count(year, sort = FALSE) |>
        arrange(year)

year_bar <- ggplot(year_summary, aes(x = factor(year), y = n)) +
        geom_bar(stat = "identity", fill = "goldenrod") +
        geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
        labs(title = "Number of Grant Proposals by Year",
             x = "Year",
             y = "Number of grant proposals") +
        theme_minimal() +
        theme(
                plot.title = element_text(hjust = 0.5),
                panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.4)
        )

ggsave("year_summary.jpeg", 
       year_bar, width = 9, height = 6, dpi = 300)

# tg = table grants

tg_country <- df |>
        tbl_summary(
                include = country,
                label = list(country ~ "Funding Source")
        )

tg_funder <- df |>
        tbl_summary(
                include = funder_clean,
                label = list(funder_clean ~ "Funder Name")
        )

tg_field <- df |>
        tbl_summary(
                include = field,
                label = list(field ~ "Research Field (non-standardised category)")
        )

tg_country
tg_funder
tg_field

# saved as png for each table grant

gt_country <- as_gt(tg_country)
gtsave(gt_country, "tg_country.png")

gt_funder <- as_gt(tg_funder)
gtsave(gt_funder, "tg_funder.png")

gt_field <- as_gt(tg_field)
gtsave(gt_field, "tg_field.png")


# Open Science scores -----------------------------------------------------

# Loading dataset 2
df2 <- read_excel("checklist.xlsx", sheet = "Sheet1")
View(df2)

# Cleaning dataset 2
names(df2) <- gsub(" ", "_", names(df2))
df2$os_reproducibility_binary <- ifelse(df2$repro_checklist_group == "yes", 1, 0)
names(df2)[names(df2) == "no"] <- "study_id"

# Summarise checklist
checklist_cols <- names(df2)[5:18]

stats_tbl <- df2 |>
        pivot_longer(
                cols = all_of(checklist_cols),
                names_to = "Checklist Item",
                values_to = "Score"
        ) |>
        group_by(`Checklist Item`) |>
        summarise(
                Total = sum(Score, na.rm = TRUE),
                .groups = "drop"
        ) |>
        
        mutate(`Checklist Item` = factor(`Checklist Item`, levels = checklist_cols)) |>
        arrange(`Checklist Item`) |>
        mutate(`Checklist Item` = str_to_title(str_replace_all(`Checklist Item`, "_", " ")))

names(stats_tbl) <- str_to_title(names(stats_tbl))

stats_tbl <- stats_tbl |>
        mutate(`Checklist Item` = str_to_sentence(`Checklist Item`))

stats_tbl

# Open science scores summary n cheklist and control group
score_summary <- df |>
        select(os_checklist_group, os_control_group) |>
        pivot_longer(
                cols = everything(),
                names_to = "Group",
                values_to = "Score"
        ) |>
        mutate(Group = ifelse(Group == "os_checklist_group", "Checklist", "Control")) |>
        group_by(Group) |>
        summarise(
                Minimum = min(Score, na.rm = TRUE),
                Median  = median(Score, na.rm = TRUE),
                Maximum = max(Score, na.rm = TRUE),
                .groups = "drop"
        )

score_summary


# Actual reproducibility --------------------------------------------------

# Reproducibility prevalence
n <- nrow(df)
k <- sum(df$reproducibility_binary == 1, na.rm = TRUE)

bt <- binom.test(k, n, conf.level = 0.95)

prevalence <- 100 * bt$estimate
ci <- 100 * bt$conf.int

cat(sprintf(
        "Prevalence: %.1f%% (95%% CI %.1f–%.1f%%)\n",
        prevalence, ci[1], ci[2]
))

# List of proposals that led to reproducible publications
repro_tbl <- df |>
        filter(tolower(actual_reproducibility) == "yes") |>
        select(year, funder_clean, field) |>
        arrange(year, funder_clean, field)

names(repro_tbl) <- str_to_title(names(repro_tbl))
names(repro_tbl)[names(repro_tbl) == "Funder_clean"] <- "Funder"

repro_tbl


# Tables 2x2 and accuracy metrics -----------------------------------------

df_std <- df |>
        mutate(
                Actual = factor(
                        reproducibility_binary,
                        levels = c(1, 0),   
                        labels = c("Reproducible", "Not reproducible")
                ),
                Pred_checklist = factor(
                        repro_checklist_group,
                        levels = c("yes", "no"),     
                        labels = c("Reproducible", "Not reproducible")
                ),
                Pred_control = factor(
                        repro_control_group,
                        levels = c("yes", "no"),     
                        labels = c("Reproducible", "Not reproducible")
                )
        )


# Building 2x2 tables
tab_check  <- table(Predicted = df_std$Pred_checklist,   Actual = df_std$Actual)
tab_ctrl   <- table(Predicted = df_std$Pred_control, Actual = df_std$Actual)

tab_check
tab_ctrl

df_std2 <- df_std |>
        rename(Predicted = Pred_checklist)

tbl_check <- df_std2 |>
        tbl_cross(
                row = Predicted,
                col = Actual,
                percent = "none",
                statistic = "{n}"
        ) |>
        modify_caption("**2×2 Table: Checklist Prediction vs Actual**")

tbl_check


df_std2 <- df_std |>
        rename(Predicted = Pred_control)

tbl_ctrl <- df_std2 |>
        tbl_cross(
                row = Predicted,
                col = Actual,
                percent = "none",
                statistic = "{n}"
        ) |>
        modify_caption("**2×2 Table: Control Prediction vs Actual**") 

tbl_ctrl

gtsave(as_gt(tbl_check), "tbl_check.png")
gtsave(as_gt(tbl_ctrl), "tbl_ctrl.png")

# Checklist group
TP <- tab_check["Reproducible", "Reproducible"]
FP <- tab_check["Reproducible", "Not reproducible"]
FN <- tab_check["Not reproducible", "Reproducible"]
TN <- tab_check["Not reproducible", "Not reproducible"]


acc_ci <- function(x, n) {
        ci <- binom::binom.wilson(x, n)
        sprintf("%.3f (%.3f–%.3f)", ci$mean, ci$lower, ci$upper)
}

Sensitivity  <- acc_ci(TP, TP + FN)
Specificity  <- acc_ci(TN, TN + FP)
PPV          <- acc_ci(TP, TP + FP)
NPV          <- acc_ci(TN, TN + FN)
Accuracy     <- acc_ci(TP + TN, TP + TN + FP + FN)

# Control group
TP2 <- tab_ctrl["Reproducible", "Reproducible"]
FP2 <- tab_ctrl["Reproducible", "Not reproducible"]
FN2 <- tab_ctrl["Not reproducible", "Reproducible"]
TN2 <- tab_ctrl["Not reproducible", "Not reproducible"]


acc_ci2 <- function(x, n) {
        ci <- binom::binom.wilson(x, n)
        sprintf("%.3f (%.3f–%.3f)", ci$mean, ci$lower, ci$upper)
}

Sensitivity2  <- acc_ci2(TP2, TP2 + FN2)
Specificity2  <- acc_ci2(TN2, TN2 + FP2)
PPV2          <- acc_ci2(TP2, TP2 + FP2)
NPV2          <- acc_ci2(TN2, TN2 + FN2)
Accuracy2     <- acc_ci2(TP2 + TN2, TP2 + TN2 + FP2 + FN2)

acc_tbl <- tibble::tibble(
        Metric = c("Sensitivity", "Specificity", "PPV", "NPV", "Overall Accuracy"),
        Checklist = c(Sensitivity, Specificity, PPV, NPV, Accuracy),
        Control = c(Sensitivity2, Specificity2, PPV2, NPV2, Accuracy2)
)

acc_gt <- acc_tbl |>
        gt() |>
        tab_header(title = md("**Accuracy Metrics per Group - Estimate (95% CI)**")) |>
        cols_label(
                Metric = md("**Metric**"),
                Checklist = md("**Checklist**"),
                Control = md("**Control**")
        ) |>
        fmt_markdown(everything())

acc_gt

gtsave(acc_gt, "accuracy_table.png")


# Area Under Curves and ROC graph -----------------------------------------

df$reproducibility_binary <- as.numeric(df$reproducibility_binary)

# Hypotheses: the more score of openness, the more likely reproducible is 
# Direction would be ">"

roc_checklist <- roc(df$reproducibility_binary, df$os_checklist_group,
                     direction = ">", quiet = TRUE)
roc_control <- roc(df$reproducibility_binary, df$os_control_group,
                   direction = ">", quiet = TRUE)

# AUC + 95% CI
auc_check <- as.numeric(auc(roc_checklist))
ci_check  <- ci.auc(roc_checklist)
auc_ctrl  <- as.numeric(auc(roc_control))
ci_ctrl   <- ci.auc(roc_control)

g <- ggroc(list(Checklist = roc_checklist, Control = roc_control), size = 0.9) +
        scale_color_manual(values = c("Checklist" = "#377EB8", 
                                      "Control"   = "#E41A1C")) + 
        coord_equal() +
        scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
        scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) +
        labs(title = "ROC curves",
             x = "False Positive Rate (1 - Specificity)",
             y = "True Positive Rate (Sensitivity)", colour = "Group") +
        annotate("text", x = 0.60, y = 0.50,
                 label = sprintf("AUC = %.3f (95%% CI %.3f–%.3f)",
                                 auc_check, ci_check[1], ci_check[3]),
                 colour = "#377EB8", size = 3.5) +
        annotate("text", x = 0.40, y = 0.10,
                 label = sprintf("AUC = %.3f (95%% CI %.3f–%.3f)",
                                 auc_ctrl, ci_ctrl[1], ci_ctrl[3]),
                 colour = "#E41A1C", size = 3.5) +
        theme_gray(base_size = 14) +
        theme(panel.grid.major = element_line(size = 0.4, colour = "grey85"),
              panel.grid.minor = element_blank(),
              plot.title = element_text(face = "bold", hjust = 0.5))

ggsave("ROC_both.jpeg", plot = g, width = 6.2, height = 5, dpi = 300)


# This is end of analysis -------------------------------------------------

# This analysis code is free for reuse.
# Author: Ayu Putu Madri Dewi (https://ayudewi.github.io/Portfolio/)

