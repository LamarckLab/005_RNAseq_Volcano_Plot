# 加载必要的R包
library(ggplot2)
library(ggpubr)
library(ggrepel)

# 读取数据
file_path <- "C:/Users/Lamarck/Desktop/volcano_plot_data.csv"
data <- read.csv(file_path)

# 确保列名正确
colnames(data) <- c("gene_name", "logFC", "P.Value")

# 数据处理
data$group <- "not-significant"
data$group[which((data$logFC > 1) & (data$P.Value < 0.05))] <- "up-regulated"
data$group[which((data$logFC < -1) & (data$P.Value < 0.05))] <- "down-regulated"
data$log10_pvalue <- -log10(data$P.Value)

# 颜色定义
if (length(unique(data$group)) == 2) {
  palette <- c("#2f5688", "#CC0000")
} else {
  palette <- c("#2f5688", "gray", "#CC0000")
}

# 打开 PDF 文件输出
pdf("C:/Users/Lamarck/Desktop/Volcano_Plot.pdf", width = 6, height = 6)

# 绘图
p <- ggscatter(
  data, x = "logFC", y = "log10_pvalue",
  color = "group", palette = palette,
  size = 1.5, label = data$Label,
  font.label = 10, repel = TRUE,
  title = "Volcano Plot",
  xlab = "log2(Fold Change)", ylab = "-log10(P.Value)"
) +
  geom_vline(xintercept = c(-1, 1), linetype = "dashed") +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
  theme_classic() +  # 可替换为 theme_classic() 或 theme_minimal()
  theme(
    legend.title = element_blank(),
    legend.justification = c(0.1, 0.97),
    legend.text = element_text(size = 12),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 14),
    plot.background = element_blank()
  )

# 输出绘图到 PDF
print(p)

# 关闭 PDF 设备
dev.off()
