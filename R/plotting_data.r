library(ggplot2)


csv_data <- read.csv("data/simulation.csv")

# captions <- paste(paste("Betted games (mean):", round(mean(csv_data$NumBets), digits = 2)), "|", paste("Max profit:", max(csv_data$Profit)))

# nvar_x_prof <- ggplot(csv_data, aes(x = NVariables, y = Profit, color = TrainingDataSize)) +
#     geom_point() +
#     labs(x = "Number of Variables", y = "Profit") +
#     ggtitle(sprintf("Number of Variables vs. Profit (Betting threshold: %.1f)", 2.5)) +
#     scale_color_discrete(name = "Traning Data Size") +
#     labs(caption = captions)

# nvar_x_acc <- ggplot(csv_data, aes(x = NVariables, y = Accuracy, color = TrainingDataSize)) +
#     geom_point() +
#     labs(x = "Number of Variables", y = "Accuracy") +
#     ggtitle(sprintf("Number of Variables vs. Accuracy (Betting threshold: %.1f)", 2.5)) +
#     scale_color_discrete(name = "Traning Data Size") +
#     labs(caption = captions)

# prof_x_acc <- ggplot(csv_data, aes(x = Profit, y = Accuracy, color = TrainingDataSize)) +
#     geom_point() +
#     labs(x = "Profit", y = "Accuracy") +
#     ggtitle(sprintf("Profit vs. Accuracy (Betting threshold: %.1f)", 2.5)) +
#     scale_color_discrete(name = "Traning Data Size") +
#     labs(caption = captions)

different_models_thresh <- ggplot(csv_data, aes(x = avgProfit, y = avgHitRate, color = Threshold)) +
    geom_point() +
    labs(x = "Average Profit", y = "Average Hit Rate") +
    ggtitle("Average Profit vs. Average Hit Rate (separated by betting threshold)") +
    scale_color_discrete(name = "Betting threshold")

different_models_vars <- ggplot(csv_data, aes(x = avgProfit, y = avgHitRate, color = Variables)) +
    geom_point() +
    labs(x = "Average Profit", y = "Average Hit Rate") +
    ggtitle("Average Profit vs. Average Hit Rate (separated by training variables)") +
    scale_color_discrete(name = "Training variables")

different_models_data <- ggplot(csv_data, aes(x = avgProfit, y = avgHitRate, color = TrainingDataSize)) +
    geom_point() +
    labs(x = "Average Profit", y = "Average Hit Rate") +
    ggtitle("Average Profit vs. Average Hit Rate (separated by training dataset size)") +
    scale_color_discrete(name = "Training dataset size")

data_size_accuracy <- ggplot(csv_data, aes(x = TrainingDataSize, y = avgHitRate, color = TrainingDataSize)) +
    geom_point() +
    labs(x = "Training dataset size", y = "Accuracy") +
    ggtitle("Accuracy for different training dataset sizes") +
    scale_color_discrete(name = "Traning Data Size")

# ggsave("plots/nvar_vs_prof.pdf", nvar_x_prof)
# ggsave("plots/nvar_vs_acc.pdf", nvar_x_acc)
# ggsave("plots/prof_vs_acc.pdf", prof_x_acc)
print(csv_data[1:1, ])

# ggsave("plots/models_thresh.png", different_models_thresh)
# ggsave("plots/models_vars.png", different_models_vars)
# ggsave("plots/models_data.png", different_models_data)
ggsave("plots/accuracy_datasize.png", data_size_accuracy)