library(ggplot2)


csv_data <- read.csv("data/simulation_D1.csv")

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
    labs(subtitle = "Bundesliga") +
    scale_color_discrete(name = "Betting threshold")

optimal_model <- which.max(csv_data$avgProfit)

different_models_data <- ggplot(csv_data, aes(x = avgProfit, y = avgHitRate, color = TrainingDataSize)) +
    # geom_point() +
    geom_point(data = csv_data, aes(x = avgProfit, y = avgHitRate, color = TrainingDataSize)) +
    geom_point(data = csv_data[optimal_model, ], aes(x = avgProfit, y = avgHitRate), color = "red") +
    labs(x = "Average Profit", y = "Average Hit Rate") +
    ggtitle("Average Profit vs. Average Hit Rate (separated by training dataset size)") +
    labs(subtitle = "Bundesliga") +
    scale_color_discrete(name = "Training dataset size")

data_size_accuracy <- ggplot(csv_data, aes(x = TrainingDataSize, y = avgHitRate, color = TrainingDataSize)) +
    geom_point() +
    labs(x = "Training dataset size", y = "Accuracy") +
    ggtitle("Accuracy for different training dataset sizes") +
    labs(subtitle = "Bundesliga") +
    scale_color_discrete(name = "Traning Data Size")

threshold_num_bets <- ggplot(csv_data, aes(x = as.factor(Threshold), y = avgBets, color = TrainingDataSize)) +
    geom_point() +
    labs(x = "Betting Threshold", y = "Average number of bets") +
    ggtitle("Average bets for different betting thresholds") +
    labs(subtitle = "Bundesliga") +
    scale_color_discrete(name = "Traning Data Size")

# ggsave("plots/nvar_vs_prof.pdf", nvar_x_prof)
# ggsave("plots/nvar_vs_acc.pdf", nvar_x_acc)
# ggsave("plots/prof_vs_acc.pdf", prof_x_acc)

ggsave("plots/models_thresh_D1.png", different_models_thresh)
ggsave("plots/models_data_D1.png", different_models_data)
ggsave("plots/accuracy_datasize_D1.png", data_size_accuracy)
ggsave("plots/threshold_bets_D1.png", threshold_num_bets)