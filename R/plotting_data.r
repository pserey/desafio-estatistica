library(ggplot2)


csv_data <- read.csv("data/simulation.csv")

captions <- paste(paste("Betted games (mean):", round(mean(csv_data$NumBets), digits = 2)), "|", paste("Max profit:", max(csv_data$Profit)))

nvar_x_prof <- ggplot(csv_data, aes(x = NVariables, y = Profit, color = TrainingDataSize)) +
    geom_point() +
    labs(x = "Number of Variables", y = "Profit") +
    ggtitle(sprintf("Number of Variables vs. Profit (Betting threshold: %.1f)", 2.5)) +
    scale_color_discrete(name = "Traning Data Size") +
    labs(caption = captions)

nvar_x_acc <- ggplot(csv_data, aes(x = NVariables, y = Accuracy, color = TrainingDataSize)) +
    geom_point() +
    labs(x = "Number of Variables", y = "Accuracy") +
    ggtitle(sprintf("Number of Variables vs. Accuracy (Betting threshold: %.1f)", 2.5)) +
    scale_color_discrete(name = "Traning Data Size") +
    labs(caption = captions)

prof_x_acc <- ggplot(csv_data, aes(x = Profit, y = Accuracy, color = TrainingDataSize)) +
    geom_point() +
    labs(x = "Profit", y = "Accuracy") +
    ggtitle(sprintf("Profit vs. Accuracy (Betting threshold: %.1f)", 2.5)) +
    scale_color_discrete(name = "Traning Data Size") +
    labs(caption = captions)

ggsave("nvar_vs_prof.pdf", nvar_x_prof)
ggsave("nvar_vs_acc.pdf", nvar_x_acc)
ggsave("prof_vs_acc.pdf", prof_x_acc)