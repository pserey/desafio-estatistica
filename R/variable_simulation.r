library(dplyr)
source("R/model_functions.r")



get_plot_data <- function(csv_file_name, training_size, bet_decision) {

    csv_data <- pre_process(csv_file_name, training_set = training_size)

    variables <- c("MFTAGt", "MFTHGt", "MFTAGm", "MFTHGm", "OddO2.5")
    selected_vars_col <- c()
    nvars_col <- c()
    accuracy_col <- c()
    profit_col <- c()
    betted_col <- c()

    for (num_vars in 1:length(variables)) {
        combinations <- combn(variables, num_vars)

        for (i in 1:ncol(combinations)) {
            selected_vars <- combinations[, i]

            # add selected variables to csv column
            sv_str <- paste(selected_vars, collapse = " , ")
            selected_vars_col <- c(selected_vars_col, sv_str)
            nvars_col <- c(nvars_col, length(selected_vars))

            simulation <- train_simulate_model(csv_data = csv_data, training_size = training_size, stake = 100, variables = selected_vars, bet_decision = bet_decision)

            # add profit and accuracy to csv columns
            profit_col <- c(profit_col, simulation[1])
            accuracy_col <- c(accuracy_col, simulation[2])
            betted_col <- c(betted_col, simulation[3])
        }
    }
    data <- data.frame(NVariables = nvars_col, Variables = selected_vars_col, Profit = profit_col, Accuracy = accuracy_col, NumBets = betted_col)
    return(data)
}


# arquivos_idx <- 3

# arquivos <- c("data/validacao/E0_2021.csv", "data/validacao/E0_2223.csv", "data/validacao/E0_2122.csv")

# arquivos <- c("data/validacao/B1_2021.csv", "data/validacao/B1_2122.csv", "data/validacao/B1_2223.csv")

# arquivos <- c("data/F1_2021.csv", "data/F1_2122.csv", "data/F1_2223.csv")

# arquivos <- c("data/D1_2021.csv", "data/D1_2122.csv", "data/D1_2223.csv")

# arquivos <- c("data/validacao/SP1_2021.csv", "data/validacao/SP1_2122.csv", "data/validacao/SP1_2223.csv")

arquivos <- c("data/validacao/E0_2021.csv", "data/validacao/E0_2223.csv", "data/validacao/E0_2122.csv",
            "data/validacao/B1_2021.csv", "data/validacao/B1_2122.csv",
            "data/validacao/B1_2223.csv", "data/validacao/SP1_2021.csv",
            "data/validacao/SP1_2122.csv", "data/validacao/SP1_2223.csv")
arquivos_idx <- 9

thresholds_idx <- 3
thresholds <- c(2.5, 2.7, 2.9)

decision_df <- data.frame()

for (arq in 1:arquivos_idx) {

    for (i in 1:thresholds_idx) {

        all_cases_threshold <- data.frame()

        data_min <- get_plot_data(csv_file_name = arquivos[arq], training_size = 40, bet_decision = i)
        data_low <- get_plot_data(csv_file_name = arquivos[arq], training_size = 140, bet_decision = i)
        data_med <- get_plot_data(csv_file_name = arquivos[arq], training_size = 240, bet_decision = i)
        data_max <- get_plot_data(csv_file_name = arquivos[arq], training_size = 340, bet_decision = i)

        data_min$TrainingDataSize <- "Min"
        data_low$TrainingDataSize <- "Low"
        data_med$TrainingDataSize <- "Med"
        data_max$TrainingDataSize <- "Max"

        all_cases_threshold <- rbind(data_min, data_low, data_med, data_max)

        all_cases_threshold <- mutate(all_cases_threshold, Threshold = thresholds[i])
        decision_df <- rbind(decision_df, all_cases_threshold)

    }
}

# decision loop: the best row is the row with larger average profit for the combination of Threshold, Variables and Dataset Size
grouped_tuples <- decision_df %>% group_by(Threshold, Variables, TrainingDataSize)
average_profit <- summarise(grouped_tuples, avgProfit = mean(Profit), avgHitRate = mean(Accuracy), avgBets = mean(NumBets))

write.csv(average_profit, "data/simulation.csv", row.names = FALSE)

best_combination <- average_profit[which.max(average_profit$avgProfit), ]
best_combination