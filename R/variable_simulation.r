source("R/model_functions.r")

csv_data <- pre_process("E0.csv")

get_plot_data <- function(csv_data, training_size) {
    variables <- c("MFTAGt", "MFTHGt", "MFTAGm", "MFTHGm", "OddO2.5")
    selected_vars_col <- c()
    nvars_col <- c()
    accuracy_col <- c()
    profit_col <- c()

    for (num_vars in 1:length(variables)) {
        combinations <- combn(variables, num_vars)

        for (i in 1:ncol(combinations)) {
            selected_vars <- combinations[, i]

            # add selected variables to csv column
            sv_str <- paste(selected_vars, collapse = " , ")
            selected_vars_col <- c(selected_vars_col, sv_str)
            nvars_col <- c(nvars_col, length(selected_vars))

            simulation <- train_simulate_model(csv_data = csv_data, training_size = training_size, stake = 100, variables = selected_vars, bet_decision = 0)

            # add profit and accuracy to csv columns
            profit_col <- c(profit_col, simulation[1])
            accuracy_col <- c(accuracy_col, simulation[2])
            betted <- simulation[3]
        }
    }

    data <- data.frame(NVariables = nvars_col, Variables = selected_vars_col, Profit = profit_col, Accuracy = accuracy_col, NumBets = betted)
    return(data)
}

data_min <- get_plot_data(csv_data = csv_data, training_size = 40)
data_low <- get_plot_data(csv_data = csv_data, training_size = 140)
data_med <- get_plot_data(csv_data = csv_data, training_size = 240)
data_max <- get_plot_data(csv_data = csv_data, training_size = 340)

data_min$TrainingDataSize <- "Min"
data_low$TrainingDataSize <- "Low"
data_med$TrainingDataSize <- "Med"
data_max$TrainingDataSize <- "Max"

combined_data <- rbind(data_min, data_low, data_med, data_max)

write.csv(combined_data, "simulation.csv", row.names = FALSE)