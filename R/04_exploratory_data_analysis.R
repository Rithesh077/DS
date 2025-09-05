library(datasets)
library(rmarkdown)
data("iris")
summarise_iris <- summary(iris)
print(summarise_iris)
library(ggplot2)

print("Select a column to find its mode:\n 1.Sepal Length\n 2.Sepal Width\n 3.Petal Length\n 4.Petal Width\n") # nolint

user_input <- as.numeric(readline(prompt = "Enter a column number to find its mode: ")) # nolint

get_mode <- function(v) {
    uniqv <- unique(v) # nolint: indentation_linter.
    uniqv[which.max(tabulate(match(v, uniqv)))]
}
print("Select the variable number to find its mode:\n1.Sepal Length.\n2.Sepal Width.\n3.Petal Length.\n4.Petal Width.") # nolint
user_input <- as.numeric(readline("Enter the variable number (1-4): ")) # nolint
if (user_input %in% 1:4) {
    mode_value <- get_mode(iris[[user_input]]) # nolint
    print(paste("The mode of the selected column is:", mode_value)) # nolint
} else {
    print("Invalid input. Please select a valid column number between 1 and 4.") # nolint
    stop()
}

print("Choose a visualization type:\n1.Historgram.\n2.Boxplot.\n3.Scatterplot.\n4.PCA Plot") # nolint
useer_viz_input <- as.numeric(readline(prompt = "Select visualization type: ")) # nolint
if (!(user_input %in% 1:4)) {
    print("Invalid input. Please select a valid column number between 1 and 4.") # nolint
    stop()
}
print("For X axis:")
print("1. Sepal Length")
user_x_axis <- as.numeric(readline(prompt = "Select X axis: "))
if (!(user_x_axis == 1)) {
    ggplot(iris, aes(x = iris[, user_input])) + # nolint
        geom_histogram(binwidth = 0.5, fill = "navy", color = "black", bins = 20) + # nolint
        labs(title = "Histogram", x = colnames(iris)[user_input], y = "Frequency") # nolint
} else if (user_vis_input == 2) {
    ggplot(iris, aes(y = iris[, user_input])) + # nolint
        geom_boxplot(fill = "orange", color = "black") + # nolint
        labs(title = "Boxplot", y = colnames(iris)[user_input])
} else if (user_vis_input == 3) {
    print("Scatterplot requires two different variables for X and Y axes.") # nolint
    print("For X axis:")
    print("1. Sepal Length")
    print("2. Sepal Width")
    print("3. Petal Length")
    print("4. Petal Width")
    user_x_axis <- as.numeric(readline(prompt = "Select X axis: "))
    user_y_axis <- as.numeric(readline(prompt = "Select Y axis: "))
    if (user_x_axis == user_y_axis) {
        print("The axes cannot be of same variables") # nolint
        stop()
    } else if (!(user_x_axis %in% 1:4) || !(user_y_axis %in% 1:4)) { # nolint
        print("Invalid input. Please select a valid column number between 1 and 4.") # nolint
        stop()
    } # nolint: indentation_linter.
    ggplot(iris, aes(x = iris[, user_x_axis], y = iris[, user_y_axis])) + # nolint
        geom_point(color = "#00eeff", size = 2) + # nolint
        labs(title = "Scatterplot", x = colnames(iris)[user_x_axis], y = colnames(iris)[user_y_axis]) + # nolint
        theme_minimal()
} else if (user_vis_input == 4) {
    pca_result <- prcomp(iris[, 1:4], center = TRUE, scale. = TRUE) # nolint
    pca_data <- as.data.frame(pca_result$x)
    pca_data$Species <- iris$Species
    ggplot(pca_data, aes(x = PC1, y = PC2, color = Species)) +
        geom_point(size = 2) + # nolint
        labs(title = "PCA Plot", x = "Principal Component 1", y = "Principal Component 2") + # nolint
        theme_minimal()
    quit(status = 0) # nolint
} else {
    print("Invalid visualization type selected.") # nolint
    stop()
}

saveRDS(iris, file = here::here("outputs/tables/iris_dataset.rds"))
rmarkdown::render("outputs/04_exploratory_data_analysis.Rmd", output_format = "html_document") # nolint
