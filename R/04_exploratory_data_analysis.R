library(datasets)

data("iris")
summarise_iris <- summary(iris)
print(summarise_iris)

print("Select a column to find its mode:\n 1.Sepal Length\n 2.Sepal Width\n 3.Petal Length\n 4.Petal Width\n") # nolint

user_input <- as.numeric(readline(prompt = "Enter a column number to find its mode: ")) # nolint

get_mode <- function(v) {
    uniqv <- unique(v) # nolint: indentation_linter.
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

if (user_input == 1) { # nolint
    print(paste("Mode of Sepal Length is:", get_mode(iris$Sepal.Length))) # nolint
} else if (user_input == 2) { # nolint
    print(paste("Mode of Sepal Width is:", get_mode(iris$Sepal.Width))) # nolint
} else if (user_input == 3) { # nolint
    print(paste("Mode of Petal Length is:", get_mode(iris$Petal.Length))) # nolint
} else if (user_input == 4) { # nolint
    print(paste("Mode of Petal Width is:", get_mode(iris$Petal.Width))) # nolint
} else {
    print("Invalid input. Please select a valid column number between 1 and 4.") # nolint
}

print("Choose the axes for visualization:\n")
print("For X axis:")
print("1. Sepal Length")
print("2. Sepal Width")
print("3. Petal Length")
print("4. Petal Width")
user_x_axis <- as.numeric(readline(prompt = "Select X axis: "))
if (!(user_x_axis %in% 1:4)) {
    print("Invalid input. Please select a valid column number between 1 and 4.") # nolint
    stop()
}
print("For Y axis:")
print("1. Sepal Length")
print("2. Sepal Width")
print("3. Petal Length")
print("4. Petal Width")
user_y_axis <- as.numeric(readline(prompt = "Select Y axis: "))
if (user_x_axis == user_y_axis) {
    print("The axes cannot be of same variables") # nolint: indentation_linter.
    stop()
} else if (!(user_y_axis %in% 1:4)) {
    print("Invalid input. Please select a valid column number between 1 and 4.") # nolint
    stop()
}

print("Choose a visualization type:")
print("1. Histogram")
print("2. Boxplot")
print("3. Scatterplot")

user_viz_type <- as.numeric(readline(prompt = "Select visualization type: "))
if (user_viz_type == 1) {
    hist(iris[, user_x_axis], main = "Histogram", xlab = colnames(iris)[user_x_axis]) # nolint
} else if (user_viz_type == 2) {
    boxplot(iris[, user_x_axis], main = "Boxplot", xlab = colnames(iris)[user_x_axis]) # nolint
} else if (user_viz_type == 3) {
    plot(iris[, user_x_axis], iris[, user_y_axis], main = "Scatterplot", xlab = colnames(iris)[user_x_axis], ylab = colnames(iris)[user_y_axis]) # nolint
} else {
    print("Invalid visualization type selected.") # nolint
}
saveRDS(iris, file = here::here("outputs/tables/iris_dataset.rds"))
