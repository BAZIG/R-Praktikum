#R-Praktikum Semesterprojekt (Teil 1)
# Name: [Baptiste Fabian Zigmann Weiß]
# Matrikelnummer: [2414242]


#Aufgabe 1: Vektoren und Matrizen
vector_ac <- c(1, 4, 2, 4, 2)
vector_b <- 2^seq(1, 5, by = 1)
vector_c <- seq(1, 5, by = 1)^2

vector_ac
vector_b
vector_c

max_element <- max(vector_c)
position <- which.max(vector_c)

max_element
position

matrix_e <- cbind(vector_ac, vector_b, vector_c)
dim(matrix_e)
matrix_e

colnames(matrix_e) <- c("Matrikel", "HochAufZwei", "HochZwei")

matrix_g <- matrix_e[, -1]
matrix_g

P <- matrix(1:100, nrow = 10, ncol = 10, byrow = FALSE)
Q <- matrix(1 / (1:100), nrow = 10, ncol = 10, byrow = TRUE)

addition <- P + Q
subtraction <- P - Q
multiplication <- P %*% Q
hadamard_product <- P * Q

addition

# Aufgabe 2: Plots

# (a) Generate a random sample of size [b e f] from a normal distribution with mean [c] and variance [a]
random_sample <- rnorm(424, mean = 1, sd = sqrt(2))

# (b) Calculate the mean, variance, and the 25%, 50%, and 75% quantiles
mean_value <- mean(random_sample)
variance_value <- var(random_sample)
quantiles <- quantile(random_sample, probs = c(0.25, 0.5, 0.75))

# (c) Store the results from part (b) in a vector
results_vector <- c(mean_value, variance_value, quantiles)

# (d) Visualize the random sample using a histogram
hist(random_sample, main = "Histogram of Random Sample", xlab = "Value", col = "lightblue")

# (e) Add vertical lines at the 25%, 50%, and 75% quantiles to the histogram
hist(random_sample, main = "Histogram of Random Sample with Quantiles", xlab = "Value", col = "lightblue")
abline(v = quantiles, col = "red", lwd = 2)
text(x = quantiles, y = 0, labels = c("25%", "50%", "75%"), col = "red", xpd = TRUE)

# (f) Calculate the values of the sin() function between -π and π in 0.1 steps and plot them
x_values <- seq(-pi, pi, by = 0.1)
sin_values <- sin(x_values)
plot(x_values, sin_values, type = "l", main = "Sine Function", xlab = "x", ylab = "sin(x)", col = "blue")

# (g) Calculate the values of the cos() function between -π and π in 0.1 steps and plot them
cos_values <- cos(x_values)
plot(x_values, cos_values, type = "l", main = "Cosine Function", xlab = "x", ylab = "cos(x)", col = "green")

# (h) Plot both the sine and cosine functions in a common plot with separate coordinate systems
par(mfrow = c(2, 1))
plot(x_values, sin_values, type = "l", main = "Sine Function", xlab = "x", ylab = "sin(x)", col = "blue")
plot(x_values, cos_values, type = "l", main = "Cosine Function", xlab = "x", ylab = "cos(x)", col = "green")

# Task 3: Advanced R

# (a) Generate random samples from a standard normal distribution and count how many fall within [-1.96, 1.96]
n1 <- 5
random_samples_n1 <- rnorm(n1)
count_in_interval_n1 <- sum(random_samples_n1 >= -1.96 & random_samples_n1 <= 1.96)

ndef <- 424
random_samples_ndef <- rnorm(ndef)
count_in_interval_ndef <- sum(random_samples_ndef >= -1.96 & random_samples_ndef <= 1.96)

count_in_interval_n1
count_in_interval_ndef

# (b) Generate random samples from a continuous uniform distribution and apply summary to positive values
n2 <- 142
random_samples_uniform <- runif(n2, min = -1, max = 1)
positive_values <- random_samples_uniform[random_samples_uniform > 0]
summary(positive_values)

# (c) Create a vector and determine where the value changes
vector_c <- c(rep(2, 5), rep(4, 3), rep(1, 6), rep(4, 6), rep(2, 2))
changes <- which(diff(vector_c) != 0)
changes

# (d) Write an if block to check divisibility
check_condition <- function(x) {
  if (x %% 2 == 0) {
    return(x / 2)
  } else {
    return("Sorry")
  }
}

check_condition(4)  # Should return 2
check_condition(5)  # Should return "Sorry"

# (e) Store all integers from [0, 100] divisible by 3 in a vector
divisible_by_three <- c()
for (i in 0:100) {
  if (i %% 3 == 0) {
    divisible_by_three <- c(divisible_by_three, i)
  }
}
divisible_by_three

# (f) Define a function to calculate the mean of values within a specified interval
calculate_mean_in_interval <- function(n = 100, lower_bound, upper_bound) {
  samples <- rnorm(n)
  subset_samples <- samples[samples >= lower_bound & samples <= upper_bound]
  mean_value <- mean(subset_samples)
  return(mean_value)
}

calculate_mean_in_interval(n = 241, lower_bound = 0.0, upper_bound = 1.0)

# Task 4: Tidyverse

# Load the necessary library
library(tidyverse)

# (a) Use readr to read the StarWars dataset
starwars_data <- read_csv("./R-Praktikum/starwars.csv")

# Display the first six rows
head(starwars_data)

# (b) Check if hair_color and species are categorical variables
str(starwars_data)
unique(starwars_data$hair_color)
unique(starwars_data$species)

# (c) Determine if birth_year is discrete or continuous and specify the scale of gender and mass
str(starwars_data$birth_year)
unique(starwars_data$gender)
str(starwars_data$mass)

# (d) Clean the dataset by removing rows with NA height values
cleaned_data <- starwars_data %>%
  filter(!is.na(height))

# Display the cleaned data
head(cleaned_data)

# (e) Create a histogram of the height variable
ggplot(cleaned_data, aes(x = height)) +
  geom_histogram(bins = 5, fill = "lightblue", color = "black") +
  ggtitle("Distribution of Heights of StarWars Characters") +
  xlab("Height") +
  ylab("Frequency")

# (f) Calculate the average BMI for male and female human characters
average_bmi <- cleaned_data %>%
  filter(species == "Human", gender %in% c("male", "female")) %>%
  mutate(bmi = mass / ((height / 100) ^ 2)) %>%
  group_by(gender) %>%
  summarize(avg_bmi = mean(bmi, na.rm = TRUE))

# Display the average BMI
average_bmi
# Task 5: Descriptive Analysis
library(tidyverse)
# (a) Use readr to read the music dataset
music_data <- read_csv("./R-Praktikum/music_data.csv")

# Display the first few rows
head(music_data)

# (b) Determine how many variables the dataset contains and the exact number of songs
num_variables <- ncol(music_data)
num_songs <- nrow(music_data)

# Display the results
num_variables
num_songs

# (c) Reduce the dataset to relevant variables
reduced_music_data <- music_data %>%
  select(danceability, energy, key, loudness, speechiness, acousticness,
         instrumentalness, liveness, valence, tempo, duration, country, continent)

# Display the first few rows of the reduced dataset
head(reduced_music_data)

# (d) Calculate the arithmetic mean for the duration of the songs and the variance within the tempo
mean_duration <- mean(reduced_music_data$duration, na.rm = TRUE)
variance_tempo <- var(reduced_music_data$tempo, na.rm = TRUE)

# Display the results
mean_duration
variance_tempo

# (e) Create a boxplot of the duration variable for all European songs
european_songs <- reduced_music_data %>% filter(continent == "EUR")
if (nrow(european_songs) > 0) {
  ggplot(european_songs, aes(x = factor(1), y = duration)) +
    geom_boxplot(fill = "lightblue") +
    ggtitle("Boxplot of Song Duration for European Songs") +
    xlab("Europe") +
    ylab("Duration (ms)")
} else {
  print("No data available for European songs.")
}

# (f) Create a histogram of the energy variable for all Asian songs
asian_songs <- reduced_music_data %>% filter(continent == "ASI")
if (nrow(asian_songs) > 0) {
  ggplot(asian_songs, aes(x = energy)) +
    geom_histogram(bins = 20, fill = "lightgreen", color = "black") +
    ggtitle("Histogram of Energy for Asian Songs") +
    xlab("Energy") +
    ylab("Frequency")
} else {
  print("No data available for Asian songs.")
}

# (g) Add vertical lines to the histogram to mark the arithmetic mean and the 25% and 75% quantiles
mean_energy <- mean(asian_songs$energy, na.rm = TRUE)
quantiles_energy <- quantile(asian_songs$energy, probs = c(0.25, 0.75), na.rm = TRUE)

if (nrow(asian_songs) > 0) {
  ggplot(asian_songs, aes(x = energy)) +
    geom_histogram(bins = 20, fill = "lightgreen", color = "black") +
    geom_vline(xintercept = mean_energy, color = "red", linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = quantiles_energy, color = "blue", linetype = "dashed", linewidth = 1) +
    ggtitle("Histogram of Energy for Asian Songs with Mean and Quantiles") +
    xlab("Energy") +
    ylab("Frequency")
} else {
  print("No data available for Asian songs.")
}

# (h) Create a density plot of the danceability variable grouped by continent
if (nrow(reduced_music_data) > 0) {
  ggplot(reduced_music_data, aes(x = danceability, fill = continent, color = continent)) +
    geom_density(alpha = 0.5) +
    ggtitle("Density Plot of Danceability by Continent") +
    xlab("Danceability") +
    ylab("Density")
} else {
  print("No data available for danceability by continent.")
}

# (i) Transform the dataset to long format
long_music_data <- reduced_music_data %>%
  pivot_longer(cols = -c(country, continent), names_to = "variable", values_to = "value")

# Display the first few rows of the long dataset
head(long_music_data)

# (j) Create density plots for all music variables and display them side by side
if (nrow(long_music_data) > 0) {
  ggplot(long_music_data, aes(x = value, fill = continent, color = continent)) +
    geom_density(alpha = 0.5) +
    facet_wrap(~ variable, scales = "free") +
    ggtitle("Density Plots of Music Variables by Continent") +
    xlab("Value") +
    ylab("Density")
} else {
  print("No data available for music variables by continent.")
}

# (k) Analyze how the distribution of the tempo of African songs differs from those in Oceania
african_songs <- reduced_music_data %>% filter(continent == "AFR")
oceanian_songs <- reduced_music_data %>% filter(continent == "OCE")

if (nrow(african_songs) > 0 && nrow(oceanian_songs) > 0) {
  ggplot() +
    geom_density(data = african_songs, aes(x = tempo, fill = "AFR", color = "AFR"), alpha = 0.5) +
    geom_density(data = oceanian_songs, aes(x = tempo, fill = "OCE", color = "OCE"), alpha = 0.5) +
    ggtitle("Density Plot of Tempo: Africa vs Oceania") +
    xlab("Tempo") +
    ylab("Density") +
    scale_fill_manual(name = "Continent", values = c("AFR" = "blue", "OCE" = "green")) +
    scale_color_manual(name = "Continent", values = c("AFR" = "blue", "OCE" = "green"))
} else {
  print("No data available for African or Oceanian songs.")
}
