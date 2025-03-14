# Packages ----------------------------------------------------------------

library(ggplot2)

# Data --------------------------------------------------------------------


df <-
        data.frame(
                price = c(
                        775,
                        625,
                        733,
                        929,
                        895,
                        749,
                        1020,
                        1349,
                        599,
                        1143,
                        1209,
                        1495,
                        879,
                        975,
                        1076,
                        1282,
                        665,
                        705,
                        799,
                        500
                )
        )


# Bootstrapping -----------------------------------------------------------
set.seed(287)

n <- 1e4
sample <- vector(mode = "list", length = n) |>
        lapply(\(list_element)
               {
                       list_element <-
                               with(df,
                                    sample(price, length(price), replace = T))
        })

sample_medians <- sample |> sapply(FUN = median)


# Bootstrap Distribution of the Median ------------------------------

ggplot(mapping = aes(x = sample_medians)) +
        geom_histogram(fill = "red3", col = "black", alpha = .8) +
        labs(
                title = "Bootstrap Distribution of the Median",
                subtitle = paste("Number of Simulations: ",
                                 n),
                x = "Median",
                y = "Frequency"
        ) +
        theme_minimal() +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                plot.subtitle = element_text(face = "bold", color = "gray",
                                             hjust = .5),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgrey"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgrey")
        )


# Boostrap Estimation: Std. Error & Confidence Interval -------------------

# Standard Error of the Sampling Distribution of the Sample Median
se <- 
        sd(sample_medians)

# Confidence Interval for the Median
CI <- 
        quantile(x = sample_medians, probs = c(.025, .975))
