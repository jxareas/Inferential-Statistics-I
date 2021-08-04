# Packages ----------------------------------------------------------------

library(ggplot2)

# Data Simulation --------------------------------------------------------------------
set.seed(287)
L <-
        lapply(X = vector(mode = "list", length = 1e3L),
               \(.)
               {
                       rbinom(n = 30,
                              size = 1,
                              prob = .2)
               })

props <-
        L |>
        sapply(\(l)
               {
                       proportions(table(l))
        }) |>
        sapply(\(t)
               {
                       ifelse(length(t) == 2,
                              t[[2]],
                              1 - t[[1]])
        })


# Plot --------------------------------------------------------------------

ggplot(mapping = aes(x = props)) +
        geom_histogram(bins = 15,
                       fill = "black",
                       color = "white") +
        
        labs(
                title = "Sampling Distribution of the Sample Proportion",
                subtitle = "Failed Success-Failure Condition",
                x = "Observed Proportion",
                y = "Count"
        ) +
        theme(
                plot.title = element_text(face = "bold", hjust = .5),
                plot.subtitle = element_text(
                        face = "bold",
                        hjust = .5,
                        color = "red"
                ),
                axis.title.x = element_text(face = "bold",
                                            color = "dimgrey"),
                axis.title.y = element_text(face = "bold",
                                            color = "dimgrey")
        )
