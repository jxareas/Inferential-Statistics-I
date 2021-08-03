# Packages ----------------------------------------------------------------

require(UsingR)
require(dplyr)
require(data.table)

# Data --------------------------------------------------------------------

data(father.son)

names(father.son) <- c("father", "son")

father.son <- melt(
        setDT(father.son),
        id.vars = character(),
        variable.name = "status",
        value.name = "height"
)



# One Sample t-Test -------------------------------------------------------


# We wish to test if the son's height is equal to their father's height

t1 <-
        t.test(
                x = father.son |>
                        filter(status == "son") |>
                        select(height),
                mu = father.son |>
                        filter(status == "father") |>
                        select(height) |>
                        unlist() |>
                        mean(),
                alternative = "two.sided",
                paired = F,
                conf.level = .95
        )


# Two-Sample t-Test -------------------------------------------------------

# It is also equivalent to a Two Sample t-test for Difference in Means

t2 <- 
t.test(formula = height ~ status,
       data = father.son)
