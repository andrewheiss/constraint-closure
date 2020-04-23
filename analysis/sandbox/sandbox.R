library(tidyverse)
library(ggridges)
library(lme4)
library(here)

constraints <- read_rds(here("data", "derived_data", "constraints.rds"))

constraint_levels <- constraints %>%
  mutate(constraint_clean = fct_inorder(constraint_clean)) %>%
  mutate(levels = map(levels_clean, names)) %>%
  unnest(levels) %>%
  mutate(levels = fct_inorder(levels))

# TODO: Switch back to full BLS.rds and BHL.rds
BHL <- read_rds(here("data", "derived_data", "BHL_small.rds"))
bhl_long <- BHL %>%
  gather(constraint, value, one_of(constraints$constraint)) %>%
  left_join(constraints, by = "constraint") %>%
  mutate(value = fct_relevel(value, levels(constraint_levels$levels)),
         constraint_clean = fct_relevel(constraint_clean,
                                        levels(constraint_levels$constraint_clean)))

BLS <- read_rds(here("data", "derived_data", "BLS_small.rds"))
bls_long <- BLS %>%
  gather(constraint, value, one_of(constraints$constraint)) %>%
  left_join(constraints, by = "constraint") %>%
  mutate(value = fct_relevel(value, levels(constraint_levels$levels)),
         constraint_clean = fct_relevel(constraint_clean,
                                        levels(constraint_levels$constraint_clean)))


plot_constraints <- BHL %>%
  mutate_at(vars(one_of(constraints$constraint)),
            list(constraint = ~as.logical(-as.integer(.) + 2L))) %>%
  mutate(n_constraints = reduce(select(., ends_with("_constraint")), `+`)) %>%
  mutate(n_constraints_f = as.factor(n_constraints))

# Points
ggplot(plot_constraints, aes(x = n_constraints_f, y = landscape_fitness_linked)) +
  geom_point(size = 0.5, alpha = 0.3, position = position_jitter(seed = 1234))

# Ridge plots
ggplot(plot_constraints, aes(x = landscape_fitness_linked, y = n_constraints_f)) +
  geom_density_ridges()

# Violin/box plots
ggplot(plot_constraints, aes(x = n_constraints_f, y = landscape_fitness_linked)) +
  # geom_violin(size = 0.25) +
  geom_boxplot() +
  stat_summary(geom = "point", fun.y = "mean",
               size = 2, pch = 21, fill = "black", color = "white")

model1 <- lm(landscape_fitness_linked ~ n_constraints + numlink + cdis1 +
  var_fitness + max_intial_proportion_links + maxsp,
  data = plot_constraints)
summary(model1)

model2 <- lmer(landscape_fitness_linked ~ n_constraints + numlink + cdis1 +
               var_fitness + max_intial_proportion_links + maxsp + (1 | create_network),
             data = plot_constraints)
summary(model2)


library(tidyverse)

set.seed(1234)
example <- tibble(id = 1:500,
                  A = sample(c(TRUE, FALSE), 500, replace = TRUE),
                  B = sample(c(TRUE, FALSE), 500, replace = TRUE),
                  C = sample(c(TRUE, FALSE), 500, replace = TRUE),
                  D = sample(c(TRUE, FALSE), 500, replace = TRUE),
                  E = sample(c(TRUE, FALSE), 500, replace = TRUE),
                  `F` = sample(c(TRUE, FALSE), 500, replace = TRUE))
head(example)
#> # A tibble: 6 x 7
#>      id A     B     C     D     E     F
#>   <int> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl>
#> 1     1 FALSE TRUE  FALSE TRUE  FALSE FALSE
#> 2     2 FALSE FALSE TRUE  FALSE TRUE  FALSE
#> 3     3 FALSE FALSE FALSE TRUE  TRUE  TRUE
#> 4     4 FALSE FALSE TRUE  FALSE FALSE TRUE
#> 5     5 TRUE  FALSE FALSE TRUE  FALSE FALSE
#> 6     6 FALSE TRUE  FALSE TRUE  TRUE  TRUE


set.seed(1)
mat <- cbind(
  A = sample(c(TRUE, TRUE, FALSE), 50, TRUE),
  B = sample(c(TRUE, FALSE), 50, TRUE),
  C = sample(c(TRUE, FALSE, FALSE, FALSE), 50, TRUE)
)
fit2 <- euler(mat)

apply(expand.grid(a=0:1, b=0:1), 2, paste, collapse="")

# Basically, if A, B, and D are true in a row, I want columns named A+B, A+D, and A+B+D to be true

library(polyreg)
getPoly()


asdf <- example %>%
  gather(key, value, -id) %>%
  filter(value != 0) %>%
  group_by(id) %>%
  nest()


# Somehow magically get indicator variables for all of the possible
# pairs, triples, quadruples, and quintuples.
example_result <- tribble(
  ~id, ~`A & B`, ~`A & C`, ~`A & D`, ~`A & E`, ~`A & F`, ~`A & B & C`, ~`A & B & D`, ~`etc`,
  501, TRUE,     FALSE,    TRUE,     FALSE,    FALSE,    FALSE,        TRUE,         FALSE
)
example_result
#> # A tibble: 1 x 9
#>      id `A & B` `A & C` `A & D` `A & E` `A & F` `A & B & C` `A & B & D`
#>   <dbl> <lgl>   <lgl>   <lgl>   <lgl>   <lgl>   <lgl>       <lgl>
#> 1   501 TRUE    FALSE   TRUE    FALSE   FALSE   FALSE       TRUE
#> # … with 1 more variable: etc <lgl>


constraint_combinations <- plot_constraints %>%
  select(runnum, ends_with("_constraint"))

result <- map(2:6, check_combinations) %>%
  bind_cols(constraint_combinations, .)

outcomes <- plot_constraints %>%
  select(runnum, n_constraints, landscape_fitness_linked)

asdf <- outcomes %>%
  left_join(result, by = "runnum") %>%
  gather(key, value, -runnum, -n_constraints, -landscape_fitness_linked)

qwer <- asdf %>%
  filter(value == TRUE) %>%
  group_by(key) %>%
  summarize(avg = mean(landscape_fitness_linked))

ggplot(qwer, aes(x = avg, y = key))

library(randomForest)

bloop <- randomForest(landscape_fitness_linked ~
                        create_network * select * disperse * compete * selectfor_d * catastrophe,
                      data = plot_constraints)
summary(bloop)
importance(bloop)
plot(bloop)
constraints$constraint
