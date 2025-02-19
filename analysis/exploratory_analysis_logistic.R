library(ggplot2)
library(tidyverse)

# read in simulation results

load(here::here("data","model_output.rda"))

# beta estimation

solution = as.data.frame(t(sapply(result, `[[`, "solution")))
se = as.data.frame(t(sapply(result, `[[`, "se")))

est = data.frame(beta0 = solution[,1], beta0_lower = solution[,1] - 1.96 * se[,1], beta0_upper = solution[,1] + 1.96 * se[,1],
                 beta1 = solution[,2], beta1_lower = solution[,2] - 1.96 * se[,2], beta1_upper = solution[,2] + 1.96 * se[,2])

write.csv(est, file = here::here("results","beta_estimate.csv"), row.names = F)

# computation time

comp_time = data.frame(time = sapply(result, `[[`, "time"),
                       method = c("Newton", "MM", "glm", "optim"))

png(here::here("results","computation_time.png"))
ggplot(comp_time, aes(y = time, x = method)) + geom_col() + theme(text = element_text(size = 22))
dev.off()

# iteration
iter = data.frame(iteration = sapply(result, `[[`, "niter"),
                       method = c("Newton", "MM", "glm", "optim"))

png(here::here("results","iter.png"))
ggplot(iter, aes(y = iteration, x = method)) + geom_col() + theme(text = element_text(size = 22))
dev.off()
