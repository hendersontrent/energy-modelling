#----------------------------------------
# This script sets out to run a Stan 
# model on gas data
#
# Source: https://www.aer.gov.au/wholesale-markets/wholesale-statistics/average-daily-gas-used-for-gas-powered-generation
#----------------------------------------

#----------------------------------------
# Author: Trent Henderson, 13 August 2020
#----------------------------------------

d <- read.csv("data/AER_Average daily gas used for gas powered generation (quarterly)_1_20200703151542.CSV") %>%
  clean_names() %>%
  gather(key = state, value = gas_usage, 2:6) %>%
  mutate(state = gsub("_terajoules", "\\1", state)) %>%
  mutate(state = str_to_upper(state)) %>%
  mutate(quarter_ending = as.Date(quarter_ending, format = "%d/%m/%Y"))

#----------------STAN MODEL------------------------

the_states <- unique(d$state)

some_list <- list()
for(i in the_states){

  shorter <- d %>%
    filter(state == i) %>%
    mutate(n = n(), sd = sd(gas_usage))
  
  d1 <- list(
    mu_start = first(shorter$gas_usage),
    n_months = nrow(shorter),
    y_values = shorter$gas_usage,
    sigma = unique(shorter$sd)
  )
  
  system.time({
    mod <- stan(file = "gas/ss-model.stan", data = d1, iter = 4000, control = list(max_treedepth = 20))
  })

  ex <- as.data.frame(rstan::extract(mod, "mu"))
  actual_dates <- data.frame(quarter_ending = c(shorter$quarter_ending))
  
  outs <- ex %>%
    gather(key = quarter_index, value = gas_usage, 1:48) %>%
    mutate(quarter_index = gsub("mu.", "\\1", quarter_index)) %>%
    mutate(quarter_index = as.numeric(quarter_index)) %>%
    group_by(quarter_index) %>%
    summarise(mean = mean(gas_usage),
              upper = quantile(gas_usage, 0.975),
              lower = quantile(gas_usage, 0.025)) %>%
    ungroup() %>%
    cbind(actual_dates) %>%
    mutate(quarter_ending = as.Date(quarter_ending, format = "%d/%m/%Y")) %>%
    mutate(state = i)
  
  some_list[[i]] <- outs
}

full_models <- rbindlist(some_list, use.names = TRUE)

#----------------OUTPUTS---------------------------

# Colour vector for ribbon

the_palette <- c("#E42256", "#FEC84D", "#00B1B0", "#FF8370", "#34586E")

p <- d %>%
  ggplot(aes(x = quarter_ending)) +
  geom_ribbon(data = full_models, aes(x = quarter_ending, ymin = lower, ymax = upper, fill = state), alpha = 0.3) +
  geom_line(data = full_models, aes(y = mean), colour = "black") +
  geom_point(aes(y = gas_usage), colour = "black") +
  labs(title = "Bayesian state space model of gas usage",
       subtitle = "Data is at the quarterly level",
       x = "Quarter Ending",
       y = "Gas Usage (Terajoules)",
       caption = "Source: Australian Energy Regulator, Analysis: Orbisant Analytics.") +
  theme_bw() +
  scale_x_date(date_breaks = "1 year", date_labels =  "%b %Y") +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~state)
print(p)

# Export

CairoPNG("output/state-space-gas-usage.png", 900, 400)
print(p)
dev.off()
