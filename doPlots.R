# To examine the contribution of each component in Moore's response function on yield losses.
# func <- function(x) {
#   y1 <- 3.7*x
#   y2 <- - 0.9*x^2
#   y3 <- - 0.4*x*23.5
#   y4 <- 0.04*x^2*23.5
#   y <- y1 + y2 + y3 + y4
#   c(y1, y2, y3, y4, y)
# }
#
# test <- data.frame(t(sapply(seq(0, 3, by = 0.1), func))) %>%
#   mutate(id = seq(0, 3, by = 0.1))
#
# ggplot(data = test) +
#   geom_line(aes(id, X1), col = 'red') +
#   geom_line(aes(id, X2), col = 'blue') +
#   geom_line(aes(id, X3), col = 'purple') +
#   geom_line(aes(id, X4), col = 'green') +
#   geom_line(aes(id, X5))


font.size <- 12

g <-
  MooreYieldProject %>%
  filter(TempChange >= 0) %>%
  ggplot(data = .) +
  geom_point(aes(TempChange, YieldChangeTmp)) +
  geom_hline(yintercept = 0, colour = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(0, 5, by = 1),
                     limits = c(0, 5)) +
  scale_y_continuous(breaks = seq(-50, 10, by = 10),
                     limits = c(-50, 10)) +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
        axis.title = element_text(color = 'black', face = 'plain', size = font.size)) +
  labs(x = 'Temperature Change (degree C)', y = 'Yield Change (%)')


ggsave('Fig_MooreTempYieldPred.png', g, width = 140, height = 160, units = 'mm', dpi = 72)

# Plot the frequency of yield losses.
HistPlot <- MooreYieldProject %>%
  dplyr::select(YieldChangeTmpPre, YieldChangeTmpPreCO2) %>%
  gather(Variable, Value, 1:2)

g <-
  ggplot(data = filter(HistPlot, abs(Value) <= 100)) +
  geom_histogram(aes(Value), bins = 50) +
  geom_vline(xintercept = 0, col = 'red') +
  scale_y_continuous(breaks = seq(0, 3000, 500)) +
  facet_wrap(~ Variable,
             labeller = as_labeller(c('YieldChangeTmpPre' = '(a) No CO2', 'YieldChangeTmpPreCO2' = '(b) With CO2'))) +
  labs(x = 'Projected yield changes (in %)', y = 'Frequnecy') +
  theme_classic() +
  theme(axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
        axis.title = element_text(color = 'black', face = 'plain', size = font.size),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text = element_text(color = 'black', face = 'plain', size = font.size))


ggsave('Fig_MooreYieldFrequency.png', g, width = 180, height = 140, units = 'mm', dpi = 72)

# Show the differences with interaction between temperature and adaptation dummy
HistPlot2 <- MooreYieldProject %>%
  mutate(YieldChangeTmpPreAdapt = YieldChangeTmpPre + YieldChangeAdaptTemp) %>%
  dplyr::select(YieldChangeTmpPre, YieldChangeTmpPreAdapt) %>%
  gather(Variable, Value, 1:2)

g <-
  ggplot(data = filter(HistPlot2)) +
  geom_density(aes(Value, col = Variable), fill = NA, lwd = 1) +
  scale_x_continuous(breaks = seq(-50, 80, 20)) +
  scale_colour_manual(limits = c('YieldChangeTmpPre', 'YieldChangeTmpPreAdapt'),
                      values = c('#d7191c', 'blue'),
                      labels = c('No adaptation', 'Adaptation'),
                      name = '') +
  labs(x = 'Projected yield changes (in %)', y = 'Density') +
  theme_classic() +
  theme(axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
        axis.title = element_text(color = 'black', face = 'plain', size = font.size),
        legend.position = 'bottom')


ggsave('Fig_MooreYieldDensityAdapt.png', g, width = 160, height = 140, units = 'mm', dpi = 72)




# Plot the changed in temperature variability.
TempDatVar <- ClimateProjDat %>%
  filter(climate_model %in% 'CRU3.23' | scenario %in% c('rcp2.6', 'rcp8.5'), variable == 'temperature', !country %in% c('ECU', 'ISR', 'HND'),
         year >= 1961, year <= 2050) %>%
  group_by(scenario, climate_model, country) %>%
  mutate(Shock = mFilter::hpfilter(value, freq = 100, type = 'lambda')$cycle,
         Trend = mFilter::hpfilter(value, freq = 100, type = 'lambda')$trend[, 1],
         Dev = Shock/Trend) %>%
  group_by(scenario, climate_model, country) %>%
  summarise(var. = var(Shock)) %>%
  group_by(scenario, country) %>%
  summarise(var. = mean(var.)) %>%
  mutate(CtyName = countrycode(country, 'iso3c', 'country.name')) %>%
  spread(scenario, var.) %>%
  mutate(rcp2.6Ratio = rcp2.6/historic,
         rcp8.5Ratio = rcp8.5/historic) %>%
  dplyr::select(CtyName, rcp2.6Ratio, rcp8.5Ratio) %>%
  gather(RCP, Ratio, 2:3)

g <-
  ggplot() +
  geom_bar(data = TempDatVar, mapping = aes(RCP, Ratio), stat = 'identity') +
  geom_hline(yintercept = 1.6, lty = 2) +
  scale_x_discrete(label = c('RCP 2.6', 'RCP 8.5')) +
  facet_wrap(~ CtyName, ncol = 4, scales = 'free') +
  theme_classic() +
  theme(strip.text.x = element_text(size = font.size),
        strip.background = element_rect(colour = "NA", fill = "NA"),
        axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
        axis.title = element_text(color = 'black', face = 'plain', size = font.size),
        legend.key.size = unit(0.8, 'cm'),
        legend.text = element_text(color = 'black', face = 'plain', size = font.size + 1)) +
  labs(x = '', y = 'Ratio of variance of future temperatures to variance of past temperatures (F-statistic)')

g
#

ggsave('Fig_TempVarChange.png', g, width = 260, height = 300, units = 'mm', dpi = 72)


# Plot the changes in precipitation variability.
PrepDatVar <- ClimateProjDat %>%
  filter(climate_model %in% 'CRU3.23' | scenario %in% c('rcp2.6', 'rcp8.5'), variable == 'precipitation', !country %in% c('ECU', 'ISR', 'HND'),
         year >= 1961, year <= 2050) %>%
  group_by(scenario, climate_model, country) %>%
  mutate(Shock = mFilter::hpfilter(value, freq = 100, type = 'lambda')$cycle,
         Trend = mFilter::hpfilter(value, freq = 100, type = 'lambda')$trend[, 1],
         Dev = Shock/Trend) %>%
  group_by(scenario, climate_model, country) %>%
  summarise(var. = var(Shock)) %>%
  group_by(scenario, country) %>%
  summarise(var. = mean(var.)) %>%
  mutate(CtyName = countrycode(country, 'iso3c', 'country.name')) %>%
  spread(scenario, var.) %>%
  mutate(rcp2.6Ratio = rcp2.6/historic,
         rcp8.5Ratio = rcp8.5/historic) %>%
  dplyr::select(CtyName, rcp2.6Ratio, rcp8.5Ratio) %>%
  gather(RCP, Ratio, 2:3)

g <-
  ggplot() +
  geom_bar(data = PrepDatVar, mapping = aes(RCP, Ratio), stat = 'identity') +
  geom_hline(yintercept = 1.6, lty = 2) +
  scale_x_discrete(label = c('RCP 2.6', 'RCP 8.5')) +
  facet_wrap(~ CtyName, ncol = 4, scales = 'free') +
  theme_classic() +
  theme(strip.text.x = element_text(size = font.size),
        strip.background = element_rect(colour = "NA", fill = "NA"),
        axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
        axis.title = element_text(color = 'black', face = 'plain', size = font.size),
        legend.key.size = unit(0.8, 'cm'),
        legend.text = element_text(color = 'black', face = 'plain', size = font.size + 1)) +
  labs(x = '', y = 'Ratio of variance of future precipitation to variance of past precipitation (F-statistic)')

#

ggsave('Fig_PrepVarChange.png', g, width = 260, height = 300, units = 'mm', dpi = 72)



# Plot yield distribution.
tmpDat <- CO2AllYieldProjections[1, ] %>%
  mutate(DevYield = NA, ID = 'FAOSTAT')

plotDat <- bind_rows(CO2AllYieldProjections, tmpDat) %>%
  mutate(label = paste0(scenario, ID))

g <-
  ggplot() +
  geom_density(data = FAOYieldDev, aes(DevYield), lwd = 0.8, fill = 'grey70', col = 'grey70') +
  geom_density(data = plotDat, aes(DevYield, lty = label, col = label, fill = label), lwd = 0.8) +
  facet_wrap( ~ Country, scales = 'free', ncol = 4) +
  scale_colour_manual(limits = c('rcp2.6Moore', 'rcp8.5Moore', 'rcp2.6AgMIP', 'rcp8.5AgMIP', 'rcp2.6FAOSTAT'),
                      values = c('#d7191c',  '#d7191c', 'blue', 'blue', 'grey80'),
                      labels = c('Moore et al.; 2.6',  'Moore et al.; 8.5', 'AgMIP; 2.6', 'AgMIP; 8.5', 'Historical'),
                      name = '') +
  scale_linetype_manual(limits = c('rcp2.6Moore', 'rcp8.5Moore', 'rcp2.6AgMIP', 'rcp8.5AgMIP', 'rcp2.6FAOSTAT'),
                        values = c(1,  2, 1, 2, 1),
                        labels = c('Moore et al.; 2.6',  'Moore et al.; 8.5', 'AgMIP; 2.6', 'AgMIP; 8.5', 'Historical'),
                        name = '')  +
  scale_fill_manual(limits = c('rcp2.6Moore', 'rcp8.5Moore', 'rcp2.6AgMIP', 'rcp8.5AgMIP', 'rcp2.6FAOSTAT'),
                    values = c('NA', 'NA', 'NA', 'NA', 'grey80'),
                    labels = c('Moore et al.; 2.6',  'Moore et al.; 8.5', 'AgMIP; 2.6', 'AgMIP; 8.5', 'Historical'),
                    name = '') +
  theme_classic() +
  theme(legend.position = 'bottom') +
  theme(strip.text.x = element_text(size = font.size),
        strip.background = element_rect(colour = "NA", fill = "NA"),
        axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
        axis.title = element_text(color = 'black', face = 'plain', size = font.size),
        legend.key.size = unit(0.8, 'cm'),
        legend.text = element_text(color = 'black', face = 'plain', size = font.size)) +
  labs(x = 'Yield deviation from trend divided by trend values', y = 'Density')

#

# g <-
#   ggplot() +
#   geom_density(data = FAOYieldDev, aes(DevYield), lwd = 0.8, fill = 'grey70', col = 'grey70') +
#   geom_density(data = bind_rows(CO2AllYieldProjections, tmpDat), aes(DevYield, lty = scenario, col = ID), lwd = 0.8, fill = 'NA') +
#   facet_wrap( ~ Country, scales = 'free', ncol = 4) +
#   scale_colour_manual(limits = c('Moore', 'AgMIP', 'FAOSTAT'),
#                       values = c('#d7191c', 'blue', 'grey80'),
#                       labels = c('Future, Moore', 'Future, AgMIP', 'Historical'),
#                       name = '') +
#   scale_linetype_manual(limits = c('rcp2.6', 'rcp8.5'),
#                         values = c(1, 2),
#                         labels = c('RCP 2.6', 'RCP 8.5'),
#                         name = '') +
#   theme_classic() +
#   theme(legend.position = 'bottom') +
#   theme(strip.text.x = element_text(size = font.size),
#         strip.background = element_rect(colour = "NA", fill = "NA"),
#         axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
#         axis.title = element_text(color = 'black', face = 'plain', size = font.size),
#         legend.key.size = unit(0.8, 'cm'),
#         legend.text = element_text(color = 'black', face = 'plain', size = font.size)) +
#   labs(x = 'Yield deviation from trend divided by trend values', y = 'Density')

ggsave('Fig_MooreAgMIPCO2.png', g, width = 300, height = 340, units = 'mm', dpi = 72)


plotDat <- bind_rows(NOCO2AllYieldProjections, tmpDat) %>%
  mutate(label = paste0(scenario, ID))

g <-
  ggplot() +
  geom_density(data = FAOYieldDev, aes(DevYield), lwd = 0.8, fill = 'grey70', col = 'grey70') +
  geom_density(data = plotDat, aes(DevYield, lty = label, col = label, fill = label), lwd = 0.8) +
  facet_wrap( ~ Country, scales = 'free', ncol = 4) +
  scale_colour_manual(limits = c('rcp2.6Moore', 'rcp8.5Moore', 'rcp2.6AgMIP', 'rcp8.5AgMIP', 'rcp2.6FAOSTAT'),
                      values = c('#d7191c',  '#d7191c', 'blue', 'blue', 'grey80'),
                      labels = c('Moore et al.; 2.6',  'Moore et al.; 8.5', 'AgMIP; 2.6', 'AgMIP; 8.5', 'Historical'),
                      name = '') +
  scale_linetype_manual(limits = c('rcp2.6Moore', 'rcp8.5Moore', 'rcp2.6AgMIP', 'rcp8.5AgMIP', 'rcp2.6FAOSTAT'),
                        values = c(1,  2, 1, 2, 1),
                        labels = c('Moore et al.; 2.6',  'Moore et al.; 8.5', 'AgMIP; 2.6', 'AgMIP; 8.5', 'Historical'),
                        name = '')  +
  scale_fill_manual(limits = c('rcp2.6Moore', 'rcp8.5Moore', 'rcp2.6AgMIP', 'rcp8.5AgMIP', 'rcp2.6FAOSTAT'),
                    values = c('NA', 'NA', 'NA', 'NA', 'grey80'),
                    labels = c('Moore et al.; 2.6',  'Moore et al.; 8.5', 'AgMIP; 2.6', 'AgMIP; 8.5', 'Historical'),
                    name = '') +
  theme_classic() +
  theme(legend.position = 'bottom') +
  theme(strip.text.x = element_text(size = font.size),
        strip.background = element_rect(colour = "NA", fill = "NA"),
        axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
        axis.title = element_text(color = 'black', face = 'plain', size = font.size),
        legend.key.size = unit(0.8, 'cm'),
        legend.text = element_text(color = 'black', face = 'plain', size = font.size)) +
  labs(x = 'Yield deviation from trend divided by trend values', y = 'Density')
# g <-
#   ggplot() +
#   geom_density(data = FAOYieldDev, aes(DevYield), lwd = 0.8, fill = 'grey70', col = 'grey70') +
#   geom_density(data = bind_rows(NOCO2AllYieldProjections, tmpDat), aes(DevYield, lty = scenario, col = ID), lwd = 0.8, fill = 'NA') +
#   facet_wrap( ~ Country, scales = 'free', ncol = 4) +
#   scale_colour_manual(limits = c('Moore', 'AgMIP', 'FAOSTAT'),
#                       values = c('#d7191c', 'blue', 'grey80'),
#                       labels = c('Future, Moore', 'Future, AgMIP', 'Historical'),
#                       name = '') +
#   scale_linetype_manual(limits = c('rcp2.6', 'rcp8.5'),
#                         values = c(1, 2),
#                         labels = c('RCP 2.6', 'RCP 8.5'),
#                         name = '') +
#   theme_classic() +
#   theme(legend.position = 'bottom') +
#   theme(strip.text.x = element_text(size = font.size),
#         strip.background = element_rect(colour = "NA", fill = "NA"),
#         axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
#         axis.title = element_text(color = 'black', face = 'plain', size = font.size),
#         legend.key.size = unit(0.8, 'cm'),
#         legend.text = element_text(color = 'black', face = 'plain', size = font.size)) +
#   labs(x = 'Yield deviation from trend divided by trend values', y = 'Density')


ggsave('Fig_MooreAgMIPNOCO2.png', g, width = 300, height = 340, units = 'mm', dpi = 72)


tmpDat <- RCP26AllYieldProjections[1, ] %>%
  mutate(DevYield = NA, ID = 'FAOSTAT')

plotDat <- bind_rows(RCP26AllYieldProjections, tmpDat) %>%
  mutate(label = paste0(co2, ID))

g <-
  ggplot() +
  geom_density(data = FAOYieldDev, aes(DevYield), lwd = 0.8, fill = 'grey70', col = 'grey70') +
  geom_density(data = plotDat, aes(DevYield, lty = label, col = label, fill = label), lwd = 0.8) +
  facet_wrap( ~ Country, scales = 'free', ncol = 4) +
  scale_colour_manual(limits = c('noMoore', 'yesMoore', 'noAgMIP', 'yesAgMIP', 'noFAOSTAT'),
                      values = c('#d7191c',  '#d7191c', 'blue', 'blue', 'grey80'),
                      labels = c('Moore et al.; No CO2',  'Moore et al.; CO2', 'AgMIP; No CO2', 'AgMIP; CO2', 'Historical'),
                      name = '') +
  scale_linetype_manual(limits = c('noMoore', 'yesMoore', 'noAgMIP', 'yesAgMIP', 'noFAOSTAT'),
                        values = c(1,  2, 1, 2, 1),
                        labels = c('Moore et al.; No CO2',  'Moore et al.; CO2', 'AgMIP; No CO2', 'AgMIP; CO2', 'Historical'),
                        name = '')  +
  scale_fill_manual(limits = c('noMoore', 'yesMoore', 'noAgMIP', 'yesAgMIP', 'noFAOSTAT'),
                    values = c('NA', 'NA', 'NA', 'NA', 'grey80'),
                    labels = c('Moore et al.; No CO2',  'Moore et al.; CO2', 'AgMIP; No CO2', 'AgMIP; CO2', 'Historical'),
                    name = '') +
  theme_classic() +
  theme(legend.position = 'bottom') +
  theme(strip.text.x = element_text(size = font.size),
        strip.background = element_rect(colour = "NA", fill = "NA"),
        axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
        axis.title = element_text(color = 'black', face = 'plain', size = font.size),
        legend.key.size = unit(0.8, 'cm'),
        legend.text = element_text(color = 'black', face = 'plain', size = font.size)) +
  labs(x = 'Yield deviation from trend divided by trend values', y = 'Density')

# g <-
#   ggplot() +
#   geom_density(data = FAOYieldDev, aes(DevYield), lwd = 0.8, fill = 'grey70', col = 'grey70') +
#   geom_density(data = bind_rows(RCP26AllYieldProjections, tmpDat), aes(DevYield, lty = co2, col = ID), lwd = 0.8, fill = 'NA') +
#   facet_wrap( ~ Country, scales = 'free', ncol = 4) +
#   scale_colour_manual(limits = c('Moore', 'AgMIP', 'FAOSTAT'),
#                       values = c('#d7191c', 'blue', 'grey80'),
#                       labels = c('Future, Moore', 'Future, AgMIP', 'Historical'),
#                       name = '') +
#   scale_linetype_manual(limits = c('yes', 'no'),
#                         values = c(1, 2),
#                         labels = c('With CO2', 'No CO2'),
#                         name = '') +
#   theme_classic() +
#   theme(legend.position = 'bottom') +
#   theme(strip.text.x = element_text(size = font.size),
#         strip.background = element_rect(colour = "NA", fill = "NA"),
#         axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
#         axis.title = element_text(color = 'black', face = 'plain', size = font.size),
#         legend.key.size = unit(0.8, 'cm'),
#         legend.text = element_text(color = 'black', face = 'plain', size = font.size)) +
#   labs(x = 'Yield deviation from trend divided by trend values', y = 'Density')


ggsave('Fig_MooreAgMIPRCP26.png', g, width = 300, height = 340, units = 'mm', dpi = 72)


plotDat <- bind_rows(RCP85AllYieldProjections, tmpDat) %>%
  mutate(label = paste0(co2, ID))

g <-
  ggplot() +
  geom_density(data = FAOYieldDev, aes(DevYield), lwd = 0.8, fill = 'grey70', col = 'grey70') +
  geom_density(data = plotDat, aes(DevYield, lty = label, col = label, fill = label), lwd = 0.8) +
  facet_wrap( ~ Country, scales = 'free', ncol = 4) +
  scale_colour_manual(limits = c('noMoore', 'yesMoore', 'noAgMIP', 'yesAgMIP', 'noFAOSTAT'),
                      values = c('#d7191c',  '#d7191c', 'blue', 'blue', 'grey80'),
                      labels = c('Moore et al.; No CO2',  'Moore et al.; CO2', 'AgMIP; No CO2', 'AgMIP; CO2', 'Historical'),
                      name = '') +
  scale_linetype_manual(limits = c('noMoore', 'yesMoore', 'noAgMIP', 'yesAgMIP', 'noFAOSTAT'),
                        values = c(1,  2, 1, 2, 1),
                        labels = c('Moore et al.; No CO2',  'Moore et al.; CO2', 'AgMIP; No CO2', 'AgMIP; CO2', 'Historical'),
                        name = '')  +
  scale_fill_manual(limits = c('noMoore', 'yesMoore', 'noAgMIP', 'yesAgMIP', 'noFAOSTAT'),
                    values = c('NA', 'NA', 'NA', 'NA', 'grey80'),
                    labels = c('Moore et al.; No CO2',  'Moore et al.; CO2', 'AgMIP; No CO2', 'AgMIP; CO2', 'Historical'),
                    name = '') +
  theme_classic() +
  theme(legend.position = 'bottom') +
  theme(strip.text.x = element_text(size = font.size),
        strip.background = element_rect(colour = "NA", fill = "NA"),
        axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
        axis.title = element_text(color = 'black', face = 'plain', size = font.size),
        legend.key.size = unit(0.8, 'cm'),
        legend.text = element_text(color = 'black', face = 'plain', size = font.size)) +
  labs(x = 'Yield deviation from trend divided by trend values', y = 'Density')

# g <-
#   ggplot() +
#   geom_density(data = FAOYieldDev, aes(DevYield), lwd = 0.8, fill = 'grey70', col = 'grey70') +
#   geom_density(data = bind_rows(RCP85AllYieldProjections, tmpDat), aes(DevYield, lty = co2, col = ID), lwd = 0.8, fill = 'NA') +
#   facet_wrap( ~ Country, scales = 'free', ncol = 4) +
#   scale_colour_manual(limits = c('Moore', 'AgMIP', 'FAOSTAT'),
#                       values = c('#d7191c', 'blue', 'grey80'),
#                       labels = c('Future, Moore', 'Future, AgMIP', 'Historical'),
#                       name = '') +
#   scale_linetype_manual(limits = c('yes', 'no'),
#                         values = c(1, 2),
#                         labels = c('With CO2', 'No CO2'),
#                         name = '') +
#   theme_classic() +
#   theme(legend.position = 'bottom') +
#   theme(strip.text.x = element_text(size = font.size),
#         strip.background = element_rect(colour = "NA", fill = "NA"),
#         axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
#         axis.title = element_text(color = 'black', face = 'plain', size = font.size),
#         legend.key.size = unit(0.8, 'cm'),
#         legend.text = element_text(color = 'black', face = 'plain', size = font.size)) +
#   labs(x = 'Yield deviation from trend divided by trend values', y = 'Density')


ggsave('Fig_MooreAgMIPRCP85.png', g, width = 300, height = 340, units = 'mm', dpi = 72)


tmpDat <- CO2PriceVolPredDat[1, ] %>% mutate(ID = 'FAOSTAT', PriceVolPred = NA)
plotDat <- bind_rows(CO2PriceVolPredDat, tmpDat) %>%
  mutate(label = paste0(scenario, ID))

g <-
ggplot() +
  geom_density(data = HistPriceVolPredDat, aes(PriceVolPred), size = 0.7, col = 'grey70', fill = 'grey70') +
  geom_density(data = plotDat, aes(PriceVolPred, lty = label, col = label, fill = label), size = 0.7) +
  theme_classic() +
  facet_wrap( ~ Country, scales = 'free', ncol = 4) +
  scale_colour_manual(limits = c('rcp2.6Moore', 'rcp8.5Moore', 'rcp2.6AgMIP', 'rcp8.5AgMIP', 'rcp2.6FAOSTAT'),
                      values = c('#d7191c',  '#d7191c', 'blue', 'blue', 'grey70'),
                      labels = c('Moore et al.; 2.6',  'Moore et al.; 8.5', 'AgMIP; 2.6', 'AgMIP; 8.5', 'Historical'),
                      name = '') +
  scale_linetype_manual(limits = c('rcp2.6Moore', 'rcp8.5Moore', 'rcp2.6AgMIP', 'rcp8.5AgMIP', 'rcp2.6FAOSTAT'),
                        values = c(1,  2, 1, 2, 1),
                        labels = c('Moore et al.; 2.6',  'Moore et al.; 8.5', 'AgMIP; 2.6', 'AgMIP; 8.5', 'Historical'),
                        name = '')  +
  scale_fill_manual(limits = c('rcp2.6Moore', 'rcp8.5Moore', 'rcp2.6AgMIP', 'rcp8.5AgMIP', 'rcp2.6FAOSTAT'),
                    values = c('NA', 'NA', 'NA', 'NA', 'grey70'),
                    labels = c('Moore et al.; 2.6',  'Moore et al.; 8.5', 'AgMIP; 2.6', 'AgMIP; 8.5', 'Historical'),
                    name = '') +
  theme(legend.position = 'bottom') +
  theme(strip.text.x = element_text(size = font.size),
        strip.background = element_rect(colour = "NA", fill = "NA"),
        axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
        axis.title.y = element_text(color = 'black', face = 'plain', size = font.size),
        legend.key.size = unit(0.8, 'cm'),
        legend.text = element_text(color = 'black', face = 'plain', size = font.size + 1)) +
  labs(x = 'Intra-annual CV of real monthly prices of maize', y = 'Density')


# g <-
# ggplot() +
#   geom_density(data = HistPriceVolPredDat, aes(PriceVolPred), size = 0.7, col = 'grey70', fill = 'grey70') +
#   geom_density(data = bind_rows(CO2PriceVolPredDat, tmpDat),
#                aes(PriceVolPred, lty = scenario, col = ID), size = 0.7, fill = NA) +
#   theme_classic() +
#   scale_colour_manual(limits = c('Moore', 'AgMIP', 'FAOSTAT'),
#                       values = c('#d7191c', 'blue', 'grey80'),
#                       labels = c('Future, Moore', 'Future, AgMIP', 'Historical'),
#                       name = '') +
#   scale_linetype_manual(limits = c('rcp2.6', 'rcp8.5'),
#                         values = c(1, 2),
#                         labels = c('RCP 2.6', 'RCP 8.5'),
#                         name = '') +
#   theme(legend.position = 'bottom') +
#   facet_wrap( ~ Country, scales = 'free', ncol = 4) +
#   theme(strip.text.x = element_text(size = font.size),
#         strip.background = element_rect(colour = "NA", fill = "NA"),
#         axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
#         axis.title.y = element_text(color = 'black', face = 'plain', size = font.size),
#         legend.key.size = unit(0.8, 'cm'),
#         legend.text = element_text(color = 'black', face = 'plain', size = font.size + 1)) +
#   labs(x = 'Intra-annual CV of real monthly prices of maize', y = 'Density')



ggsave('Fig_MooreAgMIPPriceVolCO2.png', g, width = 260, height = 300, units = 'mm', dpi = 72)


plotDat <- bind_rows(NOCO2PriceVolPredDat, tmpDat) %>%
  mutate(label = paste0(scenario, ID))

g <-
  ggplot() +
  geom_density(data = HistPriceVolPredDat, aes(PriceVolPred), size = 0.7, col = 'grey70', fill = 'grey70') +
  geom_density(data = plotDat, aes(PriceVolPred, lty = label, col = label, fill = label), size = 0.7) +
  theme_classic() +
  facet_wrap( ~ Country, scales = 'free', ncol = 4) +
  scale_colour_manual(limits = c('rcp2.6Moore', 'rcp8.5Moore', 'rcp2.6AgMIP', 'rcp8.5AgMIP', 'rcp2.6FAOSTAT'),
                      values = c('#d7191c',  '#d7191c', 'blue', 'blue', 'grey70'),
                      labels = c('Moore et al.; 2.6',  'Moore et al.; 8.5', 'AgMIP; 2.6', 'AgMIP; 8.5', 'Historical'),
                      name = '') +
  scale_linetype_manual(limits = c('rcp2.6Moore', 'rcp8.5Moore', 'rcp2.6AgMIP', 'rcp8.5AgMIP', 'rcp2.6FAOSTAT'),
                        values = c(1,  2, 1, 2, 1),
                        labels = c('Moore et al.; 2.6',  'Moore et al.; 8.5', 'AgMIP; 2.6', 'AgMIP; 8.5', 'Historical'),
                        name = '')  +
  scale_fill_manual(limits = c('rcp2.6Moore', 'rcp8.5Moore', 'rcp2.6AgMIP', 'rcp8.5AgMIP', 'rcp2.6FAOSTAT'),
                    values = c('NA', 'NA', 'NA', 'NA', 'grey70'),
                    labels = c('Moore et al.; 2.6',  'Moore et al.; 8.5', 'AgMIP; 2.6', 'AgMIP; 8.5', 'Historical'),
                    name = '') +
  theme(legend.position = 'bottom') +
  theme(strip.text.x = element_text(size = font.size),
        strip.background = element_rect(colour = "NA", fill = "NA"),
        axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
        axis.title.y = element_text(color = 'black', face = 'plain', size = font.size),
        legend.key.size = unit(0.8, 'cm'),
        legend.text = element_text(color = 'black', face = 'plain', size = font.size + 1)) +
  labs(x = 'Intra-annual CV of real monthly prices of maize', y = 'Density')

# g <-
#   ggplot() +
#   geom_density(data = HistPriceVolPredDat, aes(PriceVolPred), size = 0.7, col = 'grey70', fill = 'grey70') +
#   geom_density(data = bind_rows(NOCO2PriceVolPredDat, tmpDat), aes(PriceVolPred, lty = scenario, col = ID), size = 0.7, fill = NA) +
#   theme_classic() +
#   scale_colour_manual(limits = c('Moore', 'AgMIP', 'FAOSTAT'),
#                       values = c('#d7191c', 'blue', 'grey80'),
#                       labels = c('Future, Moore', 'Future, AgMIP', 'Historical'),
#                       name = '') +
#   scale_linetype_manual(limits = c('rcp2.6', 'rcp8.5'),
#                         values = c(1, 2),
#                         labels = c('RCP 2.6', 'RCP 8.5'),
#                         name = '') +
#   theme(legend.position = 'bottom') +
#   facet_wrap( ~ Country, scales = 'free', ncol = 4) +
#   theme(strip.text.x = element_text(size = font.size),
#         strip.background = element_rect(colour = "NA", fill = "NA"),
#         axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
#         axis.title.y = element_text(color = 'black', face = 'plain', size = font.size),
#         legend.key.size = unit(0.8, 'cm'),
#         legend.text = element_text(color = 'black', face = 'plain', size = font.size + 1)) +
#   labs(x = 'Intra-annual CV of real monthly prices of maize', y = 'Density')



ggsave('Fig_MooreAgMIPPriceVolNOCO2.png', g, width = 260, height = 300, units = 'mm', dpi = 72)

font.size <- 12

g1 <-
  ggplot(data = AllPriceProjMean) +
  geom_boxplot(aes(Group, ChangeRatio, col = scenario, lty = co2), position = position_dodge(width = 0.5), width = 0.3, lwd = 0.4) +
  geom_hline(yintercept= 0, lty = 2) +
  theme_classic() +
  labs(x = '', y = 'Percentage (%)', title = '(b) Percentage changes in average intra-annual CV of real monthly prices of maize \n from 1961-2014 to 2006-2050 across focus countries.') +
  scale_x_discrete(limits = c('rcpRawMoore', 'rcpRawAgMIP', 'rcpImportAgMIP', 'rcpStockAgMIP'),
                   labels = c('rcpRawMoore' = 'Moore et al.',
                              'rcpRawAgMIP' = 'GGCMI-AgMIP',
                              'rcpImportAgMIP' = 'GGCMI-AgMIP \n+0.1 import ratio',
                              'rcpStockAgMIP' = 'GGCMI-AgMIP \n+0.05 stock-to-use ratio')) +
  scale_y_continuous(breaks = seq(-50, 50, by = 10), limits = c(-50, 50)) +
  scale_color_manual(labels = c('rcp2.6' = 'RCP 2.6', 'rcp8.5' = 'RCP 8.5'),
                     values = c('rcp2.6' = 'blue', 'rcp8.5' = '#d7191c')) +
  scale_linetype_manual(labels = c('yes' = 'CO2', 'no' = 'No CO2'),
                        values = c('yes' = 1, 'no' = 2),
                        name  = '') +
  theme(axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
        axis.title.y = element_text(color = 'black', face = 'plain', size = font.size),
        legend.position = '',
        title = element_text(color = 'black', face = 'plain', size = font.size-1))

test <- filter(AllPriceProjExtreme, !(ExProd >= 100 & ID == 'Moore'))

g2 <-
  ggplot(data = filter(AllPriceProjExtreme, !(ExProd >= 50 & ID == 'Moore'))) +
  geom_boxplot(aes(Group, ExProd, col = scenario, lty = co2), position = position_dodge(width = 0.5), width = 0.3, lwd = 0.4) +
  geom_hline(yintercept= 10, lty = 2) +
  theme_classic() +
  labs(x = '', y = 'Percentage (%)',
       title = '(a) Relative frequency (in %) on intra-annual CV of real monthly maize prices during 2006-\n 2050 that are equal ot greater than the lower bound of the upper decile of their historical \n distribution during 1961-2014.') +
  scale_x_discrete(limits = c('rcpRawMoore', 'rcpRawAgMIP', 'rcpImportAgMIP', 'rcpStockAgMIP'),
                   labels = c('rcpRawMoore' = 'Moore et al.',
                              'rcpRawAgMIP' = 'GGCMI-AgMIP',
                              'rcpImportAgMIP' = 'GGCMI-AgMIP \n+0.1 import ratio',
                              'rcpStockAgMIP' = 'GGCMI-AgMIP \n+0.05 stock-to-use ratio')) +
  scale_y_continuous(breaks = c(0, seq(10, 80, by = 20)), limits = c(0, 80)) +
  scale_color_manual(labels = c('rcp2.6' = 'RCP 2.6', 'rcp8.5' = 'RCP 8.5'),
                     values = c('rcp2.6' = 'blue', 'rcp8.5' = '#d7191c'),
                     name  = '') +
  scale_linetype_manual(labels = c('yes' = 'CO2', 'no' = 'No CO2'),
                        values = c('yes' = 1, 'no' = 2),
                        name  = '') +
  theme(axis.text = element_text(color = 'black', face = 'plain', size = font.size - 1),
        axis.title.y = element_text(color = 'black', face = 'plain', size = font.size),
        legend.position = '',
        title = element_text(color = 'black', face = 'plain', size = font.size-1)) +
  annotate(geom = "text", x = 3.1, y = 55, label = "Egypt") +
  geom_segment(aes(x = 3.05, y = 53, xend = 2.95, yend = 44), colour = 'black', size = 0.5, arrow = arrow(length = unit(0.1, "cm"))) +
  geom_segment(aes(x = 3.08, y = 53, xend = 3.18, yend = 45), colour = 'black', size = 0.5, arrow = arrow(length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 4.1, y = 55, label = "Egypt") +
  geom_segment(aes(x = 4.05, y = 53, xend = 3.95, yend = 44), colour = 'black', size = 0.5, arrow = arrow(length = unit(0.1, "cm"))) +
  geom_segment(aes(x = 4.08, y = 53, xend = 4.18, yend = 45), colour = 'black', size = 0.5, arrow = arrow(length = unit(0.1, "cm")))


plotDat <- AllPriceProjMean %>% mutate(label = paste0(scenario, co2))
g3 <-
  ggplot(plotDat) +
  geom_boxplot(aes(Group, ChangeRatio, col = label, lty = label), position = position_dodge(width = 0.5), width = 0.3) +
  scale_color_manual(limits = c('rcp2.6no', 'rcp2.6yes', 'rcp8.5no', 'rcp8.5yes'),
                     labels = c('RCP 2.6; No CO2', 'RCP 2.6; CO2', 'RCP 8.5; No CO2', 'RCP 8.5; CO2'),
                     values = c('blue', 'blue', '#d7191c', '#d7191c'),
                     name  = '') +
  scale_linetype_manual(limits = c('rcp2.6no', 'rcp2.6yes', 'rcp8.5no', 'rcp8.5yes'),
                        labels = c('RCP 2.6; No CO2', 'RCP 2.6; CO2', 'RCP 8.5; No CO2', 'RCP 8.5; CO2'),
                        values = c(2,1,2,1),
                        name  = '') +
  theme(legend.position = 'bottom',
        legend.justification = 0.5)

g3 <- cowplot::get_legend(g3)

g <- grid.arrange(g2, g1, g3, ncol = 1, heights = unit(c(12, 12, 0.5), c("cm", "cm", 'cm')))


ggsave('Fig_MooreAgMIPPriceVolPolicy.png', g, width = 200, height = 280, units = 'mm', dpi = 72)
#ggsave('Fig_MooreAgMIPPriceVolPolicyHighRes.png', g, width = 200, height = 280, units = 'mm', dpi = 350)



g1 <-
  ggplot(data = filter(AllPriceProjMeanMoore)) +
  geom_boxplot(aes(Group, ChangeRatio, col = scenario, lty = co2), position = position_dodge(width = 0.5), width = 0.3, lwd = 0.4) +
  geom_hline(yintercept= 0, lty = 2) +
  theme_classic() +
  labs(x = '', y = 'Percentage (%)', title = '(b) Percentage changes in average intra-annual CV of real monthly prices of maize \n from 1961-2014 to 2006-2050 across focus countries.') +
  scale_x_discrete(limits = c('rcpRawMoore', 'rcpRawAgMIP', 'rcpImportMoore', 'rcpStockMoore'),
                   labels = c('rcpRawMoore' = 'Moore et al.',
                              'rcpRawAgMIP' = 'GGCMI-AgMIP',
                              'rcpImportMoore' = 'Moore et al. \n+0.1 import ratio',
                              'rcpStockMoore' = 'Moore et al. \n+0.05 stock-to-use ratio')) +
  scale_y_continuous(breaks = seq(-50, 80, by = 20), limits = c(-70, 50)) +
  scale_color_manual(labels = c('rcp2.6' = 'RCP 2.6', 'rcp8.5' = 'RCP 8.5'),
                     values = c('rcp2.6' = 'blue', 'rcp8.5' = '#d7191c')) +
  scale_linetype_manual(labels = c('yes' = 'CO2', 'no' = 'No CO2'),
                        values = c('yes' = 1, 'no' = 2),
                        name  = '') +
  theme(axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
        axis.title.y = element_text(color = 'black', face = 'plain', size = font.size),
        legend.position = '',
        title = element_text(color = 'black', face = 'plain', size = font.size))

g2 <-
  ggplot(data = filter(AllPriceProjExtremeMoore)) +
  geom_boxplot(aes(Group, ExProd, col = scenario, lty = co2), position = position_dodge(width = 0.5), width = 0.3, lwd = 0.4) +
  geom_hline(yintercept= 10, lty = 2) +
  theme_classic() +
  labs(x = '', y = 'Percentage (%)',
       title = '(a) Relative frequency (in %) on intra-annual CV of real monthly maize prices during 2006-\n 2050 that are equal ot greater than the lower bound of the upper decile of their historical \n distribution during 1961-2014.') +
  scale_x_discrete(limits = c('rcpRawMoore', 'rcpRawAgMIP', 'rcpImportMoore', 'rcpStockMoore'),
                   labels = c('rcpRawMoore' = 'Moore et al.',
                              'rcpRawAgMIP' = 'GGCMI-AgMIP',
                              'rcpImportMoore' = 'Moore et al. \n+0.1 import ratio',
                              'rcpStockMoore' = 'Moore et al. \n+0.05 stock-to-use ratio')) +
  scale_y_continuous(breaks = c(0, seq(10, 90, by = 20)), limits = c(0, 100)) +
  scale_color_manual(labels = c('rcp2.6' = 'RCP 2.6', 'rcp8.5' = 'RCP 8.5'),
                     values = c('rcp2.6' = 'blue', 'rcp8.5' = '#d7191c'),
                     name  = '') +
  scale_linetype_manual(labels = c('yes' = 'CO2', 'no' = 'No CO2'),
                        values = c('yes' = 1, 'no' = 2),
                        name  = '') +
  theme(axis.text = element_text(color = 'black', face = 'plain', size = font.size - 1),
        axis.title.y = element_text(color = 'black', face = 'plain', size = font.size),
        legend.position = '',
        title = element_text(color = 'black', face = 'plain', size = font.size))

g <- grid.arrange(g2, g1, g3, ncol = 1, heights = unit(c(12, 12, 0.5), c("cm", "cm", 'cm')))


ggsave('Fig_MooreAgMIPPriceVolPolicyMoore.png', g, width = 220, height = 280, units = 'mm', dpi = 72)


g1 <-
  ggplot(data = filter(AllPriceProjMean2)) +
  geom_boxplot(aes(Group, ChangeRatio, col = scenario, lty = co2), position = position_dodge(width = 0.5), width = 0.3, lwd = 0.4) +
  geom_hline(yintercept= 0, lty = 2) +
  theme_classic() +
  labs(x = '', y = 'Percentage (%)', title = '(b) Percentage changes in average intra-annual CV of real monthly prices of maize \n from 1961-2014 to 2006-2050 across focus countries.') +
  scale_x_discrete(limits = c('rcpRawMoore', 'rcpRawAgMIP', 'rcpImportAgMIP', 'rcpStockAgMIP'),
                   labels = c('rcpRawMoore' = 'Moore et al.',
                              'rcpRawAgMIP' = 'GGCMI-AgMIP',
                              'rcpImportAgMIP' = 'GGCMI-AgMIP \n-0.1 import ratio',
                              'rcpStockAgMIP' = 'GGCMI-AgMIP \n-0.05 stock-to-use ratio')) +
  scale_y_continuous(breaks = seq(-50, 80, by = 20), limits = c(-60, 80)) +
  scale_color_manual(labels = c('rcp2.6' = 'RCP 2.6', 'rcp8.5' = 'RCP 8.5'),
                     values = c('rcp2.6' = 'blue', 'rcp8.5' = '#d7191c')) +
  scale_linetype_manual(labels = c('yes' = 'CO2', 'no' = 'No CO2'),
                        values = c('yes' = 1, 'no' = 2),
                        name  = '') +
  theme(axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
        axis.title.y = element_text(color = 'black', face = 'plain', size = font.size),
        legend.position = '',
        title = element_text(color = 'black', face = 'plain', size = font.size-1))


g2 <-
  ggplot(data = filter(AllPriceProjExtreme2)) +
  geom_boxplot(aes(Group, ExProd, col = scenario, lty = co2), position = position_dodge(width = 0.5), width = 0.3, lwd = 0.4) +
  geom_hline(yintercept= 10, lty = 2) +
  theme_classic() +
  labs(x = '', y = 'Percentage (%)',
       title = '(a) Relative frequency (in %) on intra-annual CV of real monthly maize prices during 2006-\n 2050 that are equal ot greater than the lower bound of the upper decile of their historical \n distribution during 1961-2014.') +
  scale_x_discrete(limits = c('rcpRawMoore', 'rcpRawAgMIP', 'rcpImportAgMIP', 'rcpStockAgMIP'),
                   labels = c('rcpRawMoore' = 'Moore et al.',
                              'rcpRawAgMIP' = 'GGCMI-AgMIP',
                              'rcpImportAgMIP' = 'GGCMI-AgMIP \n-0.1 import ratio',
                              'rcpStockAgMIP' = 'GGCMI-AgMIP \n-0.05 stock-to-use ratio')) +
  scale_y_continuous(breaks = c(0, seq(10, 90, by = 20)), limits = c(0, 100)) +
  scale_color_manual(labels = c('rcp2.6' = 'RCP 2.6', 'rcp8.5' = 'RCP 8.5'),
                     values = c('rcp2.6' = 'blue', 'rcp8.5' = '#d7191c'),
                     name  = '') +
  scale_linetype_manual(labels = c('yes' = 'CO2', 'no' = 'No CO2'),
                        values = c('yes' = 1, 'no' = 2),
                        name  = '') +
  theme(axis.text = element_text(color = 'black', face = 'plain', size = font.size - 1),
        axis.title.y = element_text(color = 'black', face = 'plain', size = font.size),
        legend.position = '',
        title = element_text(color = 'black', face = 'plain', size = font.size-1))

g <- grid.arrange(g2, g1, g3, ncol = 1, heights = unit(c(12, 12, 0.5), c("cm", "cm", 'cm')))


ggsave('Fig_MooreAgMIPPriceVolPolicyNeg.png', g, width = 200, height = 280, units = 'mm', dpi = 72)





#-----------------------------------------------------------------------------------------------------------#
# Generate a graph to show how I applied Moore et al. (2017)' approach. Use China as an example.
ClimateDatChina <- MooreYieldProject %>%
  dplyr::filter(country == 'CHN', climate_model == 'gfdl', scenario == 'rcp8.5')
YieldDatChina <- MooreYieldProjectCO2NoCO2Level %>%
  dplyr::filter(country == 'CHN', climate_model == 'gfdl', scenario == 'rcp8.5', co2 == 'no') %>%
  mutate(Trend = mFilter::hpfilter(YieldPred, type = 'lambda', freq = 100)$trend)


g1 <-
  ggplot(data = ClimateDatChina) +
  geom_line(aes(year, TempChange)) +
  geom_line(aes(year, PrepChange/100)) +
  annotate(geom = "text", x = 2030, y = 2, label = "Temperature changes \n(in Celsius degree)") +
  geom_segment(aes(x = 2035, y = 2, xend = 2039.5, yend = 1.6), colour = 'black', size = 0.5, arrow = arrow(length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 2030, y = -0.6, label = "Precipitation changes,\n divided by baseline value") +
  geom_segment(aes(x = 2035, y = -0.5, xend = 2039.5, yend = -0.2), colour = 'black', size = 0.5, arrow = arrow(length = unit(0.1, "cm"))) +
  scale_y_continuous(limits = c(-1, 2.5)) +
  labs(x = '', y = '', title = '(a) Changes in growing season temperature / precipitation') +
  theme(axis.text = element_text(color = 'black', face = 'plain', size = font.size - 1),
        axis.title.y = element_text(color = 'black', face = 'plain', size = font.size),
        legend.position = '',
        title = element_text(color = 'black', face = 'plain', size = font.size))


g2 <-
ggplot(data = YieldDatChina) +
  geom_line(aes(year, YieldPred)) +
  geom_line(aes(year, Trend), col = 'red') +
  annotate(geom = "text", x = 2035, y = 4.73, label = "Projected yields") +
  geom_segment(aes(x = 2035, y = 4.7, xend = 2038.5, yend = 4.45), colour = 'black', size = 0.5, arrow = arrow(length = unit(0.1, "cm"))) +
  annotate(geom = "text", x = 2026, y = 4.06, label = "Fitted trend") +
  geom_segment(aes(x = 2026, y = 4.1, xend = 2023, yend = 4.45), colour = 'black', size = 0.5, arrow = arrow(length = unit(0.1, "cm"))) +
  scale_y_continuous(limits = c(3.8, 5)) +
  labs(x = '', y = 'Yield in tonnes per hectare', title = '(b) Projected yields and trend') +
  theme(axis.text = element_text(color = 'black', face = 'plain', size = font.size - 1),
        axis.title.y = element_text(color = 'black', face = 'plain', size = font.size),
        legend.position = '',
        title = element_text(color = 'black', face = 'plain', size = font.size))


g <- grid.arrange(g1, g2, ncol = 1)

#
ggsave('Figure_ChinaExample.png', g, width = 200, height = 240, units = 'mm', dpi = 72)



# Visualize the fitted yield trend for Historical data.
FAOYieldDat <- fread('0_Data_MaizeYield.csv') %>%   # Calculate the baseline yields.
  dplyr::filter(country %in% countrycode(Countries, 'country.name', 'iso3c'), crop_model == 'fao') %>%
  dplyr::select(country, year, value) %>%
  dplyr::rename(FAOYield = value)

FAOYieldDatQuaTrend <- FAOYieldDat %>%
  filter(!country %in% c('ECU', 'ISR', 'HND')) %>%
  group_by(country) %>%
  do(reg = lm(FAOYield ~ year + I(year^2), data = .))

FAOYieldDatQuaTrendOut <- augment(FAOYieldDatQuaTrend, object = 'reg') %>%
  group_by(country) %>%
  mutate(Trend = .fitted) %>%
  mutate(Country = countrycode(country,  'iso3c', 'country.name'),
         ID = 'Quadratic')

VisDat <- FAOYieldDev %>%
  mutate(ID = 'HT') %>%
  bind_rows(., FAOYieldDatQuaTrendOut)

g <-
  ggplot(data = VisDat) +
  geom_point(aes(year, FAOYield), size = 1, pch = 1) +
  geom_line(aes(year, Trend, col = ID)) +
  facet_wrap( ~ Country, scales = 'free', ncol = 4) +
  theme_classic() +
  scale_x_continuous(breaks = seq(1960, 2016, by = 15)) +
  scale_color_manual(labels = c('HT' = 'Hodrick-Prescott filter', 'Quadratic' = 'Quadratic'),
                     values = c('HT' = 'blue', 'Quadratic' = '#d7191c'),
                     name  = '') +
  theme(strip.text.x = element_text(size = font.size),
        strip.background = element_rect(colour = "NA", fill = "NA"),
        axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
        axis.title = element_text(color = 'black', face = 'plain', size = font.size),
        legend.key.size = unit(0.8, 'cm'),
        legend.text = element_text(color = 'black', face = 'plain', size = font.size + 1),
        legend.position = 'bottom') +
  labs(x = '', y = 'Yield in tonne per hectare')

ggsave('Fig_FAOYields.png', g, width = 260, height = 300, units = 'mm', dpi = 72)


# Use the Kenya as an example.
KenyaDat <- NOCO2AllYieldProjections %>%
  filter(Country == 'Kenya', (crop_model == 'pdssat' | is.na(crop_model)))

CorKenyaDat <- KenyaDat %>%
  dplyr::select(Country, year, climate_model, DevYield, ID, scenario) %>%
  spread(ID, DevYield) %>%
  drop_na() %>%
  group_by(Country,climate_model, scenario) %>%
  summarise(Cor. = cor(AgMIP, Moore))

g <-
  ggplot(data = KenyaDat) +
  geom_line(aes(year, DevYield, col = ID)) +
  facet_grid(climate_model ~ scenario, labeller = as_labeller(c('rcp2.6' = 'RCP 2.6', 'rcp8.5' = 'RCP 8.5',
                                                                'gfdl' = 'GFDL', 'hadgem' = 'HadGEM',
                                                                'ipsl' = 'IPSL', 'miroc' = 'MIROC', 'noresm' = 'NorESM'))) +
  scale_color_manual(labels = c('Moore' = 'Moore et al. (2017)', 'AgMIP' = 'AgMIP, pDSSAT'),
                     values = c('Moore' = 'blue', 'AgMIP' = '#d7191c'),
                     name  = '') +
  theme(strip.text = element_text(size = font.size),
        strip.background = element_rect(colour = "NA", fill = "NA"),
        axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
        axis.title = element_text(color = 'black', face = 'plain', size = font.size),
        legend.key.size = unit(0.8, 'cm'),
        legend.text = element_text(color = 'black', face = 'plain', size = font.size + 1),
        legend.position = 'bottom',
        legend.justification = 0.5) +
  labs(x = '', y = 'Yield deviations from trend divided by trend values')


ggsave('Fig_MooreAgMIPProjectYieldKEN.png', g, width = 260, height = 300, units = 'mm', dpi = 72)


### Elasticities of all variables.
VisDat <- data.frame(Coef = c(-0.29, -0.216, 0.09, 0.01, 0.006),
                     Sd = c(0.09, 0.047, 0.021, 0.01, 0.028),
                     Variable = c('Import ratio', 'Stock-to-use ratio', 'Absolute yield deviation', 'Conflict', 'Vol. of real exchange rate'))

g <-
  ggplot(data = VisDat) +
  geom_errorbar(aes(Variable, ymin = Coef - 1.96*Sd, ymax = Coef + 1.96*Sd), width = 0.15) +
  geom_point(aes(Variable, Coef)) +
  geom_hline(yintercept = 0) +
  theme_classic() +
  labs(x = '', y = 'Parameter estimates') +
  scale_x_discrete(limits = c('Import ratio', 'Stock-to-use ratio', 'Absolute yield deviation', 'Conflict', 'Vol. of real exchange rate'),
                   labels = c('Import ratio', 'Beginning stock-\nto-use ratio', 'Absolute \n yield deviation',
                              'Social \nconflict', 'Variability of real \nexchange rate')) +
  scale_y_continuous(breaks = seq(-0.5, 0.2, by = 0.1), limits = c(-0.5, 0.2)) +
  theme(axis.text = element_text(color = 'black', face = 'plain', size = font.size),
        axis.title = element_text(color = 'black', face = 'plain', size = font.size),
        legend.text = element_text(color = 'black', face = 'plain', size = font.size))

g

ggsave('Fig1_CoefEstV2.png', g, width = 170, height = 130, units = 'mm', dpi = 72)


# Compare the variance produced by AgMIP with and without CO2.
FAODATYieldVar <- FAOYieldDev %>%
  group_by(country) %>%
  summarise(YieldVAR = var(DevYield))

CropYieldDatFuture <- AgMIPCropYield %>%
  group_by(scenario, country, climate_model, crop_model, co2) %>%
  summarise(YieldVAR = var(DevYield)) %>%
  inner_join(., FAODATYieldVar, by = 'country') %>%
  mutate(Ratio = YieldVAR.x/YieldVAR.y) %>%
  group_by(scenario, country, co2) %>%
  summarise(Ratio = mean(Ratio)) %>%
  mutate(CtyName = countrycode(country, 'iso3c', 'country.name'))

g <-
  ggplot(CropYieldDatFuture) +
  geom_bar(aes(scenario, Ratio, fill = co2), stat = 'identity', position = position_dodge()) +
  theme_classic() +
  scale_fill_manual(values = c('#d7191c', '#1a9641'),
                    labels = c('No CO2', 'CO2'),
                    name = '') +
  scale_x_discrete(labels = c('RCP 2.6', 'RCP 8.5')) +
  facet_wrap(~ CtyName, ncol = 4, scales = 'free') +
  theme(strip.text.x = element_text(size = font.size),
        strip.background = element_rect(colour = "NA", fill = "NA"),
        axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
        axis.title.y = element_text(color = 'black', face = 'plain', size = font.size),
        legend.position = 'bottom') +
  labs(x = '', y = 'Ratio of variance of future maize yields to variance of past maize yields (F-statistic)') +
  geom_hline(yintercept = 1.6, lty = 2)


ggsave(g, filename = 'Fig_AgMIPVarChangeRev.png', width = 260, height = 300, units = 'mm', dpi = 72)


#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------------------#
# Slides.

### Plot the changes in maize yields.
tmpDat <- CO2AllYieldProjections[1, ] %>%
  mutate(DevYield = NA, ID = 'FAOSTAT')

plotDat <- bind_rows(CO2AllYieldProjections, tmpDat) %>%
  mutate(label = paste0(scenario, ID)) %>%
  filter(Country %in% c('Chad', 'Chile', 'China', 'Costa Rica', 'Dominican Republic', 'Egypt', 'Ghana', 'Guatemala', 'Kenya'))

plotDat1 <- filter(FAOYieldDev, Country %in% c('Chad', 'Chile', 'China', 'Costa Rica', 'Dominican Republic', 'Egypt', 'Ghana', 'Guatemala', 'Kenya'))

font.size <- 12
g <-
  ggplot() +
  geom_density(data = plotDat1, aes(DevYield), lwd = 0.8, fill = 'grey70', col = 'grey70') +
  geom_density(data = plotDat, aes(DevYield, lty = label, col = label, fill = label), lwd = 0.8) +
  facet_wrap( ~ Country, scales = 'free', ncol = 3) +
  scale_colour_manual(limits = c('rcp2.6Moore', 'rcp8.5Moore', 'rcp2.6AgMIP', 'rcp8.5AgMIP', 'rcp2.6FAOSTAT'),
                      values = c('#d7191c',  '#d7191c', 'blue', 'blue', 'grey80'),
                      labels = c('Moore et al.; 2.6',  'Moore et al.; 8.5', 'AgMIP; 2.6', 'AgMIP; 8.5', 'Historical'),
                      name = '') +
  scale_linetype_manual(limits = c('rcp2.6Moore', 'rcp8.5Moore', 'rcp2.6AgMIP', 'rcp8.5AgMIP', 'rcp2.6FAOSTAT'),
                        values = c(1,  2, 1, 2, 1),
                        labels = c('Moore et al.; 2.6',  'Moore et al.; 8.5', 'AgMIP; 2.6', 'AgMIP; 8.5', 'Historical'),
                        name = '')  +
  scale_fill_manual(limits = c('rcp2.6Moore', 'rcp8.5Moore', 'rcp2.6AgMIP', 'rcp8.5AgMIP', 'rcp2.6FAOSTAT'),
                    values = c('NA', 'NA', 'NA', 'NA', 'grey80'),
                    labels = c('Moore et al.; 2.6',  'Moore et al.; 8.5', 'AgMIP; 2.6', 'AgMIP; 8.5', 'Historical'),
                    name = '') +
  theme_classic() +
  theme(legend.position = 'bottom') +
  theme(strip.text.x = element_text(size = font.size),
        strip.background = element_rect(colour = "NA", fill = "NA"),
        axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
        axis.title = element_text(color = 'black', face = 'plain', size = font.size),
        legend.key.size = unit(0.8, 'cm'),
        legend.text = element_text(color = 'black', face = 'plain', size = font.size)) +
  labs(x = 'Yield deviation from trend divided by trend values', y = 'Density')


ggsave('Fig_MooreAgMIPCO2SelectCountries.png', g, width = 200, height = 200, units = 'mm', dpi = 320)


tmpDat <- CO2PriceVolPredDat[1, ] %>% mutate(ID = 'FAOSTAT', PriceVolPred = NA)
plotDat <- bind_rows(CO2PriceVolPredDat, tmpDat) %>%
  mutate(label = paste0(scenario, ID)) %>%
  filter(Country %in% c('Chad', 'Chile', 'China', 'Costa Rica', 'Dominican Republic', 'Egypt', 'Ghana', 'Guatemala', 'Kenya'))

plotDat1 <- filter(HistPriceVolPredDat, Country %in% c('Chad', 'Chile', 'China', 'Costa Rica', 'Dominican Republic', 'Egypt', 'Ghana', 'Guatemala', 'Kenya'))


g <-
  ggplot() +
  geom_density(data = plotDat1, aes(PriceVolPred), size = 0.7, col = 'grey70', fill = 'grey70') +
  geom_density(data = plotDat, aes(PriceVolPred, lty = label, col = label, fill = label), size = 0.7) +
  theme_classic() +
  facet_wrap( ~ Country, scales = 'free', ncol = 3) +
  scale_colour_manual(limits = c('rcp2.6Moore', 'rcp8.5Moore', 'rcp2.6AgMIP', 'rcp8.5AgMIP', 'rcp2.6FAOSTAT'),
                      values = c('#d7191c',  '#d7191c', 'blue', 'blue', 'grey70'),
                      labels = c('Moore et al.; 2.6',  'Moore et al.; 8.5', 'AgMIP; 2.6', 'AgMIP; 8.5', 'Historical'),
                      name = '') +
  scale_linetype_manual(limits = c('rcp2.6Moore', 'rcp8.5Moore', 'rcp2.6AgMIP', 'rcp8.5AgMIP', 'rcp2.6FAOSTAT'),
                        values = c(1,  2, 1, 2, 1),
                        labels = c('Moore et al.; 2.6',  'Moore et al.; 8.5', 'AgMIP; 2.6', 'AgMIP; 8.5', 'Historical'),
                        name = '')  +
  scale_fill_manual(limits = c('rcp2.6Moore', 'rcp8.5Moore', 'rcp2.6AgMIP', 'rcp8.5AgMIP', 'rcp2.6FAOSTAT'),
                    values = c('NA', 'NA', 'NA', 'NA', 'grey70'),
                    labels = c('Moore et al.; 2.6',  'Moore et al.; 8.5', 'AgMIP; 2.6', 'AgMIP; 8.5', 'Historical'),
                    name = '') +
  theme(legend.position = 'bottom') +
  theme(strip.text.x = element_text(size = font.size),
        strip.background = element_rect(colour = "NA", fill = "NA"),
        axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
        axis.title.y = element_text(color = 'black', face = 'plain', size = font.size),
        legend.key.size = unit(0.8, 'cm'),
        legend.text = element_text(color = 'black', face = 'plain', size = font.size + 1)) +
  labs(x = 'Intra-annual CV of real monthly prices of maize', y = 'Density')

g
ggsave('Fig_MooreAgMIPCO2PriceVolSelectCountries.png', g, width = 200, height = 200, units = 'mm', dpi = 320)





# Show the variation absorbed by yields.
YieldMean <- NOCO2AllYieldProjections %>%
  group_by(Country, year, scenario, co2, ID, country) %>%
  summarise(DevYield = mean(DevYield)) %>%
  filter(scenario == 'rcp8.5')

TempMean <- ClimateProjDat %>%
  filter(scenario %in% c('rcp2.6', 'rcp8.5'), variable == 'temperature',
         year >= 2006, year <= 2050) %>%
  group_by(scenario, climate_model, country) %>%
  mutate(Shock = mFilter::hpfilter(value, freq = 100, type = 'lambda')$cycle,
         Trend = mFilter::hpfilter(value, freq = 100, type = 'lambda')$trend[, 1],
         DevTemp = Shock/Trend)  %>%
  group_by(scenario,  country, year) %>%
  summarise(DevTemp = mean(DevTemp)) %>%
  filter(scenario == 'rcp8.5') %>%
  mutate(Country = countrycode(country, 'iso3c', 'country.name'))

g <- ggplot() +
  geom_line(data = YieldMean, aes(year, DevYield, col = ID)) +
  geom_line(data = TempMean, aes(year, DevTemp), col = 'black') +
  facet_wrap(~Country, ncol = 4) +
  labs(x = '', y = 'Deviations from trend divided by trend values')

ggsave('Fig1_test.png', g, width = 260, height = 240, units = 'mm')

# ECU, ISR, HND



# Kenya Data.
KenyaDatSelect <- filter(KenyaDat, climate_model %in% c('gfdl', 'hadgem'))
g <-
  ggplot(data = KenyaDatSelect) +
  geom_line(aes(year, DevYield, col = ID)) +
  facet_grid(climate_model ~ scenario,
             labeller = as_labeller(c('rcp2.6' = 'RCP 2.6', 'rcp8.5' = 'RCP 8.5',
                                      'gfdl' = 'GFDL', 'hadgem' = 'HadGEM'))) +
  scale_color_manual(labels = c('Moore' = 'Moore et al.', 'AgMIP' = 'AgMIP, pDSSAT'),
                     values = c('Moore' = 'blue', 'AgMIP' = '#d7191c'),
                     name  = '') +
  theme(strip.text = element_text(size = font.size),
        strip.background = element_rect(colour = "NA", fill = "grey70"),
        axis.text = element_text(color = 'black', face = 'plain', size = font.size-1),
        axis.title = element_text(color = 'black', face = 'plain', size = font.size),
        legend.key.size = unit(0.8, 'cm'),
        legend.text = element_text(color = 'black', face = 'plain', size = font.size + 1),
        legend.position = 'bottom',
        legend.justification = 0.5) +
  labs(x = '', y = 'Yield deviations from trend divided by trend values')

ggsave('Fig_MooreAgMIPProjectYieldKENHighRes.png', g, width = 180, height = 200, units = 'mm', dpi = 340)



### Elasticities of all variables.
VisDat1 <- data.frame(Coef = c(-0.29, -0.216, 0.09, 0.01, 0.006),
                     Sd = c(0.09, 0.047, 0.021, 0.01, 0.028),
                     Variable = c('Import ratio', 'Stock-to-use ratio', 'Absolute yield deviation', 'Conflict', 'Vol. of real exchange rate'),
                     ID = 'HP')
VisDat2 <- data.frame(Coef = c(-0.1344, -0.3, 0.07, 0.0078, 0.04)*c(0.25, 0.09, 0.11, 0.17, 0.03)/0.13,
                      Sd = c(0.047, 0.066, 0.0299, 0.0084, 0.1313)*c(0.25, 0.09, 0.11, 0.17, 0.03)/0.13,
                      Variable = c('Import ratio', 'Stock-to-use ratio', 'Absolute yield deviation', 'Conflict', 'Vol. of real exchange rate'),
                      ID = 'Quadratic')

VisDat <- rbind(VisDat1, VisDat2)
font.size <- 12
g <-
  ggplot(data = VisDat) +
  geom_errorbar(aes(Variable, ymin = Coef - 1.96*Sd, ymax = Coef + 1.96*Sd, group = ID), width = 0.15, position = position_dodge(width = 0.3)) +
  geom_point(aes(Variable, Coef, group = ID), position = position_dodge(width = 0.3)) +
  geom_hline(yintercept = 0) +
  theme_classic() +
  labs(x = '', y = 'Coefficient estimates in elasticity terms') +
  scale_x_discrete(limits = c('Import ratio', 'Stock-to-use ratio', 'Absolute yield deviation', 'Conflict', 'Vol. of real exchange rate'),
                   labels = c('Import ratio', 'Beginning stock-\nto-use ratio', 'Absolute \n yield deviation',
                              'Social \nconflict', 'Variability of real \nexchange rate')) +
  scale_y_continuous(breaks = seq(-0.5, 0.2, by = 0.1), limits = c(-0.5, 0.2)) +
  theme(axis.text = element_text(color = 'black', face = 'plain', size = font.size),
        axis.title = element_text(color = 'black', face = 'plain', size = font.size),
        legend.text = element_text(color = 'black', face = 'plain', size = font.size)) +
  annotate(geom = "text", x = 1, y = -0.48, label = "Left: HP filter", fontface = 'italic', size = 2.5) +
  annotate(geom = "text", x = 1.22, y = -0.065, label = "Right: Quadratic", fontface = 'italic', size = 2.5)


ggsave('Fig1_CoefEstV2test.png', g, width = 170, height = 130, units = 'mm')



#-------------------------------------------------------------------------------------------------------------------#
# Plot the average data.
world <- map_data("world")
{
PriceVolDat <- RegDat1 %>%
  group_by(Country) %>%
  summarise(ImportRatio = mean(NetImportRatio)*100) %>%
  dplyr::mutate(ISO = countrycode(Country, 'country.name', 'iso3c'), Value = 1) %>%
  full_join(world, by = c('Country' = 'region')) %>%
  mutate(Value = ifelse(Value == 1, 'Select', 'Other'),
         ImportRatioRank = case_when(ImportRatio <= 10 ~ '1',
                                     ImportRatio > 10 & ImportRatio <= 80 ~ '2',
                                     ImportRatio > 80 ~ '3',
                                     TRUE ~ 'No Data'))

g1 <-
  ggplot() +
  geom_map(data=world, map=world,
           aes(group=group, map_id=region),
           fill="white", colour="#7f7f7f", size=0.5) +
  expand_limits(x=world$long, y=world$lat) +
  geom_map(data=PriceVolDat, map=world,
           aes(fill=ImportRatioRank, map_id=Country),
           colour="#7f7f7f", size=0.5) +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  theme_bw()+
  scale_fill_manual(values = c('1' = 'yellow', '2' = 'blue', '3' = 'red', 'No Data' = 'white'),
                    labels = c('1' = "< 0.1", '2' = "0.1 ~ 0.8", '3' = "> 0.8", 'No Data' = 'Not in sample')) +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        title = element_text(size = 9)) +
  labs(title = '(b) Average maize net import ratio (= net imports/domestic consumption) \nduring 2000-2015')

#--------------------------------------------------------------------------------------------------------------------------------#
## Average import ratios.
PriceVolDat <- RegDat1 %>%
  group_by(Country) %>%
  summarise(PriceVol = mean(PriceCV_MktYear)*100) %>%
  dplyr::mutate(ISO = countrycode(Country, 'country.name', 'iso3c'), Value = 1) %>%
  full_join(world, by = c('Country' = 'region')) %>%
  mutate(Value = ifelse(Value == 1, 'Select', 'Other'),
         PriceVolRank = case_when(PriceVol <= 6 ~ '1',
                                  PriceVol > 6 & PriceVol <= 12 ~ '2',
                                  PriceVol > 12 ~ '3',
                                  TRUE ~ 'No Data'))

# quantile(PriceVolDat$PriceVol,na.rm =T)
g2 <-
  ggplot() +
  geom_map(data=world, map=world,
           aes(group=group, map_id=region),
           fill="white", colour="#7f7f7f", size=0.5) +
  expand_limits(x = world$long, y = world$lat) +
  geom_map(data = PriceVolDat, map = world,
           aes(fill = PriceVolRank, map_id = Country),
           colour="#7f7f7f", size=0.5) +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  theme_bw() +
  scale_fill_manual(values = c('1' = 'yellow', '2' = 'blue', '3' = 'red', 'No Data' = 'white'),
                    labels = c('1' = "< 0.06", '2' = "0.06 ~ 0.12", '3' = "> 0.12", 'No Data' = 'Not in sample')) +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        title = element_text(size = 9))  +
  labs(title = '(a) Average country-level intra-annual CV of real monthly prices of \n maize during 2000-2015')

#-----------------------------------------------------------------------------------------------------------------------------#
## Average stock-to-use ratios.
world <- map_data("world")
PriceVolDat <- RegDat1 %>%
  group_by(Country) %>%
  summarise(StockRatio = mean(StockRatio)*100) %>%
  dplyr::mutate(ISO = countrycode(Country, 'country.name', 'iso3c'), Value = 1) %>%
  full_join(world, by = c('Country' = 'region')) %>%
  mutate(Value = ifelse(Value == 1, 'Select', 'Other'),
         PriceVolRank = case_when(StockRatio <= 8 ~ '1',
                                  StockRatio > 8 & StockRatio <= 11 ~ '2',
                                  StockRatio > 11 ~ '3',
                                  TRUE ~ 'No Data'))

# quantile(PriceVolDat$PriceVol,na.rm =T)
g3 <-
  ggplot() +
  geom_map(data=world, map=world,
           aes(group=group, map_id=region),
           fill="white", colour="#7f7f7f", size=0.5) +
  expand_limits(x = world$long, y = world$lat) +
  geom_map(data = PriceVolDat, map = world,
           aes(fill = PriceVolRank, map_id = Country),
           colour="#7f7f7f", size=0.5) +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  theme_bw() +
  scale_fill_manual(values = c('1' = 'yellow', '2' = 'blue', '3' = 'red', 'No Data' = 'white'),
                    labels = c('1' = "< 0.08", '2' = "0.08 ~ 0.11", '3' = "> 0.11", 'No Data' = 'Not in sample')) +
  labs(title = '(c) Average maize stock-to-use ratio (= stocks/domestic consumption) \nduring 2000-2015') +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        title = element_text(size = 9))
}

g <- grid.arrange(g2, g1, g3, ncol = 1)

ggsave('Fig_ImportRatio_PriceCVMap_D3_color.png', g, width = 160, height = 230, units = 'mm', dpi = 72)


#------------------------------------------------------------------------------------#
# Figure 1. Show the average import ratio and price variability.
#------------------------------------------------------------------------------------#
PriceVisDat <- RegDat1 %>%
  group_by(Country) %>%
  summarise(ImportRatio = mean(NetImportRatio),
            PriceVol = mean(PriceCV_MktYear)) %>%
  mutate(Region = countrycode(Country, 'country.name', 'continent'))

font.size = 12
g <-
  ggplot(data = PriceVisDat, aes(ImportRatio, PriceVol)) +
  geom_point(aes(shape = Region), size = 3) +
  scale_shape_manual(values = c(16, 3, 7),
                     name = '') +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 0.25, by = 0.05), limits = c(0, 0.25)) +
  geom_smooth(method='lm', se = FALSE, size = 1, col = 'black', alpha = 0.6) +
  theme_classic() +
  labs(x = 'Average maize net import ratio during 2000-2015', y = 'Average country-level intra-annual CV of real \nmonthly prices of maize during 2000-2015') +
  theme(legend.position = c(0.85, 0.9),
        legend.direction = 'vertical',
        legend.key.size = unit(1, "cm"),
        axis.text = element_text(color = 'black', face = 'plain', size = font.size),
        axis.title = element_text(color = 'black', face = 'plain', size = font.size),
        legend.text = element_text(color = 'black', face = 'plain', size = font.size)) +
  annotate('text', x = 0.05, y = 0.05, label = 'China') +
  annotate("segment", x = 0.05, xend = 0.026, y = 0.048, yend = 0.039, colour = "black", size = 0.6, arrow = arrow(length=unit(0.30,"cm"))) +
  annotate('text', x = 0.89, y = 0.095, label = 'Morocco') +
  annotate("segment", x = 0.89, xend = 0.91, y = 0.093, yend = 0.086, colour = "black", size = 0.6, arrow = arrow(length=unit(0.30,"cm")))

g

ggsave('Fig1_ImportRatio_PriceCV.png', g, width = 170, height = 130, units = 'mm', dpi = 72)
