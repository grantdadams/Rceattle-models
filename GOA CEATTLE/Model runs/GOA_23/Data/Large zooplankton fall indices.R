# Extract variables we want
# ---------------------------------------
# Surface and bottom temperature
# ---------------------------------------
# -- 1) Large copepod
goa_NCa_ssp126_hind <- ssp126_biascorrected_hind %>%
  filter(varname == "NCa" & depthclass %in% c("Surface", "Bottom") & NMFS_AREA %in% c("610", "620", "630")) %>%
  mutate(simulation = "ssp126") %>%
  pivot_wider(values_from = c(value_dc, value), names_from = NMFS_AREA) %>%
  mutate(value_dc_610_to_630 = (value_dc_610 * 57225003746 + value_dc_620 * 62597059226 + value_dc_630 * 98582220025) / (57225003746 + 62597059226 + 98582220025)) # Take area weighted mean

goa_NCa_ssp245_hind <- ssp245_biascorrected_hind %>%
  filter(varname == "NCa" & depthclass %in% c("Surface", "Bottom") & NMFS_AREA %in% c("610", "620", "630")) %>% 
  mutate(simulation = "ssp245") %>%
  pivot_wider(values_from = c(value_dc, value), names_from = NMFS_AREA) %>%
  mutate(value_dc_610_to_630 = (value_dc_610 * 57225003746 + value_dc_620 * 62597059226 + value_dc_630 * 98582220025) / (57225003746 + 62597059226 + 98582220025)) # Take area weighted mean

goa_NCa_ssp585_hind <- ssp585_biascorrected_hind %>%
  filter(varname == "NCa" & depthclass %in% c("Surface", "Bottom") & NMFS_AREA %in% c("610", "620", "630")) %>% 
  mutate(simulation = "ssp585") %>%
  pivot_wider(values_from = c(value_dc, value), names_from = NMFS_AREA) %>%
  mutate(value_dc_610_to_630 = (value_dc_610 * 57225003746 + value_dc_620 * 62597059226 + value_dc_630 * 98582220025) / (57225003746 + 62597059226 + 98582220025)) # Take area weighted mean

goa_NCa_610_to_630_hind <- rbind(goa_NCa_ssp126_hind, goa_NCa_ssp245_hind, goa_NCa_ssp585_hind)
goa_NCa_610_to_630_hind <- goa_NCa_610_to_630_hind %>% mutate(hind = 'yes')


# -- 2) Large zoo-plankton
goa_mzl_ssp126_hind <- ssp126_biascorrected_hind %>%
  filter(varname == "MZL" & depthclass %in% c("Surface", "Bottom") & NMFS_AREA %in% c("610", "620", "630")) %>%
  mutate(simulation = "ssp126") %>%
  pivot_wider(values_from = c(value_dc, value), names_from = NMFS_AREA) %>%
  mutate(value_dc_610_to_630 = (value_dc_610 * 57225003746 + value_dc_620 * 62597059226 + value_dc_630 * 98582220025) / (57225003746 + 62597059226 + 98582220025)) # Take area weighted mean

goa_mzl_ssp245_hind <- ssp245_biascorrected_hind %>%
  filter(varname == "MZL" & depthclass %in% c("Surface", "Bottom") & NMFS_AREA %in% c("610", "620", "630")) %>% 
  mutate(simulation = "ssp245") %>%
  pivot_wider(values_from = c(value_dc, value), names_from = NMFS_AREA) %>%
  mutate(value_dc_610_to_630 = (value_dc_610 * 57225003746 + value_dc_620 * 62597059226 + value_dc_630 * 98582220025) / (57225003746 + 62597059226 + 98582220025)) # Take area weighted mean

goa_mzl_ssp585_hind <- ssp585_biascorrected_hind %>%
  filter(varname == "MZL" & depthclass %in% c("Surface", "Bottom") & NMFS_AREA %in% c("610", "620", "630")) %>% 
  mutate(simulation = "ssp585") %>%
  pivot_wider(values_from = c(value_dc, value), names_from = NMFS_AREA) %>%
  mutate(value_dc_610_to_630 = (value_dc_610 * 57225003746 + value_dc_620 * 62597059226 + value_dc_630 * 98582220025) / (57225003746 + 62597059226 + 98582220025)) # Take area weighted mean

goa_mzl_610_to_630_hind <- rbind(goa_mzl_ssp126_hind, goa_mzl_ssp245_hind, goa_mzl_ssp585_hind)
goa_mzl_610_to_630_hind <- goa_mzl_610_to_630_hind %>% mutate(hind = 'yes')


# -- Plot
ggplot(goa_NCa_610_to_630_hind, aes(date, value_dc_610_to_630, colour = simulation)) + geom_line() +
  ylab("SST (Celsius)") + xlab("Year") + facet_wrap(~simulation + hind + depthclass )

ggplot(goa_mzl_610_to_630_hind, aes(date, value_dc_610_to_630, colour = simulation)) + geom_line() +
  ylab("SST (Celsius)") + xlab("Year") + facet_wrap(~simulation + hind + depthclass )


# ---------------------------------------
# Calc mean winter temperature
# ---------------------------------------
goa_mzl_610_to_630_AugOct <- goa_mzl_610_to_630_hind %>%
  filter(month %in% c(7:10)) %>%
  group_by(year, depthclass, varname, simulation, hind) %>%
  summarise(mean_value_dc_610_to_630 = mean(value_dc_610_to_630)) %>% 
  mutate(varname = "Mean temp sept to dec") %>%
  arrange(depthclass, simulation, hind, year) %>%
  select(depthclass, hind, simulation, varname, year, mean_value_dc_610_to_630)

# -- Plot
ggplot(goa_mzl_610_to_630_AugOct %>% filter(depthclass == "Surface"), aes(year, mean_value_dc_610_to_630, colour = simulation)) + 
  geom_line() +
  ylab("SST (Celsius)") + 
  xlab("Year") + 
  facet_wrap(~simulation + hind)

ggplot(goa_mzl_610_to_630_AugOct %>% filter(depthclass == "Bottom"), aes(year, mean_value_dc_610_to_630, colour = simulation)) + 
  geom_line() +
  ylab("Bottom temp (Celsius)") + 
  xlab("Year") + 
  facet_wrap(~simulation + hind)

# -- Save
write.csv(goa_mzl_610_to_630_AugOct, 'Output/goa_large_zoo_610_to_630_fall_300M.csv', row.names = F)

