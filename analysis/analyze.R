write_result = function(data, name) {
  write(data, paste0('./exports/numbers/', name, '.tex'))
}

library(tidyverse)
library(xtable)

xtable.opt.subtable = function() {
  options(xtable.type='latex',
          xtable.include.rownames=FALSE,
          xtable.floating.environment='subtable',
          xtable.table.placement='t')
}
xtable.opt.table = function() {
  options(xtable.type='latex',
          xtable.include.rownames=FALSE,
          xtable.floating.environment='table',
          xtable.table.placement='tb')
}

rows = nrow(read_tsv('exports/corpus2.tsv'))
write_result(rows, 'total-number-of-papers')
rows

counts = read_tsv('exports/corpus2.tsv') %>% count(fw_types)

linux_only = counts[counts$fw_types == 'linux', 'n'] %>% as.numeric()
write_result(linux_only, 'total-linux-only')

nonlinux_only = counts[counts$fw_types == 'non-linux', 'n'] %>% as.numeric()
write_result(nonlinux_only, 'total-nonlinux-only')

both = counts[counts$fw_types == 'linux,non-linux', 'n'] %>% as.numeric()
write_result(both, 'total-both-only')

nonlinux = both + nonlinux_only
write_result(nonlinux, 'total-not-only-linux')

sprintf("Linux only: %s\nNon-linux only: %s\nboth only: %s\nNot only Linux: %s", linux_only, nonlinux_only, both, nonlinux)

LARGE_SCALE_THRESHOLD = 500 # samples
write_result(LARGE_SCALE_THRESHOLD, 'const-large-scale-thresh')

t = read_tsv('exports/corpus2.tsv') %>%
  filter(n_analysed > LARGE_SCALE_THRESHOLD) %>%
  select(fw_types) %>%
  count(fw_types) %>%
  mutate(fw_types=case_match(fw_types,
                             'linux' ~ 'Linux',
                             'linux,non-linux' ~ 'Both',
                             'non-linux' ~ 'Non-Linux'))

large_scale_nlb = as.numeric(t[t$fw_types == 'Non-Linux', 'n'])
write_result(large_scale_nlb, 'non-linux-num-large-scale')

large_scale_lb = as.numeric(t[t$fw_types == 'Linux', 'n'])
write_result(large_scale_lb, 'linux-num-large-scale')

large_scale_both = as.numeric(t[t$fw_types == 'Both', 'n'])
write_result(large_scale_both, 'both-num-large-scale')

write_result(large_scale_nlb + large_scale_both, 'non-linux-and-both-num-large-scale')

sprintf("LB %d, NLB %d, Both %d", large_scale_lb, large_scale_nlb, large_scale_both)

res = read_tsv('exports/corpus2.tsv') %>%
  select(fw_types, n_analysed) %>%
  mutate(samples=case_when(
           n_analysed <= 10 ~ '<= 10',
           n_analysed > 10 & n_analysed <= 50 ~ '11-50',
           n_analysed > 50 & n_analysed <= 100 ~ '51-100',
           n_analysed > 100 & n_analysed <= 500 ~ '101-500',
           n_analysed > 500 ~ '> 500')) %>%
  count(fw_types, samples) %>%
  transmute(fw_types=case_match(fw_types, 'linux' ~ 'LB', 'linux,non-linux' ~ 'Both', 'non-linux' ~ 'NLB'), `Firmware samples`=samples, n=n) %>%
  pivot_wider(names_from=fw_types, values_from=n, values_fill=0) %>%
  arrange(factor(`Firmware samples`, levels=c('<= 10', '11-50', '51-100', '101-500', '> 500'))) %>%
  mutate(Total=LB+Both+NLB) %>%
  relocate(`Firmware samples`, 'LB', 'NLB', 'Both', 'Total')

xtable.opt.table()
xtable(res, caption='LB and NLB firmware studies by number of samples ($>$ 500 is considered large-scale).', label='tab:num-samples-binned')

counts = read_tsv('exports/corpus2.tsv') %>%
  filter(fw_types != 'linux') %>%
  select(item,isa) %>%
  separate_longer_delim(isa, ',') %>%
  mutate(isa=if_else(str_detect(isa, '^ARM'), 'ARM', isa)) %>%
  distinct(item,isa) %>%
  count(isa) %>%
  rename(`Architecture`=isa, `Count`=n)

xtable.opt.table()
xtable(counts, caption='Number of NLB firmware studies by CPU architecture.', label='tab:nlb-archs')

archs = read_tsv('exports/corpus2.tsv') %>%
  filter(fw_types != 'linux') %>%
  select(item,isa) %>%
  separate_longer_delim(isa, ',') %>%
  mutate(isa=if_else(str_detect(isa, '^ARM'), 'ARM', isa)) %>%
  distinct(item,isa) %>%
  count(item)

write_result(median(archs$n), 'median-num-nonlinux-archs')

read_tsv('exports/corpus2.tsv') %>%
  filter(fw_types == 'linux') %>%
  select(scraping_approaches) %>%
  separate_longer_delim(scraping_approaches, ',') %>%
  count(scraping_approaches) %>%
  mutate(scraping_approaches=case_match(
           scraping_approaches,
           'app-store' ~ 'App store',
           'custom-search' ~ 'Custom web search',
           'direct-approach' ~ 'Direct extraction',
           'existing-dataset' ~ 'Existing dataset',
           'own-generation' ~ 'Manual generation',
           'unknown' ~ 'Unknown',
           'user-submission' ~ 'User submission',
           'vendor-website' ~ 'Vendor website',
           'FTP' ~ 'FTP')) %>%
  rename(`Scraping approach`=scraping_approaches, `Counts`=n)

read_tsv('exports/corpus2.tsv') %>%
  separate_longer_delim(fw_types, ",") %>%
  filter(fw_types == 'non-linux') %>%
  select(scraping_approaches) %>%
  separate_longer_delim(scraping_approaches, ',') %>%
  count(scraping_approaches) %>%
  mutate(scraping_approaches=case_match(
           scraping_approaches,
           'app-store' ~ 'App store',
           'custom-search' ~ 'Custom web search',
           'direct-approach' ~ 'Direct extraction',
           'existing-dataset' ~ 'Existing dataset',
           'own-generation' ~ 'Manual generation',
           'unknown' ~ 'Unknown',
           'user-submission' ~ 'User submission',
           'vendor-website' ~ 'Vendor website',
           'FTP' ~ 'FTP')) %>%
  rename(`Scraping approach`=scraping_approaches, `Counts`=n)

device_nums = read_tsv('exports/corpus2.tsv') %>%
  separate_longer_delim(fw_types, ',') %>%
  filter(fw_types == 'non-linux') %>%
  select(devices_sector) %>%
  separate_longer_delim(devices_sector, ',') %>%
  count(devices_sector) %>%
  mutate(devices_sector=case_match(
           devices_sector,
           'IoT' ~ 'IoT',
           'computer-peripherals'~'Computer peripherals',
           'industrial'~'Industrial',
           'medical'~'Medical',
           'personal'~'Personal',
           'other'~'Other')) %>%
  rename(`Device sector`=devices_sector, Count=n)

xtable.opt.table()
xtable(device_nums, caption='Number of NLB studies by device sector.', label='tab:nlb-device-sectors')

res = read_tsv("exports/corpus2.tsv") %>%
  select(item, fw_types, analyses, whats_analysed) %>%
  separate_longer_delim(fw_types, ',') %>%
  filter(fw_types == 'non-linux') %>%
  filter(!is.na(analyses) & !is.na(whats_analysed)) %>%
  separate_longer_delim(whats_analysed, ',') %>%
  separate_longer_delim(analyses, ',') %>%
  count(analyses, whats_analysed) %>%
  pivot_wider(names_from=analyses, values_from=n, values_fill=0) %>%
  select(whats_analysed, static, dynamic, `symbolic-execution`, taint) %>%
  rename(`Analysis focus`=whats_analysed,
         Static=static, Dynamic=dynamic,
         `Symb. exec.`=`symbolic-execution`,
         Taint=taint) %>%
  mutate(`Analysis focus`=case_match(
           `Analysis focus`,
           "code" ~ "Code",
           "config" ~ "Configuration",
           "credentials" ~ "Credentials",
           "interfaces" ~ "Interfaces"))

xtable.opt.table()
xtable(res, caption='Number of studies of NLB firmware by analysis methods and analysis focus.', label='tab:nlb-analyses')
