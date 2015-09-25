### merge males and females
rbind(
  mutate(hmd_f_lt_per, sex = "Female"),
  mutate(hmd_m_lt_per, sex = "Male")
) -> hmd_lt_per

hmd_lt_per %>%
  # remove duplicate populations
  filter(
    # Exclude the German total population data as it overlaps with data for
    # east and west Germany but has the shorter timeline.
    cntry != "DEUTNP",
    # Exclude French civil population data as it overlaps with total population
    # data and most country data is only available for total populations anyway.
    cntry != "FRACNP",
    # Exclude New Zealand Maori and Non-Maori population data as it overlaps
    # with total population data.
    cntry != "NZL_MA",
    cntry != "NZL_NM",
    # Exclude England Wales civillian/total population, Scotland and
    # Northern Ireland total population as they overlap with U.K.
    # total population.
    cntry != "GBRTENW",
    cntry != "GBRCENW",
    cntry != "GBR_SCO",
    cntry != "GBR_NIR") %>%
  # compute lx equality measures and extract e0
  mutate(lx = lx / 100000) %>%
  filter(lx != 0) %>%
  group_by(cntry, sex, Year) %>%
  summarise(
    e0        = ex[1],
    e1        = ex[2],
    avglx     = Avglx(e0 = ex[1], lx = lx),
    ginilx    = InvGinilx(e0 = ex[1], lx = lx),
    logitlx   = Logitlx(e0 = ex[1], lx = lx),
    keyfentr  = InvKeyfEntr(e0 = ex[1], lx = lx),
    orr       = ORR(e0 = ex[1], omega = max(Age)),
    mira      = MIRA(lx = lx, x = Age),
    irr       = IRR(lx = lx, x = Age, omega = max(Age)),
    iqrlx     = InvIQRlx(lx = lx, x = Age)) %>%
  group_by(sex) %>%
  mutate(avglx    = NormalizeRange(avglx, na_rm = TRUE),
         ginilx   = NormalizeRange(ginilx, na_rm = TRUE),
         logitlx  = NormalizeRange(logitlx, na_rm = TRUE),
         keyfentr = NormalizeRange(keyfentr, na_rm = TRUE),
         orr      = NormalizeRange(orr, na_rm = TRUE),
         mira     = NormalizeRange(mira, na_rm = TRUE),
         irr      = NormalizeRange(irr, na_rm = TRUE),
         iqrlx    = NormalizeRange(iqrlx, na_rm = TRUE)) %>%
  ungroup() -> e0_vs_lxequality_wide -> e0w

# convert to long format
e0_vs_lxequality_wide %>%
  gather(key = measure, value = value, -c(1:5)) %>%
  mutate(measure = factor(measure,
                     levels = unique(measure),
                     labels = c("\nAverage l(x)",
                                "\nInverse Gini l(x)",
                                "\nlogit Average l(x)",
                                "\nInverse Keyfitz' Entropy",
                                "\nOuter Rectangularization",
                                "\nMIRA",
                                "\nInner Rectangularization",
                                "\nInverse Interquartile Range l(x)"))) -> e0_vs_lxequality_long -> e0l
