# Download HMD Lifetables -------------------------------------------------

hmd_cntry <- getHMDcountries()

# female period
data_frame(cntry = hmd_cntry) %>%
  group_by(cntry) %>%
  do({
    readHMDweb(CNTRY = .$cntry,
               username = hmd_username,
               password = hmd_password,
               item = "fltper_1x1")
  }) -> hmd_f_lt_per

# male period
data_frame(cntry = hmd_cntry) %>%
  group_by(cntry) %>%
  do({
    readHMDweb(CNTRY = .$cntry,
               username = hmd_username,
               password = hmd_password,
               item = "mltper_1x1")
  }) -> hmd_m_lt_per

# save(hmd_f_lt_per, file = "./priv/data/hmd_f_lt_per.Rdata")
# save(hmd_m_lt_per, file = "./priv/data/hmd_m_lt_per.Rdata")
#
# load(file = "./priv/data/hmd_f_lt_per.Rdata")
# load(file = "./priv/data/hmd_m_lt_per.Rdata")
