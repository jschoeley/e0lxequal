# Comparision of lx Equality Measures -------------------------------------

# plot scale settings
x_breaks <- seq(10, 100, 10)
y_breaks <- c(0.05, 0.1, 0.2, 0.3, 0.5, 1)
x_lab    <- expression(italic(e[0]))
font     <- "Palatino"
colSeq <-  rainbow(262, alpha = 1)

# plot e0 against average lx (period, females and males)
plot_e0_vs_lxequality <-
  # main
  ggplot(e0_vs_lxequality_long, aes(x = e0, y = value)) +
  geom_point(alpha = 0.6, size = 0.5) +
  stat_smooth_eqn_lab(geom = "text", method = "lm",
                      xpos = 40, ypos = log(0.05),
                      hjust = 0, parse = TRUE, family = font, size = 4)  +
  geom_smooth(method = "lm", colour = rcpal$quacla[1]) +
  # scale
  scale_x_continuous(name = x_lab, breaks = x_breaks) +
  scale_y_continuous(name = "Lifespan Equality", trans = "log", breaks = y_breaks) +
  # facet
  facet_wrap(~ sex + measure, nrow = 4) +
  # guide
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1),
                               reverse = TRUE)) +
  coord_fixed(20) +
  # theme
  ggtheme_min(grid = "xy", base_family = font)

ExportPDF(plot_e0_vs_lxequality, "./out/plot_e0_vs_lx_equality.pdf",
          .width = 30, .height = 30)

# logit Average lx vs. Keyfitz' Entropy -----------------------------------

plot_logitavglx_vs_keyfentr <-
  ggplot(e0_vs_lxequality_wide, aes(x = logitlx, y = keyfentr)) +
  # main
  geom_point(alpha = 0.6, size = 0.5) +
  # annot
  geom_abline(intercept = 0, slope = 1, colour = "grey90") +
  # facet
  facet_wrap(~ sex) +
  # scale
  scale_y_continuous("Inverse Keyfitz' Entropy", breaks = seq(0.1, 1, 0.1)) +
  scale_x_continuous("logit Average l(x)", breaks = seq(0.1, 1, 0.1)) +
  # coord
  coord_equal() +
  # theme
  ggtheme_min(grid = "xy", base_family = font)

ExportPDF(plot_logitavglx_vs_keyfentr, "./out/plot_logitavglx_vs_keyfentr.pdf",
          .width = 25, .height = 20)

# logit MIRA vs. Keyfitz' Entropy -----------------------------------------

plot_mira_vs_keyfentr <-
  ggplot(e0_vs_lxequality_wide, aes(x = mira, y = keyfentr)) +
  # main
  geom_point(alpha = 0.6, size = 0.5) +
  # annot
  geom_abline(intercept = 0, slope = 1, colour = "grey90") +
  # facet
  facet_wrap(~ sex) +
  # scale
  scale_y_continuous("Inverse Keyfitz' Entropy", breaks = seq(0.1, 1, 0.1)) +
  scale_x_continuous("MIRA", breaks = seq(0.1, 1, 0.1)) +
  # coord
  coord_equal() +
  # theme
  ggtheme_min(grid = "xy", base_family = font)

ExportPDF(plot_mira_vs_keyfentr, "./out/plot_mira_vs_keyfentr.pdf",
          .width = 25, .height = 20)

# female e0 vs. Inverse Keyfitz' Entropy -----------------------------------
plot_eo_entr_fem <- ggplot(filter(e0_vs_lxequality_wide, sex == "Female"), aes(e0, keyfentr)) +
  # mai
  geom_point(alpha = 0.6, size = 0.5) +
  # scale
  scale_x_continuous(name = x_lab, breaks = x_breaks) +
  scale_y_continuous(name = "Lifespan Equality", trans = "log", breaks = y_breaks) +
  # guide
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1),
                               reverse = TRUE)) +
  coord_fixed(20)

ExportPDF(plot_eo_entr_fem, "./out/eo_entr_fem.pdf",
          .width = 25, .height = 20)

# Keyfitz entropy vs. e0 by country females with lm
plot_keyfitz_e0_cntry <-
  xyplot(keyfentr~e0|factor(cntry),
       groups = sex,
       grid = T,
       type = c("p", "r"), lwd = 2, alpha = 1/2, cex = 0.5,
       col.symbol=c(adjustcolor("#B22222", alpha.f = 0.5), adjustcolor("#1874CD", alpha.f = 0.5)),
       col.line = c("#B22222", "#1874CD"),
       xlab = "Life Expectancy",
       ylab = "Keyfitz' Entropy",
       strip = function(bg = 'white', ...)
         strip.default(bg = 'white', ...),
       data = e0w,
       scales=list(y=list(log=T, equispaced.log = FALSE)))

ExportPDF(plot_keyfitz_e0_cntry, "./out/eo_entr_fem.pdf",
          .width = 25, .height = 20)


# plot e0 against average lx (period, females and males)
plot_keyfitz_e0_cntry <-
  # main
  ggplot(filter(e0_vs_lxequality_wide, !cntry %in% c("BLR", "CHL", "LTU", "LVA", "RUS", "TWN", "UKR", "SVN")), aes(e0, keyfentr), group = sex) +
  geom_point(alpha = 0.6, size = 0.5, aes(colour = Year)) +
 # stat_smooth_eqn_lab(geom = "text", method = "lm",
  #                    xpos = 40, ypos = log(0.05),
  #                    hjust = 0, parse = TRUE, family = font, size = 4)  +
 # geom_smooth(method = "lm", colour = rcpal$quacla[1]) +
  # scale
  scale_x_continuous(name = x_lab, breaks = x_breaks) +
  scale_y_continuous(name = "Inverse Keyfitz' entropy (log scale)", trans = "log", breaks = y_breaks) +
  # facet
  facet_wrap(~ cntry, nrow = 6) +
  # guide
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1),
                               reverse = TRUE)) +
  coord_fixed(20) +
  # theme
  ggtheme_min(grid = "xy", base_family = font)

ExportPDF(plot_keyfitz_e0_cntry, "./out/eo_entr.pdf",
          .width = 25, .height = 20)


e0w$groupYearCntry <- paste(e0w$Year, e0w$cntry)
plot_keyfitz_e0_cntry_tadpoles <-
  # main
  ggplot(filter(e0w, (!cntry %in% c("BLR", "CHL", "LTU", "LVA", "RUS", "TWN", "UKR", "SVN")) & sex == "Female"), aes(e0, keyfentr)) +
  geom_point(alpha = 0.6, size = 1, aes(colour = Year)) +
  geom_line(data = filter(e0w, !cntry %in% c("BLR", "CHL", "LTU", "LVA", "RUS", "TWN", "UKR", "SVN")), aes(group = groupYearCntry, colour = Year), alpha = 0.6, size = 0.3) +
  # stat_smooth_eqn_lab(geom = "text", method = "lm",
  #                    xpos = 40, ypos = log(0.05),
  #                    hjust = 0, parse = TRUE, family = font, size = 4)  +
  # geom_smooth(method = "lm", colour = rcpal$quacla[1]) +
  # scale
  scale_x_continuous(name = x_lab, breaks = x_breaks) +
  scale_y_continuous(name = "Inverse Keyfitz' entropy (log scale)", trans = "log", breaks = y_breaks) +
  # facet
  facet_wrap(~ cntry, nrow = 6) +
  # guide
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1),
                               reverse = TRUE)) +
  coord_fixed(20) +
  # theme
  ggtheme_min(grid = "xy", base_family = font)

ExportPDF(plot_keyfitz_e0_cntry_tadpoles, "./out/eo_entr_tadpoles.pdf",
          .width = 25, .height = 20)

plot_keyfitz_e0_someCntry_tadpoles <-
  # main
  ggplot(filter(e0w, (cntry %in% c("JPN", "DNK", "BEL")) & sex == "Female"), aes(e0, keyfentr)) +
  geom_point(alpha = 0.6, size = 2.1, aes(colour = Year)) +
  geom_line(data = filter(e0w,  cntry %in% c("JPN", "DNK", "BEL")), aes(group = groupYearCntry, colour = Year), alpha = 0.6, size = 0.5) +
  # stat_smooth_eqn_lab(geom = "text", method = "lm",
  #                    xpos = 40, ypos = log(0.05),
  #                    hjust = 0, parse = TRUE, family = font, size = 4)  +
  # geom_smooth(method = "lm", colour = rcpal$quacla[1]) +
  # scale
  scale_x_continuous(name = x_lab, breaks = x_breaks) +
  scale_y_continuous(name = "Inverse Keyfitz' entropy (log scale)", trans = "log", breaks = y_breaks) +
  # facet
  facet_wrap(~ cntry, nrow = 1) +
  # guide
  guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1),
                               reverse = TRUE)) +
  coord_fixed(20) +
  # theme
  ggtheme_min(grid = "xy", base_family = font)

ExportPDF(plot_keyfitz_e0_someCntry_tadpoles, "./out/eo_entr_someC_tadpoles.pdf",
          .width = 35, .height = 10)

