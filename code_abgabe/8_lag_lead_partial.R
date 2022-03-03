#***************************** 8_Lag_Lead_window  *****************************#
#* STEP 8
#* Description:
#* This script generates the visuals, which aid in the explanation of the 
#* lag-lead window and partial effects. 
#* 
#******************************************************************************#
#* Loading in the packages
source("1_packages.R")
#***************************** Models *****************************************#
## load main model and sensitivity model
m_2_A   <- readRDS("output/models/m_2_A.Rds")
m_2_B   <- readRDS("output/models/m_2_B.Rds")

################################################################################
## heat map for partial nutrition effects
# Some data preparation 
pd <- m_2_A $model[1,]
ints <- sort(unique(m_2_A $model$int_mid))
int_left <- floor(ints)
int_right <- ceiling(ints)
days <- 1:11
pd <- pd[rep(1, length(ints)*length(days)),]
pd$tz <- rep(days, times = length(ints))
pd$t <- rep(ints, each = length(days))
pd[["I(LL * proteinCat3)"]] <-
  pd[["I(LL * proteinCat2)"]] <- 1
preds <- mgcv::predict.gam(m_2_A , newdata = pd, type = "terms", se = TRUE)
use <- grep("protein", colnames(preds$fit))
fit <- preds$fit[, use]; colnames(fit) <-  c("C_III", "C_II")
hi <- fit + 2 * preds$se.fit[, use]; colnames(hi) <- paste0("hi_", colnames(fit))
lo <- fit - 2 * preds$se.fit[, use]; colnames(lo) <- paste0("lo_", colnames(fit))
window <- function(t, te) ifelse(t > te + 4 &
    floor(t) < te + 3 + 2*(te + 4), 1, NA)
tidy <- cbind(
    select(pd, t, tz) %>% rename(t = t, te = tz),
    fit, hi, lo) %>%
  gather(key, value, -t, -te)  %>%
  mutate(value = window(t, te) * value,
    key = factor(key,
      labels = c("es_C2", "es_C3", "hi_C2", "hi_C3", "lo_C2", "lo_C3"))) %>%
  separate(key, into = c("estimate", "C"), sep = 3) %>%
  mutate(estimate =
      factor(estimate, labels = c("estimate", "upper CI", "lower CI")))

#******************************* plotting *************************************#
## partial effects main model with color: 

tidy_fit <- filter(tidy, estimate == "estimate")

elra_heat_est <- ggplot(tidy_fit) +
  geom_vline(xintercept = int_right, lty=3)+
  geom_tile(aes(x=t, y=te, fill = value, col=value)) +
  facet_wrap(~ C, nrow = 1) +
  coord_cartesian(ylim = c(0.5,11.5), xlim = c(4, 30)) +
  scale_y_continuous(breaks = days, minor_breaks = NULL,
    name = expression("Protocol day "~t[z]), expand=c(0,0)) +
  scale_x_continuous(name = expression(t[j]),
                     minor_breaks = NULL,
                     breaks = seq(4, 28, by = 4) +.5,
                     expand = c(0,0)) +
  scale_fill_viridis_c(option="E", na.value="whitesmoke",
    name = expression(~hat(h)(t[j], t[z])),
    guide = guide_colourbar(direction = "vertical")) +
  scale_colour_gradient2(na.value = 'grey90', guide = "none") +
  theme(
    axis.text.x      = element_text(size = rel(1.4)),
    axis.text.y      = element_text(size = rel(1.4)),
    axis.title       = element_text(size = rel(1.5)),
    strip.text       = element_text(size = rel(1.5)),
    legend.text      = element_text(size = rel(1.4)),
    legend.title     = element_text(size = rel(1.5)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank())
ggsave("output/visuals/lag_lead_window/elra_heat_estimation.jpeg", elra_heat_est, width = 10, height = 4)

## partial effects main model no color: 
elra_heat_est_no_color <- ggplot(tidy_fit) +
  geom_vline(xintercept = int_right, lty=3)+
  geom_tile(aes(x=t, y=te, fill = value, color = value)) +
  facet_wrap(~ C, nrow = 1) +
  coord_cartesian(ylim = c(0.5,11.5), xlim= c(4, 30)) +
  scale_y_continuous(breaks = days, minor_breaks = NULL,
                     name = expression("Protocol day "~t[z]), expand=c(0,0)) +
  scale_x_continuous(name = expression(t[j]),
                     minor_breaks = NULL,
                     breaks = seq(4, 28, by = 4) +.5,
                     expand = c(0,0)) +
  scale_fill_gradient2(low = "darkgrey", high = "darkgrey") +
  scale_colour_gradient2(low =  "grey50", high = "grey50", na.value = 'grey50', guide = "none") +
  guides(fill="none")+
  theme(
    axis.text.x      = element_text(size = rel(1.4)),
    axis.text.y      = element_text(size = rel(1.4)),
    axis.title       = element_text(size = rel(1.5)),
    strip.text       = element_text(size = rel(1.5)),
    legend.text      = element_text(size = rel(1.4)),
    legend.title     = element_text(size = rel(1.5)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank())



ggsave("output/visuals/lag_lead_window/elra_heat_estimation_no_color.jpeg", elra_heat_est_no_color, width = 10, height = 4)
# windows
### partial effects main model example
for(time in 4:21) {
  ex1_time <- data.frame(
    ymin = c(1:11)-0.5,
    ymax = c(2:12)-0.5,
    C    = rep(c("C2", "C3"), times = c(5, 6)),
    xmin = rep(time, 11),
    xmax = rep(time +1, 11))

  elra_heat_ex_time <- elra_heat_est +
      geom_rect(data = ex1_time, aes(ymin = ymin, ymax = ymax, xmax = xmax, xmin = xmin),
    alpha = 0, fill = "white", col = "black", lwd = 1.2)

  ggsave(paste0("output/visuals/lag_lead_window/elra_heat_ex", time, ".jpg"), elra_heat_ex_time, width=10, height=4)

}
