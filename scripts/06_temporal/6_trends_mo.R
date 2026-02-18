source("1_Hist_Set.R")

#month trends... probs not for this
ggplot(EC_Date,
       aes(x = Month, y = TL)) +
  #facet_wrap( ~ Zone) +
  geom_point(aes(color = Spec_Known)) +
  geom_smooth(method = "loess", span = 0.5, color = "black") +
  scale_color_manual(values = species_colors)
#interesting! only CH, GC and BT acting green-y

#pups?

#pointing to historically different species assembledges

PZ <- EC_Dens %>% 
  dplyr::filter(Species == "P. zijsron" |
                  Species == "A. cuspidata / P. zijsron" |
                  Species == "Pristis sp." |
                  Species == "Pristidae")

highdens <- PZ %>% 
  dplyr::filter(Zone != "Cape York QLD" &
                  Zone != "Mid Coast NSW" )

ggplot(highdens,
       aes(x = Month, y = TL)) +
  geom_point(aes(color = Spec_Known)) +
  facet_wrap(~ Zone, scales = "free") +
  #central had evidence of smaller individuals, SEQ was a mix
  geom_smooth(method = "loess") #Evidence likely all greens!

#gotta look at pups!

