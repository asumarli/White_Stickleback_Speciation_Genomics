# Read in csv of PSMC time and Ne. This csv as made by taking the fist two columns of the .txt file output by PSMC. 
# The .csv has info from 4 individuals. "M" and "F" are sex. "CL" and "CB" are different populations/ecotypes.
dat<-read.csv("PSMC/combined_time_Ne.csv", header = TRUE)

#Plot
PSMC <- ggplot() + geom_step(data=dat, mapping=aes(x=MCB34_time, y=MCB34_Ne), color = "green4", linewidth = .7) +
  geom_step(data=dat, mapping=aes(x=MCB34_time_bs0, y=MCB34_Ne_bs0), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=MCB34_time_bs1, y=MCB34_Ne_bs1), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=MCB34_time_bs2, y=MCB34_Ne_bs2), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=MCB34_time_bs3, y=MCB34_Ne_bs3), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=MCB34_time_bs4, y=MCB34_Ne_bs4), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=MCB34_time_bs5, y=MCB34_Ne_bs5), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=MCB34_time_bs6, y=MCB34_Ne_bs6), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCB34_time_bs7), y=MCB34_Ne_bs7), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCB34_time_bs8), y=MCB34_Ne_bs8), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCB34_time_bs9), y=MCB34_Ne_bs9), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCB34_time_bs10), y=MCB34_Ne_bs10), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCB34_time_bs11), y=MCB34_Ne_bs11), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCB34_time_bs12), y=MCB34_Ne_bs12), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCB34_time_bs13), y=MCB34_Ne_bs13), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCB34_time_bs14), y=MCB34_Ne_bs14), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCB34_time_bs15), y=MCB34_Ne_bs15), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCB34_time_bs16), y=MCB34_Ne_bs16), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCB34_time_bs17), y=MCB34_Ne_bs17), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCB34_time_bs18), y=MCB34_Ne_bs18), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCB34_time_bs19), y=MCB34_Ne_bs19), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCB34_time_bs20), y=MCB34_Ne_bs20), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCB34_time_bs21), y=MCB34_Ne_bs21), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCB34_time_bs22), y=MCB34_Ne_bs22), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCB34_time_bs23), y=MCB34_Ne_bs23), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCB34_time_bs24), y=MCB34_Ne_bs24), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCB34_time_bs25), y=MCB34_Ne_bs25), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCB34_time_bs26), y=MCB34_Ne_bs26), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCB34_time_bs27), y=MCB34_Ne_bs27), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCB34_time_bs28), y=MCB34_Ne_bs28), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCB34_time_bs29), y=MCB34_Ne_bs29), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCB34_time_bs30), y=MCB34_Ne_bs30), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=MCL13_time, y=MCL13_Ne), color = "deepskyblue3", linewidth = 0.7) +
  geom_step(data=dat, mapping=aes(x=MCL13_time_bs0, y=MCL13_Ne_bs0), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=MCL13_time_bs1, y=MCL13_Ne_bs1), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=MCL13_time_bs2, y=MCL13_Ne_bs2), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=MCL13_time_bs3, y=MCL13_Ne_bs3), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=MCL13_time_bs4, y=MCL13_Ne_bs4), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=MCL13_time_bs5, y=MCL13_Ne_bs5), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=MCL13_time_bs6, y=MCL13_Ne_bs6), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCL13_time_bs7), y=MCL13_Ne_bs7), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCL13_time_bs8), y=MCL13_Ne_bs8), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCL13_time_bs9), y=MCL13_Ne_bs9), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCL13_time_bs10), y=MCL13_Ne_bs10), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCL13_time_bs11), y=MCL13_Ne_bs11), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCL13_time_bs12), y=MCL13_Ne_bs12), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCL13_time_bs13), y=MCL13_Ne_bs13), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCL13_time_bs14), y=MCL13_Ne_bs14), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCL13_time_bs15), y=MCL13_Ne_bs15), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCL13_time_bs16), y=MCL13_Ne_bs16), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCL13_time_bs17), y=MCL13_Ne_bs17), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCL13_time_bs18), y=MCL13_Ne_bs18), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCL13_time_bs19), y=MCL13_Ne_bs19), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCL13_time_bs20), y=MCL13_Ne_bs20), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCL13_time_bs21), y=MCL13_Ne_bs21), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCL13_time_bs22), y=MCL13_Ne_bs22), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCL13_time_bs23), y=MCL13_Ne_bs23), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCL13_time_bs24), y=MCL13_Ne_bs24), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCL13_time_bs25), y=MCL13_Ne_bs25), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCL13_time_bs26), y=MCL13_Ne_bs26), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCL13_time_bs27), y=MCL13_Ne_bs27), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCL13_time_bs28), y=MCL13_Ne_bs28), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCL13_time_bs29), y=MCL13_Ne_bs29), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(MCL13_time_bs30), y=MCL13_Ne_bs30), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=FCB66_time, y=FCB66_Ne), color = "green4", linewidth = 0.7) +
  geom_step(data=dat, mapping=aes(x=FCB66_time_bs0, y=FCB66_Ne_bs0), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=FCB66_time_bs1, y=FCB66_Ne_bs1), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=FCB66_time_bs2, y=FCB66_Ne_bs2), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=FCB66_time_bs3, y=FCB66_Ne_bs3), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=FCB66_time_bs4, y=FCB66_Ne_bs4), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=FCB66_time_bs5, y=FCB66_Ne_bs5), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=FCB66_time_bs6, y=FCB66_Ne_bs6), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCB66_time_bs7), y=FCB66_Ne_bs7), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCB66_time_bs8), y=FCB66_Ne_bs8), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCB66_time_bs9), y=FCB66_Ne_bs9), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCB66_time_bs10), y=FCB66_Ne_bs10), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCB66_time_bs11), y=FCB66_Ne_bs11), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCB66_time_bs12), y=FCB66_Ne_bs12), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCB66_time_bs13), y=FCB66_Ne_bs13), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCB66_time_bs14), y=FCB66_Ne_bs14), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCB66_time_bs15), y=FCB66_Ne_bs15), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCB66_time_bs16), y=FCB66_Ne_bs16), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCB66_time_bs17), y=FCB66_Ne_bs17), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCB66_time_bs18), y=FCB66_Ne_bs18), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCB66_time_bs19), y=FCB66_Ne_bs19), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCB66_time_bs20), y=FCB66_Ne_bs20), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCB66_time_bs21), y=FCB66_Ne_bs21), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCB66_time_bs22), y=FCB66_Ne_bs22), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCB66_time_bs23), y=FCB66_Ne_bs23), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCB66_time_bs24), y=FCB66_Ne_bs24), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCB66_time_bs25), y=FCB66_Ne_bs25), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCB66_time_bs26), y=FCB66_Ne_bs26), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCB66_time_bs27), y=FCB66_Ne_bs27), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCB66_time_bs28), y=FCB66_Ne_bs28), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCB66_time_bs29), y=FCB66_Ne_bs29), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCB66_time_bs30), y=FCB66_Ne_bs30), color = "green4", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=FCL112_time, y=FCL112_Ne), color = "deepskyblue3", linewidth = 0.7) +
  geom_step(data=dat, mapping=aes(x=FCL112_time_bs0, y=FCL112_Ne_bs0), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=FCL112_time_bs1, y=FCL112_Ne_bs1), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=FCL112_time_bs2, y=FCL112_Ne_bs2), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=FCL112_time_bs3, y=FCL112_Ne_bs3), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=FCL112_time_bs4, y=FCL112_Ne_bs4), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=FCL112_time_bs5, y=FCL112_Ne_bs5), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=FCL112_time_bs6, y=FCL112_Ne_bs6), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCL112_time_bs7), y=FCL112_Ne_bs7), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCL112_time_bs8), y=FCL112_Ne_bs8), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCL112_time_bs9), y=FCL112_Ne_bs9), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCL112_time_bs10), y=FCL112_Ne_bs10), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCL112_time_bs11), y=FCL112_Ne_bs11), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCL112_time_bs12), y=FCL112_Ne_bs12), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCL112_time_bs13), y=FCL112_Ne_bs13), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCL112_time_bs14), y=FCL112_Ne_bs14), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCL112_time_bs15), y=FCL112_Ne_bs15), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCL112_time_bs16), y=FCL112_Ne_bs16), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCL112_time_bs17), y=FCL112_Ne_bs17), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCL112_time_bs18), y=FCL112_Ne_bs18), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCL112_time_bs19), y=FCL112_Ne_bs19), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCL112_time_bs20), y=FCL112_Ne_bs20), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCL112_time_bs21), y=FCL112_Ne_bs21), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCL112_time_bs22), y=FCL112_Ne_bs22), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCL112_time_bs23), y=FCL112_Ne_bs23), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCL112_time_bs24), y=FCL112_Ne_bs24), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCL112_time_bs25), y=FCL112_Ne_bs25), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCL112_time_bs26), y=FCL112_Ne_bs26), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCL112_time_bs27), y=FCL112_Ne_bs27), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCL112_time_bs28), y=FCL112_Ne_bs28), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCL112_time_bs29), y=FCL112_Ne_bs29), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  geom_step(data=dat, mapping=aes(x=(FCL112_time_bs30), y=FCL112_Ne_bs30), color = "deepskyblue3", alpha = 0.15, size = 0.3) +
  xlab("\nYears From Present") + ylab("\nNe x 10,000") 

PSMC

#Ne is so large in recent time. Need to "crop" the y-axis so that we can visualize better. 
#Crop the y axis 
crop_y <- PSMC + coord_cartesian(xlim = c(0, 100000), ylim = c(0,5.5))
crop_y + theme_bw()
ggplotly(crop_y)

