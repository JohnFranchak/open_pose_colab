library(tidyverse)
library(jsonlite)
library(here)

fpath <- "~/Google Drive/My Drive/videos/json"
files <- list.files(fpath, full.names = T)

# frame <- fromJSON(files[503])
# length(frame$people$pose_keypoints_2d)

extract_keypoints <- function(file_name) {
  frame <- fromJSON(file_name)
  file_num <- as.numeric(str_extract(file_name,"\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d"))
  labs <- rep(c("xloc","yloc","conf"),25)
  keypoints <- ceiling(seq(1:75)/3) - 1
  if (length(frame$people$pose_keypoints_2d) > 0) {
    first_loop = TRUE
    for (i in 1:length(frame$people$pose_keypoints_2d)) {
      values <- frame$people$pose_keypoints_2d[[i]]
      temp <- tibble(file_num = file_num, person = i, keypoints = keypoints, values = values, labs = labs)
      temp <- temp %>% pivot_wider(id_cols = c("file_num", "person", "keypoints"), names_from = labs, values_from = values)
      if (first_loop) {
        output <- temp
        first_loop <-  FALSE
      } else {
        output <- bind_rows(output, temp)
      }
    }
  } else {
    output <- tibble(file_num = file_num, person = NA, keypoints = NA, xloc = NA, yloc = NA, conf = NA)
  }
  return(output)
}

temp <- extract_keypoints(files[40])

walk <- map_dfr(files, ~ extract_keypoints(.x))

keypoint_lab <- c("Nose",
"Neck",
"RShoulder",
"RElbow",
"RWrist",
"LShoulder",
"LElbow",
"LWrist",
"MidHip",
"RHip",
"RKnee",
"RAnkle",
"LHip",
"LKnee",
"LAnkle",
"REye",
"LEye",
"REar",
"LEar",
"LBigToe",
"LSmallToe",
"LHeel",
"RBigToe",
"RSmallToe",
"RHeel")

walk <- walk %>% mutate(kp = factor(keypoints, levels = 0:24, labels = keypoint_lab),
                        yloc = yloc*-1) 

walk_filt <- walk %>% filter(!is.na(kp), conf > 0)

walk_filt %>% filter(kp == "Nose") %>% ggplot(aes(x = xloc, y = yloc)) + geom_point() + xlim(0,1920) + ylim(-1080, 0)
walk_filt %>% filter(kp == "RKnee") %>% ggplot(aes(x = xloc, y = loc, color = person)) + geom_point()


