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
  keypoints <- ceiling(seq(1:75)/3)
  if (length(frame$people$pose_keypoints_2d) > 0) {
    first_loop = TRUE
    for (i in 1:length(frame$people$pose_keypoints_2d)) {
      values <- frame$people$pose_keypoints_2d[[i]]
      temp <- tibble(file_num = file_num, person = i, keypoints = keypoints, values = values, labs = labs)
      temp %>% pivot_wider(id_cols = c("file_num", "person", "keypoints"), names_from = labs, values_from = values)
      if (first_loop) {
        output <- temp
        first_loop <-  FALSE
      } else {
        output <- bind_rows(output, temp)
      }
    }
  } else {
    output <- tibble(file_num = file_num, person = NA, keypoint = NA, xloc = NA, yloc = NA, conf = NA)
  }
  return(output)
}

extract_keypoints(files[503])

map_dfr(files, ~ fromJSON(.x) %>% as_tibble(.$people) %>% mutate(fname = str_extract(.x,"\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d"), .before = version) %>% print)
