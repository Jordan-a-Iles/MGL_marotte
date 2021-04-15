# timestamp_correction.R
# Post-hoc timestamp correction for Marotte HS current meters
# Date created: 2021-04-15
# Author: Jordan Iles
# Contact: jordan.iles@jcu.edu.au

# Load required packages
require(tidyverse)
require(lubridate)


#### User input ----
# Select the raw file which needs the datetime corrected
raw_file <- file.choose()

# make note of the instrument time and computer time when connecting the 
# Marotte logger to MarotteHSconfig (https://www.marinegeophysics.com.au/software/)
# update the datetimes below:
instrument_time <- as_datetime('2000-03-08 04:15:08')
computer_time <- as_datetime('2021-04-14 14:40:26')


#### Extract the data from the TXT file ----
header <- tibble(read_lines(file = raw_file, 
                            n_max = 9))

dat <- read_csv(file = raw_file,
                col_types = cols(.default = col_double(),
                                 datetime = col_datetime(format = '%Y-%m-%d %H:%M:%OS')),
                skip = 9)

#### Make the datetime correction ----
corr <- int_length(interval(instrument_time, computer_time))
dat.corr <- dat %>% 
  mutate(datetime = datetime + seconds(corr))


#### prepare a new folder and filename for the corrected file ----
serial <- str_split_fixed(header[1,], ",", 2)[,2]

new_dir <- paste0(serial, '/',
                  format(min(dat.corr$datetime), format = '%Y%m%d'))
dir.create(path = paste('output', new_dir, sep = '/'),
           recursive = TRUE)

new_file <- paste0(sub('\\.TXT$', '', 
                       basename(raw_file), 
                       ignore.case = TRUE),
                   '_corrected.TXT')


#### Change the datetime (dttm format) back to original format----
dat.corr <- dat.corr %>% 
  mutate(datetime = format(datetime, format = "%Y-%m-%d %H:%M:%OS3"))


#### Write the corrected file ----
write_delim(x = header, 
            path = paste('output', new_dir, new_file, sep = '/'),
            delim = "",
            append = FALSE, 
            col_names = FALSE)

write_delim(x = dat.corr, 
            path = paste('output', new_dir, new_file, sep = '/'),
            append = TRUE,
            col_names = TRUE,
            delim = ",")

#### END


