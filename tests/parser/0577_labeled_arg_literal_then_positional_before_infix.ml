let x = "    mean:       "
^ Time.Duration.to_secs_string ~precision:6 stats.mean
^ " +/- "
^ Time.Duration.to_secs_string ~precision:6 stats.std_dev
