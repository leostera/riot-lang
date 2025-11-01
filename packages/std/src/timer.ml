type id = Miniriot.Timer.id

let send_after pid msg ~after =
  let after_secs = Time.Duration.to_secs_float after in
  Miniriot.Timer.send_after pid msg ~after:after_secs

let send_interval pid msg ~interval =
  let interval_secs = Time.Duration.to_secs_float interval in
  Miniriot.Timer.send_interval pid msg ~interval:interval_secs

let cancel = Miniriot.Timer.cancel

let equal = Miniriot.Timer_id.equal
