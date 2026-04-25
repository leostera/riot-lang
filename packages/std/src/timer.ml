type id = Runtime.Timer.id

let send_after = fun pid msg ~after ->
  let after_secs = Time.Duration.to_secs_float after in Runtime.Timer.send_after pid msg ~after:after_secs

let send_interval = fun pid msg ~interval ->
  let interval_secs = Time.Duration.to_secs_float interval in Runtime.Timer.send_interval pid msg ~interval:interval_secs

let cancel = Runtime.Timer.cancel

let equal = Runtime.Timer_id.equal

let measure = fun f ->
  let start = Time.Instant.now () in
  let result = f () in
  let duration = Time.Instant.elapsed start in (result, duration)
