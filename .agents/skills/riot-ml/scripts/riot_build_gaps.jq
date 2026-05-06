# Summarize the largest wall-clock gaps in newline-delimited Riot --json output.
#
# Usage:
#   riot build --all -x all --json \
#     | jq -r -s -f .agents/skills/riot-ml/scripts/riot_build_gaps.jq \
#     | column -t -s $'\t'

def event_time_us:
  .emitted_at_us
  // .created_at_us
  // .started_at_us
  // .planned_at_us
  // .completed_at_us
  // .finished_at_us
  // 0;

def rounded_ms:
  (. * 100 | round) / 100;

def event_package:
  .package.name?
  // .package?
  // .target?
  // .build_target?
  // "";

def event_label:
  if .type == "BuildPhase" then
    "phase " + (.phase // "unknown")
  elif .type == "BuildCompleted" then
    "completed " + ((event_package | tostring)) + " " + (.status // "")
  elif .type == "BuildingTarget" then
    "target " + (.target // "unknown")
  else
    (.type // "event")
  end;

sort_by(event_time_us) as $events
| if ($events | length) == 0 then
    empty
  else
    (["gap_ms", "elapsed_ms", "event"] | @tsv),
    ([
      range(0; $events | length) as $i
      | ($events[$i] | event_time_us) as $ts
      | ($events[0] | event_time_us) as $start
      | (if $i == 0 then $ts else ($events[$i - 1] | event_time_us) end) as $prev
      | {
          gap_ms: (($ts - $prev) / 1000 | rounded_ms),
          elapsed_ms: (($ts - $start) / 1000 | rounded_ms),
          event: ($events[$i] | event_label)
        }
    ]
    | sort_by(-.gap_ms)
    | .[0:40][]
    | [.gap_ms, .elapsed_ms, .event]
    | @tsv)
  end
