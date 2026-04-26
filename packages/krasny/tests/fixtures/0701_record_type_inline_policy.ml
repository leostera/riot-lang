type small={left:int;right:string}

type policy={failure_threshold:int;reset_after:Time.Duration.t}

type pooled_connection={key:string;conn:Connection.t;mutable last_used_at:Time.Instant.t}
