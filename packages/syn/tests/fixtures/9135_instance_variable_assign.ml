let counter =
  object
    val mutable count = 0
    method set = count <- next
  end
