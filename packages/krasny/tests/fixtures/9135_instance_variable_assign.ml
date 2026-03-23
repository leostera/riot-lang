let counter =
  object
    val mutable count = 0
    method set next = count <- next
  end
