let connect fd addr =
  Unix.connect fd addr;
  Unix.setsockopt fd Unix.TCP_NODELAY true
