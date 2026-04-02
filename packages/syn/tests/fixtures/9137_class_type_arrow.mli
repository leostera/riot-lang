class service:request -> object
  method run: int
end

class type factory = request -> response -> service

class type grouped = (request -> service)
