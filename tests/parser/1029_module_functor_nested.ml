module Outer (X : S) = struct
  module Inner (Y : T) = struct
    let x = X.value + Y.value
  end
end
