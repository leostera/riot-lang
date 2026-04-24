type t = Kernel.System.Host.t = {
  architecture: string;
  vendor: string;
  os: string;
  abi: string option;
}

type error = Kernel.System.Host.error =
  | InvalidTripletFormat of {
      value: string;
    }

let current = Kernel.System.Host.current

let to_string = Kernel.System.Host.to_string

let error_message = Kernel.System.Host.error_message

let from_string = Kernel.System.Host.from_string

let equal = Kernel.System.Host.equal
