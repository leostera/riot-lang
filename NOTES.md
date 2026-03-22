# Macro ideas
some use cases for the macro rfd:
* include!(file)  -- basically drops the contents into the current file
* include_string!(file) -- puts the contents as a string
* include_bytes!(file) -- puts the contents as a bytes

* env!(var) -- string fetches env var at compile-time, and its compile-time error if it doesn't exist
* env!(var, default) -- same as above but returns default if not present

* quote!(code) -- macro quotation :) 

* dbg!(expr) -- hard one but basically print a debug representation of the value

* [@derive(...)] -- deriving macros ofc

* [@serde(...)] -- serde macros

* a set of macros for logging provided by std like info! debug! warn! error! trace! 

* todo!() macro -- panics saying that this is yet to be done
* unreachable!() -- panics saying this path should never have been executed
* panic!(msg) -- panics

* format!("...") -- returna a formatted String

* [@lint_rule] -- to declare a linting rule in a single function like

  [@lint_rule(id="e0001", hint="do not use stdlib!", explain=".....")]
  let no_stdlib tree = ....

  and voila that does all the plumbing for you

* package_name! module_name! function_name! loc! file! - and other context-level things that can be injected

