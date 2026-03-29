type suite_info = Intf.suite_info
module type Intf = Intf.Intf

module JUnit = Reporter_junit

module JSON = Reporter_json

module TAP = Reporter_tap

module Pretty = Reporter_pretty

module Minimal = Reporter_minimal
