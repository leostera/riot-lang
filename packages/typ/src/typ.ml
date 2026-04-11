module Model = Model
module Analysis = Analysis
module SourceAnalysis = SourceAnalysis
module ModulePairing = ModulePairing
module ModuleSurface = ModuleSurface
module Lower = Lower
module Infer = Infer
module Event = Event
module Diagnostics = Diagnostics
module MissingRequirements = MissingRequirements
module Query = Query
module Session = Session
module Store = Store
module Check = Check
module Config = TypConfig

type config = TypConfig.t
type source = Model.Source.t
type checked_source = Analysis.Check_result.t

let check = Check.check
