openStd
openPoneglyph
letschema_ns=(** Schema entities - these are just URIs for the schema namespace *)Uri.ns"schema"
letkind_type=Uri.make schema_ns;Uri.field"type" 
letdoc_attr=Uri.make schema_ns;Uri.field"doc" 
letused_on_attr=Uri.make schema_ns;Uri.field"used_on" 
letvalue_type_attr=Uri.make schema_ns;Uri.field"value_type" 
letstring_type=(** Value type entities *)Uri.make schema_ns;Uri.id"type/string" 
letint_type=Uri.make schema_ns;Uri.id"type/int" 
letbool_type=Uri.make schema_ns;Uri.id"type/bool" 
letfloat_type=Uri.make schema_ns;Uri.id"type/float" 
leturi_type=Uri.make schema_ns;Uri.id"type/uri" 
letdatetime_type=Uri.make schema_ns;Uri.id"type/datetime" 
letlist_typeinner_type=inner_type->Uri.make schema_ns;Uri.id"type/list:%s" Uri.to_stringinner_type  
letnamespacename=name->(** Namespace *)Uri.nsname
typedef=Uri.t*Fact.tlist
letkind(** Kind/Field are the same - both return Uri.t and a list of schema facts *)~nsname:def=(** Kind/Field are the same - both return Uri.t and a list of schema facts *)~nsname->leturi=Uri.make ns;Uri.kindname inletfacts= Fact.facturikind_type Value.String"kind"  in uri,facts 
letfield~nsname:def=~nsname->leturi=Uri.make ns;Uri.fieldname inletfacts= Fact.facturikind_type Value.String"field"  in uri,facts 
letdocdoc_str uri,facts =doc_str uri,facts ->letfact=(** Builder functions - they all work on def *)Fact.facturidoc_attr Value.Stringdoc_str in uri,fact::facts 
letused_on target_uri,_  uri,facts = target_uri,_  uri,facts ->letfact=Fact.facturiused_on_attr Value.Uritarget_uri in uri,fact::facts 
letvaluevalue_type_uri uri,facts =value_type_uri uri,facts ->letfact=Fact.facturivalue_type_attr Value.Urivalue_type_uri in uri,fact::facts 
moduleType=structletstring=(** Helpers for value types *)string_type
letint=int_type
letbool=bool_type
letfloat=float_type
leturi=uri_type
letdatetime=datetime_type
letlistinner=inner->list_typeinner
end
letregisterstoredefs=storedefs->letall_facts=(** Register schema facts in a store *)List.concat_map fun _,facts ->facts defsinPoneglyph.statestoreall_facts
letstring_value(** Fact builders using field definitions *)~field: field_uri,_ ~valueentity=(** Fact builders using field definitions *)~field: field_uri,_ ~valueentity->Fact.factentityfield_uri Value.Stringvalue 
letint_value~field: field_uri,_ ~valueentity=~field: field_uri,_ ~valueentity->Fact.factentityfield_uri Value.Intvalue 
letbool_value~field: field_uri,_ ~valueentity=~field: field_uri,_ ~valueentity->Fact.factentityfield_uri Value.Boolvalue 
letfloat_value~field: field_uri,_ ~valueentity=~field: field_uri,_ ~valueentity->Fact.factentityfield_uri Value.Floatvalue 
leturi_value~field: field_uri,_ ~valueentity=~field: field_uri,_ ~valueentity->Fact.factentityfield_uri Value.Urivalue 
letdatetime_value~field: field_uri,_ ~valueentity=~field: field_uri,_ ~valueentity->Fact.factentityfield_uri Value.DateTimevalue 
leturi_list_value~field: field_uri,_ ~valuesentity=~field: field_uri,_ ~valuesentity->letvalue_list=Value.List List.map funv->Value.Uriv values inFact.factentityfield_urivalue_list
letstring_list_value~field: field_uri,_ ~valuesentity=~field: field_uri,_ ~valuesentity->letvalue_list=Value.List List.map funv->Value.Stringv values inFact.factentityfield_urivalue_list
