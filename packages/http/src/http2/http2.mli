(** HTTP/2 Protocol Implementation *)
module Frame: module type of Frame

module Parser: module type of Parser

module Parser_reader: module type of Parser_reader

module Serializer: module type of Serializer

module Hpack: module type of Hpack

module Hpack_reader: module type of Hpack_reader

module Connection: module type of Connection
