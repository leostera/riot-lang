open Global

(** Grapheme break properties - simplified implementation for common cases
    
    This is a simplified implementation focused on the most critical grapheme
    cluster breaking scenarios for terminal applications:
    
    1. Combining marks (accents, diacritics)
    2. Zero-Width Joiner (ZWJ) sequences for emoji
    3. Regional indicators for flags  
    4. Hangul syllables (Korean)
    5. Extended pictographic (emoji)
    
    For full UAX #29 compliance, this would need ~2000 lines of property tables.
    This version handles ~90% of real-world cases.
*)
(** Grapheme break property types *)
type break_property =
  | Other
  (** Default - most characters *)
  | CR
  (** Carriage return U+000D *)
  | LF
  (** Line feed U+000A *)
  | Control
  (** Control characters *)
  | Extend
  (** Extending characters (combining marks, etc.) *)
  | ZWJ
  (** Zero-width joiner U+200D *)
  | Regional_Indicator
  (** Regional indicator symbols (U+1F1E6..U+1F1FF) for flags *)
  | Prepend
  (** Characters that prepend to graphemes *)
  | Spacing_Mark
  (** Spacing combining marks *)
  | L
  (** Hangul leading consonant (Jamo L) *)
  | V
  (** Hangul vowel (Jamo V) *)
  | T
  (** Hangul trailing consonant (Jamo T) *)
  | LV
  (** Hangul LV syllable *)
  | LVT
  (** Hangul LVT syllable *)
  | Extended_Pictographic
(** Emoji and pictographic characters *)

(** Check if a code point is an emoji modifier *)
let is_emoji_modifier = fun c -> c >= 0x1_f3fb && c <= 0x1_f3ff

(* Emoji skin tone modifiers *)
(** Check if a code point is a combining mark (simplified)
    Full implementation would use Unicode category Mn, Mc, Me *)
let is_combining = fun c -> let open Width_tables in in_table combining c || is_emoji_modifier c
(** Check if a code point is a control character *)
let is_control = fun c ->
  (c >= 0x0000 && c <= 0x001f)
  || (c >= 0x007f && c <= 0x009f)
  || (c = 0x00ad)
  || (c = 0x061c)
  || (c >= 0x180e && c <= 0x180e)
  || (c >= 0x200b && c <= 0x200f)
  || (c >= 0x2028 && c <= 0x202e)
  || (c >= 0x2060 && c <= 0x206f)
  || (c = 0xfeff)
  || (c >= 0xfff9 && c <= 0xfffb)

(* Interlinear annotation *)
(** Check if a code point is a prepend character *)
let is_prepend = fun c ->
  (* Simplified - includes Arabic/Hebrew formatting marks *)
  (c >= 0x0600 && c <= 0x0605)
  || (c = 0x06dd)
  || (c = 0x070f)
  || (c = 0x0890 || c = 0x0891)
  || (c = 0x08e2)

(* Arabic disputed end *)
(** Check if a code point is a spacing mark *)
let is_spacing_mark = fun c ->
  (* Simplified - common spacing combining marks *)
  (c >= 0x0903 && c <= 0x0903)
  || (c >= 0x093b && c <= 0x093b)
  || (c >= 0x093e && c <= 0x0940)
  || (c >= 0x0949 && c <= 0x094c)
  || (c >= 0x094e && c <= 0x094f)
  || (c >= 0x0982 && c <= 0x0983)
  || (c >= 0x09bf && c <= 0x09c0)
  || (c >= 0x09c7 && c <= 0x09c8)
  || (c >= 0x09cb && c <= 0x09cc)
  || (c >= 0x0a03 && c <= 0x0a03)
  || (c >= 0x0a3e && c <= 0x0a40)
  || (c >= 0x0a83 && c <= 0x0a83)

(* Gujarati visarga *)
(** Check if character is Hangul L (leading consonant) *)
let is_hangul_l = fun c -> c >= 0x1100 && c <= 0x115f
(** Check if character is Hangul V (vowel) *)
let is_hangul_v = fun c -> c >= 0x1160 && c <= 0x11a7
(** Check if character is Hangul T (trailing consonant) *)
let is_hangul_t = fun c -> c >= 0x11a8 && c <= 0x11ff
(** Check if character is Hangul LV syllable *)
let is_hangul_lv = fun c ->
  if c < 0xac00 || c > 0xd7a3 then
    false
  else
    let s_base = 0xac00 in
    let t_count = 28 in
    ((c - s_base) mod t_count) = 0
(** Check if character is Hangul LVT syllable *)
let is_hangul_lvt = fun c ->
  if c < 0xac00 || c > 0xd7a3 then
    false
  else
    not (is_hangul_lv c)
(** Check if character is Regional Indicator (for flag emoji) *)
let is_regional_indicator = fun c -> c >= 0x1_f1e6 && c <= 0x1_f1ff
(** Check if character is Extended Pictographic (emoji) *)
let is_extended_pictographic = fun c -> let open Width_tables in in_table emoji c
(** Get the grapheme break property for a code point *)
let get_break_property = fun c ->
  if c = 0x000d then
    CR
  else if c = 0x000a then
    LF
  else if c = 0x200d then
    ZWJ
  else if is_control c then
    Control
  else if is_combining c then
    Extend
  else if is_regional_indicator c then
    Regional_Indicator
  else if is_prepend c then
    Prepend
  else if is_spacing_mark c then
    Spacing_Mark
  else if is_hangul_l c then
    L
  else if is_hangul_v c then
    V
  else if is_hangul_t c then
    T
  else if is_hangul_lv c then
    LV
  else if is_hangul_lvt c then
    LVT
  else if is_extended_pictographic c then
    Extended_Pictographic
  else
    Other
(** Check if there should be a grapheme break between two code points
    
    This implements a simplified version of UAX #29 grapheme cluster boundaries.
    Returns true if a break is allowed, false if characters should cluster together.
    
    Key rules:
    - GB3: CR × LF (don't break between CR and LF)
    - GB4: (Control|CR|LF) ÷ (don't break before controls)
    - GB5: ÷ (Control|CR|LF) (break before controls)
    - GB6: L × (L|V|LV|LVT) (Hangul)
    - GB7: (LV|V) × (V|T)
    - GB8: (LVT|T) × T
    - GB9: × (Extend|ZWJ)  (don't break before extending chars or ZWJ)
    - GB9a: × SpacingMark
    - GB9b: Prepend ×
    - GB11: ExtendedPictographic Extend* ZWJ × ExtendedPictographic  (emoji ZWJ sequences)
    - GB12/13: Regional_Indicator × Regional_Indicator (flag pairs)
*)
let should_break = fun ~prev_prop ~curr_prop ~has_zwj ->
  match prev_prop, curr_prop with
  | CR, LF -> false
  | (Control | CR | LF), _ -> true
  | _, (Control | CR | LF) -> true
  | L, (L | V | LV | LVT) -> false
  | (LV | V), (V | T) -> false
  | (LVT | T), T -> false
  | _, (Extend | ZWJ) -> false
  | _, Spacing_Mark -> false
  | Prepend, _ -> false
  | Extended_Pictographic, Extended_Pictographic when has_zwj -> false
  | Regional_Indicator, Regional_Indicator -> false
  | _, _ -> true
