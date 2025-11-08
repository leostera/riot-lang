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
  | Other            (** Default - most characters *)
  | CR               (** Carriage return U+000D *)
  | LF               (** Line feed U+000A *)
  | Control          (** Control characters *)
  | Extend           (** Extending characters (combining marks, etc.) *)
  | ZWJ              (** Zero-width joiner U+200D *)
  | Regional_Indicator  (** Regional indicator symbols (U+1F1E6..U+1F1FF) for flags *)
  | Prepend          (** Characters that prepend to graphemes *)
  | Spacing_Mark     (** Spacing combining marks *)
  | L                (** Hangul leading consonant (Jamo L) *)
  | V                (** Hangul vowel (Jamo V) *)
  | T                (** Hangul trailing consonant (Jamo T) *)
  | LV               (** Hangul LV syllable *)
  | LVT              (** Hangul LVT syllable *)
  | Extended_Pictographic  (** Emoji and pictographic characters *)

(** Check if a code point is an emoji modifier *)
let is_emoji_modifier c =
  c >= 0x1F3FB && c <= 0x1F3FF  (* Emoji skin tone modifiers *)

(** Check if a code point is a combining mark (simplified)
    Full implementation would use Unicode category Mn, Mc, Me *)
let is_combining c =
  let open Width_tables in
  in_table combining c || is_emoji_modifier c

(** Check if a code point is a control character *)
let is_control c =
  (c >= 0x0000 && c <= 0x001F) ||  (* C0 controls *)
  (c >= 0x007F && c <= 0x009F) ||  (* DEL and C1 controls *)
  (c = 0x00AD) ||                   (* Soft hyphen *)
  (c = 0x061C) ||                   (* Arabic letter mark *)
  (c >= 0x180E && c <= 0x180E) ||  (* Mongolian vowel separator *)
  (c >= 0x200B && c <= 0x200F) ||  (* Zero-width space, etc *)
  (c >= 0x2028 && c <= 0x202E) ||  (* Line/paragraph separator, etc *)
  (c >= 0x2060 && c <= 0x206F) ||  (* Word joiner, invisible operators, etc *)
  (c = 0xFEFF) ||                   (* Zero-width no-break space *)
  (c >= 0xFFF9 && c <= 0xFFFB)     (* Interlinear annotation *)

(** Check if a code point is a prepend character *)
let is_prepend c =
  (* Simplified - includes Arabic/Hebrew formatting marks *)
  (c >= 0x0600 && c <= 0x0605) ||  (* Arabic number signs *)
  (c = 0x06DD) ||                   (* Arabic end of ayah *)
  (c = 0x070F) ||                   (* Syriac abbreviation mark *)
  (c = 0x0890 || c = 0x0891) ||    (* Arabic pound mark *)
  (c = 0x08E2)                      (* Arabic disputed end *)

(** Check if a code point is a spacing mark *)
let is_spacing_mark c =
  (* Simplified - common spacing combining marks *)
  (c >= 0x0903 && c <= 0x0903) ||  (* Devanagari visarga *)
  (c >= 0x093B && c <= 0x093B) ||  (* Devanagari vowel sign ooe *)
  (c >= 0x093E && c <= 0x0940) ||  (* Devanagari vowel signs *)
  (c >= 0x0949 && c <= 0x094C) ||  (* Devanagari vowel signs *)
  (c >= 0x094E && c <= 0x094F) ||  (* Devanagari vowel signs *)
  (c >= 0x0982 && c <= 0x0983) ||  (* Bengali anusvara, visarga *)
  (c >= 0x09BF && c <= 0x09C0) ||  (* Bengali vowel signs *)
  (c >= 0x09C7 && c <= 0x09C8) ||  (* Bengali vowel signs *)
  (c >= 0x09CB && c <= 0x09CC) ||  (* Bengali vowel signs *)
  (c >= 0x0A03 && c <= 0x0A03) ||  (* Gurmukhi visarga *)
  (c >= 0x0A3E && c <= 0x0A40) ||  (* Gurmukhi vowel signs *)
  (c >= 0x0A83 && c <= 0x0A83)     (* Gujarati visarga *)

(** Check if character is Hangul L (leading consonant) *)
let is_hangul_l c =
  c >= 0x1100 && c <= 0x115F

(** Check if character is Hangul V (vowel) *)
let is_hangul_v c =
  c >= 0x1160 && c <= 0x11A7

(** Check if character is Hangul T (trailing consonant) *)
let is_hangul_t c =
  c >= 0x11A8 && c <= 0x11FF

(** Check if character is Hangul LV syllable *)
let is_hangul_lv c =
  if c < 0xAC00 || c > 0xD7A3 then false
  else
    let s_base = 0xAC00 in
    let t_count = 28 in
    ((c - s_base) mod t_count) = 0

(** Check if character is Hangul LVT syllable *)
let is_hangul_lvt c =
  if c < 0xAC00 || c > 0xD7A3 then false
  else not (is_hangul_lv c)

(** Check if character is Regional Indicator (for flag emoji) *)
let is_regional_indicator c =
  c >= 0x1F1E6 && c <= 0x1F1FF

(** Check if character is Extended Pictographic (emoji) *)
let is_extended_pictographic c =
  let open Width_tables in
  in_table emoji c

(** Get the grapheme break property for a code point *)
let get_break_property c =
  if c = 0x000D then CR
  else if c = 0x000A then LF
  else if c = 0x200D then ZWJ
  else if is_control c then Control
  else if is_combining c then Extend
  else if is_regional_indicator c then Regional_Indicator
  else if is_prepend c then Prepend
  else if is_spacing_mark c then Spacing_Mark
  else if is_hangul_l c then L
  else if is_hangul_v c then V
  else if is_hangul_t c then T
  else if is_hangul_lv c then LV
  else if is_hangul_lvt c then LVT
  else if is_extended_pictographic c then Extended_Pictographic
  else Other

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
let should_break ~prev_prop ~curr_prop ~has_zwj =
  match prev_prop, curr_prop with
  (* GB3: Don't break CR × LF *)
  | CR, LF -> false
  
  (* GB4: Break after controls *)
  | (Control | CR | LF), _ -> true
  
  (* GB5: Break before controls *)
  | _, (Control | CR | LF) -> true
  
  (* GB6: Hangul L × (L|V|LV|LVT) *)
  | L, (L | V | LV | LVT) -> false
  
  (* GB7: Hangul (LV|V) × (V|T) *)
  | (LV | V), (V | T) -> false
  
  (* GB8: Hangul (LVT|T) × T *)
  | (LVT | T), T -> false
  
  (* GB9: Don't break before Extend or ZWJ *)
  | _, (Extend | ZWJ) -> false
  
  (* GB9a: Don't break before SpacingMark *)
  | _, Spacing_Mark -> false
  
  (* GB9b: Don't break after Prepend *)
  | Prepend, _ -> false
  
  (* GB11: ExtPict Extend* ZWJ × ExtPict (emoji ZWJ sequences) *)
  | Extended_Pictographic, Extended_Pictographic when has_zwj -> false
  
  (* GB12/GB13: Regional_Indicator × Regional_Indicator (flags) *)
  | Regional_Indicator, Regional_Indicator -> false
  
  (* GB999: Otherwise break everywhere *)
  | _, _ -> true
