(* Auto-generated from Go's unicode/tables.go CaseRanges - DO NOT EDIT *)

(* Unicode Version: 15.0.0 *)

open Prelude
open Collections

(** Case range with deltas for case conversion *)
type case_delta =
  | Delta of int
  (** Simple delta to add *)
  | UpperLower

(** Special alternating upper/lower sequence *)
type case_range = {
  lo: int;  (** Start of range *)
  hi: int;  (** End of range *)
  to_upper: case_delta;  (** Delta to convert to uppercase *)
  to_lower: case_delta;  (** Delta to convert to lowercase *)
  to_title: case_delta;  (** Delta to convert to titlecase *)
}

(* ============================================ *)

(* Case conversion ranges                     *)

(* ============================================ *)

(** Table of case conversion ranges *)
let case_ranges = [|{
    lo = 0x0041;
    hi = 0x005a;
    to_upper = Delta 0;
    to_lower = Delta 32;
    to_title = Delta 0;
  }; {
    lo = 0x0061;
    hi = 0x007a;
    to_upper = Delta (-32);
    to_lower = Delta 0;
    to_title = Delta (-32);
  }; {
    lo = 0x00b5;
    hi = 0x00b5;
    to_upper = Delta 743;
    to_lower = Delta 0;
    to_title = Delta 743;
  }; {
    lo = 0x00c0;
    hi = 0x00d6;
    to_upper = Delta 0;
    to_lower = Delta 32;
    to_title = Delta 0;
  }; {
    lo = 0x00d8;
    hi = 0x00de;
    to_upper = Delta 0;
    to_lower = Delta 32;
    to_title = Delta 0;
  }; {
    lo = 0x00e0;
    hi = 0x00f6;
    to_upper = Delta (-32);
    to_lower = Delta 0;
    to_title = Delta (-32);
  }; {
    lo = 0x00f8;
    hi = 0x00fe;
    to_upper = Delta (-32);
    to_lower = Delta 0;
    to_title = Delta (-32);
  }; {
    lo = 0x00ff;
    hi = 0x00ff;
    to_upper = Delta 121;
    to_lower = Delta 0;
    to_title = Delta 121;
  }; {
    lo = 0x0100;
    hi = 0x012f;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x0130;
    hi = 0x0130;
    to_upper = Delta 0;
    to_lower = Delta (-199);
    to_title = Delta 0;
  }; {
    lo = 0x0131;
    hi = 0x0131;
    to_upper = Delta (-232);
    to_lower = Delta 0;
    to_title = Delta (-232);
  }; {
    lo = 0x0132;
    hi = 0x0137;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x0139;
    hi = 0x0148;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x014a;
    hi = 0x0177;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x0178;
    hi = 0x0178;
    to_upper = Delta 0;
    to_lower = Delta (-121);
    to_title = Delta 0;
  }; {
    lo = 0x0179;
    hi = 0x017e;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x017f;
    hi = 0x017f;
    to_upper = Delta (-300);
    to_lower = Delta 0;
    to_title = Delta (-300);
  }; {
    lo = 0x0180;
    hi = 0x0180;
    to_upper = Delta 195;
    to_lower = Delta 0;
    to_title = Delta 195;
  }; {
    lo = 0x0181;
    hi = 0x0181;
    to_upper = Delta 0;
    to_lower = Delta 210;
    to_title = Delta 0;
  }; {
    lo = 0x0182;
    hi = 0x0185;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x0186;
    hi = 0x0186;
    to_upper = Delta 0;
    to_lower = Delta 206;
    to_title = Delta 0;
  }; {
    lo = 0x0187;
    hi = 0x0188;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x0189;
    hi = 0x018a;
    to_upper = Delta 0;
    to_lower = Delta 205;
    to_title = Delta 0;
  }; {
    lo = 0x018b;
    hi = 0x018c;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x018e;
    hi = 0x018e;
    to_upper = Delta 0;
    to_lower = Delta 79;
    to_title = Delta 0;
  }; {
    lo = 0x018f;
    hi = 0x018f;
    to_upper = Delta 0;
    to_lower = Delta 202;
    to_title = Delta 0;
  }; {
    lo = 0x0190;
    hi = 0x0190;
    to_upper = Delta 0;
    to_lower = Delta 203;
    to_title = Delta 0;
  }; {
    lo = 0x0191;
    hi = 0x0192;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x0193;
    hi = 0x0193;
    to_upper = Delta 0;
    to_lower = Delta 205;
    to_title = Delta 0;
  }; {
    lo = 0x0194;
    hi = 0x0194;
    to_upper = Delta 0;
    to_lower = Delta 207;
    to_title = Delta 0;
  }; {
    lo = 0x0195;
    hi = 0x0195;
    to_upper = Delta 97;
    to_lower = Delta 0;
    to_title = Delta 97;
  }; {
    lo = 0x0196;
    hi = 0x0196;
    to_upper = Delta 0;
    to_lower = Delta 211;
    to_title = Delta 0;
  }; {
    lo = 0x0197;
    hi = 0x0197;
    to_upper = Delta 0;
    to_lower = Delta 209;
    to_title = Delta 0;
  }; {
    lo = 0x0198;
    hi = 0x0199;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x019a;
    hi = 0x019a;
    to_upper = Delta 163;
    to_lower = Delta 0;
    to_title = Delta 163;
  }; {
    lo = 0x019c;
    hi = 0x019c;
    to_upper = Delta 0;
    to_lower = Delta 211;
    to_title = Delta 0;
  }; {
    lo = 0x019d;
    hi = 0x019d;
    to_upper = Delta 0;
    to_lower = Delta 213;
    to_title = Delta 0;
  }; {
    lo = 0x019e;
    hi = 0x019e;
    to_upper = Delta 130;
    to_lower = Delta 0;
    to_title = Delta 130;
  }; {
    lo = 0x019f;
    hi = 0x019f;
    to_upper = Delta 0;
    to_lower = Delta 214;
    to_title = Delta 0;
  }; {
    lo = 0x01a0;
    hi = 0x01a5;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x01a6;
    hi = 0x01a6;
    to_upper = Delta 0;
    to_lower = Delta 218;
    to_title = Delta 0;
  }; {
    lo = 0x01a7;
    hi = 0x01a8;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x01a9;
    hi = 0x01a9;
    to_upper = Delta 0;
    to_lower = Delta 218;
    to_title = Delta 0;
  }; {
    lo = 0x01ac;
    hi = 0x01ad;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x01ae;
    hi = 0x01ae;
    to_upper = Delta 0;
    to_lower = Delta 218;
    to_title = Delta 0;
  }; {
    lo = 0x01af;
    hi = 0x01b0;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x01b1;
    hi = 0x01b2;
    to_upper = Delta 0;
    to_lower = Delta 217;
    to_title = Delta 0;
  }; {
    lo = 0x01b3;
    hi = 0x01b6;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x01b7;
    hi = 0x01b7;
    to_upper = Delta 0;
    to_lower = Delta 219;
    to_title = Delta 0;
  }; {
    lo = 0x01b8;
    hi = 0x01b9;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x01bc;
    hi = 0x01bd;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x01bf;
    hi = 0x01bf;
    to_upper = Delta 56;
    to_lower = Delta 0;
    to_title = Delta 56;
  }; {
    lo = 0x01c4;
    hi = 0x01c4;
    to_upper = Delta 0;
    to_lower = Delta 2;
    to_title = Delta 1;
  }; {
    lo = 0x01c5;
    hi = 0x01c5;
    to_upper = Delta (-1);
    to_lower = Delta 1;
    to_title = Delta 0;
  }; {
    lo = 0x01c6;
    hi = 0x01c6;
    to_upper = Delta (-2);
    to_lower = Delta 0;
    to_title = Delta (-1);
  }; {
    lo = 0x01c7;
    hi = 0x01c7;
    to_upper = Delta 0;
    to_lower = Delta 2;
    to_title = Delta 1;
  }; {
    lo = 0x01c8;
    hi = 0x01c8;
    to_upper = Delta (-1);
    to_lower = Delta 1;
    to_title = Delta 0;
  }; {
    lo = 0x01c9;
    hi = 0x01c9;
    to_upper = Delta (-2);
    to_lower = Delta 0;
    to_title = Delta (-1);
  }; {
    lo = 0x01ca;
    hi = 0x01ca;
    to_upper = Delta 0;
    to_lower = Delta 2;
    to_title = Delta 1;
  }; {
    lo = 0x01cb;
    hi = 0x01cb;
    to_upper = Delta (-1);
    to_lower = Delta 1;
    to_title = Delta 0;
  }; {
    lo = 0x01cc;
    hi = 0x01cc;
    to_upper = Delta (-2);
    to_lower = Delta 0;
    to_title = Delta (-1);
  }; {
    lo = 0x01cd;
    hi = 0x01dc;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x01dd;
    hi = 0x01dd;
    to_upper = Delta (-79);
    to_lower = Delta 0;
    to_title = Delta (-79);
  }; {
    lo = 0x01de;
    hi = 0x01ef;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x01f1;
    hi = 0x01f1;
    to_upper = Delta 0;
    to_lower = Delta 2;
    to_title = Delta 1;
  }; {
    lo = 0x01f2;
    hi = 0x01f2;
    to_upper = Delta (-1);
    to_lower = Delta 1;
    to_title = Delta 0;
  }; {
    lo = 0x01f3;
    hi = 0x01f3;
    to_upper = Delta (-2);
    to_lower = Delta 0;
    to_title = Delta (-1);
  }; {
    lo = 0x01f4;
    hi = 0x01f5;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x01f6;
    hi = 0x01f6;
    to_upper = Delta 0;
    to_lower = Delta (-97);
    to_title = Delta 0;
  }; {
    lo = 0x01f7;
    hi = 0x01f7;
    to_upper = Delta 0;
    to_lower = Delta (-56);
    to_title = Delta 0;
  }; {
    lo = 0x01f8;
    hi = 0x021f;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x0220;
    hi = 0x0220;
    to_upper = Delta 0;
    to_lower = Delta (-130);
    to_title = Delta 0;
  }; {
    lo = 0x0222;
    hi = 0x0233;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x023a;
    hi = 0x023a;
    to_upper = Delta 0;
    to_lower = Delta 10_795;
    to_title = Delta 0;
  }; {
    lo = 0x023b;
    hi = 0x023c;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x023d;
    hi = 0x023d;
    to_upper = Delta 0;
    to_lower = Delta (-163);
    to_title = Delta 0;
  }; {
    lo = 0x023e;
    hi = 0x023e;
    to_upper = Delta 0;
    to_lower = Delta 10_792;
    to_title = Delta 0;
  }; {
    lo = 0x023f;
    hi = 0x0240;
    to_upper = Delta 10_815;
    to_lower = Delta 0;
    to_title = Delta 10_815;
  }; {
    lo = 0x0241;
    hi = 0x0242;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x0243;
    hi = 0x0243;
    to_upper = Delta 0;
    to_lower = Delta (-195);
    to_title = Delta 0;
  }; {
    lo = 0x0244;
    hi = 0x0244;
    to_upper = Delta 0;
    to_lower = Delta 69;
    to_title = Delta 0;
  }; {
    lo = 0x0245;
    hi = 0x0245;
    to_upper = Delta 0;
    to_lower = Delta 71;
    to_title = Delta 0;
  }; {
    lo = 0x0246;
    hi = 0x024f;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x0250;
    hi = 0x0250;
    to_upper = Delta 10_783;
    to_lower = Delta 0;
    to_title = Delta 10_783;
  }; {
    lo = 0x0251;
    hi = 0x0251;
    to_upper = Delta 10_780;
    to_lower = Delta 0;
    to_title = Delta 10_780;
  }; {
    lo = 0x0252;
    hi = 0x0252;
    to_upper = Delta 10_782;
    to_lower = Delta 0;
    to_title = Delta 10_782;
  }; {
    lo = 0x0253;
    hi = 0x0253;
    to_upper = Delta (-210);
    to_lower = Delta 0;
    to_title = Delta (-210);
  }; {
    lo = 0x0254;
    hi = 0x0254;
    to_upper = Delta (-206);
    to_lower = Delta 0;
    to_title = Delta (-206);
  }; {
    lo = 0x0256;
    hi = 0x0257;
    to_upper = Delta (-205);
    to_lower = Delta 0;
    to_title = Delta (-205);
  }; {
    lo = 0x0259;
    hi = 0x0259;
    to_upper = Delta (-202);
    to_lower = Delta 0;
    to_title = Delta (-202);
  }; {
    lo = 0x025b;
    hi = 0x025b;
    to_upper = Delta (-203);
    to_lower = Delta 0;
    to_title = Delta (-203);
  }; {
    lo = 0x025c;
    hi = 0x025c;
    to_upper = Delta 42_319;
    to_lower = Delta 0;
    to_title = Delta 42_319;
  }; {
    lo = 0x0260;
    hi = 0x0260;
    to_upper = Delta (-205);
    to_lower = Delta 0;
    to_title = Delta (-205);
  }; {
    lo = 0x0261;
    hi = 0x0261;
    to_upper = Delta 42_315;
    to_lower = Delta 0;
    to_title = Delta 42_315;
  }; {
    lo = 0x0263;
    hi = 0x0263;
    to_upper = Delta (-207);
    to_lower = Delta 0;
    to_title = Delta (-207);
  }; {
    lo = 0x0265;
    hi = 0x0265;
    to_upper = Delta 42_280;
    to_lower = Delta 0;
    to_title = Delta 42_280;
  }; {
    lo = 0x0266;
    hi = 0x0266;
    to_upper = Delta 42_308;
    to_lower = Delta 0;
    to_title = Delta 42_308;
  }; {
    lo = 0x0268;
    hi = 0x0268;
    to_upper = Delta (-209);
    to_lower = Delta 0;
    to_title = Delta (-209);
  }; {
    lo = 0x0269;
    hi = 0x0269;
    to_upper = Delta (-211);
    to_lower = Delta 0;
    to_title = Delta (-211);
  }; {
    lo = 0x026a;
    hi = 0x026a;
    to_upper = Delta 42_308;
    to_lower = Delta 0;
    to_title = Delta 42_308;
  }; {
    lo = 0x026b;
    hi = 0x026b;
    to_upper = Delta 10_743;
    to_lower = Delta 0;
    to_title = Delta 10_743;
  }; {
    lo = 0x026c;
    hi = 0x026c;
    to_upper = Delta 42_305;
    to_lower = Delta 0;
    to_title = Delta 42_305;
  }; {
    lo = 0x026f;
    hi = 0x026f;
    to_upper = Delta (-211);
    to_lower = Delta 0;
    to_title = Delta (-211);
  }; {
    lo = 0x0271;
    hi = 0x0271;
    to_upper = Delta 10_749;
    to_lower = Delta 0;
    to_title = Delta 10_749;
  }; {
    lo = 0x0272;
    hi = 0x0272;
    to_upper = Delta (-213);
    to_lower = Delta 0;
    to_title = Delta (-213);
  }; {
    lo = 0x0275;
    hi = 0x0275;
    to_upper = Delta (-214);
    to_lower = Delta 0;
    to_title = Delta (-214);
  }; {
    lo = 0x027d;
    hi = 0x027d;
    to_upper = Delta 10_727;
    to_lower = Delta 0;
    to_title = Delta 10_727;
  }; {
    lo = 0x0280;
    hi = 0x0280;
    to_upper = Delta (-218);
    to_lower = Delta 0;
    to_title = Delta (-218);
  }; {
    lo = 0x0282;
    hi = 0x0282;
    to_upper = Delta 42_307;
    to_lower = Delta 0;
    to_title = Delta 42_307;
  }; {
    lo = 0x0283;
    hi = 0x0283;
    to_upper = Delta (-218);
    to_lower = Delta 0;
    to_title = Delta (-218);
  }; {
    lo = 0x0287;
    hi = 0x0287;
    to_upper = Delta 42_282;
    to_lower = Delta 0;
    to_title = Delta 42_282;
  }; {
    lo = 0x0288;
    hi = 0x0288;
    to_upper = Delta (-218);
    to_lower = Delta 0;
    to_title = Delta (-218);
  }; {
    lo = 0x0289;
    hi = 0x0289;
    to_upper = Delta (-69);
    to_lower = Delta 0;
    to_title = Delta (-69);
  }; {
    lo = 0x028a;
    hi = 0x028b;
    to_upper = Delta (-217);
    to_lower = Delta 0;
    to_title = Delta (-217);
  }; {
    lo = 0x028c;
    hi = 0x028c;
    to_upper = Delta (-71);
    to_lower = Delta 0;
    to_title = Delta (-71);
  }; {
    lo = 0x0292;
    hi = 0x0292;
    to_upper = Delta (-219);
    to_lower = Delta 0;
    to_title = Delta (-219);
  }; {
    lo = 0x029d;
    hi = 0x029d;
    to_upper = Delta 42_261;
    to_lower = Delta 0;
    to_title = Delta 42_261;
  }; {
    lo = 0x029e;
    hi = 0x029e;
    to_upper = Delta 42_258;
    to_lower = Delta 0;
    to_title = Delta 42_258;
  }; {
    lo = 0x0345;
    hi = 0x0345;
    to_upper = Delta 84;
    to_lower = Delta 0;
    to_title = Delta 84;
  }; {
    lo = 0x0370;
    hi = 0x0373;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x0376;
    hi = 0x0377;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x037b;
    hi = 0x037d;
    to_upper = Delta 130;
    to_lower = Delta 0;
    to_title = Delta 130;
  }; {
    lo = 0x037f;
    hi = 0x037f;
    to_upper = Delta 0;
    to_lower = Delta 116;
    to_title = Delta 0;
  }; {
    lo = 0x0386;
    hi = 0x0386;
    to_upper = Delta 0;
    to_lower = Delta 38;
    to_title = Delta 0;
  }; {
    lo = 0x0388;
    hi = 0x038a;
    to_upper = Delta 0;
    to_lower = Delta 37;
    to_title = Delta 0;
  }; {
    lo = 0x038c;
    hi = 0x038c;
    to_upper = Delta 0;
    to_lower = Delta 64;
    to_title = Delta 0;
  }; {
    lo = 0x038e;
    hi = 0x038f;
    to_upper = Delta 0;
    to_lower = Delta 63;
    to_title = Delta 0;
  }; {
    lo = 0x0391;
    hi = 0x03a1;
    to_upper = Delta 0;
    to_lower = Delta 32;
    to_title = Delta 0;
  }; {
    lo = 0x03a3;
    hi = 0x03ab;
    to_upper = Delta 0;
    to_lower = Delta 32;
    to_title = Delta 0;
  }; {
    lo = 0x03ac;
    hi = 0x03ac;
    to_upper = Delta (-38);
    to_lower = Delta 0;
    to_title = Delta (-38);
  }; {
    lo = 0x03ad;
    hi = 0x03af;
    to_upper = Delta (-37);
    to_lower = Delta 0;
    to_title = Delta (-37);
  }; {
    lo = 0x03b1;
    hi = 0x03c1;
    to_upper = Delta (-32);
    to_lower = Delta 0;
    to_title = Delta (-32);
  }; {
    lo = 0x03c2;
    hi = 0x03c2;
    to_upper = Delta (-31);
    to_lower = Delta 0;
    to_title = Delta (-31);
  }; {
    lo = 0x03c3;
    hi = 0x03cb;
    to_upper = Delta (-32);
    to_lower = Delta 0;
    to_title = Delta (-32);
  }; {
    lo = 0x03cc;
    hi = 0x03cc;
    to_upper = Delta (-64);
    to_lower = Delta 0;
    to_title = Delta (-64);
  }; {
    lo = 0x03cd;
    hi = 0x03ce;
    to_upper = Delta (-63);
    to_lower = Delta 0;
    to_title = Delta (-63);
  }; {
    lo = 0x03cf;
    hi = 0x03cf;
    to_upper = Delta 0;
    to_lower = Delta 8;
    to_title = Delta 0;
  }; {
    lo = 0x03d0;
    hi = 0x03d0;
    to_upper = Delta (-62);
    to_lower = Delta 0;
    to_title = Delta (-62);
  }; {
    lo = 0x03d1;
    hi = 0x03d1;
    to_upper = Delta (-57);
    to_lower = Delta 0;
    to_title = Delta (-57);
  }; {
    lo = 0x03d5;
    hi = 0x03d5;
    to_upper = Delta (-47);
    to_lower = Delta 0;
    to_title = Delta (-47);
  }; {
    lo = 0x03d6;
    hi = 0x03d6;
    to_upper = Delta (-54);
    to_lower = Delta 0;
    to_title = Delta (-54);
  }; {
    lo = 0x03d7;
    hi = 0x03d7;
    to_upper = Delta (-8);
    to_lower = Delta 0;
    to_title = Delta (-8);
  }; {
    lo = 0x03d8;
    hi = 0x03ef;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x03f0;
    hi = 0x03f0;
    to_upper = Delta (-86);
    to_lower = Delta 0;
    to_title = Delta (-86);
  }; {
    lo = 0x03f1;
    hi = 0x03f1;
    to_upper = Delta (-80);
    to_lower = Delta 0;
    to_title = Delta (-80);
  }; {
    lo = 0x03f2;
    hi = 0x03f2;
    to_upper = Delta 7;
    to_lower = Delta 0;
    to_title = Delta 7;
  }; {
    lo = 0x03f3;
    hi = 0x03f3;
    to_upper = Delta (-116);
    to_lower = Delta 0;
    to_title = Delta (-116);
  }; {
    lo = 0x03f4;
    hi = 0x03f4;
    to_upper = Delta 0;
    to_lower = Delta (-60);
    to_title = Delta 0;
  }; {
    lo = 0x03f5;
    hi = 0x03f5;
    to_upper = Delta (-96);
    to_lower = Delta 0;
    to_title = Delta (-96);
  }; {
    lo = 0x03f7;
    hi = 0x03f8;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x03f9;
    hi = 0x03f9;
    to_upper = Delta 0;
    to_lower = Delta (-7);
    to_title = Delta 0;
  }; {
    lo = 0x03fa;
    hi = 0x03fb;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x03fd;
    hi = 0x03ff;
    to_upper = Delta 0;
    to_lower = Delta (-130);
    to_title = Delta 0;
  }; {
    lo = 0x0400;
    hi = 0x040f;
    to_upper = Delta 0;
    to_lower = Delta 80;
    to_title = Delta 0;
  }; {
    lo = 0x0410;
    hi = 0x042f;
    to_upper = Delta 0;
    to_lower = Delta 32;
    to_title = Delta 0;
  }; {
    lo = 0x0430;
    hi = 0x044f;
    to_upper = Delta (-32);
    to_lower = Delta 0;
    to_title = Delta (-32);
  }; {
    lo = 0x0450;
    hi = 0x045f;
    to_upper = Delta (-80);
    to_lower = Delta 0;
    to_title = Delta (-80);
  }; {
    lo = 0x0460;
    hi = 0x0481;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x048a;
    hi = 0x04bf;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x04c0;
    hi = 0x04c0;
    to_upper = Delta 0;
    to_lower = Delta 15;
    to_title = Delta 0;
  }; {
    lo = 0x04c1;
    hi = 0x04ce;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x04cf;
    hi = 0x04cf;
    to_upper = Delta (-15);
    to_lower = Delta 0;
    to_title = Delta (-15);
  }; {
    lo = 0x04d0;
    hi = 0x052f;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x0531;
    hi = 0x0556;
    to_upper = Delta 0;
    to_lower = Delta 48;
    to_title = Delta 0;
  }; {
    lo = 0x0561;
    hi = 0x0586;
    to_upper = Delta (-48);
    to_lower = Delta 0;
    to_title = Delta (-48);
  }; {
    lo = 0x10a0;
    hi = 0x10c5;
    to_upper = Delta 0;
    to_lower = Delta 7_264;
    to_title = Delta 0;
  }; {
    lo = 0x10c7;
    hi = 0x10c7;
    to_upper = Delta 0;
    to_lower = Delta 7_264;
    to_title = Delta 0;
  }; {
    lo = 0x10cd;
    hi = 0x10cd;
    to_upper = Delta 0;
    to_lower = Delta 7_264;
    to_title = Delta 0;
  }; {
    lo = 0x10d0;
    hi = 0x10fa;
    to_upper = Delta 3_008;
    to_lower = Delta 0;
    to_title = Delta 0;
  }; {
    lo = 0x10fd;
    hi = 0x10ff;
    to_upper = Delta 3_008;
    to_lower = Delta 0;
    to_title = Delta 0;
  }; {
    lo = 0x13a0;
    hi = 0x13ef;
    to_upper = Delta 0;
    to_lower = Delta 38_864;
    to_title = Delta 0;
  }; {
    lo = 0x13f0;
    hi = 0x13f5;
    to_upper = Delta 0;
    to_lower = Delta 8;
    to_title = Delta 0;
  }; {
    lo = 0x13f8;
    hi = 0x13fd;
    to_upper = Delta (-8);
    to_lower = Delta 0;
    to_title = Delta (-8);
  }; {
    lo = 0x1c80;
    hi = 0x1c80;
    to_upper = Delta (-6_254);
    to_lower = Delta 0;
    to_title = Delta (-6_254);
  }; {
    lo = 0x1c81;
    hi = 0x1c81;
    to_upper = Delta (-6_253);
    to_lower = Delta 0;
    to_title = Delta (-6_253);
  }; {
    lo = 0x1c82;
    hi = 0x1c82;
    to_upper = Delta (-6_244);
    to_lower = Delta 0;
    to_title = Delta (-6_244);
  }; {
    lo = 0x1c83;
    hi = 0x1c84;
    to_upper = Delta (-6_242);
    to_lower = Delta 0;
    to_title = Delta (-6_242);
  }; {
    lo = 0x1c85;
    hi = 0x1c85;
    to_upper = Delta (-6_243);
    to_lower = Delta 0;
    to_title = Delta (-6_243);
  }; {
    lo = 0x1c86;
    hi = 0x1c86;
    to_upper = Delta (-6_236);
    to_lower = Delta 0;
    to_title = Delta (-6_236);
  }; {
    lo = 0x1c87;
    hi = 0x1c87;
    to_upper = Delta (-6_181);
    to_lower = Delta 0;
    to_title = Delta (-6_181);
  }; {
    lo = 0x1c88;
    hi = 0x1c88;
    to_upper = Delta 35_266;
    to_lower = Delta 0;
    to_title = Delta 35_266;
  }; {
    lo = 0x1c90;
    hi = 0x1cba;
    to_upper = Delta 0;
    to_lower = Delta (-3_008);
    to_title = Delta 0;
  }; {
    lo = 0x1cbd;
    hi = 0x1cbf;
    to_upper = Delta 0;
    to_lower = Delta (-3_008);
    to_title = Delta 0;
  }; {
    lo = 0x1d79;
    hi = 0x1d79;
    to_upper = Delta 35_332;
    to_lower = Delta 0;
    to_title = Delta 35_332;
  }; {
    lo = 0x1d7d;
    hi = 0x1d7d;
    to_upper = Delta 3_814;
    to_lower = Delta 0;
    to_title = Delta 3_814;
  }; {
    lo = 0x1d8e;
    hi = 0x1d8e;
    to_upper = Delta 35_384;
    to_lower = Delta 0;
    to_title = Delta 35_384;
  }; {
    lo = 0x1e00;
    hi = 0x1e95;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x1e9b;
    hi = 0x1e9b;
    to_upper = Delta (-59);
    to_lower = Delta 0;
    to_title = Delta (-59);
  }; {
    lo = 0x1e9e;
    hi = 0x1e9e;
    to_upper = Delta 0;
    to_lower = Delta (-7_615);
    to_title = Delta 0;
  }; {
    lo = 0x1ea0;
    hi = 0x1eff;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x1f00;
    hi = 0x1f07;
    to_upper = Delta 8;
    to_lower = Delta 0;
    to_title = Delta 8;
  }; {
    lo = 0x1f08;
    hi = 0x1f0f;
    to_upper = Delta 0;
    to_lower = Delta (-8);
    to_title = Delta 0;
  }; {
    lo = 0x1f10;
    hi = 0x1f15;
    to_upper = Delta 8;
    to_lower = Delta 0;
    to_title = Delta 8;
  }; {
    lo = 0x1f18;
    hi = 0x1f1d;
    to_upper = Delta 0;
    to_lower = Delta (-8);
    to_title = Delta 0;
  }; {
    lo = 0x1f20;
    hi = 0x1f27;
    to_upper = Delta 8;
    to_lower = Delta 0;
    to_title = Delta 8;
  }; {
    lo = 0x1f28;
    hi = 0x1f2f;
    to_upper = Delta 0;
    to_lower = Delta (-8);
    to_title = Delta 0;
  }; {
    lo = 0x1f30;
    hi = 0x1f37;
    to_upper = Delta 8;
    to_lower = Delta 0;
    to_title = Delta 8;
  }; {
    lo = 0x1f38;
    hi = 0x1f3f;
    to_upper = Delta 0;
    to_lower = Delta (-8);
    to_title = Delta 0;
  }; {
    lo = 0x1f40;
    hi = 0x1f45;
    to_upper = Delta 8;
    to_lower = Delta 0;
    to_title = Delta 8;
  }; {
    lo = 0x1f48;
    hi = 0x1f4d;
    to_upper = Delta 0;
    to_lower = Delta (-8);
    to_title = Delta 0;
  }; {
    lo = 0x1f51;
    hi = 0x1f51;
    to_upper = Delta 8;
    to_lower = Delta 0;
    to_title = Delta 8;
  }; {
    lo = 0x1f53;
    hi = 0x1f53;
    to_upper = Delta 8;
    to_lower = Delta 0;
    to_title = Delta 8;
  }; {
    lo = 0x1f55;
    hi = 0x1f55;
    to_upper = Delta 8;
    to_lower = Delta 0;
    to_title = Delta 8;
  }; {
    lo = 0x1f57;
    hi = 0x1f57;
    to_upper = Delta 8;
    to_lower = Delta 0;
    to_title = Delta 8;
  }; {
    lo = 0x1f59;
    hi = 0x1f59;
    to_upper = Delta 0;
    to_lower = Delta (-8);
    to_title = Delta 0;
  }; {
    lo = 0x1f5b;
    hi = 0x1f5b;
    to_upper = Delta 0;
    to_lower = Delta (-8);
    to_title = Delta 0;
  }; {
    lo = 0x1f5d;
    hi = 0x1f5d;
    to_upper = Delta 0;
    to_lower = Delta (-8);
    to_title = Delta 0;
  }; {
    lo = 0x1f5f;
    hi = 0x1f5f;
    to_upper = Delta 0;
    to_lower = Delta (-8);
    to_title = Delta 0;
  }; {
    lo = 0x1f60;
    hi = 0x1f67;
    to_upper = Delta 8;
    to_lower = Delta 0;
    to_title = Delta 8;
  }; {
    lo = 0x1f68;
    hi = 0x1f6f;
    to_upper = Delta 0;
    to_lower = Delta (-8);
    to_title = Delta 0;
  }; {
    lo = 0x1f70;
    hi = 0x1f71;
    to_upper = Delta 74;
    to_lower = Delta 0;
    to_title = Delta 74;
  }; {
    lo = 0x1f72;
    hi = 0x1f75;
    to_upper = Delta 86;
    to_lower = Delta 0;
    to_title = Delta 86;
  }; {
    lo = 0x1f76;
    hi = 0x1f77;
    to_upper = Delta 100;
    to_lower = Delta 0;
    to_title = Delta 100;
  }; {
    lo = 0x1f78;
    hi = 0x1f79;
    to_upper = Delta 128;
    to_lower = Delta 0;
    to_title = Delta 128;
  }; {
    lo = 0x1f7a;
    hi = 0x1f7b;
    to_upper = Delta 112;
    to_lower = Delta 0;
    to_title = Delta 112;
  }; {
    lo = 0x1f7c;
    hi = 0x1f7d;
    to_upper = Delta 126;
    to_lower = Delta 0;
    to_title = Delta 126;
  }; {
    lo = 0x1f80;
    hi = 0x1f87;
    to_upper = Delta 8;
    to_lower = Delta 0;
    to_title = Delta 8;
  }; {
    lo = 0x1f88;
    hi = 0x1f8f;
    to_upper = Delta 0;
    to_lower = Delta (-8);
    to_title = Delta 0;
  }; {
    lo = 0x1f90;
    hi = 0x1f97;
    to_upper = Delta 8;
    to_lower = Delta 0;
    to_title = Delta 8;
  }; {
    lo = 0x1f98;
    hi = 0x1f9f;
    to_upper = Delta 0;
    to_lower = Delta (-8);
    to_title = Delta 0;
  }; {
    lo = 0x1fa0;
    hi = 0x1fa7;
    to_upper = Delta 8;
    to_lower = Delta 0;
    to_title = Delta 8;
  }; {
    lo = 0x1fa8;
    hi = 0x1faf;
    to_upper = Delta 0;
    to_lower = Delta (-8);
    to_title = Delta 0;
  }; {
    lo = 0x1fb0;
    hi = 0x1fb1;
    to_upper = Delta 8;
    to_lower = Delta 0;
    to_title = Delta 8;
  }; {
    lo = 0x1fb3;
    hi = 0x1fb3;
    to_upper = Delta 9;
    to_lower = Delta 0;
    to_title = Delta 9;
  }; {
    lo = 0x1fb8;
    hi = 0x1fb9;
    to_upper = Delta 0;
    to_lower = Delta (-8);
    to_title = Delta 0;
  }; {
    lo = 0x1fba;
    hi = 0x1fbb;
    to_upper = Delta 0;
    to_lower = Delta (-74);
    to_title = Delta 0;
  }; {
    lo = 0x1fbc;
    hi = 0x1fbc;
    to_upper = Delta 0;
    to_lower = Delta (-9);
    to_title = Delta 0;
  }; {
    lo = 0x1fbe;
    hi = 0x1fbe;
    to_upper = Delta (-7_205);
    to_lower = Delta 0;
    to_title = Delta (-7_205);
  }; {
    lo = 0x1fc3;
    hi = 0x1fc3;
    to_upper = Delta 9;
    to_lower = Delta 0;
    to_title = Delta 9;
  }; {
    lo = 0x1fc8;
    hi = 0x1fcb;
    to_upper = Delta 0;
    to_lower = Delta (-86);
    to_title = Delta 0;
  }; {
    lo = 0x1fcc;
    hi = 0x1fcc;
    to_upper = Delta 0;
    to_lower = Delta (-9);
    to_title = Delta 0;
  }; {
    lo = 0x1fd0;
    hi = 0x1fd1;
    to_upper = Delta 8;
    to_lower = Delta 0;
    to_title = Delta 8;
  }; {
    lo = 0x1fd8;
    hi = 0x1fd9;
    to_upper = Delta 0;
    to_lower = Delta (-8);
    to_title = Delta 0;
  }; {
    lo = 0x1fda;
    hi = 0x1fdb;
    to_upper = Delta 0;
    to_lower = Delta (-100);
    to_title = Delta 0;
  }; {
    lo = 0x1fe0;
    hi = 0x1fe1;
    to_upper = Delta 8;
    to_lower = Delta 0;
    to_title = Delta 8;
  }; {
    lo = 0x1fe5;
    hi = 0x1fe5;
    to_upper = Delta 7;
    to_lower = Delta 0;
    to_title = Delta 7;
  }; {
    lo = 0x1fe8;
    hi = 0x1fe9;
    to_upper = Delta 0;
    to_lower = Delta (-8);
    to_title = Delta 0;
  }; {
    lo = 0x1fea;
    hi = 0x1feb;
    to_upper = Delta 0;
    to_lower = Delta (-112);
    to_title = Delta 0;
  }; {
    lo = 0x1fec;
    hi = 0x1fec;
    to_upper = Delta 0;
    to_lower = Delta (-7);
    to_title = Delta 0;
  }; {
    lo = 0x1ff3;
    hi = 0x1ff3;
    to_upper = Delta 9;
    to_lower = Delta 0;
    to_title = Delta 9;
  }; {
    lo = 0x1ff8;
    hi = 0x1ff9;
    to_upper = Delta 0;
    to_lower = Delta (-128);
    to_title = Delta 0;
  }; {
    lo = 0x1ffa;
    hi = 0x1ffb;
    to_upper = Delta 0;
    to_lower = Delta (-126);
    to_title = Delta 0;
  }; {
    lo = 0x1ffc;
    hi = 0x1ffc;
    to_upper = Delta 0;
    to_lower = Delta (-9);
    to_title = Delta 0;
  }; {
    lo = 0x2126;
    hi = 0x2126;
    to_upper = Delta 0;
    to_lower = Delta (-7_517);
    to_title = Delta 0;
  }; {
    lo = 0x212a;
    hi = 0x212a;
    to_upper = Delta 0;
    to_lower = Delta (-8_383);
    to_title = Delta 0;
  }; {
    lo = 0x212b;
    hi = 0x212b;
    to_upper = Delta 0;
    to_lower = Delta (-8_262);
    to_title = Delta 0;
  }; {
    lo = 0x2132;
    hi = 0x2132;
    to_upper = Delta 0;
    to_lower = Delta 28;
    to_title = Delta 0;
  }; {
    lo = 0x214e;
    hi = 0x214e;
    to_upper = Delta (-28);
    to_lower = Delta 0;
    to_title = Delta (-28);
  }; {
    lo = 0x2160;
    hi = 0x216f;
    to_upper = Delta 0;
    to_lower = Delta 16;
    to_title = Delta 0;
  }; {
    lo = 0x2170;
    hi = 0x217f;
    to_upper = Delta (-16);
    to_lower = Delta 0;
    to_title = Delta (-16);
  }; {
    lo = 0x2183;
    hi = 0x2184;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x24b6;
    hi = 0x24cf;
    to_upper = Delta 0;
    to_lower = Delta 26;
    to_title = Delta 0;
  }; {
    lo = 0x24d0;
    hi = 0x24e9;
    to_upper = Delta (-26);
    to_lower = Delta 0;
    to_title = Delta (-26);
  }; {
    lo = 0x2c00;
    hi = 0x2c2f;
    to_upper = Delta 0;
    to_lower = Delta 48;
    to_title = Delta 0;
  }; {
    lo = 0x2c30;
    hi = 0x2c5f;
    to_upper = Delta (-48);
    to_lower = Delta 0;
    to_title = Delta (-48);
  }; {
    lo = 0x2c60;
    hi = 0x2c61;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x2c62;
    hi = 0x2c62;
    to_upper = Delta 0;
    to_lower = Delta (-10_743);
    to_title = Delta 0;
  }; {
    lo = 0x2c63;
    hi = 0x2c63;
    to_upper = Delta 0;
    to_lower = Delta (-3_814);
    to_title = Delta 0;
  }; {
    lo = 0x2c64;
    hi = 0x2c64;
    to_upper = Delta 0;
    to_lower = Delta (-10_727);
    to_title = Delta 0;
  }; {
    lo = 0x2c65;
    hi = 0x2c65;
    to_upper = Delta (-10_795);
    to_lower = Delta 0;
    to_title = Delta (-10_795);
  }; {
    lo = 0x2c66;
    hi = 0x2c66;
    to_upper = Delta (-10_792);
    to_lower = Delta 0;
    to_title = Delta (-10_792);
  }; {
    lo = 0x2c67;
    hi = 0x2c6c;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x2c6d;
    hi = 0x2c6d;
    to_upper = Delta 0;
    to_lower = Delta (-10_780);
    to_title = Delta 0;
  }; {
    lo = 0x2c6e;
    hi = 0x2c6e;
    to_upper = Delta 0;
    to_lower = Delta (-10_749);
    to_title = Delta 0;
  }; {
    lo = 0x2c6f;
    hi = 0x2c6f;
    to_upper = Delta 0;
    to_lower = Delta (-10_783);
    to_title = Delta 0;
  }; {
    lo = 0x2c70;
    hi = 0x2c70;
    to_upper = Delta 0;
    to_lower = Delta (-10_782);
    to_title = Delta 0;
  }; {
    lo = 0x2c72;
    hi = 0x2c73;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x2c75;
    hi = 0x2c76;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x2c7e;
    hi = 0x2c7f;
    to_upper = Delta 0;
    to_lower = Delta (-10_815);
    to_title = Delta 0;
  }; {
    lo = 0x2c80;
    hi = 0x2ce3;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x2ceb;
    hi = 0x2cee;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x2cf2;
    hi = 0x2cf3;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0x2d00;
    hi = 0x2d25;
    to_upper = Delta (-7_264);
    to_lower = Delta 0;
    to_title = Delta (-7_264);
  }; {
    lo = 0x2d27;
    hi = 0x2d27;
    to_upper = Delta (-7_264);
    to_lower = Delta 0;
    to_title = Delta (-7_264);
  }; {
    lo = 0x2d2d;
    hi = 0x2d2d;
    to_upper = Delta (-7_264);
    to_lower = Delta 0;
    to_title = Delta (-7_264);
  }; {
    lo = 0xa640;
    hi = 0xa66d;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0xa680;
    hi = 0xa69b;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0xa722;
    hi = 0xa72f;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0xa732;
    hi = 0xa76f;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0xa779;
    hi = 0xa77c;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0xa77d;
    hi = 0xa77d;
    to_upper = Delta 0;
    to_lower = Delta (-35_332);
    to_title = Delta 0;
  }; {
    lo = 0xa77e;
    hi = 0xa787;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0xa78b;
    hi = 0xa78c;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0xa78d;
    hi = 0xa78d;
    to_upper = Delta 0;
    to_lower = Delta (-42_280);
    to_title = Delta 0;
  }; {
    lo = 0xa790;
    hi = 0xa793;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0xa794;
    hi = 0xa794;
    to_upper = Delta 48;
    to_lower = Delta 0;
    to_title = Delta 48;
  }; {
    lo = 0xa796;
    hi = 0xa7a9;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0xa7aa;
    hi = 0xa7aa;
    to_upper = Delta 0;
    to_lower = Delta (-42_308);
    to_title = Delta 0;
  }; {
    lo = 0xa7ab;
    hi = 0xa7ab;
    to_upper = Delta 0;
    to_lower = Delta (-42_319);
    to_title = Delta 0;
  }; {
    lo = 0xa7ac;
    hi = 0xa7ac;
    to_upper = Delta 0;
    to_lower = Delta (-42_315);
    to_title = Delta 0;
  }; {
    lo = 0xa7ad;
    hi = 0xa7ad;
    to_upper = Delta 0;
    to_lower = Delta (-42_305);
    to_title = Delta 0;
  }; {
    lo = 0xa7ae;
    hi = 0xa7ae;
    to_upper = Delta 0;
    to_lower = Delta (-42_308);
    to_title = Delta 0;
  }; {
    lo = 0xa7b0;
    hi = 0xa7b0;
    to_upper = Delta 0;
    to_lower = Delta (-42_258);
    to_title = Delta 0;
  }; {
    lo = 0xa7b1;
    hi = 0xa7b1;
    to_upper = Delta 0;
    to_lower = Delta (-42_282);
    to_title = Delta 0;
  }; {
    lo = 0xa7b2;
    hi = 0xa7b2;
    to_upper = Delta 0;
    to_lower = Delta (-42_261);
    to_title = Delta 0;
  }; {
    lo = 0xa7b3;
    hi = 0xa7b3;
    to_upper = Delta 0;
    to_lower = Delta 928;
    to_title = Delta 0;
  }; {
    lo = 0xa7b4;
    hi = 0xa7c3;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0xa7c4;
    hi = 0xa7c4;
    to_upper = Delta 0;
    to_lower = Delta (-48);
    to_title = Delta 0;
  }; {
    lo = 0xa7c5;
    hi = 0xa7c5;
    to_upper = Delta 0;
    to_lower = Delta (-42_307);
    to_title = Delta 0;
  }; {
    lo = 0xa7c6;
    hi = 0xa7c6;
    to_upper = Delta 0;
    to_lower = Delta (-35_384);
    to_title = Delta 0;
  }; {
    lo = 0xa7c7;
    hi = 0xa7ca;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0xa7d0;
    hi = 0xa7d1;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0xa7d6;
    hi = 0xa7d9;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0xa7f5;
    hi = 0xa7f6;
    to_upper = UpperLower;
    to_lower = UpperLower;
    to_title = UpperLower;
  }; {
    lo = 0xab53;
    hi = 0xab53;
    to_upper = Delta (-928);
    to_lower = Delta 0;
    to_title = Delta (-928);
  }; {
    lo = 0xab70;
    hi = 0xabbf;
    to_upper = Delta (-38_864);
    to_lower = Delta 0;
    to_title = Delta (-38_864);
  }; {
    lo = 0xff21;
    hi = 0xff3a;
    to_upper = Delta 0;
    to_lower = Delta 32;
    to_title = Delta 0;
  }; {
    lo = 0xff41;
    hi = 0xff5a;
    to_upper = Delta (-32);
    to_lower = Delta 0;
    to_title = Delta (-32);
  }; {
    lo = 0x1_0400;
    hi = 0x1_0427;
    to_upper = Delta 0;
    to_lower = Delta 40;
    to_title = Delta 0;
  }; {
    lo = 0x1_0428;
    hi = 0x1_044f;
    to_upper = Delta (-40);
    to_lower = Delta 0;
    to_title = Delta (-40);
  }; {
    lo = 0x1_04b0;
    hi = 0x1_04d3;
    to_upper = Delta 0;
    to_lower = Delta 40;
    to_title = Delta 0;
  }; {
    lo = 0x1_04d8;
    hi = 0x1_04fb;
    to_upper = Delta (-40);
    to_lower = Delta 0;
    to_title = Delta (-40);
  }; {
    lo = 0x1_0570;
    hi = 0x1_057a;
    to_upper = Delta 0;
    to_lower = Delta 39;
    to_title = Delta 0;
  }; {
    lo = 0x1_057c;
    hi = 0x1_058a;
    to_upper = Delta 0;
    to_lower = Delta 39;
    to_title = Delta 0;
  }; {
    lo = 0x1_058c;
    hi = 0x1_0592;
    to_upper = Delta 0;
    to_lower = Delta 39;
    to_title = Delta 0;
  }; {
    lo = 0x1_0594;
    hi = 0x1_0595;
    to_upper = Delta 0;
    to_lower = Delta 39;
    to_title = Delta 0;
  }; {
    lo = 0x1_0597;
    hi = 0x1_05a1;
    to_upper = Delta (-39);
    to_lower = Delta 0;
    to_title = Delta (-39);
  }; {
    lo = 0x1_05a3;
    hi = 0x1_05b1;
    to_upper = Delta (-39);
    to_lower = Delta 0;
    to_title = Delta (-39);
  }; {
    lo = 0x1_05b3;
    hi = 0x1_05b9;
    to_upper = Delta (-39);
    to_lower = Delta 0;
    to_title = Delta (-39);
  }; {
    lo = 0x1_05bb;
    hi = 0x1_05bc;
    to_upper = Delta (-39);
    to_lower = Delta 0;
    to_title = Delta (-39);
  }; {
    lo = 0x1_0c80;
    hi = 0x1_0cb2;
    to_upper = Delta 0;
    to_lower = Delta 64;
    to_title = Delta 0;
  }; {
    lo = 0x1_0cc0;
    hi = 0x1_0cf2;
    to_upper = Delta (-64);
    to_lower = Delta 0;
    to_title = Delta (-64);
  }; {
    lo = 0x1_18a0;
    hi = 0x1_18bf;
    to_upper = Delta 0;
    to_lower = Delta 32;
    to_title = Delta 0;
  }; {
    lo = 0x1_18c0;
    hi = 0x1_18df;
    to_upper = Delta (-32);
    to_lower = Delta 0;
    to_title = Delta (-32);
  }; {
    lo = 0x1_6e40;
    hi = 0x1_6e5f;
    to_upper = Delta 0;
    to_lower = Delta 32;
    to_title = Delta 0;
  }; {
    lo = 0x1_6e60;
    hi = 0x1_6e7f;
    to_upper = Delta (-32);
    to_lower = Delta 0;
    to_title = Delta (-32);
  }; {
    lo = 0x1_e900;
    hi = 0x1_e921;
    to_upper = Delta 0;
    to_lower = Delta 34;
    to_title = Delta 0;
  }; {
    lo = 0x1_e922;
    hi = 0x1_e943;
    to_upper = Delta (-34);
    to_lower = Delta 0;
    to_title = Delta (-34);
  };|]

(** Apply case delta to a code point *)
let apply_delta: int -> case_delta -> int -> int = fun r delta case_type ->
  match delta with
  | Delta d -> r + d
  | UpperLower ->
      (* In an Upper-Lower sequence, the characters at even offsets
         are upper case; the ones at odd offsets are lower.
         UpperCase=0, LowerCase=1, TitleCase=2 *)
      if case_type = 1 then
        r lor 1
        (* Set low bit *)
      else
        r land (lnot 1)

(* Clear low bit *)

(** Binary search for case range containing code point *)
let find_case_range: case_range array -> int -> case_range option = fun ranges r ->
  let rec search lo hi =
    if lo > hi then
      None
    else
      let mid = (lo + hi) / 2 in
      let range = Array.get_unchecked ranges ~at:mid in
      if r < range.lo then
        search lo (mid - 1)
      else if r > range.hi then
        search (mid + 1) hi
      else
        Some range
  in
  search 0 (Array.length ranges - 1)

(** Convert code point to uppercase *)
let to_upper: int -> int = fun r ->
  if r >= 0x61 && r <= 0x7a then
    r - 32
    (* a-z -> A-Z *)
  else
    match find_case_range case_ranges r with
    | Some range -> apply_delta r range.to_upper 0
    | None -> r

(** Convert code point to lowercase *)
let to_lower: int -> int = fun r ->
  if r >= 0x41 && r <= 0x5a then
    r + 32
    (* A-Z -> a-z *)
  else
    match find_case_range case_ranges r with
    | Some range -> apply_delta r range.to_lower 1
    | None -> r

(** Convert code point to titlecase *)
let to_title: int -> int = fun r ->
  if r >= 0x61 && r <= 0x7a then
    r - 32
  else
    match find_case_range case_ranges r with
    | Some range -> apply_delta r range.to_title 2
    | None -> r
