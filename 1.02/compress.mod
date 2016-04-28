(*****************************************************************************
*
* Name     : Compress
*
* LastEdit : 29/03/87
* Project  : KeyDex system
* Purpose  : Packs data into 5 bits per character
* 
* Author   : Guido Hoss
* System   : LOGITECH MODULA-2/86, SYSTEM VERSION 3.00
*            Copyright 1987, 2016 by Guido Hoss. All Rights Reserved. 
*
* KeyDex is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program.  If not, see <http://www.gnu.org/licenses/>.
*
* Git repository page: https://github.com/ghoss/KeyDex
*
* Change History:
* --------------
* 29/03/87 : Reassignment of character codes.
* 11/04/87 : Another reassignment (german umlauts).
* 06/06/87 : Changed "-" to "%".
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

IMPLEMENTATION MODULE Compress;


CONST
  toggle=31;  (* Shift command character *)


VAR
  trans,trans2 : ARRAY [0..31] OF CHAR;  (* Character translation table *)
  shift        : BOOLEAN;


PROCEDURE Char(i:CARDINAL) : CHAR;
(* Returns character belonging to index i *)
BEGIN
  IF shift THEN RETURN trans2[i] ELSE RETURN trans[i] END;
END Char;



PROCEDURE Index(ch:CHAR) : CARDINAL;
(* Returns the compression code of ch in table #1 *)
BEGIN
  CASE CAP(ch) OF
    'A'..'Z' : RETURN ORD(ch)-ORD('A')+1;
   |'@' : RETURN 27;
   |'%' : RETURN 28;
   |'_' : RETURN 29;
   |'$' : RETURN 30;
   (* 31 is the digit shift command *)
  ELSE
    RETURN 0;
  END;
END Index;



PROCEDURE Index2(ch:CHAR) : CARDINAL;
(* Returns the compression code of ch in table #1 *)
BEGIN
  IF (ch>='0') AND (ch<='9') THEN
    RETURN ORD(ch)-ORD('0')+1;
  ELSE
    CASE ch OF
      '[' : RETURN 11;
    | ']' : RETURN 12;
    | '„','Ž' : RETURN 13;
    | '','š' : RETURN 14;
    | '”','™' : RETURN 15;
    ELSE
      RETURN 0;
    END; (* CASE *)
  END; (* IF *)
END Index2;



PROCEDURE ValidChar(ch:CHAR) : BOOLEAN;
BEGIN
  RETURN (Index(ch)<>0) OR (Index2(ch)<>0);
END ValidChar;



PROCEDURE Pack(VAR s,d:ARRAY OF CHAR; VAR i:CARDINAL);

  PROCEDURE Next;
  BEGIN
    IF delta THEN
      c := oldc; delta := FALSE
    ELSE
      INC(j);
      IF j<=ls THEN
        IF digit THEN c := Index2(s[j]) ELSE c := Index(s[j]) END;
        delta := (c=0);
        IF delta THEN
          IF digit THEN oldc := Index(s[j]) ELSE oldc := Index2(s[j]) END;
          digit := NOT digit; c := toggle;
        END;
      ELSE
        c := 0
      END;
    END;
  END Next;

VAR
  ls,j,oldc,c,di,cycle : CARDINAL;  (* j = Pointer to s, i = Pointer to d *)
  digit,delta : BOOLEAN;
BEGIN
  ls := ORD(s[0]); i := 0; j := 0; cycle := 0; c := 0; 
  digit := FALSE; delta := FALSE;
  REPEAT
    CASE cycle OF
       0 : di := c*8; Next; INC(di,c DIV 4);
     | 1 : di := (c MOD 4)*64; Next; INC(di,c*2); Next; INC(di,c DIV 16);
     | 2 : di := (c MOD 16)*16; Next; INC(di,c DIV 2);
     | 3 : di := (c MOD 2)*128; Next; INC(di,c*4); Next; INC(di,c DIV 8);
     | 4 : di := (c MOD 8)*32; Next; INC(di,c); Next;
    END;
    d[i] := CHR(di); INC(i);
    cycle := (cycle+1) MOD 5;
  UNTIL c=0;
  DEC(i); INC(d[0],i*8);
END Pack;



PROCEDURE Unpack(VAR d,s:ARRAY OF CHAR);

  PROCEDURE Next;
  BEGIN
    IF j<=ld THEN c := ORD(d[j]); INC(j) ELSE c := 0 END;
  END Next;

VAR
  ld,i,j,c,si,cycle : CARDINAL;  (* j = Pointer to d, i = Pointer to s *)
BEGIN
  ld := ORD(d[0]) DIV 8; i := 0; j := 0; cycle := 1; Next; shift := FALSE;
  REPEAT
    CASE cycle OF
       0 : si := (c DIV 8);
     | 1 : si := (c MOD 8)*4; Next; INC(si,c DIV 64);
     | 2 : si := (c MOD 64) DIV 2;
     | 3 : si := (c MOD 2)*16; Next; INC(si,c DIV 16);
     | 4 : si := (c MOD 16)*2; Next; INC(si,c DIV 128);
     | 5 : si := (c MOD 128) DIV 4;
     | 6 : si := (c MOD 4)*8; Next; INC(si,c DIV 32);
     | 7 : si := (c MOD 32); Next;
    END;
    IF si=toggle THEN shift := NOT shift
    ELSIF si<>0 THEN
      INC(i); s[i] := Char(si);
    END;
    cycle := (cycle+1) MOD 8;
  UNTIL c=0;
  s[0] := CHR(i);
END Unpack;



BEGIN
  trans  := 'þABCDEFGHIJKLMNOPQRSTUVWXYZ@%_$';
  trans2 := 'þ0123456789[]Žš™þþþþþþþþþþþþþþþ';
END Compress.
