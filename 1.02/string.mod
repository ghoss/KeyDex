(*****************************************************************************
*
* Name     : String
*
* LastEdit : 12/04/87
* Project  : KeyDex system
* Purpose  : Basic string handling routines.
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
* 12/04/87 : Extended "UpperCase" function to german umlauts.
* 23/04/87 : Cleaned up "WildCardMatch".
* 26/04/87 : Enhancements to "WildCardMatch". Now works with "Barbara".
* 14/06/87 : Converted nearly all parameters to VAR.
* 05/10/87 : Changed semantics of 'Compare'.
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

IMPLEMENTATION MODULE String;


PROCEDURE Clear(VAR s:ARRAY OF CHAR);
BEGIN
  s[0] := 0C;
END Clear;



PROCEDURE Append(VAR s1,s2 : ARRAY OF CHAR);
VAR
  j,max,max2,len : CARDINAL;
BEGIN
  len := ORD(s1[0]); max := HIGH(s1); max2 := ORD(s2[0]); j := 1;
  WHILE (len<max) AND (j<=max2) DO
    INC(len); s1[len] := s2[j]; INC(j);
  END;
  s1[0] := CHR(len);
END Append;



PROCEDURE AppendAOC(VAR s1:ARRAY OF CHAR; s2:ARRAY OF CHAR);
VAR
  j,max,max2,len : CARDINAL;
BEGIN
  len := ORD(s1[0]); max := HIGH(s1); max2 := HIGH(s2); j := 0;
  WHILE (len<max) AND (j<=max2) AND (s2[j]<>0C) DO
    INC(len); s1[len] := s2[j]; INC(j);
  END;
  s1[0] := CHR(len);
END AppendAOC;



PROCEDURE AppendCh(VAR s1:ARRAY OF CHAR; C:CHAR);
VAR
  l1 : CARDINAL;
BEGIN
  l1 := ORD(s1[0]) + 1;
  IF l1 <= HIGH(s1) THEN
    s1[l1] := C; INC(s1[0]);
  END;
END AppendCh;



PROCEDURE Assign(VAR s1,s2 : ARRAY OF CHAR);
VAR
  max : CARDINAL;
BEGIN
  IF HIGH(s1) <= ORD(s2[0]) THEN max := HIGH(s1) ELSE max := ORD(s2[0]) END;
  s1[0] := CHR(max);
  WHILE max > 0 DO
    s1[max] := s2[max]; DEC(max);
  END;
END Assign;



PROCEDURE AssignAOC(VAR s1:ARRAY OF CHAR; s2:ARRAY OF CHAR);
VAR
  i,j,max : CARDINAL;
BEGIN
  IF HIGH(s1) <= HIGH(s2) THEN max := HIGH(s1) ELSE max := HIGH(s2)+1 END;
  i := 1; j := 0;
  WHILE (i<=max) AND (s2[j]#0C) DO
    s1[i] := s2[j]; INC(i); INC(j);
  END;
  s1[0] := CHR(j);
END AssignAOC;



PROCEDURE AssignStr(VAR s1,s2 : ARRAY OF CHAR);
VAR
  i,j,max : CARDINAL;
BEGIN
  max := ORD(s2[0]); 
  IF max > (HIGH(s1)+1) THEN max := HIGH(s1)+1 END;
  i := 1; j := 0;

  WHILE (i<=max) DO
    s1[j] := s2[i]; INC(i); INC(j);
  END;
  IF j<=HIGH(s1) THEN s1[j] := 0C END;
END AssignStr;



PROCEDURE Insert(VAR s,substr : ARRAY OF CHAR; i:CARDINAL);
VAR
  j : CARDINAL;
BEGIN
  IF i > ORD(s[0]) THEN Append(s,substr)
  ELSE
    FOR j := 1 TO ORD(substr[0]) DO InsertCh(s,substr[j],i); INC(i) END;
  END;
END Insert;



PROCEDURE InsertCh(VAR s:ARRAY OF CHAR; c:CHAR; i:CARDINAL);
VAR
  max : CARDINAL;
BEGIN
  max := ORD(s[0])+1;
  IF max > HIGH(s) THEN max := HIGH(s) END;
  s[0] := CHR(max);

  WHILE max > i DO
    s[max] := s[max-1]; DEC(max);
  END;
  s[i] := c;
END InsertCh;



PROCEDURE Delete(VAR s:ARRAY OF CHAR; i,n:CARDINAL);
VAR
  l,max,j : CARDINAL;
BEGIN
  l := ORD(s[0]);
  IF i > l THEN RETURN ELSIF i+n > l THEN n := l-i+1 END;
  DEC(s[0],n); max := l-n; j := i+n;

  WHILE i<=max DO
    s[i] := s[j]; INC(i); INC(j);
  END;
END Delete;



PROCEDURE Copy(VAR s,s1 : ARRAY OF CHAR; i,n:CARDINAL);
VAR
  l,l1,j : CARDINAL;
BEGIN
  l1 := HIGH(s1); l := ORD(s[0]);
  IF i > l THEN RETURN
  ELSIF i+n-1 > l THEN n := l-i+1 END; 
  IF n > l1 THEN n := l1 END;
  s1[0] := CHR(n); j := i+n-1;

  WHILE n > 0 DO
    s1[n] := s[j]; DEC(j); DEC(n);
  END;
END Copy;




PROCEDURE Compare(VAR S1,S2 : ARRAY OF CHAR) : INTEGER;

  PROCEDURE Number(VAR s : ARRAY OF CHAR) : BOOLEAN;
  VAR
    i, max : CARDINAL;
    c : CHAR;
  BEGIN
    i := 1; max := ORD(s[0]);
    WHILE i <= max DO
      c := s[i]; INC(i);
      IF (c < '0') OR (c > '9') THEN RETURN FALSE END;
    END; (* WHILE *)
    RETURN TRUE;
  END Number;

VAR
  i,L1,L2,max:CARDINAL;
BEGIN
  L1 := ORD(S1[0]); L2 := ORD(S2[0]);
  IF Number(S1) & Number(S2) THEN 
    (** 05/10/87 **)
    IF L1 < L2 THEN RETURN -1 ELSIF L1 > L2 THEN RETURN +1 END;
  END; (* IF *)
  i := 1;
  IF L1 <= L2 THEN max := L1 ELSE max := L2 END;
  WHILE (i <= max) AND (S1[i] = S2[i]) DO
    INC(i);
  END;
  IF (i<=max) THEN  (* S1[i] <> S2[i] *)
    IF S1[i]<S2[i] THEN RETURN -1 ELSE RETURN 1 END;
  END;
  IF (i>L1) THEN  (* S1[i] = S2[i], so compare lengthes of strings *)
    IF (i<=L2) THEN RETURN -1 ELSE RETURN 0; END;
  ELSE
    RETURN 1;
  END;
END Compare;


PROCEDURE UpperCase(VAR s:ARRAY OF CHAR);
VAR
  i : CARDINAL;
BEGIN
  FOR i := 1 TO ORD(s[0]) DO 
    CASE s[i] OF
      '„' : s[i] := 'Ž'
    | '' : s[i] := 'š'
    | '”' : s[i] := '™'
    ELSE
      s[i] := CAP(s[i])
    END; (* CASE *)
  END; (* FOR *)
END UpperCase;


PROCEDURE WildcardMatch(VAR s,p : ARRAY OF CHAR) : BOOLEAN;
VAR
  i,j,imax,jmax,lastWildcard : CARDINAL;
BEGIN
  i := 1; j := 1; imax := ORD(s[0]); jmax := ORD(p[0]);
  lastWildcard := 0;
  REPEAT
    IF p[j]='?' THEN
      INC(i); INC(j);
    ELSIF p[j]='*' THEN
      lastWildcard := j; INC(j);
      IF j<=jmax THEN
        IF (p[j]#'?') AND (p[j]#'*') THEN
          WHILE (i<=imax) AND (s[i]#p[j]) DO INC(i) END;
        END;
        IF (i>imax) THEN RETURN FALSE END;
      ELSE
        RETURN TRUE;
      END; (* IF *)
    ELSIF (s[i]=p[j]) THEN
      INC(i); INC(j);
    ELSIF lastWildcard#0 THEN
      j := lastWildcard;  (* Expand wildcard over mismatching section *)
    ELSE
      RETURN FALSE;
    END; (* IF *)
  UNTIL (i>imax) OR (j>jmax);
  RETURN (i>imax) AND ((j>jmax) OR ((j=jmax) & (p[j]='*')));
END WildcardMatch;


PROCEDURE Pos(s,s1:ARRAY OF CHAR) : CARDINAL;
BEGIN
  HALT;
END Pos;


END String.
