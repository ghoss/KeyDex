(*****************************************************************************
*
* Name     : KDFileScan
*
* LastEdit : 03/05/87
* Purpose  : Routines to search for character sequences.
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
* 23/04/87 : Implemented wildcard search.
* 03/05/87 : Adaptations to support 32-bit record numbers.
* 06/06/87 : Changes due to new file format (date index).
*            Implemented counter display.
* 19/06/87 : Scan sequence breaks on line ends.
* 20/06/87 : Fixed bug in "SetPos".
* 18/07/87 : Improved error handling.
* 22/12/87 : Added changes to enable display of all matches in a screen.
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

IMPLEMENTATION MODULE KDFileScan;


FROM LongCardinals IMPORT
  LONGCARD, equal, zero, dec;

FROM KDBreak IMPORT
  UserBreak;

FROM String IMPORT
  UpperCase, Assign;

FROM KDConfig IMPORT
  Keyword;

FROM KDScreen IMPORT
  DispRecordKey;

FROM FileOp IMPORT
  ScanSetScreenPos, ScanReset, ScanRestorePos, ScanGetScreenPos,
  ScanNextChar, ScanEOF, lastrec, ScanGetCharPos;


VAR
  startPos,
  ownPos      : INTEGER;
  startLine,
  ownPage,
  ownLine,
  patternLength : CARDINAL;
  wordOnly,
  oneScreen,
  lastfound   : BOOLEAN;
  searchProc  : PROCEDURE(VAR BOOLEAN);
  func,
  pattern     : ARRAY [0..80] OF CHAR;
  ownScreen,
  lastScreen,
  count       : LONGCARD;


PROCEDURE Delimiter(c:CHAR) : BOOLEAN;
(* Returns TRUE if the character is a delimiter between words *)
BEGIN
  RETURN (c=' ') OR (c='.') OR (c='?') OR (c='!') OR (c=':')
         OR (c=';') OR (c=0C);
END Delimiter;



PROCEDURE dtDummy(x : CARDINAL);
END dtDummy;

PROCEDURE kwDummy(VAR x : Keyword);
END kwDummy;


PROCEDURE rcDisplay(date : CARDINAL; recno : LONGCARD);
BEGIN
  DispRecordKey(0, count); dec(count); 
  ScanEOF := UserBreak();
END rcDisplay;



PROCEDURE SearchKMP(VAR found : BOOLEAN);
(* This routine is invoked if wordsOnly=FALSE and no wildcards *)
VAR
  c : CHAR;
  s : CARDINAL;
BEGIN
  found := FALSE;
  s := 0; ScanNextChar(c);
  LOOP
    WHILE (c#0C) & (NOT ScanEOF) DO
      (** 31/07/87 : WHILE transposed before IF **)
      WHILE (s#0) & (c#pattern[s+1]) DO s := ORD(func[s]) END;
      IF s=0 THEN
        ScanGetCharPos(startLine, startPos);
      END; (* IF *)
      IF c=pattern[s+1] THEN INC(s) END;
      IF s=patternLength THEN found := TRUE; RETURN END;
      ScanNextChar(c);
    END; (* WHILE *)
    IF ScanEOF THEN EXIT
    ELSE
      REPEAT ScanNextChar(c) UNTIL (c#0C) OR ScanEOF;
      s := 0;
    END; (* IF *)
  END; (* LOOP *)
END SearchKMP;



PROCEDURE SearchWildcard(VAR found:BOOLEAN);
(* Problem with this version : "s???n" does not match "sixseven" *)

  PROCEDURE SetPos;
  BEGIN
    ScanGetCharPos(startLine, startPos);
    j := 1; (** 20/06/87 **) lastWildcard := 0;
  END SetPos;
           
VAR
  j,lastWildcard : CARDINAL;
  c : CHAR;
BEGIN
  lastWildcard := 0; found := FALSE; c := 0C;
  REPEAT
    IF c=0C THEN
      REPEAT ScanNextChar(c) UNTIL (c#0C) OR ScanEOF;
      IF ScanEOF THEN RETURN ELSE SetPos END;
    END;
    IF pattern[j]='?' THEN
      INC(j);
      IF j<=patternLength THEN ScanNextChar(c) END;
    ELSIF pattern[j]='*' THEN
      lastWildcard := j; INC(j);
      IF j<=patternLength THEN
        IF (pattern[j]#'?') AND (pattern[j]#'*') THEN
          WHILE (NOT ScanEOF) & (c#0C) & (c#pattern[j]) DO ScanNextChar(c) END;
        END; (* IF *)
      END; (* IF *)
    ELSIF (c=pattern[j]) THEN
      INC(j); 
      IF j<=patternLength THEN ScanNextChar(c) END;
    ELSIF lastWildcard#0 THEN
      j := lastWildcard;  (* Expand wildcard over mismatching section *)
    ELSE
      c := 0C;  (* Restart scan sequence *)
    END; (* IF *)
  UNTIL (j>patternLength) OR ScanEOF;
  found := (j>patternLength);
END SearchWildcard;



PROCEDURE SearchWordOnly(VAR found:BOOLEAN);
VAR
  i : CARDINAL;
  c : CHAR;
BEGIN
  (* Skip to first character which belongs to a word *)
  found := FALSE;
  REPEAT ScanNextChar(c) UNTIL (NOT Delimiter(c)) OR ScanEOF;
  LOOP
    IF ScanEOF THEN EXIT
    ELSE
      i := 1; ScanGetCharPos(startLine, startPos);
      WHILE (i<patternLength) & (c=pattern[i]) & (NOT ScanEOF) DO
        INC(i); ScanNextChar(c);
      END; (* WHILE *)
      IF (i=patternLength) & (c=pattern[i]) & (NOT ScanEOF) THEN
        ScanGetCharPos(ownLine, ownPos);
        ScanGetScreenPos(ownScreen, ownPage);
        ScanNextChar(c); found := (ScanEOF OR Delimiter(c));
        IF found THEN EXIT END;
      ELSE
        WHILE NOT (Delimiter(c) OR ScanEOF) DO ScanNextChar(c) END;
      END; (* IF *)
      WHILE Delimiter(c) & (NOT ScanEOF) DO ScanNextChar(c) END;
    END; (* IF *)
  END; (* LOOP *)
END SearchWordOnly;



PROCEDURE Search ( VAR pos:LONGCARD;
                   VAR page, fromLine, fromPos, toLine, toPos:CARDINAL; 
                   VAR same,found:BOOLEAN);
(* Returns the next matching screen *)
VAR
  t : INTEGER;
BEGIN
  IF oneScreen & (NOT lastfound) THEN
    ScanSetScreenPos(pos);
  ELSE
    ScanRestorePos;
  END;
  searchProc(found);
  lastfound := found;
  IF found THEN
    IF wordOnly THEN
      pos := ownScreen; page := ownPage;
      toLine := ownLine; toPos := VAL(CARDINAL, ownPos);
      same := equal(ownScreen,lastScreen);
      lastScreen := ownScreen;
    ELSE
      ScanGetScreenPos(pos, page); ScanGetCharPos(toLine, t);
      toPos := VAL(CARDINAL, t);
      same := equal(pos,lastScreen);
      lastScreen := pos;
    END; (* IF *)
    fromLine := startLine; fromPos := VAL(CARDINAL, startPos);
  END; (* IF *)
END Search;



PROCEDURE Scan( VAR str:ARRAY OF CHAR;
                wordonly,upcase,onescreen:BOOLEAN;
                N:LONGCARD);
(* Initializes the search variables. Does not start search. *)
VAR
  t,i,s : CARDINAL;
BEGIN
  wordOnly := wordonly; lastScreen := zero;
  Assign(pattern,str);
  IF upcase THEN UpperCase(pattern) END;
  patternLength := ORD(pattern[0]);
  oneScreen := onescreen;
  lastfound := FALSE;
  (* Test the pattern for wildcards ? and * *)
  i := patternLength;
  WHILE (i>0) & (str[i]#'?') & (str[i]#'*') DO DEC(i) END;
  IF (i>0) THEN
    (* These procedure applies also to wildCard AND wordOnly *)
    searchProc := SearchWildcard; wordOnly := FALSE;
  ELSIF wordOnly THEN
    (* Search words without wildcards *)
    searchProc := SearchWordOnly
  ELSE
    (* Full search without wildcards *)
    searchProc := SearchKMP;
    (* Calculate failure function *)
    t := 0; func[1] := 0C;
    FOR s := 1 TO ORD(pattern[0])-1 DO
      WHILE (t#0) & (pattern[s+1]#pattern[t+1]) DO t := ORD(func[t]) END;
      IF pattern[s+1]=pattern[t+1] THEN 
        INC(t); func[s+1] := CHR(t); 
      ELSE
        func[s+1] := 0C;
      END; (* IF *)
    END; (* FOR *)
  END; (* IF *)
  IF NOT oneScreen THEN count := lastrec ELSE count := N END;
  ScanReset(FALSE, upcase, dtDummy, kwDummy, rcDisplay, oneScreen);
END Scan;


END KDFileScan.
