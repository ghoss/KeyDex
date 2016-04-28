(*****************************************************************************
*
* Name     : Term
*
* LastEdit : 22/04/87
* Project  : KeyDex system
* Purpose  : Terminal-oriented I/O functions
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
* 22/04/87 : Implemented insert mode in "ReadString".
* 05/06/87 : Adjusted "ReadString" to new "HelpProc" definition.
* 06/06/87 : Implemented quote checking in "ReadString".
* 16/06/87 : first remains TRUE if it was TRUE before F1 or F2.
* 20/06/87 : Fixed bug in "ReadString" (quote toggle).
* 31/07/87 : Changed help procedure semantics in "ReadString".
* 02/08/87 : Address arithmetic in output procedures.
* 17/08/87 : Fixed bug in "ReadString".
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

IMPLEMENTATION MODULE Term;


FROM SYSTEM IMPORT
  ADR, ADDRESS, BYTE;

FROM SystemIO IMPORT
  blank, MaxX, MaxY, ScrCh, ClearScreen, ScrollArea, PutChar, white,
  CursorOff, SetCursor, ReadChar, ScrX, intensity, HelpKey;
  
FROM String IMPORT
  AssignAOC, InsertCh, Clear, Delete, Insert, Assign;
  
FROM HelpSystem IMPORT
  EnterHelpSystem;


CONST
  ESC = 27;    (* Some control character codes *)
  BS  = 8;
  CR  = 13;
  LF  = 11;
  FF  = 12;

  ctrle = 5;
  left  = 75;
  right = 77;
  home  = 71;
  end   = 79;
  del   = 83;
  altf1 = 104;
  altf2 = 105;

  MaxX1  = MaxX+1;
  indpos = MaxX-5;



PROCEDURE EraseEOL();
VAR
  x,max : ScrX;
BEGIN
  IF CursorY=0 THEN max := indpos-1 ELSE max := MaxX END;
  FOR x := CursorX TO max DO PutChar(blank,x,CursorY) END;
END EraseEOL;



PROCEDURE ValidChar(VAR C:CHAR) : BOOLEAN;
BEGIN
  RETURN ((C>=' ') AND (C<='þ')) OR (C=CHR(CR)) OR (C=CHR(BS)) OR (C=CHR(ESC));
END ValidChar;



PROCEDURE Write(c:CHAR);
VAR
  sc : ScrCh;
BEGIN
  IF (c>=' ') AND (c<='þ') THEN
    sc.ch := c; sc.at := Font; PutChar(sc, CursorX, CursorY);
    CursorX := (CursorX + 1) MOD MaxX1;
    IF CursorX=0 THEN
      IF CursorY<MaxY THEN INC(CursorY)
                      ELSE ScrollArea(0,MaxX,0,MaxY,1);
      END;
    END;
  ELSIF (c=CHR(CR)) THEN CursorX := 0;
  ELSIF (c=CHR(LF)) THEN
    IF CursorY<MaxY THEN INC(CursorY)
                    ELSE ScrollArea(0,MaxX,0,MaxY,1);
    END;
  ELSIF (c=CHR(BS)) THEN
    IF CursorX>0 THEN DEC(CursorX)
    ELSE
      IF CursorY>0 THEN
        DEC(CursorY); CursorX := MaxX;
      END;
    END;
  ELSIF (c=CHR(FF)) THEN
    ClearScreen; CursorX := 0; CursorY := 0;
  END;
END Write;


PROCEDURE WriteString(VAR s:ARRAY OF CHAR);
TYPE
  CharPtr = POINTER TO CHAR;
VAR
  i : CARDINAL;
  p : CharPtr;
BEGIN
  p := ADR(s) + 1;
  i := ORD(s[0]);
  WHILE i > 0 DO
    DEC(i); Write(p^); p := CharPtr(ADDRESS(p) + 1);
  END; (* WHILE *)
END WriteString;



PROCEDURE WriteAOC(s:ARRAY OF CHAR);
TYPE
  CharPtr = POINTER TO CHAR;
VAR
  i : INTEGER;
  p : CharPtr;
BEGIN
  p := ADR(s);
  i := HIGH(s);
  WHILE i >= 0 DO
    DEC(i); Write(p^); p := CharPtr(ADDRESS(p) + 1);
  END; (* WHILE *)
END WriteAOC;



PROCEDURE WriteCard(x,n:CARDINAL; null:BOOLEAN);
VAR
  s  : ARRAY [0..MaxX1] OF CHAR;
  n1 : CARDINAL;
  ch : CHAR;
BEGIN
  Clear(s); n1 := 0;
  REPEAT
    InsertCh(s,CHR((x MOD 10)+ORD('0')),1); INC(n1);
    x := x DIV 10;
  UNTIL x=0;
  IF null THEN ch := '0' ELSE ch := ' ' END;
  WHILE n1<n DO InsertCh(s,ch,1); INC(n1) END;
  WriteString(s);
END WriteCard;




PROCEDURE Read(VAR c:CHAR);
VAR
  FK : BOOLEAN;
BEGIN
  REPEAT ReadChar(c,FK) UNTIL (NOT FK) AND ValidChar(c);
END Read;



PROCEDURE ReadString(VAR s:ARRAY OF CHAR; hp:HelpProc; vp:ValidProc;
                     VAR newtype:BOOLEAN);

  PROCEDURE Redraw;
  VAR
    j : CARDINAL;
    i : ScrX;
  BEGIN
    j := startpos;
    FOR i := CursorX TO MaxX DO 
      WITH c DO
        at := BYTE(white+intensity);
        IF j<=ORD(s[0]) THEN ch := s[j] ELSE ch := ' ' END;  
        PutChar(c,i,CursorY);
      END;
      INC(j);
    END;
  END Redraw;

  PROCEDURE InsertString;
  (* Inserts s1 at current cursor position *)
  BEGIN
    IF first AND (s1[0]#0C) THEN
      Assign(s,s1);
    ELSE
      Insert(s,s1,i);
    END; (* IF *)
    len := ORD(s1[0]); 
    (** 19/06/87 : Changed -i+1 -> +1-i **)
    IF len>((HIGH(s)+1)-i) THEN i := HIGH(s) ELSE INC(i,len) END;
    IF (i-startpos)>displen THEN
       (** 17/08/87 **)
       startpos := i - (displen DIV 2);
    END; (* IF *)
    curpos := CursorX+(i-startpos); Redraw;
  END InsertString;

VAR
  startpos,i,j,displen,len : CARDINAL;
  curpos : ScrX;
  ch : CHAR;
  fk,first,toggle,res : BOOLEAN;
  olds,s1 : ARRAY [0..255] OF CHAR;
  c  : ScrCh;
BEGIN
  Done := FALSE;
  CursorOff; startpos := 1; i := 1; curpos := CursorX; Assign(olds,s);
  Redraw; first := TRUE; newtype := FALSE; displen := (MaxX-CursorX);
  LOOP
    SetCursor(curpos,CursorY); ReadChar(ch,fk);
    IF fk & HelpKey(ch) THEN
      EnterHelpSystem;
    ELSIF fk THEN
      CASE ORD(ch) OF
         left: IF i>1 THEN 
                 DEC(i);
                 IF curpos>CursorX THEN DEC(curpos) ELSE DEC(startpos); Redraw END;
               END;
        |right: IF i<=ORD(s[0]) THEN
                  INC(i);
                  IF curpos<MaxX THEN INC(curpos) ELSE INC(startpos); Redraw END;
                END;
        |end : i := ORD(s[0])+1; 
               IF (i-startpos)>displen THEN
                 (** 17/08/87 **)
                 startpos := i - (displen DIV 2); Redraw;
               END; (* IF *)
               curpos := CursorX+(i-startpos);
        |home: i := 1; startpos := 1; curpos := CursorX; Redraw;
        |del : IF s[0]<>0C THEN Delete(s,i,1); Redraw END;
        |altf1, altf2 : 
           res := hp(ORD(ch)-altf1,s1); 
           IF res THEN
             Assign(s, s1); Done := TRUE; newtype := TRUE;
             CursorOff; RETURN
           ELSE
             InsertString;
           END; (* IF *)
      ELSE END;
      (** 16/06/87 **)
      IF    (NOT first) 
         OR ((ch # CHAR(altf1)) & (ch # CHAR(altf2))) 
         OR (s1[0] # 0C) 
      THEN
        first := FALSE
      END; (* IF *)
    ELSE
      CASE ORD(ch) OF
         ESC : Assign(s,olds); CursorOff; RETURN;
        |CR  : CursorOff; Done := TRUE; newtype := NOT first; RETURN;
        |BS  : IF i>1 THEN
                 DEC(i); 
                 IF curpos>CursorX THEN DEC(curpos) ELSE DEC(startpos) END;
                 Delete(s,i,1); Redraw; first := FALSE;
               END;
        |ctrle : Delete(s,i,ORD(s[0])); Redraw; first := FALSE;
        |57B : (* "/" *) 
               toggle := TRUE;
               (** 20/06/87 : check only to current cursor pos **)
               FOR j := 1 TO (i-1) DO
                 IF s[j]='"' THEN toggle := NOT toggle END;
               END; (* FOR *)
               res := FALSE;
               IF toggle THEN res := hp(2,s1) ELSE AssignAOC(s1,'/') END;
               IF res THEN
                 Assign(s, s1); Done := TRUE; newtype := TRUE;
                 CursorOff; RETURN
               ELSE
                 InsertString; first := FALSE;
               END; (* IF *)
      ELSE
        IF (vp(ch) AND (i<=HIGH(s))) THEN
          IF first THEN 
            Clear(s); first := FALSE;
          END;
          InsertCh(s,ch,i); INC(i);
          IF curpos<MaxX THEN
            (* Insert character on current line *)
            INC(curpos);
          ELSE
            (* Scroll line to the right *)
            INC(startpos);
          END; (* IF *)
          Redraw;
        END; (* IF *)
      END; (* CASE *)
    END; (* IF *)
  END; (* LOOP *)
END ReadString;


BEGIN
  Done := FALSE; CursorX := 0; CursorY := 0; Font := BYTE(white);
END Term.
