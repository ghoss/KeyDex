(*****************************************************************************
*
* Name     : KDEdit
*
* LastEdit : 04/04/87
* Project  : KeyDex system
* Purpose  : Screen editor
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
* 29/03/87 : Implementation of "SpecialKey".
* 04/04/87 : Fixed typo in "EOL()".
* 25/05/87 : Modified function of "RemoveHighLight".
* 06/06/87 : Put editor pages into dynamic storage.
* 13/06/87 : Removed "oldattr".
*            Address of "pg" dynamic.
* 16/06/87 : Implemented address offset correction for editor pages.
* 17/06/87 : Implemented insert mode.
*            Fixed bug in "END" key code.
* 19/06/87 : Bug fixed in "SetAttr".
* 21/06/87 : Corrections in "UserClearPages" and "UndoClear".
* 09/08/87 : Address arithmetic for inserting and deleting characters.
*            Backspace in insert mode moves characters to the left.
* 05/11/87 : m2c 3.0 : Fixed storage problems.
* 03/12/87 : Added 'InverseNormalize' in one place due to adr subtraction.
* 24/12/87 : Added calls to help system.
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

IMPLEMENTATION MODULE KDEdit;


FROM SYSTEM IMPORT 
  TSIZE, ADR, ADDRESS, BYTE;

FROM LongCardinals IMPORT
  zero;

FROM SystemIO IMPORT
  ClearArea, white, reverse, intensity, underline, 
  blank, ScrCh, SetCursor, CursorOff, blink, ScrollArea, ReadChar,
  ReturnBufferAddress, HelpKey;

FROM KDStorage IMPORT
  ALLOCATE, DEALLOCATE, Normalize, InverseNormalize;

FROM KDScreen IMPORT
  Beep, DispRecordKey, Indicator, SetIndicator, DispPageNo, SetStatus;

FROM Term IMPORT
  EraseEOL, CursorX, CursorY, WriteAOC, Font;

FROM Keywords IMPORT
  ClearList, BackupList, RestoreList, loaded, ValidKeywordChar, FileLoaded;

FROM String IMPORT
  AppendCh, Clear;

FROM KDConfig IMPORT
  tabInterval, DateChar, DateType;

FROM Menus IMPORT
  WarningBox;
  
FROM HelpContext IMPORT
  ModuleIndex, ScreenNumber;
  
FROM HelpSystem IMPORT
  SetHelpContext, EnterHelpSystem, HelpInstalled;


CONST
  CR     = 15C;
  BS     = 10C;
  TAB    = 11C;
  ctrle  = 5C;
  ctrln  = 16C;
  ctrly  = 31C;

  f2     = 60;
  f3     = 61;
  f4     = 62;
  f5     = 63;
  f6     = 64;
  f7     = 65;
  f8     = 66;
  f9     = 67;
  f10    = 68;
  altf3  = 106;
  altf4  = 107;
  altf5  = 108;
  altf7  = 110;
  altf8  = 111;
  altf10 = 113;

  altp   = 25;
  alta   = 30;
  altk   = 37;
  altf   = 33;
  altc   = 46;
  alts   = 31;
  altq   = 16;
  home   = 71;
  up     = 72;
  pgup   = 73;
  left   = 75;
  right  = 77;
  end    = 79;
  down   = 80;
  pgdn   = 81;
  ins    = 82;
  del    = 83;
  cleft  = 115;
  cright = 116;
  cend   = 117;
  cpgdn  = 118;
  chome  = 119;
  cpgup  = 132;
  ltab   = 15;


TYPE
  CRange  = [MinC..MaxC];
  RRange  = [MinR..MaxR];
  CharPtr = POINTER TO ScrCh;


VAR
  cx : CRange;
  cy : RRange;

  (* This page is used for faster copy operations *)
  blankp,
  pg      : PagePtr;
  pg1     : ADDRESS;
  cur,hlp : [1..MaxPage];
  backup  : PagePtrArray;

  calledFromEdit,
  templateBK : BOOLEAN;

  i : CARDINAL;



PROCEDURE SetAttr(a : CARDINAL);
VAR
  a1 : CARDINAL;
BEGIN
  a1 := ORD(attr);
  CASE a OF
    white :
      a1 := a;
    (** 19/06/87 : bug fixed (inv couldn't be switched off) **)
   |reverse : 
      IF a1=reverse THEN a1 := white ELSE a1 := a END;
   |intensity : 
      CASE a1 OF
        white,reverse : a1 := white+intensity
      | white+intensity : a1 := white
      | underline : a1 := underline+intensity
      | underline+intensity : a1 := underline
      ELSE 
      END; (* CASE *)
   |underline :
      CASE a1 OF
        underline : a1 := white
      | white+intensity : a1 := underline+intensity
      | underline+intensity : a1 := white+intensity
      ELSE
        a1 := underline
      END; (* CASE *)
  ELSE
  END;  (* CASE *)
  attr := VAL(BYTE,a1);
END SetAttr; 



PROCEDURE HighLight(fromLine, fromPos, toLine, toPos:CARDINAL);
BEGIN
  hlp := cur;
  WHILE (fromLine<toLine) OR ((fromLine=toLine) & (fromPos<=toPos)) DO
    (* PutChar[..] *)
    pg^[fromLine,fromPos].at := BYTE(white+blink+intensity);
    INC(fromPos); IF fromPos>MaxC THEN fromPos := MinC; INC(fromLine) END;
  END; (* WHILE *)
END HighLight;


PROCEDURE RemoveHighLight;
BEGIN
  IF cur=hlp THEN RedrawCurrentPage END;
END RemoveHighLight;



PROCEDURE ScanSymbol(VAR ch:CHAR):BOOLEAN;
BEGIN
  RETURN ((ch>='0') AND (ch<='9')) 
         OR ((CAP(ch)>='A') AND (CAP(ch)<='Z'))
         OR ((ch>='€') AND (ch<='§'));
END ScanSymbol;



PROCEDURE SkipToNextField();
(* Active in template mode *)
VAR
  Count, Lap : CARDINAL;
BEGIN
  Count := (MaxC-MinC+1)*(MaxR-MinR+1);
  FOR Lap := 1 TO 2 DO
    WHILE (((pg^[cy,cx].at#BYTE(reverse)) AND (Lap=1)) OR
          ((pg^[cy,cx].at=BYTE(reverse)) AND (Lap=2))) AND (Count>0) DO
      DEC(Count);
      IF cx<MaxC THEN INC(cx) 
      ELSE 
        cx := MinC; 
        IF cy<MaxR THEN INC(cy) 
        ELSE
          cy := MinR 
        END;
      END;
    END;
  END;
  IF (Count>0) AND (cx<MaxC) THEN INC(cx) END;
END SkipToNextField;



PROCEDURE EOL():CRange;
VAR
  x : CRange;
BEGIN
  x := MaxC;
  WHILE (x>MinC) DO
    WITH pg^[cy,x] DO
      IF (ch=' ') THEN
        IF (at=BYTE(white)) OR (at=BYTE(white+intensity)) THEN DEC(x)
        ELSIF x<>MaxC THEN RETURN x+1 ELSE RETURN x END;
      ELSIF x<>MaxC THEN RETURN x+1 ELSE RETURN x END;
    END;
  END;
  RETURN MinC+1; 
END EOL;


PROCEDURE LeftScan(VAR cxr:CRange; VAR cyr:RRange);
(* Sets cxr/cyr to beginning of previous word *)
VAR
  Scan : BOOLEAN;
  cx1  : CRange;
  cy1  : RRange;
  c1,c2 : CHAR;
BEGIN
  cxr := cx; cyr := cy;
  Scan := TRUE; 
  IF cxr > MinC THEN DEC(cxr) ELSIF cyr > MinR THEN DEC(cyr); cxr := MaxC END;
  c2 := pg^[cyr,cxr].ch;
  WHILE Scan AND ((cyr#MinR) OR (cxr#MinC)) DO
    cx1 := cxr; cy1 := cyr;
    IF cxr > MinC THEN DEC(cxr) ELSIF cyr > MinR THEN DEC(cyr); cxr := MaxC END;
    c1 := pg^[cyr,cxr].ch;
    IF (c1=' ') AND (c2#' ') THEN
      Scan := FALSE; cxr := cx1; cyr := cy1;
    ELSE
      c2 := c1
    END;
  END;
  IF Scan THEN cxr := MinC; cyr := MinR END;
END LeftScan;



PROCEDURE DisplayPage(pageno:CARDINAL);
BEGIN
  CursorOff; cx := MinC; cy := MinR;
  pg^ := page[pageno]^;
(*  FOR y := MinR TO MaxR DO
    FOR x := MinC TO MaxC DO
      PutChar(pg[y,x],x,y);
    END;
  END; *)
  IF template THEN SkipToNextField END;
  DispPageNo(pageno); cur := pageno; new := FALSE
END DisplayPage;



PROCEDURE RedrawCurrentPage;
BEGIN
  IF new THEN cur := 1 END;
  DisplayPage(cur); SetStatus;
END RedrawCurrentPage;


PROCEDURE SpecialKey(c:CHAR);
(* Handles page up/down etc. *)
BEGIN
  IF calledFromEdit THEN page[cur]^ := pg^; calledFromEdit := FALSE END;
  CASE ORD(c) OF
    pgup  : IF cur > 1 THEN DisplayPage(cur-1) END;
  | pgdn  : IF cur < MaxPage THEN DisplayPage(cur+1) END;
  | cpgup : DisplayPage(1);
  | cpgdn : DisplayPage(MaxPage);
  ELSE
  END; (* CASE *)
END SpecialKey;



PROCEDURE Edit(VAR rt:CARDINAL);
VAR
  ch,c1,c2 : CHAR;
  fk,Scan,exit,res : BOOLEAN;
  sc     : ScrCh;
  cy1,ly : RRange;
  cx1    : CRange;
  tab    : INTEGER;
  a      : BYTE;
                                                  
    PROCEDURE EnterChar;
    VAR
      t : CRange;
    BEGIN
      sc.ch := ch; 
      IF (ch=' ') AND (attr=BYTE(white+intensity)) THEN sc.at := BYTE(white)
      ELSE sc.at := attr END;
      saved := FALSE; 
      CursorOff; (*PutChar(sc,cx,cy);*)
      (* Insert/Overwrite mode *)
      IF NOT insertMode THEN
        pg^[cy,cx] := sc;
      ELSE
        CursorOff; InsertChar(sc);
      END; (* IF *)
      IF cx < MaxC THEN
        IF cx > (MaxC - 5) THEN Beep(edit) END;
        INC(cx);
      ELSIF cy < MaxR THEN
        cy1 := cy; INC(cy); cx := MinC;
        (* Word wraparound *)
        IF wordwrap AND ScanSymbol(ch) THEN
          cx1 := MaxC;
          WHILE ScanSymbol(pg^[cy1,cx1].ch) AND (cx1 > MinC) DO DEC(cx1) END;
          FOR t := cx1+1 TO MaxC DO
            (*GetChar(sc,t,cy1); PutChar(sc,cx,cy);*)
            pg^[cy,cx] := pg^[cy1,t]; INC(cx);
            (*PutChar(blank,t,cy1);*) pg^[cy1,t] := blank;
          END;
        END;
      END;
    END EnterChar;

    PROCEDURE InsertChar(sc : ScrCh);
    (* Inserts one character at current cursor position *)
    VAR
      t      : CRange;
      q1, q2 : CharPtr;
    BEGIN
      t := EOL(); q1 := CharPtr(ADR(pg^[cy,t])); 
      (** 03/12/87 **)
      InverseNormalize(q1);
      WHILE t > cx DO
        q2 := CharPtr(ADDRESS(q1) - TSIZE(ScrCh));
        (*GetChar(sc,t-1,cy); PutChar(sc,t,cy);*)
        q1^ := q2^; DEC(t); q1 := q2;
      END; (* WHILE *)
      (*PutChar(sc,cx,cy);*)
      pg^[cy,cx] := sc; 
    END InsertChar;

    PROCEDURE DeleteChar;
    (* Deletes character at current cursor position *)
    VAR
      q1, q2 : CharPtr;
      m, t   : CRange;
    BEGIN
      q1 := CharPtr(ADR(pg^[cy,cx])); Normalize(q1);
      m := EOL(); t := cx;
      WHILE t < m DO
        q2 := CharPtr(ADDRESS(q1) + TSIZE(ScrCh)); q1^ := q2^;
        INC(t); q1 := q2;
      END; (* WHILE *)
      pg^[cy,m] := blank;
      (*
      WHILE t<m DO
        GetChar(sc,t+1,cy); PutChar(sc,t,cy);
        pg^[cy,t] := pg^[cy,t+1]; INC(t); 
      END; (* WHILE *)
      PutChar(blank,m,cy); pg^[cy,m] := blank;
      *)
    END DeleteChar;

    PROCEDURE ValidKWChar(c : CHAR) : BOOLEAN;
    BEGIN
      RETURN ValidKeywordChar(c) OR (c = DateChar[DateType]);
    END ValidKWChar;

BEGIN (* Edit *)
  SetIndicator(edit); exit := FALSE;
  CursorX := 0; CursorY := 0; Font := BYTE(white);
  IF HelpInstalled() THEN
    WriteAOC('[F9] = Activate menu. [F1] = On-line help'); 
  ELSE
    WriteAOC('[F9] = Activate menu.'); 
  END; (* IF *)
  EraseEOL; INC(CursorY); CursorX := 0;
  WriteAOC("File     Keys     Print     Clear     Attributes     Settings     Quit");
  EraseEOL;

  IF new THEN
    DisplayPage(1); saved := TRUE;
    IF NOT loaded THEN DispRecordKey(0,zero) END;
  END; (* IF *)

  LOOP
    SetStatus; SetCursor(cx,cy); ReadChar(ch,fk);

    IF fk & HelpKey(ch) THEN
      SetHelpContext(ScreenNumber(kdedit, 1)); 
      EnterHelpSystem;
      SetHelpContext(-1);
    ELSIF fk THEN
      CASE ORD(ch) OF
        (* Direction keys *)
         down  : IF cy < MaxR THEN INC(cy) END;
        |up    : IF cy > MinR THEN DEC(cy) END;
        |right : IF cx < MaxC THEN 
                   IF cx > (MaxC - 5) THEN Beep(edit) END;
                   INC(cx);
                 ELSIF cy < MaxR THEN INC(cy); cx := MinC; END;
        |left  : IF cx > MinC THEN DEC(cx)
                 ELSIF cy > MinR THEN DEC(cy); cx := MaxC; END;
        
        (* Compound keystrokes *)
        |chome : cx := MinC; cy := MinR;
        |cend  : cx := MinC; cy := MaxR;
        |home  : cx := MinC;
        |end   : cx := EOL(); a := pg^[cy,MinC].at;
                 (** Bug fixed : 17/06/87 **)
                 IF   (cx = (MinC+1)) 
                    & (pg^[cy,MinC].ch = ' ')
                    & ((a = BYTE(white)) OR (a = BYTE(white+intensity))) 
                 THEN
                   DEC(cx);
                 ELSIF (cx > MinC) & (cx < MaxC) THEN
                   IF (pg^[cy,cx-1].at = BYTE(reverse)) THEN
                     (** 09/08/87 **)
                     INC(cx);
                   END; (* IF *)
                 END; (* IF *)
        |ins   : insertMode := NOT insertMode;
        |del   : DeleteChar; saved := FALSE;
        |pgup,pgdn,cpgup,cpgdn: 
                 calledFromEdit := TRUE; SpecialKey(ch);
        |ltab  : tab := VAL(INTEGER,cx);
                 IF (tab MOD tabInterval)=0 THEN 
                   DEC(tab,tabInterval) 
                 ELSE 
                   DEC(tab,tab MOD tabInterval)
                 END;
                 IF tab<MinC THEN cx := MinC ELSE cx := VAL(CARDINAL,tab) END;
        |cleft : LeftScan(cx,cy);
        |cright: Scan := TRUE; ly := cy;
                 IF cx < MaxC THEN INC(cx) ELSIF cy < MaxR THEN INC(cy); cx := MinC END;
                 c1 := pg^[cy,cx].ch;
                 WHILE Scan AND ((cy#MaxR) OR (cx#MaxC)) DO
                   IF cx < MaxC THEN INC(cx) ELSIF cy < MaxR THEN INC(cy); cx := MinC END;
                   c2 := pg^[cy,cx].ch;
                   IF ((c1=' ') OR (c1='-')) AND ((c2#' ') AND (c2#'-')) THEN
                     Scan := FALSE;
                   ELSE
                     c1 := c2
                   END;
                 END;
                 IF Scan THEN cy := ly; cx := EOL() END;

        (* Speed keys *)
        |f10    : wordwrap := NOT wordwrap; 
        |altf10 : template := NOT template; 
        |f2   : IF (NOT saved) & FileLoaded() THEN WarningBox(res) ELSE res := TRUE END;
                IF res THEN
                  page[cur]^ := pg^; UserClearPages(TRUE);
                END; (* IF *)
        |f3   : rt := 8; exit := TRUE;   (* Keyword get *)
        |f4   : rt := 9; exit := TRUE;   (* Keyword put *)

        |f5   : (* Create default keyword *)
                LeftScan(cx1,cy1); Clear(defaultkw); c1 := pg^[cy1,cx1].ch;
                WHILE ((cy1#cy) OR (cx1#cx)) DO
                  IF ValidKWChar(c1) THEN 
                    AppendCh(defaultkw,c1);
                    IF c1='_' THEN (* PutChar(blank,cx1,cy1) *)
                      pg^[cy1,cx1].ch := ' '
                    END;
                  ELSE 
                    Clear(defaultkw)
                  END;
                  IF cx1 < MaxC THEN INC(cx1) ELSIF cy1 < MaxR THEN INC(cy1); cx1 := MinC END;
                  c1 := pg^[cy1,cx1].ch;
                END;
                rt := 10; exit := TRUE; 
        |altf5: rt := 15; exit := TRUE;
        |f6   : rt := 11; exit := TRUE;  (* File load *)

        |f7   : attr := BYTE(white);
        |f8   : SetAttr(intensity);
        |altf7   : SetAttr(reverse);
        |altf8   : SetAttr(underline);

        |f9   : rt := 0; exit := TRUE;
        |altk : rt := 2; exit := TRUE;
        |altf : rt := 1; exit := TRUE;
        |altp : rt := 3; exit := TRUE;
        |altc : rt := 4; exit := TRUE;
        |alta : rt := 5; exit := TRUE;
        |alts : rt := 6; exit := TRUE;
        |altq : rt := 7; exit := TRUE;
        |altf3 : rt := 12; exit := TRUE;
        |altf4 : rt := 13; exit := TRUE;
      ELSE
      END;
      IF exit THEN page[cur]^ := pg^; RETURN END;

    (* Alphanumeric characters *)
    ELSIF (ch>=' ') AND (ch<=CHR(254)) THEN EnterChar

    (* Other functions *)
    ELSIF (ch=CR) THEN
      IF template THEN SkipToNextField
      ELSE
        IF cy < MaxR THEN INC(cy); cx := MinC END;
      END;

    ELSIF (ch=BS) THEN
      IF ((cy > MinR) OR (cx > MinC)) THEN
        IF cx > MinC THEN
          DEC(cx); 
          IF insertMode THEN DeleteChar ELSE pg^[cy,cx] := blank END;
        ELSIF NOT insertMode THEN
          DEC(cy); cx := MaxC; 
          pg^[cy, MaxC] := blank;
        END; (* IF *)
        saved := FALSE;
      END; (* IF *)

    ELSIF (ch=TAB) THEN
      tab := cx-(cx MOD VAL(CRange,tabInterval))+VAL(CRange,tabInterval);
      IF tab>MaxC THEN cx := MaxC ELSE cx := VAL(CARDINAL,tab) END;

    ELSIF (ch=ctrle) THEN
      FOR cx1 := cx TO EOL() DO 
        pg^[cy,cx1] := blank; (*PutChar(blank,cx1,cy);*)
      END;
      saved := FALSE;

    ELSIF (ch=ctrly) THEN
      (*FOR cx1 := MinC TO MaxC DO
        FOR cy1 := cy TO MaxR-1 DO pg^[cy1,cx1] := pg^[cy1+1,cx1] END;
        pg^[MaxR,cx1] := blank;
      END;*)
      ScrollArea(MinC,MaxC,cy,MaxR,1); saved := FALSE;

    ELSIF (ch=ctrln) THEN
      CursorOff;
      (*FOR cx1 := MinC TO MaxC DO
        FOR cy1 := MaxR TO cy+1 BY -1 DO pg^[cy1,cx1] := pg^[cy1-1,cx1] END;
        pg^[cy,cx1] := blank;
      END;*)
      ScrollArea(MinC,MaxC,cy,MaxR,-1); saved := FALSE;
    END; (* IF *)
  END; (* LOOP *)
END Edit;
      


PROCEDURE ClearPages(draw:BOOLEAN);
VAR
  i : [1..MaxPage];
BEGIN
  FOR i := 1 TO MaxPage DO page[i]^ := blankp^ END;
  ClearList; loaded := FALSE; saved := TRUE;
  IF draw THEN 
    ClearArea(MinC,MaxC,MinR,MaxR);
    DispRecordKey(0,zero);  (** 08/06/87 **)
  END; (* IF *)
  attr := BYTE(white); 
  cx := MinC; cy := MinR; new := TRUE; template := FALSE; cur := 1;
END ClearPages;



PROCEDURE UserClearPages(fromF2:BOOLEAN);
VAR
  i : CARDINAL;
BEGIN
  UndoFlag := NOT (loaded OR saved);
  IF UndoFlag THEN 
    templateBK := template;
    BackupList; 
    (** 21/06/87 **)
    FOR i := 1 TO MaxPage DO
      NEW(backup[i]); backup[i]^ := page[i]^;
    END; (* FOR *)
  END; (* IF *)
  ClearPages(fromF2);
END UserClearPages;



PROCEDURE UndoClear;
VAR
  i : CARDINAL;
BEGIN
  (** 21/06/87 **)
  FOR i := 1 TO MaxPage DO
    page[i]^ := backup[i]^; DISPOSE(backup[i]);
  END; (* FOR *)
  RestoreList;
  template := templateBK; 
  UndoFlag := FALSE; saved := FALSE; loaded := FALSE;
  DisplayPage(1); 
END UndoClear;


BEGIN (* KDEdit *)
  (* Assign permanent addresses to editor pages *)
  FOR i := 1 TO MaxPage DO 
    NEW(page[i]); (** 05/11/87 **) Normalize(page[i]); 
  END; (* FOR *)
  NEW(blankp);

  (* Awful hack! *)
  ReturnBufferAddress(pg1); Normalize(pg1); 
  INC(pg1,480);  (* Equals 3 lines *)
  pg := PagePtr(pg1);

  FOR cx := MinC TO MaxC DO
    FOR cy := MinR TO MaxR DO
      blankp^[cy,cx] := blank
    END; (* FOR *)
  END; (* FOR *)
  saved := TRUE; Clear(defaultkw); wordwrap := FALSE; insertMode := FALSE;
  UndoFlag := FALSE; calledFromEdit := FALSE;
END KDEdit.
