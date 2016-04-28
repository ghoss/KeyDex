(*****************************************************************************
*
* Name     : Menus
*
* LastEdit : 29/03/87
* Project  : KeyDex system
* Purpose  : Provides menu operations
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
* 25/04/87 : Fixed bug in "ScrollBox" (END key handling).
* 05/06/87 : Added beeping.
*            Implemented dynamic menu structure.
* 14/06/87 : Fixed bug in "ScrollBox" (END key)
* 19/06/87 : Implemented "DateBox".
* 16/07/87 : Changed window stack to three.
* 23/07/87 : Implemented disk error box.
* 04/10/87 : Highlight skips non-selectable items in pull-down menu.
* 25/10/87 : Added copy-protection code.
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

IMPLEMENTATION MODULE Menus;


FROM SYSTEM IMPORT 
  TSIZE, BYTE;

FROM SystemIO IMPORT
  white, reverse, blank, MaxX, MaxY, ScrX, ScrY, ScrCh, Handle,
  ClearArea, ScrollArea, SaveArea, RestoreArea, intensity, PutChar, GetChar,
  ReadChar, SetCursor, underline, HelpKey;

FROM KDStorage IMPORT
  ALLOCATE;

FROM TimeDate IMPORT
  Time, GetTime;

FROM Term IMPORT
  WriteAOC, Write, WriteString, CursorX, CursorY, Font, HelpProc, ValidProc,
  ReadString, Read, EraseEOL, Done;

FROM KDScreen IMPORT 
  SetIndicator, Indicator, Beep;

FROM KDConfig IMPORT
  DateType, DateChar, StrToDate, DateToStr;

FROM String IMPORT
  Clear, Assign;
  
FROM HelpSystem IMPORT
  EnterHelpSystem;

FROM CopyProtect IMPORT
  CheckPermission;


CONST
  BoxLen    = 15;     (* Vertical length of a scroll box *)
  inputText = 'Enter input string; [RETURN] = accept, [ESC] = abort            ';
  maxWin    = 3;

  left  = 75;         (* keyboard scan codes *)
  right = 77;
  up    = 72;
  down  = 80;
  home  = 71;
  end   = 79;
  pgup  = 73;
  pgdn  = 81;
  CR    = 15C;
  ESC   = 33C;


TYPE
  MessageType = ( mtErrorMsg, mtNoteMsg, mtDiskError, mtCancelOffer );


VAR
  menus : ARRAY [1..maxWin] OF Handle;     (* Window stack *)
  ptr   : [0..maxWin];
  pos   : ARRAY EntryRange OF ScrX;
  oldsel: CARDINAL;



PROCEDURE CreateMenu(n:CARDINAL; VAR mb:MenuBarPtr);
BEGIN
  ALLOCATE(mb, TSIZE(EntryRange)+TSIZE(ScrX)+TSIZE(BITSET)+
               n*(TSIZE(Keyword)+TSIZE(HelpText)) );
  mb^.entries := n;
END CreateMenu;



PROCEDURE DrawBox(x0,x1:ScrX; y0,y1:ScrY; VAR h:Handle; 
                  VAR res:BOOLEAN);
(* Draws a box at the specified position and returns a handle to the
   background *)
VAR
  c : ScrCh;
  y : ScrY;
  x : ScrX;
  b : BOOLEAN;
BEGIN
  SaveArea(x0,x1,y0,y1,h,res);
  GetChar(c,x0,y0); b := (c.ch='Ä');
  WITH c DO IF b THEN ch := 'Ò'; ELSE ch :='É' END; at := BYTE(white); END;
  PutChar(c,x0,y0); GetChar(c,x1,y0);
  WITH c DO IF b THEN ch := 'Ò' ELSE ch :='»' END; at := BYTE(white); END;
  PutChar(c,x1,y0);

  WITH c DO ch := 'º'; at := BYTE(white) END;
  FOR y := y0+1 TO y1-1 DO PutChar(c,x0,y); PutChar(c,x1,y) END;

  c.ch := 'È'; PutChar(c,x0,y1); c.ch := '¼'; PutChar(c,x1,y1); c.ch := 'Í';
  INC(x0); DEC(x1);
  FOR x := x0 TO x1 DO PutChar(c,x,y1) END;
  IF NOT b THEN FOR x := x0 TO x1 DO PutChar(c,x,y0) END END;
  ClearArea(x0,x1,y0+1,y1-1);
  DEC(x0); INC(x1);
END DrawBox;




PROCEDURE GenericMessage
            ( VAR s      : ARRAY OF CHAR; 
              typ        : MessageType;
              VAR action : BOOLEAN );
VAR
  f : BYTE;
  x0,len : ScrX;
  res,
  err : BOOLEAN;
  ch  : CHAR;
  h   : Handle;
BEGIN
  IF typ # mtCancelOffer THEN
    CursorX := 0; CursorY := 0; EraseEOL;
  END; (* IF *)
  f := Font; len := HIGH(s)+1; x0 := (MaxX-len-8) DIV 2;
  err := (typ = mtErrorMsg) OR (typ = mtDiskError);
  IF err THEN 
    SetIndicator(error); INC(len,7); Beep(error);
  ELSE 
    SetIndicator(note); Beep(note);
  END; (* IF *)
  DrawBox(x0,x0+len+8,9,15,h,res);
  IF NOT res THEN RETURN END;
  CursorX := x0+4; CursorY := 11; Font := BYTE(white+intensity);
  IF err THEN WriteAOC("ERROR: ") END;
  WriteAOC(s); INC(CursorY,2); 
  CursorX := x0+4+(len-15) DIV 2;
  res := (typ = mtDiskError);
  IF res OR (typ = mtCancelOffer) THEN
    Font := BYTE(underline + intensity);
    IF res THEN Write('R') ELSE Write('C') END;
    Font := BYTE(white); 
    IF res THEN WriteAOC('etry or ') ELSE WriteAOC('ontinue or ') END;
    Font := BYTE(underline + intensity); 
    IF res THEN Write('C') ELSE Write('A') END;
    Font := BYTE(white);
    IF res THEN WriteAOC('ancel?') ELSE WriteAOC('bort?') END;
    REPEAT
      Read(ch); ch := CAP(ch) 
    UNTIL (ch = 'R') OR (ch = 'C') OR (ch = 'A');
    action := (res & (ch = 'R')) OR ((NOT res) & (ch = 'C'));
  ELSE
    Font := BYTE(white); WriteAOC('Press [SPACE]...');
    REPEAT Read(ch) UNTIL (ch=' ') OR (ch = ESC); 
  END; (* IF *)
  RestoreArea(h); Font := f;
END GenericMessage;



PROCEDURE MessageBox(s:ARRAY OF CHAR; err:BOOLEAN);
VAR
  b : BOOLEAN;
  t : MessageType;
BEGIN
  IF err THEN t := mtErrorMsg ELSE t := mtNoteMsg END;
  GenericMessage(s, t, b);
END MessageBox;



PROCEDURE DiskErrorBox
            ( errtext    : ARRAY OF CHAR;
              VAR action : BOOLEAN );
BEGIN
  GenericMessage(errtext, mtDiskError, action);
END DiskErrorBox;



PROCEDURE WarningBox
            ( VAR action : BOOLEAN );
VAR
  s : ARRAY [0..29] OF CHAR;
BEGIN
  s := 'Current screen is not stored !';
  GenericMessage(s, mtCancelOffer, action);
END WarningBox;



PROCEDURE DialogBox(prompt:ARRAY OF CHAR; VAR s:ARRAY OF CHAR; hp:HelpProc;
                    vp:ValidProc; VAR res,newtype:BOOLEAN);
VAR
  h : Handle;
  y0,y1 : ScrY;
  x     : ScrX;
  c     : ScrCh;
BEGIN
  y0 := MaxY-4; y1 := MaxY-1;
  SaveArea(0,MaxX,MaxY-4,MaxY-1,h,res);
  SetIndicator(input); Beep(input);
  CursorX := 0; CursorY := 0; WriteAOC(inputText);
  EraseEOL; c.ch := 'Ä'; c.at := BYTE(white);
  FOR x := 0 TO MaxX DO 
    PutChar(c,x,y0); PutChar(c,x,y1); 
    PutChar(blank,x,y0+1); PutChar(blank,x,y1-1);
  END;
  Font := BYTE(white); CursorX := 0; CursorY := y0+1; WriteAOC(prompt); 
  INC(CursorY); CursorX := 0; WriteAOC('-> '); ReadString(s,hp,vp,newtype);
  RestoreArea(h); res := Done;
  CursorX := 0; CursorY := 0; EraseEOL;  
END DialogBox;




PROCEDURE ScrollBox(x0:ScrX; y0:ScrY; ScrollLen:CARDINAL; resetproc:PROC; 
                    prevproc,nextproc:ListProc; m:PROC; op:Operation; 
                    VAR item:ARRAY OF CHAR);
  PROCEDURE GotoTop;
  BEGIN
    resetproc; 
    Font := BYTE(white); CursorY := min;
    FOR y := min TO max DO
      CursorX := xs; res := nextproc(item); WriteString(item); 
      WHILE CursorX<(x1-1) DO PutChar(blank,CursorX,y); INC(CursorX) END;
      IF op=Mark THEN
        IF res THEN PutChar(toggle,x1-1,y) ELSE PutChar(blank,x1-1,y) END;
      END;
      INC(CursorY);
    END;
    resetproc; res := nextproc(item); selpos := min; 
  END GotoTop;

VAR
  x1,x,xs    : ScrX;
  b,toggle   : ScrCh;
  y,min,max, 
  selpos,y1  : ScrY;
  hndle      : Handle;
  res,res1   : BOOLEAN;
  ch         : CHAR;
  i1,i2      : ARRAY [0..30] OF CHAR;
  count,height : CARDINAL;
BEGIN
  IF x0+ScrollLen+3 > MaxX THEN x0 := MaxX-ScrollLen-3 END;
  x1 := x0+ScrollLen; y1 := y0+BoxLen+1;
  IF op=Mark THEN 
    INC(x1,5); WITH toggle DO ch := 'þ'; at := BYTE(white) END;
  ELSE INC(x1,3) END;

  (* Draw box and initial contents *)
  DrawBox(x0,x1,y0,y1,hndle,res);
  IF NOT res THEN RETURN END;
  (* Error handling : memory overflow *)
  CursorX := 0; CursorY := 0; Font := BYTE(white);

  CASE op OF
    Scroll : SetIndicator(scroll);
             WriteAOC('[RETURN] = menu, [ESC] = abort');
   |Mark   : SetIndicator(mark);
             WriteAOC('[SPACE] = toggle marker, [RETURN] = do, [ESC] = abort');
   |Select : SetIndicator(select);
             WriteAOC('[RETURN] = select, [ESC] = abort');
  END;

  EraseEOL; min := y0+1; max := y1-1; height := (max-min+1);
  DEC(x1); INC(x0); xs := x0+1;
  GotoTop; IF item[0] = 0C THEN RestoreArea(hndle); RETURN END;

  (* Selection loop *)
  LOOP
    (* Highlight current selection *)
    WITH b DO
      ch := CHR(16);
      IF op<>Scroll THEN at := BYTE(white+intensity) ELSE at := BYTE(white) END; 
      PutChar(b,x0,selpos); ch := CHR(17); PutChar(b,x1,selpos);
    END; (* WITH *)
    IF op<>Scroll THEN
      FOR x := xs TO x1-1 DO 
        GetChar(b,x,selpos); b.at := BYTE(white+intensity); PutChar(b,x,selpos);
      END;
    END;

    (* Get keystroke *)
    ReadChar(ch,res);
    IF res & HelpKey(ch) THEN
      EnterHelpSystem;
    ELSIF res THEN
      (* Set current entry back to normal *)
      PutChar(blank,x0,selpos); PutChar(blank,x1,selpos);
      FOR x := xs TO x1-1 DO
        GetChar(b,x,selpos); b.at := BYTE(white); PutChar(b,x,selpos);
      END;

      CASE ORD(ch) OF
         down : res := nextproc(i1); 
                IF i1[0]<>0C THEN
                  Assign(item,i1);
                  IF selpos<max THEN INC(selpos)
                  ELSE
                    ScrollArea(x0,x1,min,max,1);
                    CursorX := xs; CursorY := max;
                    WriteString(item);
                    IF op=Mark THEN 
                      IF res THEN PutChar(toggle,x1-1,CursorY) ELSE PutChar(blank,x1-2,CursorY) END;
                    END;
                  END;
                END;
        |up    : res := prevproc(i1); 
                 IF i1[0]<>0C THEN
                   Assign(item,i1);
                   IF selpos>min THEN DEC(selpos)
                   ELSE
                     ScrollArea(x0,x1,min,max,-1);
                     CursorX := xs; CursorY := min;
                     WriteString(item);
                     IF op=Mark THEN 
                       IF res THEN PutChar(toggle,x1-1,CursorY) ELSE PutChar(blank,x1-2,CursorY) END;
                     END;
                   END;
                 END;
        |home  : GotoTop;
        |end   : count := 0; res := nextproc(i1); 
                 (** 26/04/87 : Bug fixed **)
                 Assign(i2,item);
                 (**)
                 WHILE i1[0]<>0C DO 
                   i2 := i1; res1 := res; INC(count); res := nextproc(i1)
                 END;
                 Assign(item,i2);
                 IF count>(max-selpos) THEN
                   ClearArea(x0,x1,min+1,max); selpos := min;
                   CursorX := xs; CursorY := min; WriteString(item);
                   WHILE CursorX<(x1-1) DO PutChar(blank,CursorX,min); INC(CursorX) END;
                   IF (op=Mark) THEN
                     (** 14/06/87 : (op=Mark) & res1 replaced **)
                     IF res1 THEN 
                       PutChar(toggle,x1-1,CursorY) 
                     ELSE 
                       PutChar(blank,x1-1,CursorY)
                     END; (* IF *)
                   END; (* IF *)
                 ELSE
                   INC(selpos,count);
                 END; (* IF *)
        |pgdn  : count := 0; Assign(i2,item); res := nextproc(i1);
                 WHILE (i1[0]#0C) AND (selpos<max) DO 
                   INC(selpos); i2 := i1; res := nextproc(i1); 
                 END;
                 CursorY := max;
                 WHILE (i1[0]#0C) AND (count<height) DO 
                   ScrollArea(x0,x1,min,max,1);
                   CursorX := xs; WriteString(i1);
                   IF op=Mark THEN 
                     IF res THEN PutChar(toggle,x1-1,CursorY) ELSE PutChar(blank,x1-2,CursorY) END;
                   END;
                   INC(count); i2 := i1; res := nextproc(i1)
                 END;
                 Assign(item,i2); IF i1[0]#0C THEN res := prevproc(i1) END;
        |pgup  : count := 0; Assign(i2,item); res := prevproc(i1);
                 WHILE (i1[0]#0C) AND (selpos>min) DO 
                   DEC(selpos); i2 := i1; res := prevproc(i1);
                 END;
                 CursorY := min;
                 WHILE (i1[0]#0C) AND (count<height) DO 
                   ScrollArea(x0,x1,min,max,-1);
                   CursorX := xs; WriteString(i1);
                   IF op=Mark THEN 
                     IF res THEN PutChar(toggle,x1-1,CursorY) ELSE PutChar(blank,x1-2,CursorY) END;
                   END;
                   INC(count); i2 := i1; res := prevproc(i1)
                 END;
                 Assign(item,i2); IF i1[0]#0C THEN res := nextproc(i1) END;
      ELSE END;

    ELSIF (ch=' ') AND (op=Mark) THEN
      m; GetChar(b,x1-1,selpos); 
      WITH b DO IF ch='þ' THEN ch := ' ' ELSE ch := 'þ' END END;
      PutChar(b,x1-1,selpos);

    ELSIF (ch=CR) THEN
      RestoreArea(hndle); RETURN

    ELSIF (ch=ESC) THEN 
      Clear(item); RestoreArea(hndle); RETURN

    END (* IF *);
  END (* LOOP *);
END ScrollBox;



PROCEDURE DateBox(VAR s:ARRAY OF CHAR);
CONST
  x0 = 23;
  y0 = 15;
  w  = 36;
  h  = 4;
  dx = 12;
VAR
  hnd : Handle;
  res : BOOLEAN;
  state,d : CARDINAL;
  oldX : ScrX;
  oldY : ScrY;
  c,ch,c1,c2,c3  : CHAR;
  t : Time;
BEGIN
  (* Draw date box *)
  Clear(s);
  oldX := CursorX; oldY := CursorY;
  SetIndicator(date);
  Font := BYTE(white);
  CursorX := 0; CursorY := 0; ch := DateChar[DateType];
  WriteAOC('Use digits 0-9, "'); Write(ch);
  WriteAOC('", [BS] to correct, and [RETURN] to accept');
  DrawBox(x0,x0+(w-1),y0,y0+h,hnd,res);
  CursorX := x0+3; CursorY := y0+1;
  c1 := 'd'; c2 := 'm'; c3 := 'y';
  WriteAOC('Enter Date (Press '); Font := BYTE(white+intensity);
  Write('T'); Font := BYTE(white); WriteAOC(' for today)');
  s[3] := ch; s[6] := ch; state := 0;
  REPEAT
    (* Finite automaton *)
    IF state=0 THEN
      (* Initialization *)
      CursorX := x0+dx; CursorY := y0+3; Font := BYTE(white);
      CASE DateType OF
        0 : WriteAOC('mm / dd / yy'); c1 := 'm'; c2 := 'd';
      | 1 : WriteAOC('dd / mm / yy')
      | 2 : WriteAOC('dd . mm . yy')
      | 3 : WriteAOC('yy - mm - dd'); c1 := 'y'; c3 := 'd';
      END; (* CASE *)
      state := 1; Font := BYTE(white+intensity);
    END; (* IF *)
    CASE state OF
      1 : CursorX := x0+dx;
    | 2 : CursorX := x0+(dx+1);
    | 3 : CursorX := x0+(dx+5);
    | 4 : CursorX := x0+(dx+6);
    | 5 : CursorX := x0+(dx+10);
    | 6 : CursorX := x0+(dx+11);
    | 7 : CursorX := x0+(dx+12);
    END; (* CASE *)
    SetCursor(CursorX, CursorY);
    REPEAT 
      Read(c) 
    UNTIL (c=15C) OR (c=10C) OR (c=33C) OR ((c>='0') & (c<='9')) OR (c=ch)
          OR (CAP(c) = 'T');
    IF (c=15C) THEN
      (* Return key pressed *)
      IF (state=6) & (DateType=3) THEN
        s[8] := s[7]; s[7] := '0'; state := 7;
      END; (* IF *)
      IF (state = 7) THEN
        s[0] := 10C;  (* Length of date = 8 *)
        StrToDate(s,d); 
        IF d # 0 THEN state := 8 ELSE s[0] := 0C; Beep(error); state := 0 END;
      END; (* IF *)
    ELSIF (c=10C) THEN
      (* Backspace *)
      Font := BYTE(white); 
      IF state IN {3,5} THEN DEC(CursorX,3) END;
      IF state # 1 THEN DEC(state); DEC(CursorX) END;
      IF state IN {1,2} THEN Write(c1);
      ELSIF state IN {3,4} THEN Write(c2);
      ELSIF state IN {5,6} THEN Write(c3);
      END; (* IF *)
      Font := BYTE(white+intensity);
    ELSIF (c=33C) THEN
      (* Escape *)
      state := 8; Clear(s);
    ELSIF (c=ch) THEN
      (* Date char abbreviation *)
      IF state=2 THEN
        s[2] := s[1]; s[1] := '0';
        CursorX := x0+dx; Write('0'); Write(s[2]); INC(state);
      ELSIF state=4 THEN
        s[5] := s[4]; s[4] := '0';
        CursorX := x0+dx+5; Write('0'); Write(s[5]); INC(state);
      END; (* IF *)
    ELSIF CAP(c) = 'T' THEN
      GetTime(t); DateToStr(t.day, s);
      CursorX := x0+dx; CursorY := y0+3;
      Write(s[1]); Write(s[2]); INC(CursorX, 3);
      Write(s[4]); Write(s[5]); INC(CursorX, 3);
      Write(s[7]); Write(s[8]);
      state := 7;
    ELSE
      (* digit *)
      IF (state IN {1,2}) THEN
        s[state] := c;
      ELSIF (state IN {3,4}) THEN
        s[state+1] := c;
      ELSIF (state IN {5,6}) THEN
        s[state+2] := c;
      END; (* IF *)
      Write(c); INC(state);
    END; (* IF *)
  UNTIL (state=8);
  (* Restore dialog box *)
  RestoreArea(hnd); CursorX := 0; CursorY := 0;
  Font := BYTE(white); WriteAOC(inputText); SetIndicator(input);
  CursorX := oldX; CursorY := oldY;
END DateBox;



PROCEDURE PullDownMenu
            ( x0 : ScrX; 
              y0 : ScrY; 
              VAR mb : MenuBar; 
              VAR sel : CARDINAL;
              hp : HelpProcType );
VAR
  hndle     : Handle;
  x1,x      : ScrX;
  quit,res  : BOOLEAN;
  min,max,y : ScrY;
  selpos    : ScrY;
  c         : ScrCh;
  ch        : CHAR;
  sel1      : CARDINAL;
  b         : BYTE;
  keylen    : ScrX;
BEGIN
  (** 25/10/87 **) CheckPermission;
  (* Calculate width of window *)
  quit := (sel>100); IF quit THEN DEC(sel,100) END;
  keylen := mb.width;

  IF x0+keylen+3 > MaxX THEN x0 := MaxX-keylen-3 END;
  x1 := x0+keylen+3;
  IF y0+mb.entries+1 > MaxY THEN y0 := MaxY-mb.entries-1 END;

  min := y0+1; max := y0+mb.entries+1;
  IF (sel=0) OR quit THEN
    DrawBox(x0,x1,y0,max,hndle,res); 
    IF sel=0 THEN
      (* Find first selectable item (must be at least one) *)
      sel := 1;
      WITH mb DO
        WHILE (sel <= entries) & (NOT (sel IN on)) DO INC(sel) END;
        IF sel > entries THEN HALT END;
      END; (* WITH *)
    END; (* IF *)
    IF NOT res THEN (* MEMORY OVERFLOW *) RETURN 
    ELSE 
      INC(ptr); menus[ptr] := hndle;
    END;
  END;

  (* Draw box and initial contents *)
  CursorY := min; sel1 := 1;
  FOR y := min TO max-1 DO
    Font := BYTE(white); CursorX := x0+2;
    IF sel1=sel THEN Font := BYTE(reverse)
    ELSIF sel1 IN mb.on THEN INC(Font,intensity) END;
    WriteString(mb.option[sel1].key); INC(sel1); INC(CursorY);
  END;

  DEC(max); DEC(x1); INC(x0);

  (* Selection loop *)
  SetIndicator(select); sel1 := 0; 
  Font := BYTE(white);
  LOOP
    (* Highlight current selection *)
    IF (sel1#sel) THEN
      (* Set current entry back to normal *)
      b := BYTE(white); IF sel1 IN mb.on THEN INC(b,intensity) END;
      INC(sel1,y0); selpos := sel+y0;
      FOR x := x0 TO x1 DO
        GetChar(c,x,sel1); c.at := b; PutChar(c,x,sel1);
        GetChar(c,x,selpos); c.at := BYTE(reverse); PutChar(c,x,selpos);
      END;
      CursorX := 0; CursorY := 0; 
      WriteString(mb.option[sel].help);
      EraseEOL; sel1 := sel;
      hp(sel);
    END;
    IF quit THEN RETURN END;

    (* Get keystroke *)
    ReadChar(ch,res);
    IF res & HelpKey(ch) THEN
      EnterHelpSystem;
    ELSIF res THEN
      CASE ORD(ch) OF
         down : WITH mb DO
                  REPEAT
                    IF sel<entries THEN INC(sel) ELSE sel := 1 END
                  UNTIL (sel IN on) & (option[sel].key[0] # 0C);
                END; (* WITH *)
        |up   : WITH mb DO 
                  REPEAT
                    IF sel>1 THEN DEC(sel) ELSE sel := entries END
                  UNTIL (sel IN on) & (option[sel].key[0] # 0C);
                END; (* WITH *)
        |left : IF y0=2 THEN sel := 100; RemovePDMenu; RETURN END;
        |right: IF y0=2 THEN sel := 101; RemovePDMenu; RETURN END;
        |home : sel := 1;
                WITH mb DO
                  WHILE (sel <= entries) & (NOT (sel IN on)) DO INC(sel) END;
                  IF sel > entries THEN HALT END;
                END; (* WITH *)
        |end  : sel := mb.entries;
                WITH mb DO
                  WHILE (sel >= 1) & (NOT (sel IN on)) DO DEC(sel) END;
                  IF sel = 0 THEN HALT END;
                END; (* WITH *)
      ELSE
      END;

    ELSIF (ch=CR) AND (sel IN mb.on) THEN RETURN

    ELSIF (ch=ESC) THEN 
      sel := 0; RemovePDMenu;  RETURN

    ELSE
      ch := CAP(ch);
      IF ((ch>='A') AND (ch<='Z')) OR ((ch>='0') AND (ch<='9')) THEN
        WITH mb DO
          sel := 1; 
          REPEAT
            IF (CAP(option[sel].key[1])=ch) AND (sel IN mb.on) 
              THEN quit := TRUE ELSE INC(sel) END;
          UNTIL quit OR (sel>entries);
          IF NOT quit THEN sel := sel1 END;
        END;
      END;
    END (* IF *);
  END (* LOOP *);
END PullDownMenu;




PROCEDURE RemovePDMenu();
BEGIN
  IF ptr>0 THEN RestoreArea(menus[ptr]); DEC(ptr) END;
END RemovePDMenu;



PROCEDURE MenuLine(VAR mb:MenuBar; VAR sel:CARDINAL; cp:CharProc);
VAR
  i    : EntryRange;
  ch   : CHAR;
  res  : BOOLEAN;
  quit : BOOLEAN;
  pi   : ScrX;
  sel1 : CARDINAL;
  b    : ScrCh;

  PROCEDURE rd;
  BEGIN
    CursorX := 0;
    FOR i := 1 TO mb.entries DO
      pi := CursorX; WriteString(mb.option[i].key); WriteAOC('     ');
      GetChar(b,pi,CursorY); b.at := BYTE(white+intensity);
      PutChar(b,pi,CursorY); pos[i] := pi;
    END;
    EraseEOL;
  END rd;

BEGIN (* MenuLine *)
  CursorY := 1; Font := BYTE(white);
  IF sel>100 THEN
    DEC(sel,100); oldsel := 1; rd; quit := TRUE; 
  ELSE
    quit := FALSE;
    IF sel=0 THEN
      sel := 1; oldsel := 1; rd; Beep(menu);
    END;
  END;
  IF sel#oldsel THEN
    pi := pos[oldsel]; CursorX := pi; WriteString(mb.option[oldsel].key);
    GetChar(b,pi,CursorY); b.at := BYTE(white+intensity);
    PutChar(b,pi,CursorY);
  END;

  Font := BYTE(reverse); CursorX := pos[sel];
  WriteString(mb.option[sel].key); Font := BYTE(white);
  CursorX := 0; CursorY := 0; WriteString(mb.option[sel].help); EraseEOL;
  
  (* Selection loop *)
  SetIndicator(menu); Font := BYTE(white);
  sel1 := sel;
  LOOP
    (* Highlight current selection *)
    IF sel1#sel THEN
      (* Set current entry back to normal *)
      CursorX := pos[sel1]; CursorY := 1;
      WriteString(mb.option[sel1].key); GetChar(b,pos[sel1],CursorY);
      b.at := BYTE(white+intensity); PutChar(b,pos[sel1],CursorY);
      CursorX := pos[sel]; Font := BYTE(reverse);
      WriteString(mb.option[sel].key); Font := BYTE(white);
      CursorX := 0; CursorY := 0; WriteString(mb.option[sel].help); 
      EraseEOL; sel1 := sel;
    END;
    IF quit THEN oldsel := sel; RETURN END;

    (* Get keystroke *)
    ReadChar(ch,res);
    IF res & HelpKey(ch) THEN
      EnterHelpSystem;
    ELSIF res THEN
      CASE ORD(ch) OF
         right: IF sel<mb.entries THEN INC(sel) ELSE sel := 1 END;
        |left : IF sel>1 THEN DEC(sel) ELSE sel := mb.entries END; 
        |down : oldsel := sel; RETURN;
      ELSE
        cp(ch);  (* Call user-defined character handler *)
      END;

    ELSIF (ch=CR) THEN oldsel := sel; RETURN

    ELSIF (ch=ESC) THEN sel := 0; RETURN

    ELSE
      ch := CAP(ch);
      IF (ch>='A') AND (ch<='Z') THEN
        WITH mb DO
          sel := 1; 
          REPEAT
            IF CAP(option[sel].key[1])=ch THEN quit := TRUE ELSE INC(sel) END;
          UNTIL quit OR (sel>entries);
          IF NOT quit THEN sel := sel1 END;
        END; (* WITH *)
      END; (* IF *)
    END (* IF *);
  END (* LOOP *);
END MenuLine;




BEGIN (* MODULE *)
  ptr := 0;
END Menus.
