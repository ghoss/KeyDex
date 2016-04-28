(*****************************************************************************
*
* Name     : KDScreen
*
* LastEdit : 03/05/87
* Project  : KeyDex system
* Purpose  : Status line management.
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
* 29/03/87 : Replaced elapsed time by day display.
* 04/04/87 : Replaced page descr. "p." by "1."
* 05/04/87 : Prepared for insert mode and beep stuff.
* 03/05/87 : 32-bit numbers implemented. WARNING : Procedures "DispMatch"
*            and "DispRecordKey" will truncate long cardinals to 65535.
* 05/06/87 : Implemented "Beep".
* 06/06/87 : Changed semantics of "DispRecordKey".
* 16/06/87 : Implemented insert mode indicator.
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

IMPLEMENTATION MODULE KDScreen;


FROM SYSTEM IMPORT 
  SETREG, GETREG, AX, SWI, BYTE;

FROM LongCardinals IMPORT
  LONGCARD, equal, zero;

FROM Sound IMPORT
  BeepTone;

FROM TimeDate IMPORT
  GetTime, Time;

FROM Term IMPORT
  WriteString, WriteCard, Write, WriteAOC, Font, CursorX, CursorY;

FROM KDConfig IMPORT
  DispTime, DispDate, DateType, TimeType, beepMode, beepFrequency;

FROM SystemIO IMPORT
  ScrX, ScrY, MaxX, MaxY, PutChar, ScrCh,
  white, reverse, blink, InstallHandler, CursorOff, ClearScreen;

FROM KDEdit IMPORT
  saved, wordwrap, template, insertMode;


CONST
  DTpos  = 1;
  T1pos  = 14;
  fnpos  = 22;
  pgtpos = 33;
  pgpos  = 35;
  rdpos  = 38;
  mtpos  = 54;
  svpos  = 66;
  ispos  = 69;
  tmpos  = 72;
  wrpos  = 76;


VAR
  lasttime    : Time;
  oldsaved, 
  oldwrap,
  oldinsert,
  oldtemp     : BOOLEAN;
  OldDateType,
  OldTimeType : CARDINAL;
  DayAbbr     : ARRAY [0..6],[0..2] OF CHAR;


PROCEDURE Today():CARDINAL;
(* Returns the index of the current day. 0=Sun; 6=Sat *)
VAR 
  x : CARDINAL;
BEGIN
  SETREG(AX,2A00H); SWI(21H); GETREG(AX,x); RETURN (x MOD 256);
END Today;


PROCEDURE Beep(how:Indicator);
VAR
  i : CARDINAL;
BEGIN
  IF beepMode THEN
    CASE how OF
      input, date :
        (* two short beeps *)
        BeepTone(beepFrequency, 10);
        FOR i := 1 TO 10000 DO END;
        BeepTone(beepFrequency, 10);
    | error :
        (* one long beep *)
        BeepTone(beepFrequency, 200);
    | note :
        (* one medium beep *)
        BeepTone(beepFrequency, 80);
    | menu, edit :
        (* one short beep *)
        BeepTone(beepFrequency, 20);
    END; (* CASE *)
  END; (* IF *)
END Beep;



PROCEDURE SetIndicator(i:Indicator);
VAR
  cx : ScrX;
  cy : ScrY;
  fn : BYTE;
BEGIN
  cx := CursorX; cy := CursorY; fn := Font;
  CursorX := indpos; CursorY := 0; Font := BYTE(reverse);
  CASE i OF
     edit   : WriteAOC(" EDIT ")
    |input  : WriteAOC("INPUT ")
    |wait   : INC(Font,blink); WriteAOC(" WAIT ")
    |select : WriteAOC("SELECT")
    |menu   : WriteAOC(" MENU ")
    |scroll : WriteAOC("SCROLL")
    |error  : INC(Font,blink); WriteAOC("ERROR!");
    |mark   : WriteAOC(" MARK ")
    |note   : WriteAOC(" NOTE ")
    |date   : WriteAOC(" DATE ")
  END; (* CASE *)
  CursorX := cx; CursorY := cy; Font := fn;
END SetIndicator;



PROCEDURE UpdateTime;
VAR
  x1 : ScrX;
  y1 : ScrY; 
  f1 : BYTE;
  t  : Time;
BEGIN
  GetTime(t);
  x1 := CursorX; y1 := CursorY; CursorY := MaxY; f1 := Font;
  Font := BYTE(reverse);
  WITH t DO
    IF (day#lasttime.day) OR (DateType#OldDateType) THEN
      CursorX := DTpos; 
      WriteAOC(DayAbbr[Today()]); Write(' ');
      DispDate(day); OldDateType := DateType;
    END;
    IF (minute#lasttime.minute) OR (TimeType#OldTimeType) THEN
      (* Real time *)
      CursorX := T1pos; DispTime(minute); 
      OldTimeType := TimeType;
    END;
  END;
  Font := f1; CursorX := x1; CursorY := y1; lasttime := t;
END UpdateTime;
        



PROCEDURE DispFilename(VAR name:ARRAY OF CHAR);
VAR
  x1 : ScrX; 
  y1 : ScrY;
  f1 : BYTE;
  i  : CARDINAL;
BEGIN
  x1 := CursorX; y1 := CursorY; f1 := Font;
  CursorX := fnpos; CursorY := MaxY; Font := BYTE(reverse);
  IF name[0]#0C THEN
    WriteString(name); FOR i := ORD(name[0]) TO 8 DO Write(' ') END;
  ELSE
    WriteAOC('no file ');
  END;
  CursorX := x1; CursorY := y1; Font := f1;
END DispFilename;



PROCEDURE SetStatus;
VAR
  x1 : ScrX; 
  y1 : ScrY;
  f1 : BYTE;
BEGIN
  x1 := CursorX; y1 := CursorY; f1 := Font; 
  CursorY := MaxY; Font := BYTE(reverse);
  IF (saved<>oldsaved) THEN
    CursorX := svpos;
    IF saved THEN WriteAOC('  ') ELSE WriteAOC('²²') END;
    oldsaved := saved;
  END; (* IF *)
  IF (wordwrap<>oldwrap) THEN
    CursorX := wrpos;
    IF wordwrap THEN WriteAOC('WRP') ELSE WriteAOC('   ') END;
    oldwrap := wordwrap;
  END; (* IF *)
  IF (template<>oldtemp) THEN
    CursorX := tmpos;
    IF template THEN WriteAOC('TMP') ELSE WriteAOC('   ') END;
    oldtemp := template;
  END; (* IF *)
  IF (insertMode<>oldinsert) THEN
    CursorX := ispos;
    IF insertMode THEN WriteAOC('IN') ELSE WriteAOC('   ') END;
    oldinsert := insertMode;
  END; (* IF *)
  CursorX := x1; CursorY := y1; Font := f1;
END SetStatus;



PROCEDURE DispPageNo(p:CARDINAL);
VAR
  x1 : ScrX; 
  y1 : ScrY;
  f1 : BYTE;
BEGIN
  x1 := CursorX; y1 := CursorY; f1 := Font;
  CursorX := pgpos; CursorY := MaxY; Font := BYTE(reverse);
  WriteCard(p,1,FALSE); CursorX := x1; CursorY := y1; Font := f1;
END DispPageNo;




PROCEDURE DispRecordKey( date  : CARDINAL;
                         recno : LONGCARD );
VAR
  x1 : ScrX; 
  y1 : ScrY;
  f1 : BYTE;
BEGIN
  x1 := CursorX; y1 := CursorY; f1 := Font;
  CursorX := rdpos; CursorY := MaxY; Font := BYTE(reverse);
  IF NOT equal(recno, zero) THEN
    IF date = 0 THEN
      WriteAOC('SCANNING')
    ELSIF date = 1 THEN
      WriteAOC('PRINTING')
    ELSE
      DispDate(date)
    END; (* IF *)
    WriteAOC(' #');
    (** Implementation restriction (bottomline too small) **)
    IF recno.high > 0 THEN 
      WriteCard(65535,1,FALSE);
    ELSE
      WriteCard(recno.low,1,FALSE);
    END; (* IF *)
  ELSE
    WriteAOC('(NEW)');
  END;
  WHILE CursorX<=rdpos+14 DO Write(' ') END;
  CursorX := x1; CursorY := y1; Font := f1;
END DispRecordKey;



PROCEDURE InitScreen;
(* Initialize the screen layout *)
VAR
  x    : ScrX;
  b,c  : ScrCh;
BEGIN
  CursorOff; ClearScreen; 
  DayAbbr[0] := 'Sun'; DayAbbr[1] := 'Mon'; DayAbbr[2] := 'Tue';
  DayAbbr[3] := 'Wed'; DayAbbr[4] := 'Thu'; DayAbbr[5] := 'Fri';
  DayAbbr[6] := 'Sat';
  OldDateType := 0; OldTimeType := 0; 
  InstallHandler(UpdateTime);
  b.ch := 'Ä'; b.at := BYTE(white);
  c.ch := ' '; c.at := BYTE(reverse); 
  FOR x := 0 TO MaxX DO PutChar(b,x,2); PutChar(c,x,MaxY) END;
  c.ch := 'º'; 
  PutChar(c,fnpos-2,MaxY); PutChar(c,pgtpos-2,MaxY); PutChar(c,ispos-1,MaxY);
  CursorX := pgtpos; CursorY := MaxY; Font := BYTE(reverse);
  WriteAOC('1.'); CursorX := fnpos; WriteAOC('no file ');
  WITH lasttime DO day := 0; minute := 0 END; UpdateTime;
  oldsaved := TRUE; oldwrap := FALSE; oldtemp := FALSE;
END InitScreen;


PROCEDURE DispMatch(x,y:LONGCARD);
VAR
  x1 : ScrX; 
  y1 : ScrY;
  f1 : BYTE;
BEGIN
  x1 := CursorX; y1 := CursorY; f1 := Font;
  CursorX := mtpos; CursorY := MaxY; Font := BYTE(reverse);
  IF NOT equal(x,zero) THEN
    IF x.high > 0 THEN 
      WriteCard(65535,1,FALSE);
    ELSE
      WriteCard(x.low,1,FALSE);
    END; (* IF *)
    Write('/');
    IF y.high > 0 THEN 
      WriteCard(65535,1,FALSE);
    ELSE
      WriteCard(y.low,1,FALSE);
    END; (* IF *)
  END; (* IF *)
  WHILE CursorX<=mtpos+10 DO Write(' ') END;
  CursorX := x1; CursorY := y1; Font := f1;
END DispMatch;


END KDScreen.
