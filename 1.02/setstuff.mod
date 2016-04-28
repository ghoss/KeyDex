(*****************************************************************************
*
* Name     : SetStuff
*
* LastEdit : 30/05/87
* Project  : KeyDex system
* Purpose  : Settings menu.
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
* 30/05/87 : Fixed bug in "ValidDir" and "Program execute".
* 03/06/87 : Adjusted path routines to new directory naming.
*            Fixed bug in program routine.
* 05/06/87 : Dynamic menus.
* 08/06/87 : Altered date/time settings now dispose screen list.
* 23/06/87 : Bugs fixed in "File Path" and "Index Path".
*            (It was possible to erase current database if case different).
* 13/07/87 : Changed to new file system.
* 18/07/87 : Critical error handling.
* 10/09/87 : Bug fixed in "ValidDir".
* 25/10/87 : Implemented code to allow creation of null directory strings.
* 05/11/87 : Fixed bug in 'ValidDir': KeyDex always switches back to old
*            dir, even if error occured.
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

IMPLEMENTATION MODULE SetStuff;


FROM Menus IMPORT
  CreateMenu, PullDownMenu, DialogBox, MessageBox, MenuBarPtr;

FROM String IMPORT
  AppendCh, Compare, AssignStr, AssignAOC, Assign, Clear, Append, Copy,
  UpperCase;

FROM MsFileSystem IMPORT
  fsGetCurrentDir, fsChangeDir, fsResult, fsErrorType, fsOpen, fsClose,
  fsShareMode, fsAccessMode, fsFile;

FROM KDConfig IMPORT
  DataPath, IndexPath, Program, SaveConfigFile, DateType, TimeType,
  beepMode, beepFrequency, tabInterval, FileNameChar, pageLength, printerCode,
  ScrAttribute, prcLength, prcString;

FROM KDScreen IMPORT
  DispRecordKey, SetIndicator, Indicator;

FROM KDConversions IMPORT
  CardToStr, StrToCard;

FROM Keywords IMPORT
  loaded, recdate, recno, DisposeScreenList;

FROM KDCritErr IMPORT
  InvokeHandler, DispatchHandler;

FROM HelpSystem IMPORT
  SetHelpContext;
  
FROM HelpContext IMPORT
  ModuleIndex, ScreenNumber;


CONST
  str02 = 'Illegal value. Try again';

VAR
  mb6, mb7,
  mb10      : MenuBarPtr;
  i         : CARDINAL;



PROCEDURE dummy(n:CARDINAL; VAR kw:ARRAY OF CHAR) : BOOLEAN;
BEGIN kw[0] := 0C; RETURN FALSE END dummy;

PROCEDURE FileCharValid(ch:CHAR) : BOOLEAN;
BEGIN RETURN (FileNameChar(ch)=CAP(ch)) END FileCharValid;

PROCEDURE Digit(ch:CHAR) : BOOLEAN;
BEGIN RETURN (ch>='0') & (ch<='9') END Digit;

PROCEDURE AllValid(ch : CHAR) : BOOLEAN;
BEGIN RETURN TRUE END AllValid;


PROCEDURE HelpProc( sel : CARDINAL );
(* Help procedure for Settings menu *)
BEGIN
  SetHelpContext(ScreenNumber(setstuff, sel));
END HelpProc;


PROCEDURE HelpProc2( sel : CARDINAL );
(* Dummy help procedure for 'Settings/Printer' and 'Date/Time' *)
END HelpProc2;


PROCEDURE PrinterMenu;

  PROCEDURE EnterCode(at : ScrAttribute; on : BOOLEAN);
  (* Requests char sequence for printerCode[at].(T -> atOn, F -> atOff) *)
  VAR
    s : ARRAY [0..prcLength*4] OF CHAR;
    p : prcString;
    c : CHAR;
    b : BOOLEAN;
    i, max : CARDINAL;
  BEGIN
    WITH printerCode[at] DO
      IF on THEN p := atOn ELSE p := atOff END; Clear(s);
      FOR i := 1 TO ORD(p[0]) DO
        c := p[i];
        IF (c < ' ') OR (c > '~') THEN
          AppendCh(s, '^'); CardToStr(ORD(c), pgLen);
          IF ORD(c) < 100 THEN AppendCh(s, '0') END;
          IF ORD(c) < 10 THEN AppendCh(s, '0') END;
          Append(s, pgLen);               
        ELSE
          AppendCh(s, c);
        END; (* IF *)
      END; (* FOR *)
      DialogBox("Enter character sequence; for ASCII code nnn, type ^nnn",
                 s, dummy, AllValid, done, newtype);
      IF done & newtype THEN
        i := 1; max := ORD(s[0]); Clear(p);
        WHILE (i <= max) DO
          c := s[i];
          b := (c = '^') & (i+3 <= max); 
          b := b & Digit(s[i+1]) & Digit(s[i+2]) & Digit(s[i+3]);
          IF b THEN  
            Copy(s, pgLen, i+1, 3); StrToCard(pgLen, temp, done);
            IF done & (temp <= 255) THEN AppendCh(p, CHR(temp)) END;
            INC(i, 4);
          ELSE
            AppendCh(p, c); INC(i);
          END; (* IF *)
        END; (* WHILE *)
        IF on THEN atOn := p ELSE atOff := p END;
        IF atOn[0] = 0C THEN Clear(atOff) END;
      END; (* IF *)
    END; (* WITH *)
  END EnterCode;

VAR
  sel,
  temp  : CARDINAL;
  pgLen : ARRAY [0..3] OF CHAR;
  done,
  newtype : BOOLEAN;
  at    : ScrAttribute;
BEGIN (* PrinterMenu *)
  sel := 0;
  LOOP
    WITH mb7^ DO
      FOR at := atReverse TO atTitle DO
        WITH printerCode[at] DO
          temp := 10 - 2*ORD(at);
          IF (atOn[0] = 0C) THEN EXCL(on, temp) ELSE INCL(on, temp) END;
        END; (* WITH *)
      END; (* FOR *)
    END; (* WITH *)
    PullDownMenu(63, 9, mb7^, sel, HelpProc2);
    CASE sel OF
      0 : RETURN;
    | 1 : LOOP
            CardToStr(pageLength, pgLen);
            DialogBox("Enter page length (0 or 10..255 lines); 0 lines = no page breaks",
                       pgLen, dummy, Digit, done, newtype);
            IF (pgLen[0] # 0C) AND done THEN
              StrToCard(pgLen, temp, done); 
              done := done & ((temp = 0) OR ((temp >= 10) & (temp <= 255)));
              IF done THEN
                pageLength := temp; EXIT;
              ELSE 
                MessageBox(str02, TRUE);
              END; (* IF *)
            ELSE
              EXIT;
            END; (* IF *)
          END; (* LOOP *)

    | 3,4  : EnterCode(atTitle,     (sel=3));
    | 5,6  : EnterCode(atBold,      (sel=5));
    | 7,8  : EnterCode(atUnderline, (sel=7));
    | 9,10 : EnterCode(atReverse,   (sel=9));
    END; (* CASE *)
  END; (* LOOP *)
END PrinterMenu;



PROCEDURE Settings;
(* Opens settings pull down menu *)

  PROCEDURE SelectFormat;
  (* Responsible for Settings/Time,Date *)
  VAR
    sel,oldDT : CARDINAL;
  BEGIN
    sel := 0; oldDT := DateType;
    LOOP
      WITH mb10^ DO
        option[sel1].key[10] := ' '; 
        option[DateType+1].key[10] := 'þ'; option[TimeType+6].key[10] := 'þ';
      END; (* WITH *)
      PullDownMenu(60, 8, mb10^, sel, HelpProc2);
      CASE sel OF
        0    : IF DateType # oldDT THEN DisposeScreenList END; RETURN;
      | 1..4 : sel1 := DateType+1; DateType := (sel-1);
      | 6..9 : sel1 := TimeType+6; TimeType := (sel-6);
      END; (* CASE *)
      IF loaded THEN 
        DispRecordKey(recdate, recno);
      END; (* IF *)
    END; (* LOOP *)
  END SelectFormat;

  PROCEDURE ValidDir(VAR dir:ARRAY OF CHAR; VAR fake : BOOLEAN) : BOOLEAN;
  VAR
    magic : ARRAY [0..5] OF CHAR;
    res   : BOOLEAN;
  BEGIN
    (** 25/10/87 **)
    AssignAOC(magic, '.....'); fake := (Compare(dir, magic) = 0);
    IF fake THEN Clear(dir); RETURN TRUE END;
    (** 25/10/87 **)
    SetIndicator(wait);
    AssignStr(fn,dir);
    InvokeHandler(FALSE);
    fsGetCurrentDir(d); 
    IF (fsResult() # etOK) THEN RETURN FALSE END;
    fsChangeDir(fn); 
    IF (fsResult() = etOK) THEN
      (** 05/11/87 **)
      fsGetCurrentDir(fn);
      IF (fsResult() = etOK) THEN
        AssignAOC(dir, fn);
      END; (* IF *)
    END; (* IF *)
    res := (fsResult() = etOK);
    fsChangeDir(d);
    DispatchHandler;
    RETURN res;
  END ValidDir;

CONST
  str01 = 'Could not find specified directory. Try again.';
VAR     
  sel,sel1,ti,bf : CARDINAL;
  fn,d,old: ARRAY [0..64] OF CHAR;
  sti : ARRAY [0..2] OF CHAR;
  sbf : ARRAY [0..4] OF CHAR;
  valid,Done,newtype,r : BOOLEAN;
  temp : fsFile;
  fake : BOOLEAN;
  ch   : CHAR;
BEGIN
  sel := 0; quit := FALSE; motion := 0;
  REPEAT
    IF beepMode THEN 
      mb6^.option[7].key[13] := 'þ'
    ELSE 
      mb6^.option[7].key[13] := ' '
    END; (* IF *)
    PullDownMenu(52, 2, mb6^, sel, HelpProc);
    CASE sel OF
        0 : RETURN

     |  1 : Assign(old,DataPath);
            (** 25/10/87 **)
            ch := DataPath[0];
            IF (ch # 3C) & (ch # 0C) & (DataPath[ORD(ch)] = '\') THEN
              DEC(DataPath[0]) 
            END; (* IF *)
            (**)
            REPEAT 
              DialogBox("Enter DOS directory path to data file",
                        DataPath,dummy,FileCharValid,Done,newtype);
              Done := Done AND (DataPath[0]<>0C) AND (Compare(old,DataPath)#0); 
              (** 23/06/87 **)
              IF Done THEN
                UpperCase(DataPath);
                IF (DataPath[0] = 2C) & (DataPath[2] = ':') THEN
                  AppendCh(DataPath,'\')
                END; (* IF *)
              END; (* IF *)
              valid := Done AND ValidDir(DataPath, fake);
              IF Done AND (NOT valid) THEN 
                MessageBox(str01, TRUE);
              END;
            UNTIL (NOT Done) OR valid;
            IF NOT Done THEN Assign(DataPath,old) END;
            (** 03/06/87 **)
            IF (DataPath[ORD(DataPath[0])] # '\') & (NOT fake) THEN 
              AppendCh(DataPath,'\')
            END; (* IF *)

     |  2 : Assign(old,IndexPath);
            (** 25/10/87 **)
            ch := IndexPath[0];
            IF (ch # 3C) & (ch # 0C) & (IndexPath[ORD(ch)] = '\') THEN
              DEC(IndexPath[0]) 
            END; (* IF *)
            (**)
            REPEAT 
              DialogBox("Enter DOS directory path to index file",
                        IndexPath,dummy,FileCharValid,Done,newtype);
              Done := Done AND (IndexPath[0]<>0C) AND (Compare(old,IndexPath)#0); 
              (** 23/06/87 **)
              IF Done THEN
                UpperCase(IndexPath);
                IF (IndexPath[0] = 2C) & (IndexPath[2] = ':') THEN
                  AppendCh(IndexPath,'\')
                END; (* IF *)
              END; (* IF *)
              valid := Done AND ValidDir(IndexPath, fake);
              IF Done AND (NOT valid) THEN 
                MessageBox(str01, TRUE)
              END; (* IF *)
            UNTIL (NOT Done) OR valid;
            IF NOT Done THEN Assign(IndexPath,old) END;
            (** 03/06/87 **)
            IF (IndexPath[ORD(IndexPath[0])] # '\') & (NOT fake) THEN 
              AppendCh(IndexPath,'\')
            END; (* IF *)

     |  3 : Assign(old,Program);
            (** Rewritten 30/05/87 **)
            REPEAT 
              DialogBox("Enter directory path and name of default DOS program to execute",
                         Program,dummy,FileCharValid,Done,newtype); 

              (** 03/06/87 : "IndexPath" => "Program" **)
              Done := Done AND (Program[0] # 0C) AND (Compare(old,Program)#0); 
              IF Done THEN
                SetIndicator(wait);
                UpperCase(Program); AssignStr(fn,Program); 
                InvokeHandler(FALSE);
                fsOpen(fn, smDenyNone, amRead, temp);
                DispatchHandler;
                r := (fsResult() = etOK);
                IF NOT r THEN 
                  MessageBox('Could not find specified program. Try again.', TRUE);
                ELSE
                  fsClose(temp)
                END; (* IF *)
              END; (* IF *)
            UNTIL (NOT Done) OR r;
            IF NOT Done THEN Assign(Program,old) END;

     |  5 : sel1 := 1; SelectFormat;

     |  6 : LOOP
              CardToStr(VAL(CARDINAL,tabInterval),sti); 
              DialogBox("Enter number of spaces between tab stops (1-79):",
                         sti,dummy,Digit,Done,newtype); 
              IF (sti[0]<>0C) AND Done THEN
                StrToCard(sti,ti,Done); Done := Done & (ti>0) & (ti<80);
                IF Done THEN tabInterval := VAL(INTEGER,ti); EXIT;
                        ELSE MessageBox(str02, TRUE)
                END;
              ELSE
                EXIT;
              END; (* IF *)
            END; (* LOOP *)

     |  7 : IF beepMode THEN
              beepMode := NOT beepMode;
            ELSE
              LOOP
                CardToStr(beepFrequency,sbf);
                DialogBox("Enter beep frequency (100-4000 Hertz):",
                           sbf,dummy,Digit,Done,newtype); 
                IF (sbf[0]<>0C) AND Done THEN
                  StrToCard(sbf,bf,Done); Done := Done & (bf>=100) & (bf<=4000);
                  IF Done THEN beepFrequency := bf; beepMode := TRUE; EXIT;
                          ELSE MessageBox(str02, TRUE);
                  END;
                ELSE
                  beepMode := FALSE; EXIT;
                END; (* IF *)
              END; (* LOOP *)
            END; (* IF *)

     |  8 : PrinterMenu;

     | 10 : SetIndicator(wait);
            InvokeHandler(FALSE); SaveConfigFile; DispatchHandler; 
            IF fsResult() # etOK THEN
              MessageBox('Could not write configuration file.', TRUE);
            ELSE
              quit := TRUE;
            END; (* IF *)

     | 11 : quit := TRUE;
     |100 : motion := -1; RETURN
     |101 : motion := +1; RETURN
    ELSE END;
  UNTIL quit;
END Settings;



BEGIN
  CreateMenu(11,mb6); 
  WITH mb6^ DO
    width := 13; on := {1,2,3,5,6,7,8,10,11};
    AssignAOC(option[1].key,"File Path");
    AssignAOC(option[2].key,"Index Path");
    AssignAOC(option[3].key,"Program");
    Clear(option[4].key);
    AssignAOC(option[5].key,"Date/Time");
    AssignAOC(option[6].key,"Tab Stops");
    AssignAOC(option[7].key,"Beep         ");
    AssignAOC(option[8].key,"Printer...");
    Clear(option[9].key);
    AssignAOC(option[10].key,"Update");
    AssignAOC(option[11].key,"Return");
    AssignAOC(option[1].help,"Set directory path to data file");
    AssignAOC(option[2].help,"Set directory path to index file");
    AssignAOC(option[3].help,"Default program to execute");
    AssignAOC(option[5].help,"Set time/date display format");
    AssignAOC(option[6].help,"Set tab stop interval");
    AssignAOC(option[7].help,"Toggle beep mode");
    AssignAOC(option[8].help,"Printer settings");
    AssignAOC(option[10].help,"Save settings to disk");
    AssignAOC(option[11].help,"Return to EDIT mode");
  END;
  CreateMenu(10, mb7);
  WITH mb7^ DO
    width := 13; on := {1,3,4,5,6,7,8,9,10};
    AssignAOC(option[1].key,"Page Length");
    Clear(option[2].key);
    AssignAOC(option[3].key,"Before Title");
    AssignAOC(option[4].key,"After Title");
    AssignAOC(option[5].key,"Bold On");
    AssignAOC(option[6].key,"Bold Off");
    AssignAOC(option[7].key,"Underline On");
    AssignAOC(option[8].key,"Underline Off");
    AssignAOC(option[9].key,"Inverse On");
    AssignAOC(option[10].key,"Inverse Off");
    AssignAOC(option[1].help,"Set page length of printer");
    AssignAOC(option[3].help,"Code before title in print header");
    AssignAOC(option[4].help,"Code after title in print header");
    AssignAOC(option[5].help,"Code before bold characters");
    AssignAOC(option[6].help,"Code after bold characters");
    AssignAOC(option[7].help,"Code before underlined characters");
    AssignAOC(option[8].help,"Code after underlined characters");
    AssignAOC(option[9].help,"Code before inverse characters");
    AssignAOC(option[10].help,"Code after inverse characters");
  END; (* WITH *)
  CreateMenu(9, mb10);
  WITH mb10^ DO
    width := 10; on := {1,2,3,4,6,7,8,9};
    AssignAOC(option[1].key,'mm/dd/yy  ');
    AssignAOC(option[2].key,'dd/mm/yy  ');
    AssignAOC(option[3].key,'dd.mm.yy  ');
    AssignAOC(option[4].key,'yy-mm-dd  ');
    Clear(option[5].key);
    AssignAOC(option[6].key,'00:00     ');
    AssignAOC(option[7].key,'00.00     ');
    AssignAOC(option[8].key,'00,00     ');
    AssignAOC(option[9].key,'00h00     ');
    FOR i := 1 TO 9 DO Clear(option[i].help) END;
  END;
END SetStuff.
