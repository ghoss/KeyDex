(*****************************************************************************
*
* Name    : KDPrint
*
* Created : 16/07/87
* Project : KeyDex system
* 
* Author  : Guido Hoss
* System  : LOGITECH MODULA-2/86, SYSTEM VERSION 3.00
*           Copyright 1987, 2016 by Guido Hoss. All Rights Reserved. 
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
* 07/08/87 : Several small changes. Criteria eliminated.
* 23/08/87 : Waiting implemented.
* 03/09/87 : Major changes with new page headers etc.
* 15/10/87 : Now switches all codes of before starting printout.
* 25/10/87 : Init printer codes only send for NOT extended.
* 05/11/87 : Wrong error msg appeared if opening text file and disk error.
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)


IMPLEMENTATION MODULE KDPrint;


FROM SYSTEM IMPORT
  BYTE, ADR, ADDRESS;

FROM ASCII IMPORT
  cr, lf;

FROM SystemIO IMPORT
  white, underline, reverse, intensity;

FROM KDBreak IMPORT
  UserBreak;

FROM KDStorage IMPORT
  ALLOCATE, DEALLOCATE;

FROM LongCardinals IMPORT
  LONGCARD, equal, zero, one, less, inc, dec;

FROM KDConfig IMPORT
  pageLength, printerCode, ScrAttribute, FileNameChar, Keyword, 
  DateToStr, TimeChar, TimeType;

FROM KDConversions IMPORT
  StrToCard, CardToStr;

FROM TimeDate IMPORT
  GetTime, Time;

FROM KDScreen IMPORT
  Indicator, SetIndicator, DispRecordKey;

FROM KWParser IMPORT
  FirstScr, NextScr, ReturnTotal;

FROM Menus IMPORT
  CreateMenu, MenuBarPtr, PullDownMenu, RemovePDMenu, DialogBox, MessageBox;

FROM MsFileSystem IMPORT
  fsDelete, fsCreate, fsOpen, fsResult, fsClose, fsWriteBlock, fsWriteByte,
  fsFile, fsErrorType, fsShareMode, fsAccessMode, fsLength, fsSetPos;

FROM FileOp IMPORT
  dbname, ScanReset, ScanNextChar, ScanSetScreenPos, EOL, EOP, EOR,
  wh, re, iu, wu, wi, ScanEOF, dateCreated;

FROM Keywords IMPORT
  IndexEmpty, empty, NextKeyword, NextDate, ResetList, KwIndexType,
  loaded, recdate, recno, FileLoaded;

FROM KDEdit IMPORT
  MaxPage, MinR, MaxR, MinC, MaxC, PagePtr, page;

FROM BTrees IMPORT
  BrowseKeywords, BrowseDates;

FROM String IMPORT
  AssignAOC, Clear, AssignStr, InsertCh, Copy;

FROM KDCritErr IMPORT
  InvokeHandler, DispatchHandler; 

FROM HelpSystem IMPORT
  SetHelpContext;
  
FROM HelpContext IMPORT
  ModuleIndex, ScreenNumber;


CONST
  str01        = 'Could not complete print operation.';
  str02        = 'Please insert new paper sheet.';
  bottomMargin = 7;

TYPE
  PrintDevice = (par1, par2, par3, ser1, ser2, text);
  fName       = ARRAY [0..64] OF CHAR;
  PrintType   = (range, dump, index, direct);
  IndexPhase  = (phBegin, phTemplate, phNumeric, phAlpha, phOthers, phDump);
  KeywordBuffer = POINTER TO KWBufferRec;
  KWBufferRec =
    RECORD
      key : Keyword; next : KeywordBuffer;
    END; (* RECORD *)

VAR
  mb1, mb3,
  mb4,
  mb9        : MenuBarPtr;
  marker     : ARRAY BOOLEAN OF CHAR;
  dev        : ARRAY [1..5],[0..5] OF CHAR;
  drStr,
  titStr     : ARRAY [0..60] OF CHAR;
  dashes     : ARRAY [0..79] OF CHAR;
  fileName   : fName;
  currentDev : PrintDevice;
  inversePr,
  extendedPr,
  blanklinePr,
  firstOnly,
  headerPr,
  waitBreak,
  first,
  remember    : BOOLEAN;
  f           : fsFile;
  lastDate    : CARDINAL;
  printTime   : Time;
  lastRec,
  rCount,
  totalRec    : LONGCARD;
  lineCount,
  columnCount,
  pageNo      : CARDINAL;
  typ         : PrintType;
  firstKW,
  lastKW      : KeywordBuffer;
  phase       : IndexPhase;



PROCEDURE fResult() : BOOLEAN;
BEGIN
  RETURN fsResult() = etOK;
END fResult;



PROCEDURE ValidChar(c : CHAR) : BOOLEAN;
BEGIN
  RETURN (FileNameChar(c) = CAP(c)) 
END ValidChar;

PROCEDURE ValidChar2(c : CHAR) : BOOLEAN;
BEGIN
  RETURN TRUE;
END ValidChar2;

PROCEDURE dummy(n : CARDINAL; VAR s : ARRAY OF CHAR) : BOOLEAN;
BEGIN
  Clear(s); RETURN FALSE;
END dummy;


PROCEDURE WriteSpace(n : CARDINAL);
BEGIN
  WHILE fResult() & (n > 0) DO fsWriteByte(f, ' '); DEC(n) END;
END WriteSpace;


PROCEDURE SendCode(at : ScrAttribute; on : BOOLEAN);
VAR
  n : CARDINAL;
BEGIN
  IF fResult() THEN
    WITH printerCode[at] DO
      IF on THEN
        fsWriteBlock(f, ADR(atOn) + 1, ORD(atOn[0]), n);
      ELSE
        fsWriteBlock(f, ADR(atOff) + 1, ORD(atOff[0]), n);
      END; (* IF *)
    END; (* WITH *)
  END; (* IF *)
END SendCode;



PROCEDURE InitPrinter;
(* Sends initialization codes to the printer. *)
BEGIN
  IF NOT extendedPr THEN
    SendCode(atUnderline, FALSE); SendCode(atReverse, FALSE);
    SendCode(atBold, FALSE);
  END; (* IF *)
END InitPrinter;



PROCEDURE PageHeader;

  PROCEDURE WriteLine;
  (* Own version to avoid recursion *)
  BEGIN
    IF fResult() THEN
      fsWriteByte(f, cr); IF fResult() THEN fsWriteByte(f,lf) END;
      IF lineCount > 0 THEN DEC(lineCount) END;
    END; (* IF *)
  END WriteLine;

  PROCEDURE NewPage;
  BEGIN
    IF pageLength = 0 THEN lineCount := 1 END;
    WHILE (lineCount > 0) & fResult() DO
      fsWriteByte(f, cr); IF fResult() THEN fsWriteByte(f,lf) END;
      DEC(lineCount);
    END; (* WHILE *)
  END NewPage;

BEGIN (* PageHeader *)
  INC(pageNo);
  IF headerPr & (pageLength # 0) THEN
    IF pageNo # 1 THEN 
      IF waitBreak THEN
        WriteLine;
        MessageBox(str02, FALSE);
      ELSE
        NewPage;
      END; (* IF *)
    END; (* IF *)
    lineCount := pageLength;
    IF (typ # range) OR (titStr[0] # 0C) THEN
      WriteStr('TITLE          : '); 
    END; (* IF *)
    SendCode(atTitle, TRUE);
    CASE typ OF
      range :
        IF titStr[0] # 0C THEN PrintDirect(titStr) END;
    | dump :
        WriteStr('SCREEN DUMP');
    | index :
        WriteStr('LIST OF KEYWORDS');
    END; (* CASE *)
    SendCode(atTitle, FALSE);
    IF (typ # range) OR (titStr[0] # 0C) THEN
      WriteLine;
    END; (* IF *)
    IF (typ # dump) OR (FileLoaded() & loaded) THEN
      WriteStr('FILE           : '); WriteFileName;
      WriteLine;
      WriteStr('CREATED ON     : '); WriteDate(dateCreated);
      WriteLine;
    END; (* IF *)
    IF (typ = range) OR ((typ = dump) & loaded) THEN
      WriteStr('RECORD CREATED : '); 
      IF typ = range THEN 
        WriteDate(lastDate);
        WriteSpace(33); WriteRecOf;
      ELSE
        WriteDate(recdate)
      END; (* IF *)
      WriteLine;
    END; (* IF *)
    WriteStr('PRINTOUT DATE  : '); WriteDate(printTime.day); WriteStr(' ');
    WriteTime(printTime.minute); 
    IF typ # index THEN
      WriteSpace(45); WriteStr('1.1');
    END; (* IF *)
    WriteLine; WriteDashes;
  ELSIF NOT extendedPr THEN
    IF waitBreak & (pageLength # 0) & (pageNo # 1) THEN
      WriteLine;
      MessageBox(str02, FALSE);
      lineCount := pageLength;
    ELSIF (typ = range) & (pageLength = 0) THEN
      WriteLine
    ELSE
      NewPage; lineCount := pageLength;
    END; (* IF *)
  END; (* IF *)
END PageHeader;



PROCEDURE NewPage;
BEGIN
  IF NOT extendedPr THEN
    IF waitBreak THEN lineCount := 1 END;
    WHILE (lineCount > 0) & fResult() DO
      fsWriteByte(f, cr); IF fResult() THEN fsWriteByte(f,lf) END;
      DEC(lineCount);
    END; (* WHILE *)
  ELSE
    lineCount := 0;
  END; (* IF *)
END NewPage;



PROCEDURE WriteLine;
(* Begin new line *)
BEGIN
  IF fResult() THEN
    fsWriteByte(f, cr); IF fResult() THEN fsWriteByte(f,lf) END;
    IF ((NOT headerPr) & (lineCount > bottomMargin)) 
       OR (headerPr & (lineCount > 0))
    THEN
      DEC(lineCount) 
    ELSIF NOT extendedPr THEN
      NewPage; lineCount := pageLength
    END;
  END; (* IF *)
END WriteLine;



PROCEDURE WriteStr(s : ARRAY OF CHAR);
(* Writes a sequence of characters to the device *)
VAR
  i, max : CARDINAL;
BEGIN
  i := 0; max := HIGH(s);
  WHILE (i <= max) & (s[i] # 0C) & fResult() DO
    fsWriteByte(f, s[i]); INC(i);
  END; (* WHILE *)
END WriteStr;



PROCEDURE WriteUnderline(s : ARRAY OF CHAR);
(* Used by index procedure *)
BEGIN
  IF NOT headerPr THEN RETURN END;
  IF columnCount # 0 THEN WriteLine; columnCount := 0 END; WriteLine;
  SendCode(atUnderline, TRUE);
  WriteStr(s);
  SendCode(atUnderline, FALSE);
  WriteLine; WriteLine;       
END WriteUnderline;



PROCEDURE WriteCard(x,n : CARDINAL; c : CHAR);
VAR
  s  : ARRAY [0..5] OF CHAR;
  n1 : CARDINAL;
BEGIN
  IF fResult() THEN
    Clear(s); n1 := 0;
    REPEAT
      InsertCh(s,CHR((x MOD 10)+ORD('0')),1); INC(n1);
      x := x DIV 10;
    UNTIL x = 0;
    WHILE n1<n DO InsertCh(s,c,1); INC(n1) END;
    fsWriteBlock(f, ADR(s) + 1, ORD(s[0]), n);
  END; (* IF *)
END WriteCard;



PROCEDURE WriteFileName;
(* Writes the name of the current file *)
VAR
  n : CARDINAL;
BEGIN
  IF fResult() THEN
    fsWriteBlock(f, ADR(dbname) + ORD(dbname[0]) - 11, 8, n);
  END; (* IF *)
END WriteFileName;



PROCEDURE WriteDate(t : CARDINAL);
(* Writes date *)
VAR
  s : ARRAY [0..8] OF CHAR;
  h : CARDINAL;
BEGIN
  IF fResult() THEN
    DateToStr(t, s); fsWriteBlock(f, ADR(s[1]), 8, h);
  END; (* IF *)
END WriteDate;



PROCEDURE WriteTime(t : CARDINAL);
(* Writes time *)
BEGIN
  IF fResult() THEN
    WriteCard(t DIV 60,2,'0');
    IF fResult() THEN
      fsWriteByte(f, TimeChar[TimeType]); WriteCard(t MOD 60, 2, '0');
    END; (* IF *)
  END; (* IF *)
END WriteTime;



PROCEDURE WriteRecOf;
(* Writes 'RECORD X OF Y' *)
VAR
  c : ARRAY [0..5] OF CHAR;
BEGIN
  CardToStr(pageNo, c); WriteSpace(5 - ORD(c[0]));
  CardToStr(totalRec.low, c); WriteSpace(5 - ORD(c[0]));
  WriteStr('RECORD '); WriteCard(pageNo, 1, ' '); WriteStr(' OF ');
  WriteCard(totalRec.low, 1, ' ');
END WriteRecOf;



PROCEDURE OpenFile(VAR res : fsErrorType);
(* Performs an "open" operation on selected device *)
VAR
  fn   : fName;
  h, l : CARDINAL;
BEGIN
  SetIndicator(wait); AssignStr(fn, fileName);
  fsOpen(fn, smDenyReadWrite, amWrite, f);
  (* Append to end of file if text *)
  IF (currentDev = text) & fResult() THEN
    fsLength(f, h, l); fsSetPos(f, h, l);
  END; (* IF *)
  GetTime(printTime); pageNo := 0;
  res := fsResult();
END OpenFile;



PROCEDURE BrowseProc(VAR kw : Keyword; p : LONGCARD) : BOOLEAN;
(* Browse procedure for "PrintIndex". *)

  PROCEDURE AddOthers(VAR kw : Keyword);
  (* Adds a keyword to the buffer list *)
  BEGIN
    IF lastKW # NIL THEN
      NEW(lastKW^.next); lastKW := lastKW^.next;
    ELSE
      NEW(lastKW); firstKW := lastKW;
    END; (* IF *)
    WITH lastKW^ DO key := kw; next := NIL END;
  END AddOthers;

  PROCEDURE Number(VAR kw : Keyword) : BOOLEAN;
  VAR 
    c : CHAR;
  BEGIN
    FOR i := 1 TO ORD(kw[0]) DO
      c := kw[i];
      IF (c < '0') OR (c > '9') THEN RETURN FALSE END;
    END; (* FOR *)
    RETURN TRUE;
  END Number;

VAR
  i   : CARDINAL;
  t   : KeywordBuffer;
  new : BOOLEAN;
BEGIN
  (* Determine type of keyword *)
  new := FALSE;
  LOOP
    CASE phase OF
      phBegin :
        INC(phase); new := TRUE;
    | phTemplate :
        IF kw[1] = '$' THEN
          IF new THEN
            WriteUnderline('TEMPLATES');
          END; (* IF *)
          EXIT
        ELSE
          INC(phase); new := TRUE; remember := TRUE;
        END; (* IF *)
    | phNumeric :
        IF Number(kw) OR (NOT headerPr) THEN
          IF new OR remember THEN
            WriteUnderline('NUMERIC'); remember := FALSE;
          END; (* IF *)
          EXIT
        ELSIF kw[1] < 'A' THEN 
          AddOthers(kw); RETURN TRUE;
        ELSE
          INC(phase); new := TRUE;
        END; (* IF *)
    | phAlpha :
        IF kw[1] <= 'Z' THEN
          IF new THEN
            WriteUnderline('ALPHA-NUMERIC');
          END; (* IF *)
          EXIT 
        ELSE
          INC(phase); new := TRUE;
        END; (* IF *)
    | phOthers :
        IF new THEN
          WriteUnderline('OTHERS');
        END; (* IF *)
        IF firstKW # NIL THEN
          AddOthers(kw);
          kw := firstKW^.key; t := firstKW; firstKW := firstKW^.next;
          (** 24/04/88 **)
          DISPOSE(t);
        END; (* IF *)
        (** DISPOSE(t); **)
        EXIT;
    | phDump :
        EXIT;
    END; (* CASE *)
  END; (* LOOP *)

  fsWriteBlock(f, ADR(kw[1]), ORD(kw[0]), i); INC(columnCount);
  IF columnCount # 3 THEN
    WriteSpace(25 - i);
  ELSE
    WriteLine; columnCount := 0;
  END; (* IF *)
  RETURN fResult() & (NOT UserBreak());
END BrowseProc;



PROCEDURE BrowseProc2(d : CARDINAL; p : LONGCARD) : BOOLEAN;
(* Browse procedure for "PrintIndex". *)
VAR
  kw : ARRAY [0..8] OF CHAR;
  i  : CARDINAL;
BEGIN
  IF phase = phBegin THEN
    WriteUnderline('DATES'); INC(phase);
  END; (* IF *)
  DateToStr(d, kw);
  fsWriteBlock(f, ADR(kw[1]), 8, i); INC(columnCount);
  IF columnCount # 3 THEN
    WriteSpace(17);
  ELSE
    WriteLine; columnCount := 0;
  END; (* IF *)
  RETURN fResult() & (NOT UserBreak());
END BrowseProc2;



PROCEDURE SwitchAttr(from, to : BYTE);
(* Switch to new attribute *)
VAR
  b  : BOOLEAN;
  at : BYTE;
BEGIN
  IF NOT extendedPr THEN
    b := (from = to); at := from;
    LOOP
      CASE ORD(at) OF
        white :
          (* nothing to switch off *)
      | reverse :
          SendCode(atReverse, b);
      | underline :
          SendCode(atUnderline, b);
      | underline + intensity :
          SendCode(atBold, b); SendCode(atUnderline, b);
      | white + intensity :
          SendCode(atBold, b);
      END; (* CASE *)
      IF NOT b THEN b := TRUE; at := to ELSE EXIT END;
    END; (* LOOP *)
  END; (* IF *)
END SwitchAttr;


PROCEDURE PrintCurrent;
(* Print current screen contents *)

  PROCEDURE EndOfLine(i : INTEGER) : INTEGER;
  VAR
    x : INTEGER;
  BEGIN
    x := MaxC;
    LOOP
      IF x < MinC THEN EXIT END;
      WITH p^[i, x] DO
        IF (ch=' ') & ((at=BYTE(white)) OR (at=BYTE(white+intensity))) THEN
          DEC(x)
        ELSE
          EXIT
        END; (* IF *)
      END; (* WITH *)
    END; (* LOOP *)
    RETURN x;
  END EndOfLine;

VAR
  row, col,
  lastRow,
  lastCol  : INTEGER;
  i, n,
  lastPage : CARDINAL;
  attr     : BYTE;
  p        : PagePtr;
  ignore,t : BOOLEAN;
  adr      : ADDRESS;
  kw       : Keyword;
BEGIN (* PrintCurrent *)
  attr := BYTE(white); ignore := FALSE;
  typ := dump; pageNo := 0; lineCount := 0; PageHeader;
  IF firstOnly THEN lastPage := 1 ELSE lastPage := MaxPage END;
  FOR i := 1 TO lastPage DO
    lastRow := MaxR; p := page[i];
    IF NOT blanklinePr THEN 
      (* estimate real last row *)
      WHILE (lastRow >= MinR) & (EndOfLine(lastRow) < MinC) DO
        DEC(lastRow);
      END; (* WHILE *)
    END; (* IF *)
    SwitchAttr(attr, white); attr := BYTE(white);
    IF i = 2 THEN ScrPageHeader(FALSE) END;
    row := MinR;
    WHILE row <= lastRow DO
      lastCol := EndOfLine(row);
      FOR col := MinC TO lastCol DO
        IF fsResult() # etOK THEN RETURN END;
        WITH p^[row, col] DO
          IF at # attr THEN
            IF (at = BYTE(reverse)) & (NOT inversePr) THEN
              ignore := TRUE;
            ELSIF extendedPr THEN
              IF NOT ignore THEN
                CASE ORD(at) OF
                  white :
                    WriteAttr('N');
                | reverse :
                    WriteAttr('I');
                | underline :
                    WriteAttr('U');
                | underline + intensity :
                    WriteAttr('I');
                | white + intensity :
                    WriteAttr('B');
                ELSE
                END; (* CASE *)
              ELSE
                ignore := FALSE;
              END; (* IF *)
            ELSIF NOT ignore THEN
              SwitchAttr(attr, at);
            ELSE
              ignore := FALSE;
            END; (* IF *)
            attr := at;
          END; (* IF *)
          IF fResult() THEN
            IF NOT ignore THEN
              fsWriteByte(f, ch) 
            ELSE 
              fsWriteByte(f, ' ')
            END; (* IF *)
          END; (* IF *)
        END; (* WITH *)
      END; (* FOR *)
      WriteLine; INC(row);
      (* Repeat attribute code for new line *)
      SwitchAttr(attr, attr);
    END; (* WHILE *)
    IF extendedPr & (i < lastPage) THEN 
      WriteAttr('P'); WriteLine;
    END; (* IF *)
  END; (* FOR *)
  SwitchAttr(attr, white);
  IF extendedPr THEN
    (* Print keywords if any *)
    adr := ADR(kw[1]);
    IF NOT empty[KeywordIndex] THEN
      ResetList;
      t := NextKeyword(kw);
      WHILE (kw[0] # 0C) & fResult() DO
        WriteStr('<$K ');
        IF fResult() THEN
          fsWriteBlock(f, adr, ORD(kw[0]), n); 
          IF fResult() THEN
            fsWriteByte(f, '>'); WriteLine;
          END; (* IF *)
        END; (* IF *)
        t := NextKeyword(kw);
      END; (* WHILE *)
    END; (* IF *)
    IF NOT empty[DateIndex] THEN 
      ResetList;
      t := NextDate(kw);
      WHILE (kw[0] # 0C) & fResult() DO
        WriteStr('<$D '); 
        IF fResult() THEN
          fsWriteBlock(f, adr, 8, n); 
          IF fResult() THEN
            fsWriteByte(f, '>'); WriteLine;
          END; (* IF *)
        END; (* IF *)
        t := NextDate(kw);
      END; (* WHILE *)
    END; (* IF *)
    WriteAttr('R'); WriteLine;
  END; (* FOR *)
  NewPage;
END PrintCurrent;



PROCEDURE PrintIndex;
(* Prints the keyword index *)
VAR
  res : fsErrorType;
  str : Keyword;
  ok  : BOOLEAN;
BEGIN
  InvokeHandler(currentDev # text);
  OpenFile(res);
  InitPrinter; IF NOT fResult() THEN DispatchHandler; RETURN END;
  IF res = etOK THEN
    Clear(str); columnCount := 0; pageNo := 0;
    firstKW := NIL; lastKW := NIL;
    typ := index; lineCount := 0; PageHeader;
    phase := phBegin; remember := FALSE;
    BrowseKeywords(str, BrowseProc);
    phase := phDump; ok := fResult();
    WHILE firstKW # NIL DO
      lastKW := firstKW; firstKW := firstKW^.next; 
      IF ok THEN ok := BrowseProc(lastKW^.key, zero) END;
      DISPOSE(lastKW);
    END; (* WHILE *)
    IF fResult() THEN
      phase := phBegin;
      BrowseDates(0, BrowseProc2); NewPage; 
    END; (* IF *)
    fsClose(f);
  END; (* IF *)
  IF NOT fResult() THEN
    MessageBox(str01, TRUE);
  END; (* IF *)
  DispatchHandler;
END PrintIndex;



PROCEDURE PrintDirect(VAR drStr : ARRAY OF CHAR);
(* Prompts for a string and prints it directly to printer. *)

  PROCEDURE Digit(ch:CHAR) : BOOLEAN;
  BEGIN RETURN (ch>='0') & (ch<='9') END Digit;

VAR
  i, max : CARDINAL;
  c      : CHAR;
  b,
  done   : BOOLEAN;
  temp   : ARRAY [0..3] OF CHAR;
  code   : CARDINAL;
BEGIN
  i := 1; max := ORD(drStr[0]);
  WHILE (i <= max) & fResult() DO
    c := drStr[i];
    b := (c = '^') & (i+3 <= max); 
    b := b & Digit(drStr[i+1]) & Digit(drStr[i+2]) & Digit(drStr[i+3]);
    IF b THEN
      Copy(drStr, temp, i+1, 3); StrToCard(temp, code, done);
      IF done & (code <= 255) THEN fsWriteByte(f, CHR(code)) END;
      INC(i, 4);
    ELSE
      fsWriteByte(f, drStr[i]); INC(i);
    END; (* IF *)
  END; (* WHILE *)
END PrintDirect;



PROCEDURE WriteDashes;
VAR
  n : CARDINAL;
BEGIN
  IF fResult() THEN
    fsWriteBlock(f, ADR(dashes), 79, n);
    WriteLine;
  END; (* IF *)
END WriteDashes;




 
PROCEDURE ScrPageHeader(rec : BOOLEAN);
BEGIN
  IF NOT extendedPr THEN WriteLine END;
  IF headerPr THEN
    WriteLine;
    IF rec THEN
      WriteStr('RECORD CREATED : '); WriteDate(lastDate);
      WriteSpace(33); WriteRecOf;
      WriteLine;
    END; (* IF *)
    WriteStr('                                                                            1.');
    IF rec THEN WriteStr('1') ELSE WriteStr('2') END;
    WriteLine; WriteDashes;
  END; (* IF *)
END ScrPageHeader;



PROCEDURE DateProc(d : CARDINAL);
VAR
  s : ARRAY [0..8] OF CHAR;
  n : CARDINAL;
BEGIN
  IF extendedPr THEN
    IF first THEN
      WriteLine; first := FALSE;
    END; (* IF *)
    DateToStr(d, s); WriteStr('<$D ');
    IF fResult() THEN
      fsWriteBlock(f, ADR(s[1]), 8, n); 
      IF fResult() THEN 
        fsWriteByte(f, '>'); WriteLine;
      END; (* IF *)
    END; (* IF *)
  END; (* IF *)
END DateProc;


PROCEDURE KeywordProc(VAR k : Keyword);
VAR
  n : CARDINAL;
BEGIN
  IF extendedPr THEN
    IF first THEN
      WriteLine; first := FALSE;
    END; (* IF *)
    WriteStr('<$K ');
    IF fResult() THEN 
      fsWriteBlock(f, ADR(k[1]), ORD(k[0]), n);
      IF fResult() THEN
        fsWriteByte(f, '>'); WriteLine;
      END; (* IF *)
    END; (* IF *)
  END; (* IF *)
END KeywordProc;


PROCEDURE RecordProc(d : CARDINAL; n : LONGCARD);
BEGIN
  lastDate := d; lastRec := n;
  DispRecordKey(1, rCount); dec(rCount);
END RecordProc;



PROCEDURE WriteAttr(c : CHAR);
BEGIN
  WriteStr('<$'); 
  IF fResult() THEN 
    fsWriteByte(f,c);
    IF fResult() THEN
      fsWriteByte(f,'>');
    END; (* IF *)
  END; (* IF *)
END WriteAttr;



PROCEDURE PrintRange;
(* Prints a range of screens *)
VAR
  i,
  pos : LONGCARD;
  c   : CHAR;
  scrPage,
  lCount  : CARDINAL;
  count,
  firstPg,
  ignore  : BOOLEAN;
  attr    : BYTE;
BEGIN
  typ := range; pageNo := 0; lineCount := 0;
  count := TRUE; firstPg := TRUE; attr := BYTE(white);
  ReturnTotal(totalRec); i := one; rCount := totalRec; ignore := FALSE;
  ScanReset(TRUE, FALSE, DateProc, KeywordProc, RecordProc, TRUE);
  LOOP
    IF    ((NOT less(i, totalRec)) & (NOT equal(i, totalRec))) 
       OR (NOT fResult()) OR UserBreak()
    THEN
      EXIT
    END; (* IF *)
    (* Print screen at position "pos" *)
    IF equal(i, one) THEN FirstScr(pos) ELSE NextScr(pos) END; inc(i);
    ScanSetScreenPos(pos);
    IF NOT firstOnly THEN 
      IF (NOT firstPg) & headerPr THEN NewPage ELSE firstPg := FALSE END;
      PageHeader;
    ELSE
      IF count THEN
        IF (NOT firstPg) & headerPr THEN NewPage ELSE firstPg := FALSE END;
        PageHeader;
      ELSE
        INC(pageNo); ScrPageHeader(TRUE);
      END; (* IF *)
      count := NOT count;
    END; (* IF *)
    scrPage := 0; first := TRUE;
    LOOP
      INC(scrPage); lCount := MinR; 
      SwitchAttr(attr, white); attr := BYTE(white);
      IF firstOnly & (scrPage = 2) THEN
        IF extendedPr THEN
          (* Must skip rest until end of screen to get keywords *)
          WHILE NOT ScanEOF DO ScanNextChar(c) END;
          WriteAttr('R'); WriteLine;
        END; (* IF *)
        EXIT;
      ELSIF scrPage > 2 THEN
        EXIT;
      ELSIF scrPage = 2 THEN
        ScrPageHeader(FALSE);
      END; (* IF *)
      IF ScanEOF THEN EXIT END;
      LOOP
        (* Writes a single page *)
        ScanNextChar(c); 
        IF ScanEOF THEN 
          IF blanklinePr THEN
            WHILE (lCount < MaxR) DO 
              WriteLine; INC(lCount);
            END; (* WHILE *)
          END; (* IF *)
          IF extendedPr & (NOT firstOnly) THEN
            WriteAttr('R'); WriteLine;
          END; (* IF *)
          EXIT;
        END; (* IF *)
        CASE ORD(c) OF
          EOL :
            WriteLine; SwitchAttr(attr, attr); INC(lCount);
        | EOP :
            IF blanklinePr THEN
              WHILE (lCount < MaxR) DO 
                WriteLine; INC(lCount);
              END; (* WHILE *)
            END; (* IF *)
            IF extendedPr & (NOT firstOnly) THEN 
              WriteLine; WriteAttr('P'); WriteLine;
            END; (* IF *)
            ignore := FALSE;
            EXIT;
        | EOR :
            (* nothing *)
            IF blanklinePr THEN
              WHILE (lCount < MaxR) DO 
                WriteLine; INC(lCount);
              END; (* WHILE *)
            END; (* IF *)
        | wh..iu : 
            IF (ORD(c) = re) & (NOT inversePr) THEN
              ignore := TRUE;
            ELSE
              IF extendedPr THEN 
                IF NOT ignore THEN
                  CASE ORD(c) OF
                    wh : IF NOT ignore THEN WriteAttr('N') END;
                  | re : WriteAttr('I');
                  | wu : WriteAttr('U');
                  | wi : WriteAttr('B');
                  | iu : WriteAttr('H');
                  END; (* CASE *)
                ELSE
                  ignore := FALSE;
                END; (* IF *)
              ELSIF NOT ignore THEN
                CASE ORD(c) OF
                  wh : 
                    SwitchAttr(attr, white); 
                    attr := BYTE(white);
                | re : 
                    SwitchAttr(attr, reverse);
                    attr := BYTE(reverse);
                | wu : 
                    SwitchAttr(attr, underline); 
                    attr := BYTE(underline);
                | wi : 
                    SwitchAttr(attr, white+intensity); 
                    attr := BYTE(white+intensity);
                | iu : 
                    SwitchAttr(attr, underline+intensity); 
                    attr := BYTE(underline+intensity);
                END; (* CASE *)
              ELSE
                ignore := FALSE;
              END; (* IF *)
            END; (* IF *)
        ELSE
          (* Standard character, print *)
          IF fResult() THEN
            IF NOT ignore THEN
              fsWriteByte(f, c) 
            ELSE 
              fsWriteByte(f, ' ')
            END; (* IF *)
          END; (* IF *)
        END; (* CASE *)
        IF NOT fResult() THEN EXIT END;
      END; (* LOOP *)
      IF NOT fResult() THEN EXIT END;
    END; (* LOOP *)
    IF NOT fResult() THEN EXIT END;
  END; (* LOOP *)
  SwitchAttr(attr, white);
  NewPage;
  FirstScr(pos);  (* Reset screen range *)
  IF loaded THEN
    DispRecordKey(recdate, recno);
  ELSE
    DispRecordKey(0, zero);
  END; (* IF *)
END PrintRange;


PROCEDURE HelpProc2( sel : CARDINAL );
(* Dummy help procedure *)
END HelpProc2;


PROCEDURE HelpProc3( sel : CARDINAL );
(* Help procedure for Print/Settings menu *)
VAR
  cnt : INTEGER;
BEGIN
  IF sel < 3 THEN
    cnt := ScreenNumber(kdprint, 9);
  ELSIF sel < 8 THEN
    cnt := ScreenNumber(kdprint, 10);
  ELSE
    cnt := ScreenNumber(kdprint, 11);
  END; (* IF *)
  SetHelpContext(cnt);    
END HelpProc3;


PROCEDURE HelpProc( sel : CARDINAL );
(* Help procedure for 'Print' menu *)
BEGIN
  SetHelpContext(ScreenNumber(kdprint, sel));
END HelpProc;



PROCEDURE PrintMenu;
VAR     
  sel,
  sel2 : CARDINAL;
  t    : LONGCARD;
  res,
  new  : BOOLEAN;
  r    : fsErrorType;
BEGIN
  ReturnTotal(t);
  WITH mb3^ DO
    IF equal(t, zero) THEN EXCL(on, 1) ELSE INCL(on, 1) END;
    IF IndexEmpty(KeywordIndex) & IndexEmpty(DateIndex) THEN 
      EXCL(on, 3) 
    ELSE 
      INCL(on, 3) 
    END; (* IF *)
  END; (* WITH *)
  sel := 0; quit := FALSE; motion := 0;
  REPEAT
    PullDownMenu(17, 2, mb3^, sel, HelpProc); sel2 := 0;
    CASE sel OF
      0 :
        RETURN
    | 1 :
        res := TRUE;
        IF headerPr THEN
          DialogBox('You may specify an optional title; type ^nnn for ASCII code nnn.',
                     titStr, dummy, ValidChar2, res, new);
        END; (* IF *)
        IF res THEN
          IF currentDev = text THEN
            sel2 := 2;
          ELSE
            PullDownMenu(33, 3, mb4^, sel2, HelpProc2);
          END; (* IF *)
          IF sel2 = 2 THEN
            IF (currentDev # text) THEN RemovePDMenu END;
            InvokeHandler(currentDev # text);
            OpenFile(r);
            IF r = etOK THEN
              InitPrinter; 
              IF fResult() THEN PrintRange; fsClose(f) END;
              quit := fResult();
            END; (* IF *)
            DispatchHandler;
          ELSIF sel2 = 1 THEN
            RemovePDMenu;
          END; (* IF *)
        END; (* IF *)
    | 2 :
        IF currentDev = text THEN
          sel2 := 2;
        ELSE
          PullDownMenu(33, 3, mb4^, sel2, HelpProc2);
        END; (* IF *)
        IF sel2 = 2 THEN
          IF (currentDev # text) THEN RemovePDMenu END;
          InvokeHandler(currentDev # text);
          OpenFile(r); 
          IF r = etOK THEN
            InitPrinter; IF fResult() THEN PrintCurrent; fsClose(f) END;
            quit := fResult();
          END; (* IF *)
          DispatchHandler; 
        ELSIF sel2 = 1 THEN
          RemovePDMenu;
        END; (* IF *)
    | 3 :
        IF currentDev = text THEN
          sel2 := 2;
        ELSE
          PullDownMenu(33, 4, mb4^, sel2, HelpProc2);
        END; (* IF *)
        IF sel2 = 2 THEN
          IF (currentDev # text) THEN RemovePDMenu END;
          PrintIndex; quit := fResult(); 
        ELSIF sel2 = 1 THEN
          RemovePDMenu;
        END; (* IF *)
    | 4 :
        DialogBox('Enter character sequence; ^nnn =  ASCII code nnn.',
                   drStr, dummy, ValidChar2, res, new);
        IF res & (drStr[0] # 0C) THEN
          InvokeHandler(currentDev # text); 
          OpenFile(r);
          IF r # etOK THEN MessageBox(str01, TRUE)
          ELSE
            InitPrinter;
            PrintDirect(drStr);
            fsClose(f);
            IF NOT fResult() THEN MessageBox(str01, TRUE) END;
          END; (* IF *)
          DispatchHandler;
          quit := TRUE;
        END; (* IF *)
    | 6 :
        Settings;
    | 8 :
        quit := TRUE;
    |100 :
        motion := -1; RETURN
    |101 : 
        motion := +1; RETURN
    ELSE END; (* CASE *)
  UNTIL quit;
  RemovePDMenu;
END PrintMenu;



PROCEDURE Settings;
VAR     
  sel,i,j : CARDINAL;
  res,new : BOOLEAN;
  fn      : fName;
BEGIN
  sel := 0;
  LOOP
    WITH mb9^ DO
      res := (currentDev # text);
      IF res THEN INCL(mb9^.on, 11) END;
      option[1].key[13] := marker[res];
      option[2].key[13] := marker[NOT res];
      option[4].key[13] := marker[NOT extendedPr];
      option[5].key[13] := marker[extendedPr];
      option[7].key[13] := marker[headerPr];
      option[8].key[13] := marker[blanklinePr];
      option[9].key[13] := marker[inversePr];
      option[10].key[13] := marker[firstOnly];
      option[11].key[13] := marker[waitBreak];
    END; (* WITH *)
    PullDownMenu(30, 7, mb9^, sel, HelpProc3);
    CASE sel OF
      0 : RETURN;
    | 1 : i := 0;
          LOOP
            WITH mb1^ DO
              FOR j := 1 TO 5 DO option[j].key[6] := ' ' END;
              IF currentDev # text THEN 
                option[ORD(currentDev) + 1].key[6] := 'þ'
              END; (* IF *)
            END; (* WITH *)
            PullDownMenu(47, 7, mb1^, i, HelpProc2);
            IF i = 0 THEN EXIT
            ELSE
              currentDev := VAL(PrintDevice, i-1);
              AssignAOC(fileName, dev[i]);
            END; (* IF *)
          END; (* LOOP *)
    | 2 : IF currentDev = text THEN
            fn := fileName
          ELSE
            Clear(fn)
          END; (* IF *)
          DialogBox('Enter DOS path and name of output file:',fn,
                    dummy, ValidChar, res, new);
          IF res & (fn[0] # 0C) THEN
            (** 05/11/87 **)
            InvokeHandler(FALSE);
            AssignStr(fn, fn); fsDelete(fn); fsCreate(fn);
            DispatchHandler;
            IF fResult() THEN
              AssignAOC(fileName, fn);
              currentDev := text; waitBreak := FALSE;
              EXCL(mb9^.on, 11);
            ELSE
              MessageBox('Could not create text file.', TRUE);
            END; (* IF *)
          END; (* IF *)

    | 4,5 : 
          extendedPr := NOT extendedPr;
          IF extendedPr THEN 
            headerPr := FALSE; blanklinePr := FALSE;
            WITH mb9^ DO on := on - {7,8} END;
          ELSE
            WITH mb9^ DO on := on + {7,8} END;
          END; (* IF *)

    | 7 : headerPr := NOT headerPr;
          IF headerPr THEN extendedPr := FALSE END;

    | 8 : blanklinePr := NOT blanklinePr;
          IF blanklinePr THEN extendedPr := FALSE END;

    | 9 : inversePr := NOT inversePr;
    |10 : firstOnly := NOT firstOnly;
    |11 : waitBreak := (NOT waitBreak) & (currentDev # text);
    END; (* CASE *)
  END; (* LOOP *)
  RemovePDMenu;
END Settings;


VAR
  i : CARDINAL;
BEGIN (* KDPrint *)
  inversePr := TRUE; extendedPr := FALSE;
  firstOnly := FALSE; blanklinePr := TRUE;
  headerPr := TRUE; waitBreak := FALSE;
  marker[FALSE] := ' '; marker[TRUE] := 'þ';
  dev[1] := 'LPT1  '; dev[2] := 'LPT2  '; dev[3] := 'LPT3  ';
  dev[4] := 'COM1  '; dev[5] := 'COM2  ';
  currentDev := par1; AssignAOC(fileName, dev[1]);
  Clear(drStr); Clear(titStr);
  dashes := '-------------------------------------------------------------------------------';
  CreateMenu(5,mb1);
  WITH mb1^ DO
    width := 6; on := {1,2,3,4,5};
    FOR i := 1 TO 5 DO AssignAOC(option[i].key, dev[i]) END;
    AssignAOC(option[1].help,"First parallel port");
    AssignAOC(option[2].help,"Second parallel port");
    AssignAOC(option[3].help,"Third parallel port");
    AssignAOC(option[4].help,"First serial port");
    AssignAOC(option[5].help,"Second serial port");
  END; (* WITH *)
  CreateMenu(8,mb3);
  WITH mb3^ DO
    width := 11; on := {2,4,6,8};
    AssignAOC(option[1].key,"Range");
    AssignAOC(option[2].key,"Current");
    AssignAOC(option[3].key,"Index");
    AssignAOC(option[4].key,"Direct");
    Clear(option[5].key);
    AssignAOC(option[6].key,"Settings...");
    Clear(option[7].key);
    AssignAOC(option[8].key,"Return");
    AssignAOC(option[1].help,"Print range of screens");
    AssignAOC(option[2].help,"Print current screen");
    AssignAOC(option[3].help,"Print sorted index of keywords");
    AssignAOC(option[4].help,"Print characters entered by hand");
    AssignAOC(option[6].help,"Specify how and where to print");
    AssignAOC(option[8].help,"Return to EDIT mode");
  END; (* WITH *)
  CreateMenu(2,mb4);
  WITH mb4^ DO
    width := 8; on := {1,2};
    AssignAOC(option[1].key, "Abort");
    AssignAOC(option[2].key, "Continue");
    AssignAOC(option[1].help, "Abort this function");
    AssignAOC(option[2].help, "Start printing. (Align paper)");
  END; (* WITH *)
  CreateMenu(11,mb9);
  WITH mb9^ DO
    width := 13; on := {1,2,4,5,7,8,9,10,11};
    AssignAOC(option[1].key,"Printer...   ");
    AssignAOC(option[2].key,"Text File    ");
    Clear(option[3].key);
    AssignAOC(option[4].key,"Report       ");
    AssignAOC(option[5].key,"Interchange  ");
    Clear(option[6].key);
    AssignAOC(option[7].key,"Page Header  ");
    AssignAOC(option[8].key,"Blank Lines  ");
    AssignAOC(option[9].key,"Inverse      ");
    AssignAOC(option[10].key,"First Only   ");
    AssignAOC(option[11].key,"Wait         ");
    AssignAOC(option[1].help,"Output to printer");
    AssignAOC(option[2].help,"Output to text file");
    AssignAOC(option[4].help,"Print without format information");
    AssignAOC(option[5].help,"Print with format information");
    AssignAOC(option[7].help,"Print page breaks and titles");
    AssignAOC(option[8].help,"Print trailing blank lines");
    AssignAOC(option[9].help,"Include inverse characters");
    AssignAOC(option[10].help,"Print 1st page of screen only");
    AssignAOC(option[11].help,"Wait until page is inserted");
  END; (* WITH *)
END KDPrint.