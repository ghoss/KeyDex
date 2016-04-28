(*****************************************************************************
*
* Name    : KDImport 
*
* Created : 30/07/87
* Project : KeyDex system.
* Purpose : Handles import of DOS text files into editor.
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
* 14/08/87 : Bug fixed (page switch not correct).
* 26/08/87 : Changed behavior in case of page overflow.
* 27/08/87 : Modifications in 'ImportScreen'.
* 03/09/87 : Screen now cleared in any case.
* 06/09/87 : Changes in behavior at end of file.
* 15/10/87 : Bug fixed in ImportScreen (would not read things like 'i < 5');
* 05/11/87 : m2c 3.0 : Fixed storage problems.
* 03/12/87 : Fixed a DEALLOC statement (forgotten on 05/11).
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)


IMPLEMENTATION MODULE KDImport;


FROM SYSTEM IMPORT
  BYTE, ADDRESS;

FROM ASCII IMPORT
  cr, lf, ff;

FROM SystemIO IMPORT
  white, underline, reverse, intensity;

FROM KDBreak IMPORT
  UserBreak;

FROM KDStorage IMPORT
  ALLOCATE, DEALLOCATE, Normalize;

FROM KDConfig IMPORT
  StrToDate, Keyword;

FROM MsFileSystem IMPORT
  fsFile, fsResult, fsErrorType, fsOpen, fsShareMode,
  fsClose, fsAccessMode, fsReadBlock;

FROM KDEdit IMPORT
  MaxPage, MinC, MaxC, MinR, MaxR, page, PagePtr, DisplayPage, ClearPages;

FROM Keywords IMPORT
  ValidKeywordChar, AddKeyword, AddDate, ResetList, NextKeyword, 
  NextDate, SaveScreen;

FROM String IMPORT
  AppendCh, Delete, Clear, AssignAOC;


CONST
  bSize = 1024;

TYPE
  CharPtr = POINTER TO CHAR;

VAR
  f      : fsFile;
  opened,
  lastCR,
  EOF    : BOOLEAN;
  buf,
  bufOrig,
  cur    : CharPtr;
  max    : INTEGER;
  buffer : ARRAY [0..30] OF CHAR;
  terminated,
  charEntered : BOOLEAN;



PROCEDURE ReadChar(VAR c : CHAR);
VAR
  n : CARDINAL;
BEGIN
  IF max = 0 THEN
    fsReadBlock(f, buf, bSize, n); max := VAL(INTEGER, n);
    EOF := (max = 0); cur := buf; IF EOF THEN cur^ := 0C END;
  END; (* IF *)
  c := cur^; cur := CharPtr(ADDRESS(cur) + 1); DEC(max);
END ReadChar;



PROCEDURE InitImportFile(VAR s : ARRAY OF CHAR; VAR res : BOOLEAN);
BEGIN
  IF opened THEN 
    fsClose(f) 
  ELSE 
    ALLOCATE(bufOrig, bSize); buf := bufOrig; Normalize(buf);
  END; (* IF *)
  fsOpen(s, smDenyWrite, amRead, f);
  res := (fsResult() = etOK); opened := res; max := 0;
  IF NOT opened THEN
    DEALLOCATE(bufOrig, bSize);
  ELSE
    Clear(buffer);
  END; (* IF *)
END InitImportFile;




PROCEDURE ImportScreen(VAR all, err : BOOLEAN);

  PROCEDURE SkipToEOL;
  (* Skips to end of line *)
  VAR
    c : CHAR;
  BEGIN
    WHILE (NOT EOF) & (c # cr) DO
      ReadChar(c);
    END; (* WHILE *)
    IF c = cr THEN
      ReadChar(c);
      IF c # lf THEN AppendCh(buffer, c) END;
    END; (* IF *)
  END SkipToEOL;

VAR
  c      : CHAR;
  attr,
  block,
  fromBuffer : BOOLEAN;
  row, col,
  pg,d   : CARDINAL;
  curAttr: BYTE;
  p      : PagePtr;
BEGIN
  ClearPages(FALSE);
  terminated := FALSE; charEntered := FALSE;
  row := MinR; col := MinC; pg := 1; attr := FALSE; err := FALSE;
  p := page[1]; lastCR := FALSE;
  curAttr := BYTE(white);
  block := FALSE;
  LOOP
    (* Load the characters *)
    fromBuffer := (buffer[0] # 0C);
    IF NOT fromBuffer THEN
      ReadChar(c);
    ELSE
      c := buffer[1]; Delete(buffer,1,1);
    END; (* IF *)
    IF (c = '<') & (NOT fromBuffer) THEN
      (* Maybe control code *) 
      AppendCh(buffer, '<');
      ReadChar(c); AppendCh(buffer, c);
      IF c = '$' THEN
        ReadChar(c); attr := TRUE; c := CAP(c);
        CASE c OF
          'P' : (** 14/08/87 : AND -> OR **)
                IF ((row # MinR) OR (col # MinC)) & (NOT block) THEN
                  (* We were not at end of previous page *)
                  INC(pg); row := MinR; col := MinC; 
                  IF pg <= MaxPage THEN p := page[pg] ELSE block := TRUE END;
                END; (* IF *)
                SkipToEOL; attr := FALSE; Clear(buffer); 
        | 'R' : attr := FALSE; Clear(buffer); SkipToEOL;
                terminated := TRUE; EXIT;
        | 'I' : curAttr := BYTE(reverse);
        | 'U' : curAttr := BYTE(underline);
        | 'B' : curAttr := BYTE(white + intensity);
        | 'N' : curAttr := BYTE(white);
        | 'H' : curAttr := BYTE(underline + intensity);
        | 'K' : Clear(buffer);
                REPEAT
                  ReadChar(c);
                  IF ValidKeywordChar(c) THEN AppendCh(buffer, c) END; 
                UNTIL (c = '>') OR EOF;
                IF buffer[0] > 0C THEN AddKeyword(buffer) END;
                SkipToEOL; attr := FALSE; Clear(buffer);
        | 'D' : Clear(buffer);
                REPEAT
                  ReadChar(c);
                  IF (c # ' ') & (c # '>') THEN AppendCh(buffer, c) END;
                UNTIL (c = '>') OR EOF;
                IF buffer[0] > 0C THEN
                  StrToDate(buffer, d); 
                  IF d # 0 THEN AddDate(d) END;
                END; (* IF *)
                SkipToEOL; attr := FALSE; Clear(buffer);
        ELSE
          attr := FALSE; AppendCh(buffer, c);
        END; (* CASE *)
        IF attr THEN
          ReadChar(c); 
          IF c # '>' THEN AppendCh(buffer, c) ELSE Clear(buffer) END;
          attr := FALSE;
        END; (* IF *)
      END; (* IF *)
    ELSIF (c = cr) & (NOT (err OR block)) THEN
      ReadChar(c);
      IF (c # lf) THEN AppendCh(buffer, c) END;
      IF NOT lastCR THEN INC(row) ELSE lastCR := FALSE END;
      col := MinC;
      IF row > MaxR THEN
        row := MinR; INC(pg); 
        IF (pg <= MaxPage) THEN p := page[pg] ELSE block := TRUE END;
      END; (* IF *)
    ELSIF (c = ff) THEN
      (* equivalent to <$R>; test if next char equals cr/lf *)
      ReadChar(c); 
      IF c # cr THEN 
        AppendCh(buffer, c)
      ELSE
        SkipToEOL;
      END; (* IF *)
      EXIT;
    ELSIF NOT (attr OR err OR block OR (ORD(c) < 32)) THEN
      (* Put character into screen buffer *)
      charEntered := TRUE;
      IF    (c # ' ') 
         OR ((curAttr # BYTE(white)) & (curAttr # BYTE(white+intensity)))
      THEN
        (* This makes blanks transparent *)
        WITH p^[row, col] DO
          ch := c; at := curAttr;
        END; (* WITH *)
      END; (* IF *)
      INC(col); 
      IF col > MaxC THEN 
        col := MinC; INC(row); 
        IF row > MaxR THEN
          row := MinR; INC(pg);
          IF (pg <= MaxPage) THEN p := page[pg] ELSE block := TRUE END;
        ELSE 
          lastCR := TRUE; 
        END; (* IF *)
      END; (* IF *)
    ELSE
      attr := FALSE;
    END; (* IF *)
    IF EOF THEN EXIT END;
  END; (* LOOP *)
  DisplayPage(1);
  all := EOF; err := (NOT all) & (fsResult() # etOK) & (fsResult() # etEndOfFile);
  opened := NOT (all OR err);
  IF NOT opened THEN
    fsClose(f); DEALLOCATE(bufOrig, bSize)
  END; (* IF *)
END ImportScreen;



PROCEDURE ImportAllScreens(VAR err : BOOLEAN);
VAR
  all,
  res : BOOLEAN;
  kw  : Keyword;
BEGIN
  LOOP
    ImportScreen(all, err);
    IF (all AND (NOT (terminated OR charEntered))) OR err OR UserBreak() THEN
      ClearPages(FALSE); EXIT
    ELSE
      (* Test if keywords or dates entered *)
      ResetList; res := NextKeyword(kw);
      IF kw[0] = 0C THEN
        ResetList; res := NextDate(kw);
        IF kw[0] = 0C THEN
          (* Nothing entered, enter default *)
          AssignAOC(kw, '[DEFAULT]'); AddKeyword(kw);
        END; (* IF *)
      END; (* IF *)
      SaveScreen(TRUE); IF fsResult() # etOK THEN EXIT END;
      ClearPages(FALSE);
      IF all THEN EXIT END;
    END; (* IF *)
  END; (* LOOP *)
END ImportAllScreens;


BEGIN (* KDImport *)
  opened := FALSE;
END KDImport.
