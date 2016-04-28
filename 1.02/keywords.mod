(*****************************************************************************
*
* Name     : Keywords
*
* LastEdit : 28/04/87
* Project  : KeyDex system
* Purpose  : Maintains local keyword list and conducts search procedure
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
* 04/04/87 : PrevKeyword and NextKeyword cleaned up.
*            Bugs fixed in "SaveScreen" and "AddKeyword".
* 11/04/87 : Implemented "EnteredTemplateKW".
* 18/04/87 : Partially fixed bug with 'templateloaded'.
* 28/04/87 : Adjusted "OpenFile/CreateFile/CloseFile" to new definition.
* 03/05/87 : 32-bit record numbers.
* 23/05/87 : Modified "InitScreen".
* 06/06/87 : dir and backup are dynamic.
*            Fixed bug in "Cleanup". Deleted keywords are invisible but
*            still use space in the array.
* 13/06/87 : $keyword -> TMP on. No $keyword -> TMP off prior to saving.
* 23/06/87 : Changed "Cleanup" according to DEF module.
* 18/07/87 : Some error handling in "LoadScreen".
* 29/07/87 : Changes in "SaveScreen".
* 31/07/87 : Bug in "DelScreen" fixed (delete date from empty index).
* 10/08/87 : Test for double keywords in "SaveScreen".
* 15/08/87 : Implemented automatic "[ALL]" adding.
* 16/08/87 : Bugs in "Cleanup" fixed.
* 10/09/87 : Bug fix in "RealCleanup".
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

IMPLEMENTATION MODULE Keywords;

FROM LongCardinals IMPORT
  LONGCARD, equal, zero;

FROM KDStorage IMPORT
  ALLOCATE, DEALLOCATE;

FROM String IMPORT
  Assign, Clear, Compare, UpperCase, Delete, AssignAOC;

FROM BTrees IMPORT
  LookupKeyword, DeleteKeyword, LookupDate, DeleteDate, ResetTemplateEntered;

FROM TimeDate IMPORT
  GetTime, Time;

FROM KDConfig IMPORT
  DateToStr, Keyword;

FROM KDScreen IMPORT
  DispRecordKey, SetStatus, Indicator, SetIndicator;

FROM KDEdit IMPORT
  template, saved;

FROM KWParser IMPORT
  DeleteScr, Parse, DisposeList, FirstScr, LastScr, NextScr, PrevScr,
  CurrentScr;

FROM FileOp IMPORT
  FileType, FlushFile, OpResult, fileLoaded;
  
FROM CopyProtect IMPORT
  EvalRelease, ScreenLimit;
  
FROM Menus IMPORT
  MessageBox;
  
 
IMPORT
  FileOp,
  KDEdit,
  KWParser,
  Compress;


CONST
  N = 50;   (* Number of keywords/dates per screen *)

VAR
  current,
  currentDt       : CARDINAL;
  max, maxBK      : ARRAY KwIndexType OF CARDINAL;
  defaultKW       : ARRAY [0..5] OF CHAR;
 
  templateloaded  : BOOLEAN;
  pos             : LONGCARD;  (* position of current screen *)

  dir,backup :
    POINTER TO ARRAY [1..N] OF 
      RECORD 
        kw     : Keyword;
        mark,
        erased : BOOLEAN;
      END; (* RECORD *)

  datedir,datebackup :
    POINTER TO ARRAY [1..N] OF
      RECORD
        kw     : CARDINAL;
        mark,
        erased : BOOLEAN;
      END; (* RECORD *)



PROCEDURE IndexEmpty(f:KwIndexType) : BOOLEAN;
BEGIN
  RETURN FileOp.empty[f];
END IndexEmpty;


PROCEDURE FileLoaded() : BOOLEAN;
BEGIN
  RETURN fileLoaded;
END FileLoaded;



PROCEDURE OpenFile( VAR db,ix : ARRAY OF CHAR;
                    VAR res   : ErrorType );
BEGIN
  SetIndicator(wait); loaded := FALSE; templateloaded := FALSE;
  FileOp.OpenFile(db,ix,res); DisposeList;
  ResetTemplateEntered;
END OpenFile;


PROCEDURE CreateFile( VAR db,ix : ARRAY OF CHAR;
                      VAR res   : ErrorType);
BEGIN
  SetIndicator(wait); loaded := FALSE; templateloaded := FALSE;
  FileOp.CreateFile(db,ix,res); DisposeList;
  ResetTemplateEntered;
END CreateFile;


PROCEDURE CloseFile(VAR res:ErrorType);
BEGIN 
  SetIndicator(wait); FileOp.CloseFile(res);
  ResetTemplateEntered;
END CloseFile;



PROCEDURE UpdateFuelLevel(f:KwIndexType);
BEGIN
  empty[f] := (max[f]=0); full[f] := (max[f]=N);
END UpdateFuelLevel;


PROCEDURE ClearList;
BEGIN 
  loaded := FALSE; 
  max[KeywordIndex] := 0; max[DateIndex] := 0; 
  UpdateFuelLevel(KeywordIndex); UpdateFuelLevel(DateIndex);
  templateloaded := FALSE;
  (* FOR i := 1 TO max DO dir^[i].mark := FALSE END; --- deleted 04/04/87 *)
END ClearList;


PROCEDURE BackupList;
BEGIN
  NEW(backup); NEW(datebackup);
  backup^ := dir^; datebackup^ := datedir^;
  maxBK := max;
END BackupList;


PROCEDURE RestoreList;
VAR
  b : BOOLEAN;
BEGIN
  dir^ := backup^; datedir^ := datebackup^;
  max := maxBK;
  UpdateFuelLevel(KeywordIndex); UpdateFuelLevel(DateIndex); 
  (** Added 06/06/87 **) Cleanup(FALSE, b);
  DISPOSE(backup); DISPOSE(datebackup);
END RestoreList;



PROCEDURE ResetList;
BEGIN
  current := 0; currentDt := 0;
END ResetList;


PROCEDURE ValidKeywordChar(c:CHAR) : BOOLEAN;
BEGIN
  RETURN Compress.ValidChar(c);
END ValidKeywordChar;


 
PROCEDURE EnteredTemplateKW() : BOOLEAN;
VAR
  i : CARDINAL;
BEGIN
  FOR i := 1 TO max[KeywordIndex] DO
    WITH dir^[i] DO
      IF (NOT erased) & (kw[1]='$') THEN RETURN TRUE END;
    END; (* WITH *)
  END; (* FOR *)
  RETURN FALSE;
END EnteredTemplateKW;



PROCEDURE EnterKeywordList(VAR k:Keyword);
(* Called to load the keyword list *)
BEGIN
  INC(max[KeywordIndex]);
  WITH dir^[max[KeywordIndex]] DO kw := k; mark := FALSE; erased := FALSE END;
END EnterKeywordList;


PROCEDURE EnterDateList(k:CARDINAL);
(* Called to load the keyword list *)
BEGIN
  INC(max[DateIndex]); 
  WITH datedir^[max[DateIndex]] DO kw := k; mark := FALSE; erased := FALSE END;
END EnterDateList;



PROCEDURE AddKeyword(VAR key:ARRAY OF CHAR);
VAR
  i,j,m : CARDINAL;
  c     : INTEGER;
BEGIN
  (* Insert into list *)
  IF (key[1] = '$') & ((key[0] = 1C) OR (key[2] = '$')) THEN RETURN END;
  UpperCase(key);
  IF max[KeywordIndex]=0 THEN
    max[KeywordIndex] := 1;
    WITH dir^[1] DO Assign(kw,key); mark := FALSE; erased := FALSE END;
    saved := FALSE;
  ELSIF max[KeywordIndex] < N THEN
    i := 1; j := max[KeywordIndex];
    REPEAT 
      m := (i+j) DIV 2; c := Compare(key,dir^[m].kw);
      IF c<=0 THEN j := m-1 END; IF c>=0 THEN i := m+1 END;
    UNTIL i>j;
    IF i<=j+1 THEN
      INC(j); FOR i := max[KeywordIndex] TO j BY -1 DO dir^[i+1] := dir^[i] END; 
      WITH dir^[j] DO Assign(kw,key); erased := FALSE; mark := FALSE END;
      INC(max[KeywordIndex]); saved := FALSE;
    ELSE
      (** added 04/04/87 **)
      WITH dir^[m] DO erased := FALSE; mark := FALSE END;
    END;
  END;
  UpdateFuelLevel(KeywordIndex); SetStatus;
END AddKeyword;



PROCEDURE AddDate(key:CARDINAL);
VAR
  i,j,m : CARDINAL;
BEGIN
  (* Insert into list *)
  IF max[DateIndex]=0 THEN
    max[DateIndex] := 1;
    WITH datedir^[1] DO kw := key; mark := FALSE; erased := FALSE END;
    saved := FALSE;
  ELSIF max[DateIndex] < N THEN
    i := 1; j := max[DateIndex];
    REPEAT 
      m := (i+j) DIV 2;
      IF key <= datedir^[m].kw THEN j := m-1 END;
      IF key >= datedir^[m].kw THEN i := m+1 END;
    UNTIL i>j;
    IF i<=j+1 THEN
      INC(j); 
      FOR i := max[DateIndex] TO j BY -1 DO datedir^[i+1] := datedir^[i] END; 
      WITH datedir^[j] DO kw := key; erased := FALSE; mark := FALSE END;
      INC(max[DateIndex]); saved := FALSE;
    ELSE
      (** added 04/04/87 **)
      WITH datedir^[m] DO erased := FALSE; mark := FALSE END;
    END;
  END;
  UpdateFuelLevel(DateIndex); SetStatus;
END AddDate;



PROCEDURE MarkKeyword;
BEGIN
  WITH dir^[current] DO mark := NOT mark END;
END MarkKeyword;


PROCEDURE MarkDate;
BEGIN
  WITH datedir^[currentDt] DO mark := NOT mark END;
END MarkDate;



PROCEDURE NextKeyword(VAR fn:ARRAY OF CHAR) : BOOLEAN;
BEGIN
  WHILE (current<max[KeywordIndex]) DO
    INC(current);  
    WITH dir^[current] DO
      IF NOT erased THEN Assign(fn,kw); RETURN(mark) END;
    END;
  END;
  Clear(fn); RETURN FALSE
END NextKeyword;



PROCEDURE NextDate(VAR fn:ARRAY OF CHAR) : BOOLEAN;
BEGIN
  WHILE (currentDt<max[DateIndex]) DO
    INC(currentDt);  
    WITH datedir^[currentDt] DO
      IF NOT erased THEN DateToStr(kw,fn); RETURN(mark) END;
    END;
  END;
  Clear(fn); RETURN FALSE
END NextDate;



PROCEDURE NextBinaryDate(VAR fn:CARDINAL);
BEGIN
  WHILE (currentDt<max[DateIndex]) DO
    INC(currentDt);  
    WITH datedir^[currentDt] DO
      IF NOT erased THEN fn := kw; RETURN END;
    END;
  END;
  fn := 0;
END NextBinaryDate;



PROCEDURE Cleanup(er : BOOLEAN; VAR doneSomething : BOOLEAN);
(* Pretends to erase all marked entries in keyword list *)
VAR
  i,max1 : CARDINAL;
BEGIN
  (* Keyword index *)
  doneSomething := FALSE; max1 := max[KeywordIndex];
  FOR i := 1 TO max[KeywordIndex] DO 
    WITH dir^[i] DO 
      IF mark & er THEN
        erased := TRUE; doneSomething := TRUE; saved := FALSE; DEC(max1)
      ELSIF erased THEN
        (** 16/08/87 **)
        DEC(max1);
      END; (* IF *)
      mark := FALSE;
    END; (* WITH *)
  END; (* FOR *)
  empty[KeywordIndex] := (max1=0);
  (* Date index *)
  max1 := max[DateIndex];
  FOR i := 1 TO max[DateIndex] DO 
    WITH datedir^[i] DO 
      IF mark & er THEN 
        doneSomething := TRUE; erased := TRUE; saved := FALSE; DEC(max1)
      ELSIF erased THEN
        (** 16/08/87 **)
        DEC(max1);
      END; (* IF *)
      mark := FALSE;
    END; (* WITH *)
  END; (* FOR *)
  empty[DateIndex] := (max1=0);
  SetStatus;
END Cleanup;



PROCEDURE PrevKeyword(VAR fn:ARRAY OF CHAR) : BOOLEAN;
BEGIN
  WHILE (current>1) DO
    DEC(current); 
    WITH dir^[current] DO
      IF NOT erased THEN Assign(fn,kw); RETURN(mark) END;
    END;
  END;
  Clear(fn); RETURN FALSE 
END PrevKeyword;


PROCEDURE PrevDate(VAR fn:ARRAY OF CHAR) : BOOLEAN;
BEGIN
  WHILE (currentDt>1) DO
    DEC(currentDt); 
    WITH datedir^[currentDt] DO
      IF NOT erased THEN DateToStr(kw,fn); RETURN(mark) END;
    END;
  END;
  Clear(fn); RETURN FALSE 
END PrevDate;



PROCEDURE RealCleanup;
(* Removes all erased keywords from list *)
VAR
  i, j, m : CARDINAL;
BEGIN
  i := 1; m := max[KeywordIndex];
  LOOP
    IF i > m THEN EXIT END;
    IF dir^[i].erased THEN
      FOR j := i + 1 TO m DO
        dir^[j-1] := dir^[j]
      END; (* FOR *)
      DEC(m);
    ELSE
      (** 10/09/87 **)
      INC(i);
    END; (* IF *)
  END; (* LOOP *)
  max[KeywordIndex] := m;
  UpdateFuelLevel(KeywordIndex);
  i := 1; m := max[DateIndex];
  LOOP
    IF i > m THEN EXIT END;
    IF datedir^[i].erased THEN
      FOR j := i + 1 TO m DO
        datedir^[j-1] := datedir^[j]
      END; (* FOR *)
      DEC(m);
    ELSE
      (** 10/09/87 **)
      INC(i);
    END; (* IF *)
  END; (* LOOP *)
  max[DateIndex] := m;
  UpdateFuelLevel(DateIndex);
END RealCleanup;



PROCEDURE SaveScreen(new:BOOLEAN);
VAR
  t    : Time;
  pos1 : LONGCARD;
  i,m,j: CARDINAL;
  c    : INTEGER;
  f    : BOOLEAN;
BEGIN
  SetIndicator(wait); 
  GetTime(t); recdate := t.day;
  (** 21/08/87 : new **) template := EnteredTemplateKW();
  IF NOT new THEN 
    DelScreen; (* Delete pos from node blocks *)
    IF NOT OpResult() THEN template := FALSE; RETURN END;
    templateloaded := FALSE;
  ELSIF NOT templateloaded THEN
    RealCleanup;
  END; (* IF *)
  IF templateloaded THEN
    (* Clear all $ keywords - 29/07/87*)
    template := FALSE; 
    i := 1; m := max[KeywordIndex];
    LOOP
      IF i > m THEN EXIT END;
      WITH dir^[i] DO
        IF (kw[1] = '$') THEN
          IF NOT erased THEN 
            Delete(kw,1,1);
            (* Test if created keyword is already in list *)
            j := i + 1;
            LOOP
              IF j > m THEN
                EXIT
              ELSIF NOT dir^[j].erased THEN
                c := Compare(kw, dir^[j].kw);
                IF (c <= 0) THEN
                  erased := (c = 0); EXIT;
                END; (* IF *)
              END; (* IF *)
              INC(j);
            END; (* LOOP *)
          END; (* IF *)
        ELSE
          EXIT
        END; (* IF *)
      END; (* WITH *)
      INC(i);
    END; (* LOOP *)
    RealCleanup;
  END; (* IF *)
  IF NOT full[KeywordIndex] THEN
    AddKeyword(defaultKW);
  ELSE
    (* force entry *)
    i := max[KeywordIndex];
    WHILE (i >= 1) & (Compare(dir^[i].kw, defaultKW) > 0) DO
      DEC(i)
    END; (* WHILE *)
    IF (i = 0) THEN INC(i) END;
    Assign(dir^[i].kw, defaultKW);
  END; (* IF *)
  ResetList; FileOp.SaveScreen(pos1,recdate,recno,NextKeyword,NextBinaryDate);
  IF EvalRelease() & (recno.low > ScreenLimit) THEN
    MessageBox("Please order the full release to store more screens", FALSE);
    RETURN;
  END; (* IF *)
  IF equal(recno, zero) THEN template := FALSE; RETURN END;
  (* Enter keywords *)
  FOR i := 1 TO max[KeywordIndex] DO 
    LookupKeyword(dir^[i].kw,pos1,TRUE,f);
    IF NOT OpResult() THEN template := FALSE; recno := zero; RETURN END;
  END; (* FOR *)
  FlushFile(KeywordIndex);
  IF NOT OpResult() THEN template := FALSE; recno := zero; RETURN END;
  (* Enter dates *)
  FOR i := 1 TO max[DateIndex] DO 
    LookupDate(datedir^[i].kw,pos1,TRUE,f);
    IF NOT OpResult() THEN template := FALSE; recno := zero; RETURN END;
  END; (* FOR *)
  FlushFile(DateIndex);
  IF NOT OpResult() THEN template := FALSE; recno := zero; RETURN END;
  DisposeList;
  IF templateloaded THEN
    (* reload template *)
    LoadScr(pos); IF equal(recno, zero) THEN RETURN END;
  ELSE
    pos := pos1; 
  END; (* IF *)
  (* This is only executed if template loaded successfully or no template
     at all. *)
  KDEdit.new := template;
  templateloaded := template; loaded := TRUE;
  DispRecordKey(recdate, recno); saved := TRUE; SetStatus;
  (* TreeWalk; *)
END SaveScreen;



PROCEDURE DelScreen;
(* Delete current screen list from index file *)
VAR
  i : CARDINAL;
BEGIN
  FileOp.DeleteScreen;
  IF NOT OpResult() THEN recno := zero; RETURN END;
  (* Delete all list entries from the node blocks *)
  i := max[KeywordIndex];
  (** 30/07/87 **)
  WHILE (i > 0) & (NOT FileOp.empty[KeywordIndex]) DO
    DeleteKeyword(dir^[i].kw,pos);
    IF NOT OpResult() THEN recno := zero; RETURN END;
    DEC(i);
  END; (* WHILE *)
  FlushFile(KeywordIndex);
  IF NOT OpResult() THEN recno := zero; RETURN END;
  i := max[DateIndex];
  (** 30/07/87 **)
  WHILE (i > 0) & (NOT FileOp.empty[DateIndex]) DO
    DeleteDate(datedir^[i].kw,pos);
    IF NOT OpResult() THEN recno := zero; RETURN END;
    DEC(i);
  END;
  RealCleanup;
  FlushFile(DateIndex);
  IF NOT OpResult() THEN recno := zero; RETURN END;
  UpdateFuelLevel(KeywordIndex); UpdateFuelLevel(DateIndex);
  DeleteScr(pos);
  (* TreeWalk; *)
END DelScreen;


PROCEDURE DeleteScreen(physical:BOOLEAN);
BEGIN
  IF physical THEN
    SetIndicator(wait); DelScreen;
  ELSE
    DeleteScr(pos);
  END;
  loaded := FALSE; templateloaded := FALSE; SetStatus;
END DeleteScreen;



PROCEDURE InitScreen( VAR cr     : ARRAY OF CHAR; 
                      wordOnly,
                      ignoreCase,
                      prompt     : BOOLEAN;
                      VAR found,
                          foundOne,
                          scan,
                          res    : BOOLEAN);
BEGIN
  DisposeList; SetIndicator(wait);
  Parse(cr, wordOnly, ignoreCase, prompt, found, foundOne, scan, res);
  res := NOT res;
END InitScreen;



PROCEDURE ExistLink() : BOOLEAN;
(* True if a screen list for Keyword/Get exists *)
VAR
  t : LONGCARD;
BEGIN
  KWParser.ReturnTotal(t); RETURN NOT equal(t,zero);
END ExistLink;


PROCEDURE LoadScr(pos : LONGCARD);
VAR
  olddate : CARDINAL;
BEGIN
  ClearList;
  olddate := recdate;
  FileOp.LoadScreen(pos,recdate,recno,EnterKeywordList,EnterDateList);
  IF NOT equal(recno, zero) THEN
    templateloaded := template; 
    UpdateFuelLevel(KeywordIndex); UpdateFuelLevel(DateIndex);
    saved := TRUE; SetStatus;
    loaded := TRUE; DispRecordKey(recdate, recno);
  ELSE
    recdate := olddate;
  END; (* IF *)
END LoadScr;


PROCEDURE LoadScreen(p:LONGCARD);
BEGIN
  LoadScr(p); IF NOT equal(recno, zero) THEN pos := p END;
END LoadScreen;


PROCEDURE DisposeScreenList;
BEGIN
  DisposeList;
END DisposeScreenList;



PROCEDURE NextScreen;
BEGIN NextScr(pos); LoadScr(pos) END NextScreen;


PROCEDURE PrevScreen;
BEGIN PrevScr(pos); LoadScr(pos) END PrevScreen;


PROCEDURE FirstScreen;
BEGIN FirstScr(pos); LoadScr(pos) END FirstScreen;


PROCEDURE LastScreen;
BEGIN LastScr(pos); LoadScr(pos) END LastScreen;


PROCEDURE CurrentScreen;
BEGIN CurrentScr(pos); LoadScr(pos) END CurrentScreen;


PROCEDURE ReturnTotal(VAR t:LONGCARD);
BEGIN
  KWParser.ReturnTotal(t);
END ReturnTotal;


BEGIN
  (* loaded=false done by kdedit *)
  NEW(dir); NEW(datedir);
  templateloaded := FALSE; max[KeywordIndex] := 0; max[DateIndex] := 0;
  UpdateFuelLevel(KeywordIndex); UpdateFuelLevel(DateIndex);
  AssignAOC(defaultKW, '[ALL]');
END Keywords.
