(*****************************************************************************
*
* Name     : FileStuff
*
* LastEdit : 03/05/87
* Project  : KeyDex system
* Purpose  : Implementation of the File Menu.
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
* 03/05/87 : Implemented changes for '.KD3' file.
* 03/06/87 : Adjusted procedures to new directory naming convention.
* 05/06/87 : Dynamic menus.
* 06/06/87 : String constants.
*            File directory into dynamic storage.
* 08/06/87 : "Erase" now only erases KeyDex files.
*            (It was possible to erase current files; see "SetStuff").
* 23/06/87 : Warning message if exited Erase menu w/o having marked anything.
* 13/07/87 : Changed to new file system. 
*            Bugs fixed in program execute section.
* 30/07/87 : Import menu.
* 10/08/87 : Fixed bugs in Import and Program.
* 20/08/87 : Bug fixed in 'BuildList2'.
* 24/09/87 : [RETURN] in Erase goes back to scroll box instead of menu.
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

IMPLEMENTATION MODULE FileStuff;


FROM LongCardinals IMPORT
  zero;

FROM KDStorage IMPORT
  ALLOCATE;

FROM KDConfig IMPORT
  Program, DataPath, IndexPath, FileNameChar;

FROM KDScreen IMPORT
  DispFilename, DispRecordKey, SetIndicator, Indicator, InitScreen;

FROM KDEdit IMPORT
  RedrawCurrentPage, ClearPages, saved;

FROM Menus IMPORT
  MenuBarPtr, PullDownMenu, ScrollBox, Operation, MessageBox, 
  DialogBox, CreateMenu, RemovePDMenu, WarningBox;

FROM String IMPORT
  Clear, AssignStr, Assign, AppendAOC, AssignAOC, Append, Compare, UpperCase,
  Delete, AppendCh;

FROM KDExec IMPORT
  ProgExec;

FROM FileOp IMPORT
  dbname, ixname, fileLoaded;

FROM MsFileSystem IMPORT
  fsDelete, fsDirQuery, fsResult, fsErrorType, fsGetCommandLine;

FROM KDCritErr IMPORT
  InvokeHandler, DispatchHandler;

FROM KDImport IMPORT
  ImportScreen, ImportAllScreens, InitImportFile;

FROM HelpSystem IMPORT
  SetHelpContext;
  
FROM HelpContext IMPORT
  ModuleIndex, ScreenNumber;

IMPORT 
  Keywords;


CONST
  N     = 150;   (* No. of filenames in directory list *)
  str01 = 'Could not find file. Try again.';

TYPE
  fname = ARRAY [0..12] OF CHAR;
  sname = ARRAY [0..8] OF CHAR;
  pname = ARRAY [0..64] OF CHAR;
  item  = RECORD kw:fname; mark:BOOLEAN; END;
 
VAR
  mb1, mb2,
  mb3        : MenuBarPtr;
  dirptr,max : CARDINAL;
  dir        : POINTER TO ARRAY [1..N] OF item; 
  spec       : pname;
  current    : sname;
  fileSpecified,
  startup    : BOOLEAN;


PROCEDURE CleanFileName(VAR s,s1:ARRAY OF CHAR);
(* Removes directory and drive designators from ASCIIZ-string s and returns
   modified string in s1 in PASCAL format *)
VAR
  i,j,m : CARDINAL;
BEGIN
  i := 0; j := 0; m := HIGH(s);
  WHILE (s[i] # 0C) AND (i <= m) DO
    IF s[i] # '.' THEN
      INC(j); s1[j] := s[i];
    ELSE
      INC(j); WHILE j < 9 DO s1[j] := ' '; INC(j) END; s1[9] := '.' 
    END; (* IF *)
    INC(i);
  END; (* WHILE *)
  s1[0] := CHR(j); 
END CleanFileName;



PROCEDURE BuildList(VAR s:ARRAY OF CHAR) : BOOLEAN;
(* Gathers filenames and constructs an alphabetic list *)
VAR
  s1 : pname;
  i  : CARDINAL;
BEGIN
  CleanFileName(s,s1);
  i := max; INC(max);
  WHILE (i > 0) AND (Compare(s1,dir^[i].kw) < 0) DO
    dir^[i+1] := dir^[i]; DEC(i)
  END; (* WHILE *)
  WITH dir^[i+1] DO Assign(kw,s1); mark := FALSE END;
  RETURN (max < N);
END BuildList;

    

PROCEDURE BuildList2(VAR s : ARRAY OF CHAR) : BOOLEAN;
(* This modified procedure is used by the load function *)
VAR
  s1 : pname;
  i  : CARDINAL;
  c  : INTEGER;
BEGIN
  CleanFileName(s,s1); i := 0; DEC(s1[0],4);
  REPEAT
    INC(i); c := Compare(s1,dir^[i].kw);
  UNTIL (c<=0) OR (i=max);
  (** 20/08/87 **)
  IF c = 0 THEN dir^[i].mark := FALSE END;
  RETURN TRUE;
END BuildList2;



(* Procedures to maneuver in the directory list *)

PROCEDURE Reset;
BEGIN dirptr := 0 END Reset;


PROCEDURE dummy(n:CARDINAL; VAR fn:ARRAY OF CHAR) : BOOLEAN;
BEGIN Clear(fn); RETURN FALSE END dummy;


PROCEDURE del;
BEGIN WITH dir^[dirptr] DO mark := NOT mark END END del;


PROCEDURE Prev(VAR fn:ARRAY OF CHAR) : BOOLEAN;
BEGIN 
  IF dirptr>1 THEN 
    DEC(dirptr); WITH dir^[dirptr] DO Assign(fn,kw); RETURN mark END; 
  ELSE Clear(fn); RETURN FALSE END;
END Prev;


PROCEDURE Next(VAR fn:ARRAY OF CHAR) : BOOLEAN;
BEGIN
  IF dirptr<max THEN 
    INC(dirptr); WITH dir^[dirptr] DO Assign(fn,kw); RETURN mark END;
  ELSE Clear(fn); RETURN FALSE END;
END Next;




PROCEDURE ValidChar(ch:CHAR) : BOOLEAN;
(* File names with path or wildcards *)
BEGIN 
  RETURN (FileNameChar(ch)=CAP(ch)) OR (ch='*') OR (ch='?');
END ValidChar;


PROCEDURE ValidChar2(ch:CHAR) : BOOLEAN;
(* File names without extensions or path names *)
BEGIN
  RETURN (FileNameChar(ch)=CAP(ch)) AND (ch#':') AND (ch#'\') AND (ch#'.');
END ValidChar2;



PROCEDURE DiskError(status:Keywords.ErrorType);
BEGIN
  CASE status OF
     Keywords.error :
       MessageBox('Disk error - operation not completed.', TRUE);
    |Keywords.diskfull :
       MessageBox('Disk full - operation not completed.', TRUE);
    |Keywords.fileexists :
       MessageBox('Required file names are already in use.', TRUE);
    |Keywords.consistencyerr :
       MessageBox('Consistency error in database - not loaded.', TRUE);
    |Keywords.illegalversion :
       MessageBox('Illegal version of database - not loaded.', TRUE);
  ELSE
    MessageBox('Could not complete operation.', TRUE);
  END; (* CASE *)
END DiskError;



PROCEDURE Create;
(* Create a new database *)
VAR
  db,ix,fn : pname;
  new      : sname;
  res,
  newtype  : BOOLEAN;
  status   : Keywords.ErrorType;
BEGIN
  IF (NOT saved) & Keywords.FileLoaded() THEN
    WarningBox(res); IF NOT res THEN quit := TRUE; RETURN END;
  END; (* IF *)
  Clear(new);
  DialogBox('Enter name of new database (max. 8 characters):',new,dummy,
            ValidChar2,res,newtype);
  res := (new[0] # 0C);
  IF res THEN
    (* Convert to full file spec *)
    UpperCase(new); WHILE new[0] < 10C DO AppendCh(new, ' ') END;
    Assign(db,DataPath); Append(db,new); AppendAOC(db,'.KD1');
    Assign(ix,IndexPath); Append(ix,new); AppendAOC(ix,'.KD2');
    SetIndicator(wait); 
    InvokeHandler(FALSE);
    Keywords.CreateFile(db,ix,status);
    DispatchHandler;
    IF status=Keywords.ok THEN
      Assign(current,new);
    ELSE
      IF (status = Keywords.diskfull) OR (status = Keywords.error) THEN
        (* Remove traces of unsuccessful attempt *)
        AssignStr(fn, db); fsDelete(fn);
        ix[ORD(ix[0])] := '2'; AssignStr(fn, ix); fsDelete(fn);
        ix[ORD(ix[0])] := '3'; AssignStr(fn, ix); fsDelete(fn);
      END; (* IF *)
      DiskError(status); Clear(current);
    END;
    ClearPages(FALSE); DispFilename(current); quit := TRUE;
  END;
END Create;




PROCEDURE FileBox(op:Operation);
(* Load or erase existing file. Gets file from command line if startup=T. *)

  PROCEDURE Cleanup;
  VAR
    i,j : CARDINAL;
  BEGIN
    i := max;
    WHILE i >= 1 DO
      IF dir^[i].mark THEN
        FOR j := i TO (max-1) DO dir^[j] := dir^[j+1] END; DEC(max);
      END;
      DEC(i);
    END;
  END Cleanup;

  PROCEDURE FbError;
  BEGIN
    IF cmdline THEN
      MessageBox(str04, TRUE)
    ELSIF op=Select THEN 
      MessageBox(str01, FALSE) 
    ELSE 
      MessageBox(str02,FALSE) 
    END; (* IF *)
  END FbError;

CONST
  str01 = 'No file found. You first must create a file.';
  str02 = 'No KeyDex file found.';
  str03 = 'You must mark the file with [SPACE] to delete it.';
  str04 = 'Could not find file specified on command line.';
VAR
  s,s1 : pname;
  fn   : fname;
  i,
  count   : CARDINAL;
  res,
  error,
  errorD,
  erased,
  cmdline : BOOLEAN;
  status  : Keywords.ErrorType;
BEGIN
  IF (op = Select) & (NOT saved) & Keywords.FileLoaded() THEN
    WarningBox(res); IF NOT res THEN quit := TRUE; RETURN END;
  END; (* IF *)
  SetIndicator(wait);
  (* Search for all data files *)
  max := 0; Assign(s,DataPath);
  cmdline := startup & (op = Select); startup := FALSE;
  IF cmdline THEN
    fsGetCommandLine(s1);
    CleanCommandLine(s1);
    IF s1[0] # 0C THEN
      Append(s, s1);
    ELSE
      AppendCh(s, '*'); cmdline := FALSE;
    END; (* IF *)
  ELSE
    AppendCh(s, '*'); cmdline := FALSE;
  END; (* IF *)
  AppendAOC(s, '.KD1'); AssignStr(s1,s);
  InvokeHandler(FALSE);
  fsDirQuery(s1, BuildList);
  DispatchHandler;
  IF max = 0 THEN
    FbError;
  ELSE
    (* Remove extensions and set marks *)
    FOR i := 1 TO max DO WITH dir^[i] DO DEC(kw[0],4); mark := TRUE END END;
    Assign(s,IndexPath); AppendAOC(s,'*.KD2'); AssignStr(s1,s);
    InvokeHandler(FALSE);
    fsDirQuery(s1, BuildList2);
    DispatchHandler;
    Cleanup;
    (* Search for .KD3 files in index directory *)
    FOR i := 1 TO max DO WITH dir^[i] DO mark := TRUE END END;
    s[ORD(s[0])] := '3'; AssignStr(s1,s);
    InvokeHandler(FALSE);
    fsDirQuery(s1, BuildList2); 
    DispatchHandler;
    Cleanup;
    IF max=0 THEN
      FbError;
    ELSE
      LOOP
        IF (max > 1) OR (NOT cmdline) THEN
          ScrollBox(18,4,8,Reset,Prev,Next,del,op,fn);
        ELSE 
          (* max = 1 AND cmdline *)
          Assign(fn, dir^[1].kw);
        END; (* IF *)
        IF (op=Select) & (fn[0]#0C) THEN
          (* Load selected file *)
          Assign(s,DataPath); Assign(s1,IndexPath);
          Append(s,fn); Append(s1,fn);
          AppendAOC(s,'.KD1'); AppendAOC(s1,'.KD2');
          SetIndicator(wait); 
          InvokeHandler(FALSE);
          Keywords.OpenFile(s,s1,status);
          DispatchHandler;
          IF status=Keywords.ok THEN
            Assign(current,fn); quit := TRUE;
          ELSE
            DiskError(status); Clear(current);
          END;
          ClearPages(FALSE); DispFilename(current);
          EXIT;
        ELSIF (op=Mark) & (fn[0] # 0C) THEN
          (* Delete all marked files *)
          error := FALSE; erased := FALSE; SetIndicator(wait);
          IF Keywords.FileLoaded() THEN
            ixname[ORD(ixname[0])] := '2'
          ELSE
            Clear(dbname); Clear(ixname)
          END; (* IF *)
          InvokeHandler(FALSE); errorD := FALSE; count := 0;
          FOR i := 1 TO max DO
            WITH dir^[i] DO
              IF mark THEN
                INC(count);
                Assign(s,DataPath); Append(s,kw); AppendAOC(s,'.KD1');
                Assign(s1,IndexPath); Append(s1,kw); AppendAOC(s1,'.KD2');
                IF (Compare(s,dbname) # 0) & (Compare(s1,ixname) # 0) THEN
                  AssignStr(s,s); fsDelete(s);
                  errorD := errorD OR (fsResult() # etOK);
                  AssignStr(s,s1); fsDelete(s);
                  errorD := errorD OR (fsResult() # etOK);
                  s1[ORD(s1[0])] := '3'; AssignStr(s,s1); 
                  fsDelete(s);
                  errorD := errorD OR (fsResult() # etOK);
                  erased := TRUE;
                ELSIF fileLoaded THEN
                  error := TRUE;
                END; (* IF *)
              END; (* IF *)
            END; (* WITH *)
          END; (* FOR *)
          DispatchHandler;
          IF error THEN
            IF (count = 1) THEN
              MessageBox('You attempted to erase current KeyDex file - not done.',TRUE);
            END; (* IF *)
            EXIT;
          ELSIF errorD THEN
            MessageBox('Could not erase all files.', FALSE);
            EXIT;
          ELSIF NOT erased THEN
            (** 23/06/87 **)
            MessageBox(str03,FALSE);
          ELSE
            EXIT;
          END; (* IF *)
        ELSE
          EXIT;
        END; (* IF *)
      END; (* LOOP *)
    END; (* IF *)
  END; (* IF *)
END FileBox;




PROCEDURE ContainsWildcard(VAR s : ARRAY OF CHAR) : BOOLEAN;
(* Returns TRUE if s contains ? or * *)
VAR
  i, m  : CARDINAL;
  wcard : BOOLEAN;
BEGIN
  i := 1; m := ORD(s[0]); wcard := FALSE;
  LOOP 
    IF (i > m) OR wcard THEN EXIT END;
    wcard := wcard OR (s[i] = '*') OR (s[i] = '?');
    INC(i);
  END; (* LOOP *)
  RETURN wcard;
END ContainsWildcard;



PROCEDURE HelpProc( sel : CARDINAL );
(* Help procedure for 'File' menu *)
BEGIN
  SetHelpContext(ScreenNumber(filestuff, sel));
END HelpProc;


      
PROCEDURE FileMenu(rt:CARDINAL);
VAR     
  sel,i : CARDINAL;
  s,s1 : pname;
  done,newtype, wcard : BOOLEAN;
BEGIN
  IF rt#0 THEN 
    sel := rt+100 
  ELSIF startup THEN
    sel := 101;
  ELSE
    sel := 0 
  END; (* IF *)
  quit := FALSE; motion := 0;
  REPEAT
    WITH mb2^ DO 
      IF Keywords.FileLoaded() THEN INCL(on,3) ELSE EXCL(on,3) END;
    END; (* WITH *)
    PullDownMenu(0, 2, mb2^, sel, HelpProc);
    CASE sel OF
        0 : RETURN;
     |  1 : FileBox(Select);
     |  2 : Create;
     |  3 : IF (3 IN mb2^.on) THEN ImportMenu(rt = 3) END;
     |  4 : FileBox(Mark);

     |  5 : Assign(spec,Program);
            REPEAT
              DialogBox("Enter name of file to run. Wildcards * and ? are allowed.",
                        spec,dummy,ValidChar,done,newtype);
              done := done AND (spec[0]<>0C);
              IF done THEN
                SetIndicator(wait);
                UpperCase(spec);
                AssignStr(s,spec); max := 0;
                InvokeHandler(FALSE);
                fsDirQuery(s, BuildList);
                DispatchHandler;
                IF max # 0 THEN
                  wcard := ContainsWildcard(spec);
                  IF wcard THEN
                    InvokeHandler(FALSE);
                    ScrollBox(18,5,12,Reset,Prev,Next,del,Select,s1);
                    DispatchHandler;
                    done:=(s1[0]<>0C);
                  ELSE
                    done := TRUE;
                  END; (* IF *)
                  IF done THEN 
                    (* Construct a full filename of spec and s1 *)
                    IF wcard THEN
                      i := ORD(spec[0]); 
                      WHILE (spec[i]<>'\') AND (spec[i]<>':') AND (i>0) DO DEC(i) END;
                      spec[0] := CHR(i); Append(spec,s1); 
                    END; (* IF *)
                    ProgExec(spec);
                    InitScreen; RedrawCurrentPage;
                    IF Keywords.FileLoaded() THEN
                      DispFilename(current); 
                      IF Keywords.loaded THEN
                        DispRecordKey(Keywords.recdate, Keywords.recno)
                      ELSE
                        DispRecordKey(0,zero)
                      END; (* IF *)
                    ELSE
                      DispRecordKey(0,zero)
                    END; (* IF *) 
                    quit := TRUE;
                  END;
                  done := NOT done;
                ELSE
                  MessageBox(str01, TRUE)
                END; (* IF *)
              END; (* IF *)
            UNTIL (NOT done);

     |  6 : AssignAOC(spec,'*.*');
            REPEAT
              DialogBox("Enter filename or directory specification. Wildcards * and ? allowed.",
                        spec,dummy,ValidChar,done,newtype);
              done := done AND (spec[0]<>0C);
              IF done THEN
                SetIndicator(wait);
                AssignStr(s,spec); max := 0; 
                InvokeHandler(FALSE);
                fsDirQuery(s, BuildList);
                DispatchHandler;
                IF max # 0 THEN
                  ScrollBox(18,5,12,Reset,Prev,Next,del,Scroll,s);
                  done:=(s[0]=0C);
                ELSE
                  MessageBox(str01, FALSE);
                END;
              END;
            UNTIL (NOT done);
     |  8 : quit := TRUE;
     |100 : motion := -1; RETURN
     |101 : motion := +1; RETURN
    END; (* CASE *)
  UNTIL quit;
END FileMenu;


PROCEDURE ImportMenu(exec : BOOLEAN);
CONST
  str02 = 'Could not open import file';
VAR
  sel,i  : CARDINAL;
  err,
  res,
  wcard  : BOOLEAN;
  s,spec : pname;
BEGIN
  IF exec & fileSpecified THEN
    sel := 101 
  ELSIF NOT fileSpecified THEN
    sel := 103
  ELSE
    sel := 0 
  END; (* IF *)
  LOOP
    WITH mb3^ DO 
      IF fileSpecified THEN on := on + {1,2} ELSE on := on - {1,2} END;
    END; (* WITH *)
    PullDownMenu(17, 4, mb3^, sel, HelpProc);
    IF sel IN {1,2} THEN
      RemovePDMenu; RemovePDMenu;
    END; (* IF *)
    err := FALSE;
    CASE sel OF
      0 : EXIT;

    | 1,2 :
          IF NOT saved THEN
            WarningBox(res); IF NOT res THEN quit := TRUE; EXIT END;
          END; (* IF *)
          InvokeHandler(FALSE); SetIndicator(wait);
          IF sel = 1 THEN
            ImportScreen(res, err); 
          ELSE
            ImportAllScreens(err); res := TRUE;
          END; (* IF *)
          DispatchHandler;

    | 3 : Clear(spec);
          REPEAT
            DialogBox('Enter file path and name...   Wildcards, such as *.* or TE?T.??? are valid.',
                       spec,dummy,ValidChar,res,err);
            res := res AND (spec[0]<>0C);
            IF res THEN
              SetIndicator(wait);
              AssignStr(s,spec); max := 0;
              InvokeHandler(FALSE);
              fsDirQuery(s, BuildList);
              DispatchHandler;
              IF max # 0 THEN
                wcard := ContainsWildcard(spec);
                IF wcard THEN
                  InvokeHandler(FALSE);
                  ScrollBox(18,5,12,Reset,Prev,Next,del,Select,s);
                  DispatchHandler;
                  res:=(s[0]<>0C);
                ELSE
                  res := TRUE;
                END; (* IF *)
                IF res THEN 
                  (* Construct a full filename of spec and s1 *)
                  IF wcard THEN
                    i := ORD(spec[0]); 
                    WHILE (spec[i]<>'\') AND (spec[i]<>':') AND (i>0) DO DEC(i) END;
                    spec[0] := CHR(i); Append(spec,s); 
                  END; (* IF *)
                  InvokeHandler(FALSE);
                  AssignStr(s, spec);
                  InitImportFile(s, fileSpecified);
                  DispatchHandler;
                  IF NOT fileSpecified THEN MessageBox(str02, TRUE) END;
                  res := NOT res;
                END; (* IF *)
              ELSE
                MessageBox(str01, TRUE); res := TRUE;
                fileSpecified := FALSE;
              END; (* IF *)
            END; (* IF *)
          UNTIL (NOT res);
    END; (* CASE *)
    IF (sel IN {1,2}) THEN
      IF err THEN
        MessageBox('Could not import screen', TRUE);
        fileSpecified := FALSE;
      ELSIF res THEN
        MessageBox('No more screens to import', FALSE);
        fileSpecified := FALSE;
      END; (* IF *)
      quit := TRUE; EXIT;
    END; (* IF *)
  END; (* LOOP *)
END ImportMenu;


PROCEDURE CleanCommandLine(VAR s : ARRAY OF CHAR);
BEGIN
  WHILE (s[0] # 0C) & (FileNameChar(s[1]) # CAP(s[1])) DO 
    Delete(s, 1, 1);
  END; (* WHILE *)
END CleanCommandLine;



PROCEDURE FileSpec() : BOOLEAN;
VAR
  s : ARRAY [0..127] OF CHAR;
BEGIN
  fsGetCommandLine(s); CleanCommandLine(s);
  startup := (s[0] # 0C);
  RETURN startup;
END FileSpec;


BEGIN (* FileStuff *)
  NEW(dir);
  CreateMenu(2,mb1);
  WITH mb1^ DO
    width := 3; on := {1,2};
    AssignAOC(option[1].key,"No"); AssignAOC(option[2].key,"Yes");
    AssignAOC(option[1].help,"Delete this file?");
    option[2].help := option[1].help;
  END; (* WITH *)
  CreateMenu(8,mb2);
  WITH mb2^ DO
    width := 13; on := {1,2,4,5,6,8};
    AssignAOC(option[1].key,"Load");
    AssignAOC(option[2].key,"Create");
    AssignAOC(option[3].key,"Import...");
    AssignAOC(option[4].key,"Erase");
    AssignAOC(option[5].key,"Program");
    AssignAOC(option[6].key,"Directory");
    Clear(option[7].key);
    AssignAOC(option[8].key,"Return");
    AssignAOC(option[1].help,"Load KeyDex file");
    AssignAOC(option[2].help,"Create new KeyDex file");
    AssignAOC(option[3].help,"Import text from DOS text file");
    AssignAOC(option[4].help,"Erase KeyDex files");
    AssignAOC(option[5].help,"Execute program");
    AssignAOC(option[6].help,"View DOS directory");
    AssignAOC(option[8].help,"Return to EDIT mode");
  END; (* WITH *)
  CreateMenu(3,mb3);
  WITH mb3^ DO
    width := 13; on := {3};
    AssignAOC(option[1].key,"Manual");
    AssignAOC(option[2].key,"Automatic");
    AssignAOC(option[3].key,"From File");
    AssignAOC(option[1].help,"Import one screen");
    AssignAOC(option[2].help,"Import all screens without asking");
    AssignAOC(option[3].help,"Specify file to import from");
  END; (* WITH *)
  fileSpecified := FALSE; 
END FileStuff.
