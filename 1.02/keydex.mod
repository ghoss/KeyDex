(*****************************************************************************
*
* Name     : KeyDex
*
* LastEdit : 23/05/87
* Project  : KeyDex system
* Purpose  : Main program.
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
* 28/03/87 : Fixed bug (KW index popped up after ALT-F4, ESC, ESC, RETURN).
* 11/04/87 : Break handler.
* 28/04/87 : Eliminated dependency on "FileOp".
* 05/06/87 : Dynamic menus.
* 13/06/87 : Removed inverse toggle (template).
* 30/06/87 : Modified "MainMenu".
* 16/07/87 : Connected to Print menu.
* 04/10/87 : Removed bug in MainMenu (Keys/Enter was always selectable).
* 11/10/87 : Menu box removed before undo clears screen.
* 24/12/87 : Implemented connection to SoftScreen/Help.
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

MODULE KeyDex;


FROM SYSTEM IMPORT 
  BYTE;

IMPORT 
  Break, KDHeader;

FROM String IMPORT
  AssignAOC, Clear, AppendAOC, AssignStr, AppendCh;

FROM SystemIO IMPORT
  white, reverse, intensity, underline, ClearScreen, CursorOff;

FROM KDScreen IMPORT
  SetStatus, DispPageNo, InitScreen;

FROM KDEdit IMPORT
  new, wordwrap, template, Edit, UserClearPages, attr, SetAttr,
  UndoClear, UndoFlag, insertMode, ClearPages, saved;

FROM Menus IMPORT
  CreateMenu, MenuBarPtr, MenuLine, PullDownMenu, RemovePDMenu, WarningBox,
  MessageBox;

FROM Keywords IMPORT
  ErrorType, CloseFile, empty, full, IndexEmpty, FileLoaded, KwIndexType;
  
FROM CopyProtect IMPORT
  EvalRelease, CheckPermission;
  
FROM HelpSystem IMPORT
  OpenHelpFile, CloseHelpFile, HelpErrorType, SetHelpContext;
  
FROM HelpContext IMPORT
  ModuleIndex, ScreenNumber;
  
FROM MsFileSystem IMPORT
  fsGetCurrentDir;
  
IMPORT 
  SetStuff, FileStuff, KWStuff, KDPrint;


VAR
  quit,exit : BOOLEAN;
  motion    : INTEGER;  (* 0=none, -1=left, +1=right *)
  rt        : CARDINAL;
  mb0,mb4,mb5,mb7 : MenuBarPtr;
  marker    : ARRAY BOOLEAN OF CHAR;


PROCEDURE IgnoreKey(ch:CHAR);
END IgnoreKey;


PROCEDURE HelpError(err : HelpErrorType);
BEGIN
  CASE err OF
    hetNotInstalled :
      MessageBox('Online help is not available', FALSE);
  | hetInternal :
      MessageBox('Internal problem in help system', TRUE);
  | hetNoFile :
      MessageBox('Could not open help file', TRUE);
  | hetDisk :
      MessageBox('Disk problem with help file', TRUE);
  ELSE
  END; (* CASE *)
END HelpError;


PROCEDURE MainMenu(VAR rt:CARDINAL);
VAR
  sel, mode : CARDINAL;
BEGIN (* MainMenu *)
  (* Check if speed keys are legal for keyword menu *)
  CASE rt OF
     8,9,10,12,13 :
       IF FileLoaded() AND
         ((((rt=8) OR (rt=13)) & (NOT IndexEmpty(KeywordIndex))) OR
         (((rt=9) OR (rt=12)) & (NOT (empty[KeywordIndex] & empty[DateIndex])) OR
         ((rt=10) & (NOT (full[KeywordIndex] & full[DateIndex]))))) 
         (** 04/10/87 : above line added again **)
       THEN sel := 102 ELSE RETURN END;
   | 11 : sel := 101;
   | 15 : IF FileLoaded() THEN sel := 101 ELSE RETURN END;
  ELSE
    sel := rt; IF sel#0 THEN INC(sel,100) END;
  END; (* IF *)
  REPEAT
    SetHelpContext(ScreenNumber(keydex, 2));
    motion := 0; MenuLine(mb0^,sel,IgnoreKey);
    SetHelpContext(-1);
    CASE sel OF 
       0 : RETURN;
      |2 : CASE rt OF
               8 : mode := 1;
            |  9 : mode := 3;
            | 10 : mode := 5;
            | 12 : mode := 7;
            | 13 : mode := 8;
           ELSE
             mode := 0;
           END; (* CASE *)
           KWStuff.KeywordMenu(mode);
           quit := KWStuff.quit; motion := KWStuff.motion;
      |1 : CASE rt OF
             11 : mode := 1;
           | 15 : mode := 3;
           ELSE
             mode := 0;
           END; (* CASE *)
           FileStuff.FileMenu(mode);
           quit := FileStuff.quit; motion := FileStuff.motion;
      |3 : KDPrint.PrintMenu;
           quit := KDPrint.quit; motion := KDPrint.motion;
      |4 : SClear; 
      |5 : Attributes;
      |6 : SetStuff.Settings; quit := SetStuff.quit; motion := SetStuff.motion;
      |7 : Quit;
    ELSE
    END; (* CASE *)
    IF motion=-1 THEN 
      IF sel=1 THEN sel := mb0^.entries+100 ELSE INC(sel,99) END;
    ELSIF motion=+1 THEN
      IF sel=mb0^.entries THEN sel := 101 ELSE INC(sel,101) END;
    END; (* IF *)
    rt := 0;
  UNTIL quit OR exit;
  (** 11/10/87 **)
  IF sel # 4 THEN RemovePDMenu END;
END MainMenu;



PROCEDURE HelpProc1( sel : CARDINAL );
(* Help procedure for 'Clear' menu *)
BEGIN
  SetHelpContext(ScreenNumber(keydex, 1));
END HelpProc1;



PROCEDURE SClear;
VAR     
  sel : CARDINAL;
  b   : BOOLEAN;
BEGIN
  sel := 0;
  REPEAT
    IF UndoFlag THEN INCL(mb4^.on,3) ELSE EXCL(mb4^.on,3) END;
    PullDownMenu(27, 2, mb4^, sel, HelpProc1);
    SetHelpContext(-1);
    CASE sel OF
         0 : RETURN
      |  1 : IF (NOT saved) & FileLoaded() THEN
               WarningBox(b); IF NOT b THEN quit := TRUE END;
             ELSE
               b := TRUE;
             END; (* IF *)
             IF b THEN UserClearPages(FALSE); quit := TRUE END;
      |  2 : quit := TRUE;
      |  3 : RemovePDMenu; UndoClear; quit := TRUE; RETURN; (** 11/10/87 **)
      |100 : motion := -1; RETURN
      |101 : motion := +1; RETURN
    ELSE
    END; (* CASE *)
  UNTIL quit;
  (** 11/10/87 **)
  RemovePDMenu;
END SClear;


PROCEDURE HelpProc2( sel : CARDINAL );
(* Help procedure for 'Attributes' menu *)
VAR
  cnt : INTEGER;
BEGIN
  IF sel <= 5 THEN
    cnt := 3;
  ELSIF sel # 10 THEN
    cnt := 5;
  ELSE
    cnt := 6;
  END; (* IF *)
  SetHelpContext(ScreenNumber(keydex, cnt));
END HelpProc2;


PROCEDURE Attributes;
VAR     
  sel  : CARDINAL;
BEGIN
  sel := 0;
  REPEAT
    WITH mb5^ DO
      option[1].key[11] := marker[attr=BYTE(white)];
      option[2].key[11] := marker[(attr=BYTE(white+intensity)) OR (attr=BYTE(underline+intensity))];
      option[3].key[11] := marker[(attr=BYTE(underline)) OR (attr=BYTE(underline+intensity))];
      option[4].key[11] := marker[attr=BYTE(reverse)];
      option[6].key[11] := marker[wordwrap];
      option[7].key[11] := marker[template];
      option[8].key[11] := marker[insertMode];
    END; (* WITH *)
    PullDownMenu(37, 2, mb5^, sel, HelpProc2);
    CASE sel OF
         0 : RETURN
      |  1 : SetAttr(white)
      |  2 : SetAttr(intensity)
      |  3 : SetAttr(underline)
      |  4 : SetAttr(reverse)
      |  6 : wordwrap := NOT wordwrap; 
      |  7 : template := NOT template; 
      |  8 : insertMode := NOT insertMode;
      | 10 : quit := TRUE
      |100 : motion := -1; RETURN
      |101 : motion := +1; RETURN
    ELSE END;
    IF sel IN {6,7,8} THEN SetStatus END;
  UNTIL quit;
END Attributes;


PROCEDURE HelpProc3( sel : CARDINAL );
(* Help procedure for 'Quit' menu *)
BEGIN
  SetHelpContext(ScreenNumber(keydex, 4));
END HelpProc3;


PROCEDURE Quit;
VAR     
  sel : CARDINAL;
  b   : BOOLEAN;
  res : ErrorType;
BEGIN
  sel := 0;
  REPEAT
    PullDownMenu(65, 2, mb7^, sel, HelpProc3);
    CASE sel OF
        0 : RETURN
     |  1 : quit := TRUE;
     |  2 : IF (NOT saved) & FileLoaded() THEN
              WarningBox(b); IF NOT b THEN quit := TRUE END;
            ELSE
              b := TRUE;
            END; (* IF *)
            IF b THEN
              exit := TRUE; CloseFile(res);
            END; (* IF *)
     |100 : motion := -1; RETURN
     |101 : motion := +1; RETURN
    ELSE END;
  UNTIL quit OR exit;
END Quit;
   


VAR
  s : ARRAY [0..64] OF CHAR;
BEGIN (* KeyDex *)
  Break.DisableBreak;
  CreateMenu(7,mb0);
  WITH mb0^ DO
    AssignAOC(option[1].key,"File");
    AssignAOC(option[2].key,"Keys");
    AssignAOC(option[3].key,"Print");
    AssignAOC(option[4].key,"Clear");
    AssignAOC(option[5].key,"Attributes");
    AssignAOC(option[6].key,"Settings");
    AssignAOC(option[7].key,"Quit");
    AssignAOC(option[1].help,"File operations");
    AssignAOC(option[2].help,"Keyword operations");
    AssignAOC(option[3].help,"Print screens to printer or text file");
    AssignAOC(option[4].help,"Clear current screen");
    AssignAOC(option[5].help,"Change character attributes");
    AssignAOC(option[6].help,"Keydex configuration settings");
    AssignAOC(option[7].help,"Terminate KeyDex and return to DOS");
  END;
  CreateMenu(3,mb4);
  WITH mb4^ DO
    width := 4; on := {1,2,3};
    AssignAOC(option[1].key,"Yes");
    AssignAOC(option[2].key,"No");
    AssignAOC(option[3].key,"Undo");
    AssignAOC(option[1].help,"Clear pages of current screen [F2]?");
    option[2].help := option[1].help;
    AssignAOC(option[3].help,"Restore cleared screen");
  END;
  CreateMenu(10,mb5);
  WITH mb5^ DO
    width := 12; on := {1,2,3,4,6,7,8,10};
    AssignAOC(option[1].key,"Normal      ");
    AssignAOC(option[2].key,"Bold        ");
    AssignAOC(option[3].key,"Underline   ");
    AssignAOC(option[4].key,"Inverse     ");
    Clear(option[5].key);
    AssignAOC(option[6].key,"Word Wrap   ");
    AssignAOC(option[7].key,"Template    ");
    AssignAOC(option[8].key,"Insert      ");
    Clear(option[9].key);
    AssignAOC(option[10].key,"Return");
    AssignAOC(option[1].help,"White on black [F7]");
    AssignAOC(option[2].help,"Highlighted characters [F8]");
    AssignAOC(option[3].help,"Underlined characters [ALT-F8]");
    AssignAOC(option[4].help,"Black on white [ALT-F7]");
    AssignAOC(option[6].help,"Toggle wraparound mode [F10]");
    AssignAOC(option[7].help,"Toggle template mode [ALT-F10]");
    AssignAOC(option[8].help,"Toggle insert mode [Ins]");
    AssignAOC(option[10].help,"Return to EDIT mode");
  END; (* WITH *)
  CreateMenu(2,mb7);
  WITH mb7^ DO
    width := 3; on := {1,2};
    AssignAOC(option[1].key,"No");
    AssignAOC(option[2].key,"Yes");
    (* AssignAOC(option[3].key,"TW"); *)
    AssignAOC(option[1].help,"Return to DOS?");
    option[2].help := option[1].help;
    (* AssignAOC(option[3].help,"Tree Walker (don't select!)"); *)
  END; (* WITH *)

  IF NOT FileStuff.FileSpec() THEN 
    KDHeader.TitlePage;
  END; (* IF *)
  InitScreen; ClearPages(TRUE);
  marker[FALSE] := ' '; marker[TRUE] := 'þ';
  new := TRUE; exit := FALSE; rt := 1; DispPageNo(1); (* Call file menu first *)

  CheckPermission;
  fsGetCurrentDir(s); AssignAOC(s, s);
  IF (s[ORD(s[0])] # '\') THEN AppendCh(s, '\') END;
  AppendAOC(s, 'KEYDEX');
  AssignStr(s, s);
  OpenHelpFile(s, HelpError);

  REPEAT
    quit := FALSE; new := FALSE;
    CursorOff; MainMenu(rt);
    IF NOT exit THEN Edit(rt) END;
  UNTIL exit;
  CloseHelpFile;
  ClearScreen;
END KeyDex.
