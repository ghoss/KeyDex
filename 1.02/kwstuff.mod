(*****************************************************************************
*
* Name     : KWStuff
*
* LastEdit : 22/05/87
* Project  : KeyDex system
* Purpose  : Handles keyword menu
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
* 29/03/87 : Implementation of search menu paging functions
* 11/04/87 : Added template item to menu.
* 12/04/87 : Fixed bug in function "Enter".
* 18/04/87 : Changed module because of new "ScreenSelection".
* 29/05/87 : Built in additional messages after termination of scan.
* 05/06/87 : Dynamic menus.
* 06/06/87 : Date index implementation.
* 13/06/87 : Template menu in "Keyword Store" removed.
*            Extension of "InitScreen".
* 17/06/87 : Adjusted code for "Get Template". Changed "dummy(..)".
*            Modified "Keyword Enter".
* 23/06/87 : Bug fixed in init code of "KeywordMenu".
* 30/06/87 : Bug fixed in "Keyword Enter".
* 18/07/87 : Implemented error handling for screen loading.
* 31/07/87 : Changed "dummy".
* 24/09/87 : [RETURN] after Drop goes back to scroll box instead of menu.
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

IMPLEMENTATION MODULE KWStuff;


FROM LongCardinals IMPORT
  LONGCARD, zero, one, less, inc, dec, equal;

FROM Menus IMPORT
  MenuBarPtr, PullDownMenu, DialogBox, MessageBox, ScrollBox, Operation, 
  RemovePDMenu, CreateMenu, DateBox, WarningBox;

FROM String IMPORT
  Assign, AssignAOC, Clear;

FROM KDConfig IMPORT
  StrToDate, Keyword, KeywordLength;

FROM Keywords IMPORT
  empty, AddKeyword, Cleanup, ResetList, PrevKeyword, NextKeyword,
  MarkKeyword, SaveScreen, loaded, InitScreen, NextScreen, KwIndexType,
  DeleteScreen, PrevScreen, FirstScreen, LastScreen, ExistLink, CurrentScreen,
  ValidKeywordChar, ReturnTotal, DisposeScreenList, criteria, oldcrit,
  PrevDate, NextDate, MarkDate, full, IndexEmpty, AddDate, recdate, recno,
  FileLoaded;

FROM BTrees IMPORT
  ResetKeywordIndex, NextKeywordIndex, PrevKeywordIndex, 
  NextTemplate, PrevTemplate, TemplateEntered;

FROM KDEdit IMPORT
  RedrawCurrentPage, defaultkw, RemoveHighLight, saved;

FROM ScreenSelection IMPORT
  ScreenMenu, GetSearchExpr;

FROM KDScreen IMPORT
  DispMatch, DispRecordKey;

FROM KDCritErr IMPORT
  InvokeHandler, DispatchHandler;

FROM HelpSystem IMPORT
  SetHelpContext;
  
FROM HelpContext IMPORT
  ModuleIndex, ScreenNumber;


CONST
  str10 = 'Disk problem; failed to load screen from disk';
  str11 = 'Disk problem; failed to save screen to disk';
  str12 = 'Disk problem; failed to delete screen';

VAR
  mb1,mb2,
  mb3,mb4     : MenuBarPtr;
  current     : LONGCARD;
  ignoreCase,
  prompt,
  wordOnly    : BOOLEAN;
  marker      : ARRAY BOOLEAN OF CHAR;


PROCEDURE dummy(n:CARDINAL; VAR x:ARRAY OF CHAR) : BOOLEAN;
BEGIN 
  IF n=2 THEN
    DateBox(x)
  ELSE
    Clear(x);
  END; (* IF *)
  RETURN (n = 2) & (x[0] # 0C);
END dummy;


PROCEDURE HelpProc( sel : CARDINAL );
(* Dummy help procedure *)
END HelpProc;


PROCEDURE KeywordBox(typ : Operation);
VAR
  sel3 : CARDINAL;
  s    : Keyword;
  er,
  del  : BOOLEAN;
BEGIN 
  sel3 := 0; 
  WITH mb4^ DO
    IF empty[KeywordIndex] THEN EXCL(on,1) ELSE INCL(on,1) END;
    IF empty[DateIndex] THEN EXCL(on,2) ELSE INCL(on,2) END;
  END; (* WITH *)
  PullDownMenu(23, 7, mb4^, sel3, HelpProc);
  LOOP
    CASE sel3 OF
      0 : (* Nothing *)
    | 1 : ScrollBox(35,7,KeywordLength,ResetList,PrevKeyword,NextKeyword,
                    MarkKeyword,typ,s);
    | 2 : ScrollBox(35,7,8,ResetList,PrevDate,NextDate,MarkDate,typ,s);
    END; (* CASE *)
    IF sel3 # 0 THEN
      del := (typ = Mark) & (s[0] # 0C);
      Cleanup(del, er);
      IF del & (NOT er) THEN 
        MessageBox('You must mark a key with [SPACE] to delete it.',FALSE);
      ELSE
        RemovePDMenu; EXIT;
      END; (* IF *)
    ELSE
      EXIT;
    END; (* IF *)
  END; (* LOOP *)
END KeywordBox;



PROCEDURE MenuFunction(i:CARDINAL; VAR res,term:BOOLEAN);
(* Handles the stop/next/previous menu line after loading a screen *)

  PROCEDURE ErrorCheck;
  (* Checks if screen has been loaded successfully *)
  BEGIN
    IF equal(recno, zero) THEN
      term := TRUE;
      MessageBox(str10, TRUE);
      DisposeScreenList;
    END; (* IF *)
  END ErrorCheck;

VAR
  total : LONGCARD;
BEGIN
  res := FALSE; term := FALSE; ReturnTotal(total);
  CASE i OF
    0 : IF less(total,current) THEN current := total END;
        res := TRUE;
  | 1 : IF less(current,total) THEN 
          NextScreen; ErrorCheck; inc(current); res := TRUE
        END; (* IF *)
  | 2 : IF less(one,current) THEN 
          PrevScreen; ErrorCheck; dec(current); res := TRUE
        END; (* IF *)
  | 3 : IF NOT equal(current,one) THEN 
          FirstScreen; ErrorCheck; current := one; res := TRUE 
        END; (* IF *)
  | 4 : IF NOT equal(current,total) THEN
          LastScreen; ErrorCheck; current := total; res := TRUE
        END; (* IF *)
  | 5,6 : 
        IF equal(current,total) THEN dec(current) END; dec(total);
        DeleteScreen(i=5); term := NOT ExistLink(); res := TRUE;
        IF (i = 5) & equal(recno, zero) THEN
          MessageBox(str12, TRUE); term := TRUE;
          DisposeScreenList;
        END; (* IF *)
        IF NOT term THEN CurrentScreen END;
  END; (* CASE *)
  DispMatch(current,total); 
END MenuFunction;


PROCEDURE HelpProc2( sel : CARDINAL );
(* Help procedure for 'Keys' menu *)
BEGIN
  SetHelpContext(ScreenNumber(kwstuff, sel));
END HelpProc2;



PROCEDURE KeywordMenu(rt:CARDINAL);
VAR
  sel,
  sel2,
  sel3  : CARDINAL;
  crit  : criteria;
  s     : Keyword;
  done,
  res,
  found,
  newtype,
  foundOne,
  scan      : BOOLEAN;
BEGIN  (* KeywordMenu *)
  IF rt#0 THEN sel := 100+rt ELSE sel := 0 END;
  quit := (rt=5); IF rt#5 THEN Clear(defaultkw) END; motion := 0;
  REPEAT
    WITH mb1^ DO
      IF FileLoaded() THEN
        (* Keyword Enter *)
        IF (full[KeywordIndex] & full[DateIndex]) THEN
          EXCL(on,5)
        ELSE
          INCL(on,5)
        END; (* IF *)
        (* Keyword Drop/List/Store *)
        IF (empty[KeywordIndex] & empty[DateIndex]) THEN
          on := on - {3,6,7}
        ELSE
          on := on + {3,6,7}
        END; (* IF *)
        (* Keyword Index *)
        IF IndexEmpty(KeywordIndex) THEN
          on := on - {1,8,9}
        ELSE
          on := on + {1,8,9}
        END; (* IF *)
        (* Get Template *)
        (** 23/06/87 **)
        InvokeHandler(FALSE);
        IF (NOT IndexEmpty(KeywordIndex)) & TemplateEntered() THEN
          INCL(on,2)
        ELSE
          EXCL(on,2)
        END; (* IF *)
        DispatchHandler;
      ELSE
        on := {11};
      END; (* IF *)
    END; (* WITH *)
    PullDownMenu(8, 2, mb1^, sel, HelpProc2);
    CASE sel OF
        0 : RETURN
     |  1 : (* Keyword Get *)
            IF NOT saved THEN
              WarningBox(res); IF NOT res THEN quit := TRUE; RETURN END;
            END; (* IF *)
            IF NOT ExistLink() THEN Clear(oldcrit) END;
            Assign(crit,oldcrit);
            REPEAT
              quit := FALSE; done := TRUE;
              InvokeHandler(FALSE);
              GetSearchExpr(crit,res,newtype);
              DispatchHandler;
              IF (crit[0]#0C) AND res THEN
                quit := TRUE;
                IF (NOT newtype) AND ExistLink() THEN
                  RemovePDMenu;
                  InvokeHandler(FALSE);
                  CurrentScreen;
                  DispatchHandler;
                  IF NOT equal(recno, zero) THEN
                    InvokeHandler(FALSE); 
                    ScreenMenu(MenuFunction); DispMatch(zero,zero);
                    DispatchHandler;
                  ELSE
                    MessageBox(str10, TRUE);
                  END; (* IF *)
                ELSE
                  InvokeHandler(FALSE);
                  InitScreen(crit, wordOnly, ignoreCase,
                             prompt, found, foundOne, scan, res); 
                  DispatchHandler;
                  IF loaded THEN
                    DispRecordKey(recdate,recno)
                  ELSE
                    DispRecordKey(0,zero)
                  END; (* IF *)
                  IF res AND found THEN 
                    IF prompt & scan THEN
                      MessageBox('Returning to list of selected screens.', FALSE);
                    END; (* IF *)
                    Assign(oldcrit,crit); RemovePDMenu;
                    InvokeHandler(FALSE);
                    FirstScreen; current := one;
                    DispatchHandler;
                    IF NOT equal(recno, zero) THEN
                      InvokeHandler(FALSE);
                      ScreenMenu(MenuFunction); DispMatch(zero,zero);
                      DispatchHandler;
                    ELSE
                      MessageBox(str10, TRUE);
                    END; (* IF *)
                  ELSIF NOT res THEN
                    MessageBox('Mistake in search expression, try again.', TRUE);
                    done := FALSE;
                  ELSIF NOT foundOne THEN
                    MessageBox('No matching screens found, try again.', FALSE);
                    done := FALSE;
                  ELSE
                    MessageBox('No screens selected, returning to EDIT mode.', FALSE);
                    RemoveHighLight;
                  END; (* IF *)
                END; (* IF *)
              END; (* IF *)
            UNTIL done;
     |  2 : (* Keyword Get Template *)
            IF NOT saved THEN
              WarningBox(res); IF NOT res THEN quit := TRUE; RETURN END;
            END; (* IF *)
            ScrollBox(23,4,KeywordLength,ResetKeywordIndex,PrevTemplate,NextTemplate,
                      MarkKeyword,Select,crit);
            IF crit[0]#0C THEN
              InvokeHandler(FALSE);
              InitScreen(crit, wordOnly, ignoreCase, prompt, 
                         found, foundOne, scan, res);
              DispatchHandler;
              InvokeHandler(FALSE);
              FirstScreen;
              DispatchHandler;
              RemovePDMenu; RedrawCurrentPage; quit := TRUE;
              DisposeScreenList;
            END; (* IF *)
     |  3 : (* Keyword Store *)
            IF loaded THEN
              sel2 := 0; PullDownMenu(23, 4, mb2^, sel2, HelpProc);
              IF (sel2#0) THEN
                RemovePDMenu;
                InvokeHandler(FALSE); SaveScreen(sel2=1); DispatchHandler;
                IF equal(recno, zero) THEN MessageBox(str11, TRUE) END;
                quit := TRUE;
              END; (* IF *)
            ELSE
              InvokeHandler(FALSE);
              SaveScreen(TRUE); quit := TRUE;
              IF equal(recno, zero) THEN MessageBox(str11, TRUE) END;
              DispatchHandler;
            END; (* IF *)
     |  5 : (* Keyword Enter *)
            s := defaultkw;
            REPEAT
              DialogBox('Enter keyword or "/" and keydate - will be added to list for this screen.',
                        s, dummy, ValidKeywordChar, res, newtype);
              (*** 12/04/87 : "AND res" added ***)
              done := TRUE;
              IF (s[0]#0C) AND res THEN 
                StrToDate(s,sel3);
                IF sel3 = 0 THEN 
                  (* Maybe date characters in keyword - illegal *)
                  res := TRUE;
                  FOR sel3 := 1 TO ORD(s[0]) DO
                    res := res & ValidKeywordChar(s[sel3]);
                  END; (* FOR *)
                  IF res THEN
                    (** 30/06/87 **)
                    IF NOT full[KeywordIndex] THEN AddKeyword(s)
                    ELSE
                      MessageBox('Keyword index for this screen full - not entered', FALSE);
                    END; (* IF *)
                  ELSE
                    MessageBox('This entry is illegal - try again', TRUE);
                    done := FALSE;
                  END; (* IF *)
                ELSE
                  (** 30/06/87 **)
                  IF NOT full[DateIndex] THEN AddDate(sel3);
                  ELSE
                    MessageBox('Keydate index for this screen full - not entered', FALSE);
                  END; (* IF *)
                END; (* IF *)
              END; (* IF *)
            UNTIL done;
     |  6 : (* Keyword Drop *)
            KeywordBox(Mark);
     |  7 : (* Keyword List *)
            KeywordBox(Scroll);
     |  8 : (* Index List *)
            InvokeHandler(FALSE);
            ScrollBox(23,6,KeywordLength,ResetKeywordIndex,PrevKeywordIndex,NextKeywordIndex,
                      MarkKeyword,Scroll,s);
            DispatchHandler;
     |  9 : (* Options *)
            sel3 := 0; 
            REPEAT
              WITH mb3^ DO
                option[1].key[13] := marker[wordOnly];
                option[2].key[13] := marker[ignoreCase];
                option[3].key[13] := marker[prompt];
              END; (* WITH *)
              PullDownMenu(23, 10, mb3^, sel3, HelpProc);
              IF sel3 IN {1,2,3} THEN DisposeScreenList END;
              CASE sel3 OF
                1 : wordOnly := NOT wordOnly;
              | 2 : ignoreCase := NOT ignoreCase;
              | 3 : prompt := NOT prompt;
              ELSE
              END; (* CASE *)
            UNTIL sel3=0;
     | 11 : quit := TRUE;
     |100 : motion := -1; RETURN
     |101 : motion := +1; RETURN
    ELSE END;
  UNTIL quit;
END KeywordMenu;



BEGIN
  marker[FALSE] := ' '; marker[TRUE] := 'þ';
  CreateMenu(11,mb1);
  WITH mb1^ DO
    width := 10; on := {11};
    AssignAOC(option[1].key,"Get");
    AssignAOC(option[2].key,"Template");
    AssignAOC(option[3].key,"Store");
    Clear(option[4].key);
    AssignAOC(option[5].key,"Enter");
    AssignAOC(option[6].key,"Drop");
    AssignAOC(option[7].key,"List");
    AssignAOC(option[8].key,"Index");
    AssignAOC(option[9].key,"Options...");
    Clear(option[10].key);
    AssignAOC(option[11].key,"Return");
    AssignAOC(option[1].help,"Retrieve screen");
    AssignAOC(option[2].help,"Retrieve template screen");
    AssignAOC(option[3].help,"Store this screen in database");
    AssignAOC(option[5].help,"Enter keyword for this screen");
    AssignAOC(option[6].help,"Delete keyword");
    AssignAOC(option[7].help,"List all keywords for this screen");
    AssignAOC(option[8].help,"List all keywords for entire file");
    AssignAOC(option[9].help,"Change settings for text scan");
    Clear(option[10].help);
    AssignAOC(option[11].help,"Return to EDIT mode");
  END;
  CreateMenu(2,mb2);
  WITH mb2^ DO
    width := 7; on := {1,2};
    AssignAOC(option[1].key,"Add");
    AssignAOC(option[2].key,"Replace");
    AssignAOC(option[1].help,"Add screen and leave old one");
    AssignAOC(option[2].help,"Add screen and delete old screen");
  END;
  CreateMenu(3,mb3);
  WITH mb3^ DO
    width := 13; on := {1,2,3};
    AssignAOC(option[1].key,"Words Only   ");
    AssignAOC(option[2].key,"Ignore Case  ");
    AssignAOC(option[3].key,"Pause        ");
    AssignAOC(option[1].help,"Match the search string with words");
    AssignAOC(option[2].help,"Consider upper/lowercase as equal");
    AssignAOC(option[3].help,"Pause after each matching screen");
  END;
  CreateMenu(2,mb4);
  WITH mb4^ DO
    width := 8; on := {};
    AssignAOC(option[1].key,"Keywords");
    AssignAOC(option[2].key,"Keydates");
    Clear(option[1].help); Clear(option[2].help);
  END; (* WITH *)
  ignoreCase := TRUE; wordOnly := FALSE; prompt := TRUE;
END KWStuff.
