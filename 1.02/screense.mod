(*****************************************************************************
*
* Name     : ScreenSelection
*
* LastEdit : 23/05/87
* Project  : KeyDex system
* Purpose  : Operates the menu line for Keyword/Get and File/Scan
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
* 05/06/87 : Fixed bug in "IndexHelp".
*            Dynamic menus.
* 17/06/87 : Fixed bug in "IndexHelp".
* 31/07/87 : Changed "GetSearchExpr" to new spec's.
* 22/12/87 : Added capability to display all matches in a screen.
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

IMPLEMENTATION MODULE ScreenSelection;


FROM LongCardinals IMPORT
  LONGCARD;
  
FROM KDStorage IMPORT
  ALLOCATE, DEALLOCATE;

FROM KDConfig IMPORT
  KeywordLength, DateToStr;

FROM Menus IMPORT
  MenuBarPtr, MenuLine, PullDownMenu, RemovePDMenu, ScrollBox, Operation,
  DialogBox, CreateMenu, DateBox;

FROM Term IMPORT 
  CursorX, CursorY, WriteAOC;

FROM Keywords IMPORT
  LoadScreen, MarkKeyword, MarkDate, IndexEmpty, KwIndexType;

FROM KDEdit IMPORT
  SpecialKey, RedrawCurrentPage, DisplayPage, HighLight, MaxPage;

FROM KDScreen IMPORT
  Indicator, SetIndicator;

FROM BTrees IMPORT
  ResetKeywordIndex, NextKeywordIndex, PrevKeywordIndex,
  ResetDateIndex, NextDateIndex, PrevDateIndex;

FROM String IMPORT
  AssignAOC, Clear;


TYPE
  MatchListPtr = POINTER TO MatchListEntry;
  MatchListEntry =
    RECORD
      page, fromLine, fromPos, toLine, toPos : CARDINAL;
      next : MatchListPtr;
    END; (* RECORD *)
    
    
VAR
  mb, mb1, mb2 : MenuBarPtr;
  matchList    : MatchListPtr;
  ourPage      : CARDINAL;
  firstPage    : CARDINAL;  (* First page where something found *)
  

PROCEDURE ClearMatchList();
VAR
  p : MatchListPtr;
BEGIN
  WHILE matchList # NIL DO
    p := matchList; matchList := matchList^.next;
    DISPOSE(p);
  END; (* WHILE *)
  firstPage := MaxPage;
END ClearMatchList;


PROCEDURE AddMatch( pg,
                    fromL, fromP,
                    toL, toP      : CARDINAL );
VAR
  p : MatchListPtr;
BEGIN
  NEW(p);
  WITH p^ DO
    page := pg;
    fromLine := fromL; fromPos := fromP;
    toLine := toL; toPos := toP;
    next := matchList;
  END; (* WITH *)
  matchList := p;
  IF pg < firstPage THEN firstPage := pg END;
END AddMatch;


PROCEDURE OwnSpecialKey(c : CHAR);
CONST
  pgup = 73;
  pgdn = 81;
VAR
  oldp : CARDINAL;
BEGIN
  (* Highlight all matches on current page *)
  oldp := ourPage;
  CASE ORD(c) OF
    pgup : IF ourPage > 1 THEN DEC(ourPage) END;
  | pgdn : IF ourPage < MaxPage THEN INC(ourPage) END;
  ELSE
  END; (* CASE *)
  SpecialKey(c);
  IF (ourPage # oldp) THEN
    HighlightAll(ourPage);
  END; (* IF *)
END OwnSpecialKey;

    

PROCEDURE HighlightAll(pg : CARDINAL);
(* Highlights all matches on specified page *)
VAR
  p : MatchListPtr;
BEGIN
  p := matchList;
  WHILE (p # NIL) DO
    WITH p^ DO
      IF page = pg THEN
        HighLight(fromLine, fromPos, toLine, toPos);
      END; (* IF *)
    END; (* WITH *)
    p := p^.next;
  END; (* WHILE *)
END HighlightAll;


    
PROCEDURE ValidChar(c:CHAR) : BOOLEAN;
BEGIN
  RETURN (c>=' ') & (c<CHR(255));
END ValidChar;



PROCEDURE PrevDate(VAR kw:ARRAY OF CHAR) : BOOLEAN;
VAR
  d : CARDINAL;
BEGIN
  PrevDateIndex(d); 
  IF d # 0 THEN DateToStr(d,kw) ELSE Clear(kw) END;
  RETURN FALSE;
END PrevDate;


PROCEDURE NextDate(VAR kw:ARRAY OF CHAR) : BOOLEAN;
VAR
  d : CARDINAL;
BEGIN
  NextDateIndex(d);
  IF d # 0 THEN DateToStr(d,kw) ELSE Clear(kw) END;
  RETURN FALSE;
END NextDate;



PROCEDURE IndexHelp(n:CARDINAL; VAR kw:ARRAY OF CHAR) : BOOLEAN;
VAR
  cx,cy : CARDINAL;
BEGIN
  cx := CursorX; cy := CursorY; (** 17/06/87 **) Clear(kw);
  CASE n OF
    0 : IF NOT IndexEmpty(KeywordIndex) THEN
          ScrollBox(23,3,KeywordLength,ResetKeywordIndex,PrevKeywordIndex,NextKeywordIndex,
                    MarkKeyword,Select,kw);
        END; (* IF *)
  | 1 : IF NOT IndexEmpty(DateIndex) THEN
          ScrollBox(23,3,8,ResetDateIndex,PrevDate,NextDate,
                  MarkDate,Select,kw);
        END; (* IF *)
  | 2 : DateBox(kw);
  END; (* CASE *)
  IF n # 2 THEN
    CursorX := 0; CursorY := 0;
    WriteAOC('Enter input string; [RETURN] = accept, [ESC] = abort');
    (** 05/06/87 **) SetIndicator(input);
    CursorX := cx; CursorY := cy;
  END; (* IF *)
  RETURN FALSE;
END IndexHelp;



PROCEDURE GetSearchExpr(VAR s:ARRAY OF CHAR; VAR done,newtype:BOOLEAN);
BEGIN
  DialogBox('Enter search expression; [ALT-F1]=Keywords, [ALT-F2]=Keydates, "/"=Date box',
             s, IndexHelp, ValidChar, done, newtype);
END GetSearchExpr;



PROCEDURE Action( pos : LONGCARD ) : ActionType;
VAR
  sel : CARDINAL;
BEGIN
  RemovePDMenu; (* Do this just to be sure *)
  LoadScreen(pos); DisplayPage(firstPage);
  HighlightAll(firstPage);
  ourPage := firstPage;
  sel := 0; MenuLine(mb2^, sel, OwnSpecialKey);  
  CASE sel OF
    0,3 : RETURN abortSearch;
  | 1   : RETURN addScreen;
  | 2   : RETURN ignoreScreen;
  END; (* CASE *)
END Action;



PROCEDURE HelpProc( sel : CARDINAL );
END HelpProc;


PROCEDURE ScreenMenu(p:FuncProc);
VAR
  res,term : BOOLEAN;
  motion,sel,sel2 : CARDINAL;
BEGIN
  motion := 0; sel :=0; 
  p(0,res,term); IF res THEN RedrawCurrentPage END;
  LOOP
    MenuLine(mb^,sel,SpecialKey);
    IF sel IN {0,7} THEN RETURN
    ELSIF sel=5 THEN
      (* Delete *)
      sel2 := 0; PullDownMenu(40, 2, mb1^, sel2, HelpProc);
      IF sel2#0 THEN RemovePDMenu END;
      IF (sel2=2) THEN
        p(sel,res,term);
        IF term THEN RETURN
        ELSIF res THEN RedrawCurrentPage
        END;
      END; (* IF *)
    ELSE
      p(sel,res,term); 
      IF term THEN RETURN
      ELSIF res THEN RedrawCurrentPage
      END;
    END; (* IF *)
  END; (* LOOP *)
END ScreenMenu;


BEGIN (* ScreenSelection *)
  matchList := NIL;
  CreateMenu(7,mb);
  WITH mb^ DO
    AssignAOC(option[1].key,"Next"); AssignAOC(option[2].key,"Previous");
    AssignAOC(option[3].key,"First"); AssignAOC(option[4].key,"Last");
    AssignAOC(option[5].key,"Delete"); AssignAOC(option[6].key,"Remove");
    AssignAOC(option[7].key,"Exit");
    AssignAOC(option[1].help,"Display next matching screen");
    AssignAOC(option[2].help,"Display previous screen");
    AssignAOC(option[3].help,"Display first matching screen");
    AssignAOC(option[4].help,"Display last matching screen");
    AssignAOC(option[5].help,"Delete this screen from database");
    AssignAOC(option[6].help,"Remove this screen from range");
    AssignAOC(option[7].help,"Stop and return to EDIT mode");
  END; (* WITH *)
  CreateMenu(2,mb1);
  WITH mb1^ DO
    width := 4; on := {1,2};
    AssignAOC(option[1].key,"No");
    AssignAOC(option[2].key,"Yes");
    AssignAOC(option[1].help,"Delete current screen?");
    option[2].help := option[1].help;
  END; (* WITH *)
  CreateMenu(3,mb2);
  WITH mb2^ DO
    AssignAOC(option[1].key,"Add"); 
    AssignAOC(option[2].key,"Ignore");
    AssignAOC(option[3].key,"Stop"); 
    AssignAOC(option[1].help,"Add to list and continue");
    AssignAOC(option[2].help,"Ignore and continue");
    AssignAOC(option[3].help,"Stop scan and return to EDIT mode");
  END; (* WITH *)
END ScreenSelection.
