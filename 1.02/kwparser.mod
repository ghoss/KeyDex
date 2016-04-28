(*****************************************************************************
*
* Name     : KWParser
*
* LastEdit : 29/05/87
* Project  : KeyDex system
* Purpose  : Accepts regular expressions and searches for matching keywords
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
* 18/04/87 : Bug fixed in "DeleteScreen".
* 25/04/87 : Changes to accomodate file scanning.
* 03/05/87 : Changes to allow retrieval of 2^32 records.
*            Added "prompt" feature.
* 10/06/87 : Most parts of the parser completely rewritten.
* 13/06/87 : Bug fixed in the "NOT ScanAll" part of the parser.
* 16/08/87 : Optimized "NegateList".
* 18/08/87 : Optimized ranges and wildcards.
* 05/11/87 : Fixed bug in 'term'.
* 22/12/87 : Added ability to display all matches in a screen.
* 26/03/88 : Fixed bug in 'scan all' part of search procedure.
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

IMPLEMENTATION MODULE KWParser;


FROM KDStorage IMPORT
  ALLOCATE, DEALLOCATE;

FROM KDConfig IMPORT
  Keyword, StrToDate, DateChar, DateType;

FROM String IMPORT
  UpperCase, Delete, WildcardMatch, Compare, Copy, Clear, AssignAOC,
  AppendCh;

FROM LongCardinals IMPORT
  LONGCARD, equal, less, zero, one, inc, dec;

FROM BTrees IMPORT
  LookupKeyword, InitLink, NextLink, BrowseKeywords, BrowseDates, LookupDate,
  IndexType;

FROM KDFileScan IMPORT
  Scan, Search;

FROM Compress IMPORT
  ValidChar;

FROM ScreenSelection IMPORT
  Action, ActionType, ClearMatchList, AddMatch;


TYPE
   (* Matching screens form a double linked list *)
   RecPtr    = POINTER TO ScreenRec;
   ScreenRec = 
     RECORD
       pos : LONGCARD; next,prev : RecPtr;
     END; (* RECORD *)

   FactorType = (dateTyp, kwTyp, wildkwTyp, illTyp);
   Factor =
     RECORD
       CASE BOOLEAN OF
         TRUE  : kw:Keyword;
       | FALSE : dt:CARDINAL;
       END; (* CASE *)
     END; (* RECORD *)


VAR
  testRes : RecPtr;
  totalNo : LONGCARD;
  head,
  tail,
  current : RecPtr;
  lowKW,                 (* Global variables for browse procedures *)
  highKW,
  wcKW,
  allKW    : Keyword;    (* allKW contains the [ALL] default keyword *)
  lowDate,
  highDate : CARDINAL;
  limit    : INTEGER;



PROCEDURE ReturnTotal(VAR t:LONGCARD);
BEGIN t := totalNo END ReturnTotal;


PROCEDURE ValidDateChar(c:CHAR) : BOOLEAN;
(* Returns TRUE if c is a valid part of a date *)
BEGIN
  RETURN ((c >= '0') & (c <= '9')) OR (c = DateChar[DateType]);
END ValidDateChar;



PROCEDURE ValidParseChar(c:CHAR) : BOOLEAN;
(* Returns TRUE if c is a part of a keyword expression *)
BEGIN
  RETURN ValidChar(c) OR ValidDateChar(c) OR (c='(') OR (c=')') OR
         (c='&') OR (c='|') OR (c='<') OR (c='>') OR (c='*') OR (c='?')
         OR (c='#') OR (c='-');
END ValidParseChar;



PROCEDURE AddToList(f : IndexType; pos : LONGCARD; head : RecPtr);
(* Checks if "pos" is already in list "head" and merges its link list if not.
   The link list and "head" are both sorted and there are no double 
   occurences in either. List "head" must begin with a sentinel. *)
VAR
  p,p1,q : RecPtr;
BEGIN
  InitLink(f,pos); NextLink(f,pos);
  p := head^.next; p1 := head;
  WHILE (p # NIL) AND (NOT equal(pos,zero)) DO
    IF less(pos, p^.pos) THEN 
      (* add pos to list *)
      NEW(q); q^.pos := pos; p1^.next := q; q^.next := p; p := q;
      NextLink(f, pos);
    ELSIF equal(pos, p^.pos) THEN
      NextLink(f, pos);
    END; (* IF *)
    p1 := p; p := p^.next;
  END; (* WHILE *)
  (* Add remaining list *)
  WHILE NOT equal(pos,zero) DO
    NEW(q); q^.pos := pos; p1^.next := q; p1 := q;
    NextLink(f,pos);
  END; (* WHILE *)
  IF (p = NIL) THEN p1^.next := NIL END;
END AddToList;



PROCEDURE OrConnect(A,B : RecPtr; VAR Result : RecPtr);
(* The two lists A and B (each beginning with a sentinel) are merged into
   a single one without duplicating elements. A pointer to the resulting
   list is returned in "Result". A and B are distroyed. *)
VAR
  p,p1,q,r : RecPtr;
BEGIN
  p := A^.next; q := B^.next; 
  (* Save one sentinel and dispose the other *)
  Result := A; p1 := A; DISPOSE(B);
  WHILE (p # NIL) & (q # NIL) DO
    IF less(q^.pos, p^.pos) THEN
      (* Insert q before p, step q *)                   
      r := q^.next;
      q^.next := p; p1^.next := q; p1 := q;
      q := r;
    ELSIF equal(p^.pos, q^.pos) THEN
      (* Dispose q and step both *)
      p1 := p; p := p^.next; 
      r := q^.next; DISPOSE(q); q := r;
    ELSE
      (* Step p *)
      p1 := p; p := p^.next;
    END; (* IF *)
  END; (* WHILE *)
  (* Add remaining list *)
  IF p = NIL THEN p1^.next := q END; 
END OrConnect;



PROCEDURE AndConnect(A,B : RecPtr; VAR Result : RecPtr);
(* Creates a list which contains elements occuring in both A and B. 
   A pointer to this list is returned in "Result". A and B are destroyed. *)
VAR
  p,p1,q,r : RecPtr;
BEGIN
  p := A^.next; q := B^.next; 
  (* Save one sentinel and dispose the other *)
  Result := A; p1 := A; DISPOSE(B);
  WHILE (p # NIL) & (q # NIL) DO
    IF less(p^.pos,q^.pos) THEN
      (* Advance p until p>=q *)
      r := p^.next; DISPOSE(p); p := r; 
      p1^.next := p;
    ELSIF equal(p^.pos, q^.pos) THEN
      (* Only retain p and advance both *)
      r := q^.next; DISPOSE(q); q := r;
      p1 := p; p := p^.next;
    ELSE
      (* Advance q until q>=p *)
      r := q^.next; DISPOSE(q); q := r;
    END; (* IF *)
  END; (* WHILE *)
  (* Dispose remaining elements *)
  IF p # NIL THEN
    p1^.next := NIL; ClearTempList(p);
  END; (* IF *)
  ClearTempList(q);
END AndConnect;



PROCEDURE KeywordRange(VAR s:Keyword; pos:LONGCARD) : BOOLEAN;
(* Browse procedure for keyword ranges *)
BEGIN
  IF (Compare(s,highKW) < limit) THEN
    IF (Compare(s, allKW) # 0) THEN
      AddToList(KeywordIndex, pos, testRes);
    END; (* IF *)
  ELSE
    RETURN FALSE;
  END; (* IF *)
  RETURN TRUE;
END KeywordRange;



PROCEDURE DateRange(date : CARDINAL; pos : LONGCARD) : BOOLEAN;
(* Browse procedure for date ranges *)
BEGIN
  IF (date <= highDate) THEN
    AddToList(DateIndex, pos, testRes);
  ELSE
    RETURN FALSE;
  END; (* IF *)
  RETURN TRUE;
END DateRange;

    


PROCEDURE NegateList(head : RecPtr; VAR Result : RecPtr);
(* Creates a list in "Result" which contains all data screens NOT in head.
   head is destroyed. *)
VAR
  p,q,q1 : RecPtr;
  found  : BOOLEAN;
  pos    : LONGCARD;
BEGIN
  CreateEmptyList(testRes);
  (** 16/08/87 : added optimization **)
  LookupKeyword(allKW, pos, FALSE, found);
  IF found THEN
    AddToList(KeywordIndex, pos, testRes);
  END; (* IF *)
  (* Dispose sentinel *)
  p := head^.next; DISPOSE(head); head := p;
  q1 := testRes; q := testRes^.next;

  WHILE (head # NIL) & (q # NIL) DO
    IF less(head^.pos,q^.pos) THEN
      p := head^.next; DISPOSE(head); head := p; 
    ELSIF equal(head^.pos, q^.pos) THEN
      q1^.next := q^.next; DISPOSE(q); q := q1^.next;
    ELSE
      q1 := q; q := q^.next;
    END; (* IF *)
  END; (* WHILE *)
  ClearTempList(head); Result := testRes;
END NegateList;



PROCEDURE CombineWC(VAR s:Keyword; pos:LONGCARD) : BOOLEAN;
(* Tests if s matches on wildcard expression s *)
BEGIN
  IF WildcardMatch(s, wcKW) & (Compare(s, allKW) # 0) THEN
    AddToList(KeywordIndex, pos, testRes) 
  END; (* IF *)
  RETURN Compare(s, highKW) < 0;
END CombineWC;



PROCEDURE CreateEmptyList(VAR r : RecPtr);
(* Creates an empty list consisting of a sentinel. *)
BEGIN
  NEW(r); r^.next := NIL;
END CreateEmptyList;


PROCEDURE ClearTempList(VAR r : RecPtr);
(* Clears any temporary lists in case of a parse error *)
VAR
  p : RecPtr;
BEGIN
  WHILE r # NIL DO
    p := r; r := r^.next; DISPOSE(p)
  END; (* WHILE *)
END ClearTempList;



PROCEDURE Parse( s          : ARRAY OF CHAR;
                 wordOnly, 
                 ignoreCase,
                 prompt     : BOOLEAN;
                 VAR found,
                     foundOne,
                     scan,
                     Err    : BOOLEAN );

  PROCEDURE GetFactor(VAR f : Factor; VAR typ : FactorType);
  (* Trys to read a keyword or date from input string *)
  VAR
    c  : CHAR;
    kw : Keyword;
    dt,kwlen : CARDINAL;
    valid,wc : BOOLEAN;
  BEGIN
    IF (i > slen) THEN typ := illTyp
    ELSE
      wc := FALSE; Clear(kw);
      (* Try to get factor *)
      LOOP
        IF (i > slen) THEN EXIT
        ELSE
          c := s[i]; INC(i);
          IF (c='*') OR (c='?') THEN
            wc := TRUE; AppendCh(kw,c);
          ELSIF ValidChar(c) OR ValidDateChar(c) THEN
            AppendCh(kw,c)
          ELSE
            DEC(i); EXIT;
          END;
        END; (* IF *)
      END; (* LOOP *)
      (* Check validity of keyword or date *)
      StrToDate(kw,dt);
      IF dt # 0 THEN
        typ := dateTyp; f.dt := dt;
      ELSE
        (* Check if it is a keyword or just an illegal date *)
        valid := TRUE; j := 1; kwlen := ORD(kw[0]);
        LOOP
          c := kw[j]; INC(j);
          valid := valid & (ValidChar(c) OR (c='*') OR (c='?'));
          IF (j > kwlen) OR (NOT valid) THEN EXIT END;
        END; (* LOOP *)
        IF valid THEN
          f.kw := kw;
          IF wc THEN typ := wildkwTyp ELSE typ := kwTyp END;
        ELSE
          typ := illTyp;
        END; (* IF *)
      END; (* IF *)
    END; (* IF *)
  END GetFactor;

  
  PROCEDURE expr(VAR list : RecPtr; eval : BOOLEAN);
  (* expr ::= term {['&'|'|'] term} *)
  VAR
    b : RecPtr;
    c : CHAR;
    oldeval : BOOLEAN;
  BEGIN
    IF (i > slen) THEN Err := TRUE; RETURN END;
    oldeval := eval;
    term(list, eval);
    IF (NOT Err) & (i <= slen) THEN
      c := s[i];
      WHILE (i < slen) & ((c='&') OR (c='|')) DO
        (* Conditional evaluation of AND expressions *)
        IF (c = '&') & (list^.next = NIL) THEN
          eval := FALSE
        ELSE
          eval := oldeval 
        END; (* IF *)
        INC(i); term(b, eval); IF Err THEN ClearTempList(list); RETURN END;
        IF eval THEN
          IF (c='&') THEN 
            AndConnect(list,b,list);
          ELSE
            OrConnect(list,b,list);
          END; (* IF *)
        END; (* IF *)
        c := s[i];
      END; (* WHILE *)
    END; (* IF *)
  END expr;


  PROCEDURE term(VAR list : RecPtr; eval : BOOLEAN);
  (* term ::= (('<'|'>') factor) | (factor ['-' factor]) | ('(' expr ')')
              | ('#' term)
     "eval"=FALSE -> Evaluation of expression is only simulated. *)
  VAR
    f1,f2     : Factor;
    typ1,typ2 : FactorType;
    c         : CHAR;
    l         : RecPtr;
    found     : BOOLEAN;
    k         : CARDINAL;
  BEGIN
    IF i > slen THEN Err := TRUE; RETURN END;
    c := s[i];
    CASE c OF
      '<','>' :
        (* Keyword or date relations *)
        INC(i); GetFactor(f1,typ1);
        IF typ1 = kwTyp THEN
          CreateEmptyList(testRes);
          IF eval THEN
            IF (c = '<') THEN
              Clear(lowKW); highKW := f1.kw;
            ELSE 
              lowKW := f1.kw; AssignAOC(highKW,CHR(255));
              INC(lowKW[ORD(lowKW[0])]);
            END; (* IF *)
            limit := 0;  (* compare relation is < 0 *)
            BrowseKeywords(lowKW, KeywordRange);
          END; (* IF *)
          list := testRes;
        ELSIF typ1 = dateTyp THEN
          CreateEmptyList(testRes);
          IF eval THEN
            IF (c = '<') THEN
              lowDate := 0; highDate := f1.dt - 1;
            ELSE 
              lowDate := f1.dt + 1; highDate := 65535;
            END; (* IF *)
            BrowseDates(lowDate, DateRange);
          END; (* IF *)
          list := testRes; 
        ELSE
          Err := TRUE;
        END; (* IF *)

    | '#' :
        (* NOT - operation on a term *)
        INC(i); term(l, eval); 
        IF NOT Err THEN
          IF eval THEN NegateList(l,list) ELSE CreateEmptyList(list) END;
        END; (* IF *)

    | '(' :
        (* Expression in parantheses *)
        INC(i); expr(list, eval);
        IF NOT Err THEN
          IF s[i] = ')' THEN INC(i)
          ELSE
            Err := TRUE; ClearTempList(list);
          END; (* IF *)
        END; (* IF *)

    ELSE
      (* Either a factor or a factor range *)
      GetFactor(f1,typ1);
      IF (i <= slen) & (s[i] = '-') THEN
        (* Keyword or date range *)
        INC(i); GetFactor(f2,typ2);
        IF (typ1 = typ2) THEN
          (* Type compatibility ok *)
          IF (typ1 = dateTyp) THEN
            CreateEmptyList(testRes); 
            IF eval THEN 
              lowDate := f1.dt; highDate := f2.dt;
              BrowseDates(lowDate, DateRange);
            END; (* IF *)
          ELSIF (typ1 = kwTyp) THEN
            CreateEmptyList(testRes); 
            IF eval THEN
              lowKW := f1.kw; highKW := f2.kw; 
              limit := 1;   (* compare relation is <= 0 *)
              BrowseKeywords(lowKW, KeywordRange);
            END; (* IF *)
          ELSE
            Err := TRUE; RETURN;
          END; (* IF *)
          list := testRes;
        ELSE
          Err := TRUE;
        END; (* IF *)
      ELSE
        (* Simple factor, can be date, keyword or wildcard. *)
        CASE typ1 OF
          dateTyp :
            CreateEmptyList(list);
            IF eval THEN
              LookupDate(f1.dt, pos, FALSE, found);
              IF found THEN AddToList(DateIndex, pos, list) END;
            END; (* IF *)
        | kwTyp :
            CreateEmptyList(list);
            IF eval THEN
              LookupKeyword(f1.kw, pos, FALSE, found);
              IF found THEN AddToList(KeywordIndex, pos, list) END;
            END; (* IF *)
        | wildkwTyp :
            CreateEmptyList(testRes);
            IF eval THEN
              wcKW := f1.kw;
              (* Calculate low and high bounds *)
              lowKW := wcKW; highKW := lowKW;
              lowKW[0] := 1C; c := lowKW[1];
              k := 1;
              WHILE (c # '*') & (c # '?') DO
                INC(k); c := lowKW[k];
              END; (* WHILE *)
              (** 05/11/87 **)
              highKW[k] := CHR(255); highKW[0] := CHR(k);
              lowKW[0] := CHR(k - 1);
              BrowseKeywords(lowKW, CombineWC);
              list := testRes;
            END; (* IF *)
        | illTyp :
            Err := TRUE;
        END; (* CASE *)
      END; (* IF *)
    END; (* CASE *)
  END term;


  PROCEDURE Evaluate(VAR S:ARRAY OF CHAR; VAR Result:RecPtr);
  VAR
    p : RecPtr;
  BEGIN
    slen := ORD(S[0]); i := 1; expr(p, TRUE);
    IF (NOT Err) THEN
      IF (i > slen) THEN
        Result := p^.next; DISPOSE(p);
      ELSE
        Err := TRUE; ClearTempList(p);
      END; (* IF *)
    END; (* IF *)
  END Evaluate;

VAR
  S1  : ARRAY [0..255] OF CHAR;
  pos, pos2 : LONGCARD;
  p,q,p1 : RecPtr;
  fnd, fnd2, scanAll, same, buffered : BOOLEAN;
  i,j,len,page,fromL,fromP,toL,toP,slen : CARDINAL;
  page2, fromL2, fromP2, toL2, toP2 : CARDINAL;
  ac : ActionType;
BEGIN
  Err := FALSE; foundOne := FALSE; scan := FALSE; 
  (* Clean up input string and remove blanks *)
  i := 1; len := ORD(s[0]); 
  WHILE (i<=len) & (s[i]=' ') DO INC(i) END;
  IF i>len THEN Err := TRUE; RETURN (* string empty *)
  ELSE
    IF s[i]='"' THEN
      (* String coming first *)
      j := i+1; scan := TRUE;
      WHILE (j<=len) & (s[j]#'"') DO INC(j) END;
      IF j>len THEN Err := TRUE; RETURN
      ELSIF (i+1<j) THEN
        (* Copy characters i+1..j-1 to the scan pattern *)
        Copy(s,S1,i+1,j-i-1);
      END; (* IF *)
      (* Search colon *)
      REPEAT INC(j) UNTIL (j>len) OR (s[j]#' ');
      IF (j>len) THEN
        (* No colon -> No keyword expression *)
        scanAll := TRUE;
      ELSIF s[j]=':' THEN
        (* Keyword expression follows *)
        Delete(s,1,j); scanAll := FALSE;
      ELSE
        Err := TRUE; RETURN
      END; (* IF *)
      IF NOT scanAll THEN
        (* Clean up keyword expression *)
        i := 1;
        WHILE i<=ORD(s[0]) DO
          IF s[i]=' ' THEN Delete(s,i,1) 
          ELSIF NOT ValidParseChar(s[i]) THEN Err := TRUE; RETURN;
          ELSE INC(i) END;
        END; (* WHILE *)
        IF s[0]#0C THEN
          UpperCase(s); Evaluate(s, head);
          totalNo := zero; p := head;
          WHILE p#NIL DO p := p^.next; inc(totalNo) END;
        ELSE
          Err := TRUE; RETURN;
        END; (* IF *)
        (* Test each screen in list and remove it if it does not match *)
        Scan(S1, wordOnly, ignoreCase, TRUE, totalNo);
        NEW(p1); p1^.next := head; head := p1;
        p := head^.next; q := head;
        (** 13/06/87 : WHILE -> LOOP **)
        LOOP
          IF p = NIL THEN EXIT END;
          ClearMatchList;
          Search(p^.pos, page, fromL, fromP, toL, toP, same, fnd);
          fnd2 := fnd;
          IF fnd2 THEN
            LOOP
              AddMatch(page, fromL, fromP, toL, toP);
              Search(pos2, page, fromL, fromP, toL, toP, same, fnd2);
              IF NOT (same & fnd2) THEN EXIT END;
            END; (* LOOP *)
          END; (* IF *)  
          IF prompt & fnd THEN
            foundOne := TRUE;
            CASE Action(p^.pos) OF
              abortSearch :
                (* Remove current and all subsequent records *)
                (** 11/09/87 : current not removed **)
                q := p^.next; p^.next := NIL; fnd := FALSE;
                WHILE q # NIL DO p1 := q; q := q^.next; DISPOSE(p1) END;
                EXIT;  (** 13/06/87 **)
            | addScreen :
                (* Screen will be added below *)
            | ignoreScreen :
                fnd := FALSE; (* Fake it *)
            END; (* CASE *)
          END; (* IF *)
          IF NOT fnd THEN
            q^.next := p^.next; p1 := p; p := q^.next; 
            DISPOSE(p1);
          ELSE
            q := p; p := p^.next;
          END; (* IF *)
        END; (* LOOP*)
        p1 := head; head := head^.next; DISPOSE(p1);
      ELSE
        (* Scan all *)
        Scan(S1, wordOnly, ignoreCase, FALSE, zero);
        NEW(q); q^.next := NIL; head := q;
        buffered := FALSE;
        REPEAT
          ClearMatchList;
          IF NOT buffered THEN
            (** 23/05/87 : Replaced p^.pos by pos **)
            Search(pos, page, fromL, fromP, toL, toP, same, fnd);
          ELSE
            fnd := TRUE; buffered := FALSE;
            page := page2; fromL := fromL2; fromP := fromP2;
            toL := toL2; toP := toP2; pos := pos2;
          END; (* IF *)
          (** 26/03/88 removed : same := TRUE; fnd2 := fnd; **)
          IF fnd THEN
            LOOP
              AddMatch(page, fromL, fromP, toL, toP);
              Search(pos2, page2, fromL2, fromP2, toL2, toP2, same, fnd2);
              IF (same & fnd2) THEN
                pos := pos2; page := page2;
                fromL := fromL2; fromP := fromP2;
                toL := toL2; toP := toP2;
              ELSE
                buffered := fnd2; EXIT;
              END; (* IF *)
            END; (* LOOP *)
          END; (* IF *)
          (** 26/03/88 changed : fnd2 -> fnd **)
          IF fnd THEN
            IF prompt THEN
              foundOne := TRUE;
              (** 23/05/87 : dito **)
              ac := Action(pos);
              IF (ac # ignoreScreen) THEN
                (** 11/09/87 : current not removed **)
                NEW(p); q^.next := p; p^.next := NIL; p^.pos := pos; q := p;
                fnd := (ac # abortSearch);
              END; (* CASE *)
            ELSE
              NEW(p); q^.next := p; p^.next := NIL; p^.pos := pos; q := p;
            END; (* IF *)
          END; (* IF *)
        UNTIL NOT fnd;
        p := head^.next; DISPOSE(head); head := p;
      END; (* IF *)
    ELSE
      (* Keyword search only; Clean up keyword expression *)
      i := 1;
      WHILE i<=ORD(s[0]) DO
        IF s[i]=' ' THEN Delete(s,i,1) 
        ELSIF NOT ValidParseChar(s[i]) THEN Err := TRUE; RETURN;
        ELSE INC(i) END;
      END; (* WHILE *)
      IF s[0]#0C THEN
        UpperCase(s); Evaluate(s, head);
      ELSE
        Err := TRUE;
      END; (* IF *)
    END; (* IF *)
  END; (* IF *)
  (* Update the prev-fields *)
  found := (head # NIL);
  IF found THEN
    p := head; p^.prev := NIL; q := head^.next; 
    totalNo := one;
    WHILE q#NIL DO 
      q^.prev := p; p := q; q := q^.next;
      inc(totalNo);
    END; (* WHILE *)
    tail := p;
  ELSE
    totalNo := zero;
  END; (* IF *)
END Parse;




PROCEDURE NextScr(VAR pos:LONGCARD);
BEGIN
  current := current^.next; pos := current^.pos
END NextScr;


PROCEDURE PrevScr(VAR pos:LONGCARD);
BEGIN
  current := current^.prev; pos := current^.pos
END PrevScr;


PROCEDURE FirstScr(VAR pos:LONGCARD);
BEGIN
  current := head; pos := current^.pos
END FirstScr;


PROCEDURE LastScr(VAR pos:LONGCARD);
BEGIN
  current := tail; pos := current^.pos
END LastScr;


PROCEDURE CurrentScr(VAR pos:LONGCARD);
BEGIN
  pos := current^.pos
END CurrentScr;


PROCEDURE DeleteScr(p:LONGCARD);
(* Deletes screen p from linked list *)
VAR
  c,cur : RecPtr;
BEGIN
  cur := head; 
  WHILE (cur#NIL) & less(cur^.pos,p) DO
    cur := cur^.next;
  END;
  IF (cur#NIL) & equal(cur^.pos,p) THEN
    IF cur^.prev#NIL THEN
      cur^.prev^.next := cur^.next;
    ELSE 
      head := head^.next
    END;
    IF cur^.next#NIL THEN c := cur^.next; cur^.next^.prev := cur^.prev;
                     ELSE c := cur^.prev; tail := c;
    END;
    IF cur=current THEN
      current := c;
    END;
    DISPOSE(cur); dec(totalNo);
  END; (* IF *)
END DeleteScr;



PROCEDURE DisposeList;
(* Dispose all elements of linked list *)
BEGIN
  ClearTempList(head); totalNo := zero;
END DisposeList;



BEGIN
  NEW(testRes); testRes^.next := NIL;
  head := NIL; totalNo := zero;
  AssignAOC(allKW, '[ALL]');
END KWParser.
