(*****************************************************************************
*
* Name     : BTrees
*
* LastEdit : 18/04/87
* Project  : KeyDex system
* Purpose  : Implements insert/delete/lookup operations on B-Tree structures
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
* 18/04/87 : Removed lots of bugs in "DeleteList".
* 06/06/87 : Implemented date index procedures.
*            Put global variables into dynamic storage.
* 17/06/87 : Implemented "TemplateEntered".
*            Fixed bugs in "BrowseDate" and "BrowseKeyword".
* 28/06/87 : Bugs fixed in "Next/PrevKeyword".
* 30/06/87 : Bug fixed in "DeleteKeyword".
* 02/07/87 : "SaveNode" relocated in "Lookup" procedures.
* 12/07/87 : Bugs in Lookup procedures fixed. 
*            Index window procedures rewritten.
* 21/07/87 : Error handling implemented.
* 11/08/87 : Bugs in delete and insert procedures fixed.
* 18/08/87 : Browse procedures optimized.
* 22/08/87 : Bug fixed in 'TemplateEntered'.
* 23/08/87 : Bug which caused heap overflow fixed in delete procedures.
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

IMPLEMENTATION MODULE BTrees;

FROM SYSTEM IMPORT 
  ADR, TSIZE;

FROM KDStorage IMPORT
  ALLOCATE, DEALLOCATE;

FROM FileOp IMPORT
  LoadBlock, SaveBlock, LoadNode, SaveNode, empty, RootPos, GetNextBlockPos,
  DeleteBlock, DeleteNode, FileType, OpResult;

FROM KDConfig IMPORT
  Keyword;

FROM LongCardinals IMPORT
  LONGCARD, zero, equal, less;

FROM String IMPORT
  Assign, Clear, Compare;


CONST
  N = 4;   (* Order of B-Tree = Number of file entries / 2 *)
  B = 5;   (* Number of entries per linked block *)


TYPE 
  Index     = [1..N*2];
  HalfIndex = [1..N];
  SHORTCARD = CHAR;

  PtrToDateNode    = POINTER TO DateNode;
  PtrToKeywordNode = POINTER TO KeywordNode;

  (* File information *)
  KeywordEntry =
    RECORD
      key : Keyword; first : LONGCARD;
    END; (* RECORD *)
   
  DateEntry =
    RECORD
      key : CARDINAL; first : LONGCARD;
    END; (* RECORD *)

  KeywordNode = 
    RECORD    
      max : SHORTCARD; (* Amount of entries in this node *) 
      p0  : LONGCARD;  (* Pointers to subtrees *)
      CASE BOOLEAN OF  (* Keyword entries *)
        TRUE  : a : ARRAY Index OF KeywordEntry;
      | FALSE : aleft,aright : ARRAY HalfIndex OF KeywordEntry;
      END; (* CASE *)
      CASE BOOLEAN OF  (* Pointers to sons *)
        TRUE  : p : ARRAY Index OF LONGCARD;
      | FALSE : pleft,pright : ARRAY HalfIndex OF LONGCARD;
      END; (* CASE *)
    END; (* RECORD *)

  DateNode = 
    RECORD    
      max : SHORTCARD; (* Amount of entries in this node *) 
      p0  : LONGCARD;  (* Pointers to subtrees *)
      CASE BOOLEAN OF  (* Keyword entries *)
        TRUE  : a : ARRAY Index OF DateEntry;
      | FALSE : aleft,aright : ARRAY HalfIndex OF DateEntry;
      END; (* CASE *)
      CASE BOOLEAN OF  (* Pointers to sons *)
        TRUE  : p : ARRAY Index OF LONGCARD;
      | FALSE : pleft,pright : ARRAY HalfIndex OF LONGCARD;
      END; (* CASE *)
    END; (* RECORD *)

  Block = RECORD
            max : SHORTCARD;
            pos : ARRAY [1..B] OF LONGCARD; (* regular block *)
                  (* In case this is the root block, pos[1]=adr of last block *)
            next: LONGCARD;
          END;

  StackPtr = POINTER TO StackElt;   (* Stack structure used by index proc's *)
  StackElt = RECORD
               cur : CARDINAL;
               curpos : LONGCARD;
               next : StackPtr;
             END;

  MvType   = (dLeft, dRight);


VAR
  rootptr   : StackPtr;      (* Used by index procedures *)
  root,leaf,
  idle      : BOOLEAN;
  movement  : MvType;


  inde :
      POINTER TO 
        RECORD
          CASE BOOLEAN OF
            TRUE : nk:KeywordNode;
          |FALSE : nd:DateNode;
          END; (* CASE *)
        END; (* RECORD *)

  cur,cmax : CARDINAL;      (* cur points to the last entity returned *)
  curpos   : LONGCARD;
  linkblock : Block;      (* Current block *)
  linkcur   : SHORTCARD;  (* Position in block *)

  templateEstablished,
  templateInIndex      : BOOLEAN;


PROCEDURE InsertList(f:IndexType; first,p : LONGCARD); 
(* Inserts pos p into list f with first block at "f". *)
VAR
  root,last,new : LONGCARD;
  lastblock,newblock,rootblock : Block;
BEGIN
  root := first; LoadBlock(f,root,ADR(rootblock),TSIZE(Block));
  last := rootblock.pos[1];
  IF NOT OpResult() THEN
    RETURN
  ELSIF equal(root,last) THEN 
    lastblock := rootblock
  ELSE
    LoadBlock(f,last,ADR(lastblock),TSIZE(Block));
    IF NOT OpResult() THEN RETURN END;
  END;

  IF ORD(lastblock.max)<B THEN
    (* Insert into last block *)
    WITH lastblock DO INC(max); pos[ORD(max)] := p END;
    SaveBlock(f,ADR(lastblock),TSIZE(Block),last,FALSE);
  ELSE
    (* Create new node and append to end of list *)
    WITH newblock DO max := 1C; pos[1] := p; next := zero END;
    SaveBlock(f,ADR(newblock),TSIZE(Block),new,TRUE); 
    IF equal(root,last) THEN rootblock.next := new ELSE lastblock.next := new END;
    rootblock.pos[1] := new;
    SaveBlock(f,ADR(rootblock),TSIZE(Block),root,FALSE);
    IF NOT equal(root,last) THEN
      SaveBlock(f,ADR(lastblock),TSIZE(Block),last,FALSE)
    END; (* IF *)
  END; (* IF *)
END InsertList;



PROCEDURE DeleteList(f:IndexType; first,p : LONGCARD; VAR empty : BOOLEAN);
(* Deletes p from list with head at first. empty=T if p was the only entry *)
VAR
  root,b,b1   : Block;
  bp,bp1 : LONGCARD;  (* Positions of b and b1 *)
  i,j    : CARDINAL;  (* Offset in block *)
  rootSave : BOOLEAN;
BEGIN
  empty := FALSE; LoadBlock(f,first,ADR(root),TSIZE(Block)); i := 2;
  bp := first; bp1 := first; b := root;
  WHILE less(b.pos[i],p) & OpResult() DO
    INC(i); 
    IF i>ORD(b.max) THEN
      bp1 := bp; bp := b.next;
      IF equal(bp,zero) THEN RETURN (* Nothing found - ignore request *)
      ELSE
        b1 := b; LoadBlock(f,bp,ADR(b),TSIZE(Block)); i := 1;
      END;
    END; (* IF *)
  END; (* WHILE *)
  IF NOT (equal(p,b.pos[i]) & OpResult()) THEN RETURN END;

  (* Now, we found the position at pos[i] in b *)
  WITH b DO
    DEC(max); FOR j := i TO ORD(max) DO pos[j] := pos[j+1] END;
  END;
  IF b.max=0C THEN
    (* Dispose this block (this is certainly not the first one) *)
    rootSave := FALSE;
    IF equal(first,bp1) THEN root.next := b.next; rootSave := TRUE;
                        ELSE b1.next := b.next;
    END; (* IF *)
    IF equal(b.next,zero) THEN
      (* This was the last block in list *)
      root.pos[1] := bp1; rootSave := TRUE;
    END;
    IF rootSave OR equal(first,bp1) THEN
      SaveBlock(f,ADR(root),TSIZE(Block),first,FALSE);
    END;
    IF NOT equal(first,bp1) THEN
      SaveBlock(f,ADR(b1),TSIZE(Block),bp1,FALSE);
    END;
    DeleteBlock(f,bp);
  ELSIF equal(bp,first) AND (b.max=1C) THEN
    (* Dispose the root block, tricky *)
    empty := equal(root.next,zero);
    IF NOT empty THEN
      bp1 := root.next; LoadBlock(f,bp1,ADR(b1),TSIZE(Block)); 
      (* b1 is now the successor of b! *)
      IF NOT OpResult() THEN
        RETURN
      ELSIF ORD(b1.max)<B THEN
        (* Move all elements of b1 to root and delete b1 *)
        FOR j := 1 TO ORD(b1.max) DO root.pos[j+1] := b1.pos[j] END;
        root.max := CHR(ORD(b1.max)+1); root.next := b1.next; 
        DeleteBlock(f,bp1);
      ELSE
        (* Move B-1 elements from b1 to root, leaving one element in b1 *)
        FOR j := 1 TO B-1 DO root.pos[j+1] := b1.pos[j] END;
        b1.pos[1] := b1.pos[B]; b1.max := 1C; root.max := CHR(B);
        SaveBlock(f,ADR(b1),TSIZE(Block),bp1,FALSE);
      END; (* IF *)
      SaveBlock(f,ADR(root),TSIZE(Block),first,FALSE);
    ELSE
      DeleteBlock(f,first);
    END; (* IF *)
  ELSE
    SaveBlock(f,ADR(b),TSIZE(Block),bp,FALSE);
  END; (* IF *)
END DeleteList;



PROCEDURE CreateList
            ( f         : IndexType;
              VAR first : LONGCARD;
              p         : LONGCARD );
(* Creates new list with head at "first" and first entry pos *)
VAR 
  b : Block;
BEGIN
  WITH b DO max := 2C; next := zero; pos[2] := p END;
  (* Get EOF position into "first" *)
  GetNextBlockPos(f,first); b.pos[1] := first; 
  SaveBlock(f,ADR(b),TSIZE(Block),first,TRUE);
END CreateList;



PROCEDURE LookupKeyword
            ( VAR kw    : Keyword;
              VAR pos   : LONGCARD; 
              new       : BOOLEAN;
              VAR found : BOOLEAN );

  PROCEDURE ins(VAR k:KeywordEntry; VAR ptr:LONGCARD);
  (* new=TRUE: Trys to insert k into node ptr^. If successful then up=FALSE, 
     else up=TRUE and k contains a key and its associated pointer which must
     be inserted by the caller. found=TRUE if filename was already entered.
     new=FALSE: Searches for k. "found" returns the result of the operation *)
  VAR
    j       : INTEGER;
    pj,ptr1 : LONGCARD;
    pr      : PtrToKeywordNode;
  BEGIN
    IF equal(ptr,zero) THEN
      (* At leaf level and k not found. Return k to caller if insert mode *)
      leaf := TRUE; up := new; 
    ELSE
      NEW(pr); LoadNode(KeywordIndex, ptr, pr, TSIZE(KeywordNode)); ptr1 := ptr;
      IF (NOT OpResult()) OR (pr^.max > CHR(2*N)) THEN DISPOSE(pr); RETURN END;
      WITH pr^ DO
        (* Binary search for k *)
        l := 1; j := ORD(max);
        REPEAT
          m := (l+j) DIV 2; i := Compare(k.key,a[m].key);
          IF i<=0 THEN j := m-1 END; IF i>=0 THEN l := m+1 END;
        UNTIL (l>j);
        IF (l>j+1) THEN (* Keyword entry exists *)
          found := TRUE;
          IF new THEN 
            InsertList(KeywordIndex,a[m].first,pos)
          ELSE
            pos := a[m].first
          END; (* IF *)
        ELSE
          (* Search son of current node for k *)
          IF (j#0) THEN pj := p[j] ELSE pj := p0 END; ins(k,pj);
          IF up THEN
            IF leaf THEN CreateList(KeywordIndex,k.first,pos); leaf := FALSE END;
            (* k has been inserted into a full node and we have to handle the
               resulting k and its associated pointer ptr *)
            IF (ORD(max)<N*2) THEN
              (* k can be inserted into current node between j and j+1 *)
              FOR i := ORD(max) TO j+1 BY -1 DO a[i+1] := a[i]; p[i+1] := p[i] END;
              INC(max); a[j+1] := k; p[j+1] := pj; up := FALSE;
            ELSE
              (* This node is full, create a new one and load it with N elements
                 of current node *)
              NEW(n); 
              n^.aleft := aright; n^.pleft := pright;
              n^.max := CHR(N); max := CHR(N);
              (** 10/08/87 : removed n^.p0 := pj; *)
              (* Decide whether to insert k into ptr^ or n2^ *)
              IF (j <= N) THEN
                (* Insert into ptr^ and assign a[N+1] to k *)
                FOR i := N TO j+1 BY -1 DO a[i+1] := a[i]; p[i+1] := p[i]; END;
                a[j+1] := k; p[j+1] := pj; k := a[N+1];
                (** 10/08/87 **)
                n^.p0 := p[N+1];
              ELSE
                (* Insert into n^ and assign a[1] to k *)
                WITH n^ DO
                  (** 10/08/87 **)
                  (* Insert *)
                  DEC(j, N);
                  FOR i := N TO j+1 BY -1 DO a[i+1] := a[i]; p[i+1] := p[i]; END;
                  a[j+1] := k; p[j+1] := pj;
                  k := a[1]; 
                  (* Move all to the left (p0 not occupied yet) *)
                  p0 := p[1];
                  FOR i := 1 TO N DO a[i] := a[i+1]; p[i] := p[i+1] END;
                END; (* WITH *)
              END; (* IF *)
              (* replaces ptr:=npos *)
              SaveNode(KeywordIndex,n,TSIZE(KeywordNode),ptr,TRUE); 
              DISPOSE(n);
            END;
            SaveNode(KeywordIndex,pr,TSIZE(KeywordNode),ptr1,FALSE);
          END;
          (** 02/07/87 : move upwards **)
        END;
      END;
      DISPOSE(pr);
    END;
  END ins;

VAR
  r1,
  oldroot : LONGCARD;
  n       : PtrToKeywordNode;
  up,leaf : BOOLEAN;
  i,l,m   : INTEGER;
  k       : KeywordEntry;
BEGIN (* LookupKeyword *)
  k.key := kw; r1 := RootPos; found := FALSE;
  (* Check if this is a template *)
  IF new & (kw[1] = '$') THEN
    templateInIndex := TRUE; templateEstablished := TRUE;
  END; (* IF *)
  IF empty[KeywordIndex] THEN 
    IF new THEN
      NEW(n);
      SaveNode(KeywordIndex,n,TSIZE(KeywordNode),r1,TRUE);
      IF OpResult() THEN
        CreateList(KeywordIndex,k.first,pos); 
        IF OpResult() THEN
          WITH n^ DO a[1] := k; max := 1C; p0 := zero; p[1] := zero END;
          SaveNode(KeywordIndex,n,TSIZE(KeywordNode),r1,FALSE);
        END; (* IF *)
      END; (* IF *)
      DISPOSE(n);
    END; (* IF *)
  ELSE
    up := FALSE; leaf := FALSE; ins(k,r1);
    IF up THEN
      (* Root overflow; relocate old root *)
      NEW(n);
      LoadNode(KeywordIndex,RootPos,n,TSIZE(KeywordNode)); 
      IF OpResult() & (n^.max <= CHR(2*N)) THEN
        SaveNode(KeywordIndex,n,TSIZE(KeywordNode),oldroot,TRUE);
        IF OpResult() THEN
          (* Create new root *)
          WITH n^ DO a[1] := k; max := 1C; p0 := oldroot; p[1] := r1 END;
          SaveNode(KeywordIndex,n,TSIZE(KeywordNode),RootPos,FALSE); 
        END; (* IF *)
      END; (* IF *)
      DISPOSE(n);
    END; (* IF *)
  END; (* IF *)
END LookupKeyword;



PROCEDURE LookupDate
            ( kw        : CARDINAL;
              VAR pos   : LONGCARD; 
              new       : BOOLEAN;
              VAR found : BOOLEAN );

  PROCEDURE ins(VAR k:DateEntry; VAR ptr:LONGCARD);
  (* new=TRUE: Trys to insert k into node ptr^. If successful then up=FALSE, 
     else up=TRUE and k contains a key and its associated pointer which must
     be inserted by the caller. found=TRUE if filename was already entered.
     new=FALSE: Searches for k. "found" returns the result of the operation *)
  VAR
    j    : INTEGER;
    pj,
    ptr1 : LONGCARD;
    pr   : PtrToDateNode;
  BEGIN
    IF equal(ptr,zero) THEN
      (* At leaf level and k not found. Return k to caller if insert mode *)
      leaf := TRUE; up := new;
    ELSE
      NEW(pr); LoadNode(DateIndex, ptr, pr, TSIZE(DateNode)); ptr1 := ptr;
      IF (NOT OpResult()) OR (pr^.max > CHR(2*N)) THEN DISPOSE(pr); RETURN END;
      WITH pr^ DO
        (* Binary search for k *)
        l := 1; j := ORD(max);
        REPEAT
          m := (l+j) DIV 2;
          IF k.key <= a[m].key THEN j := m-1 END; 
          IF k.key >= a[m].key THEN l := m+1 END;
        UNTIL (l>j);
        IF (l>j+1) THEN (* Keyword entry exists *)
          found := TRUE;
          IF new THEN 
            InsertList(DateIndex, a[m].first,pos) 
          ELSE
            pos := a[m].first 
          END; (* IF *)
        ELSE
          (* Search son of current node for k *)
          IF (j#0) THEN pj := p[j] ELSE pj := p0 END; ins(k,pj);
          IF up THEN
            IF leaf THEN CreateList(DateIndex, k.first,pos); leaf := FALSE END;
            (* k has been inserted into a full node and we have to handle the
               resulting k and its associated pointer ptr *)
            IF (ORD(max)<N*2) THEN
              (* k can be inserted into current node between j and j+1 *)
              FOR i := ORD(max) TO j+1 BY -1 DO a[i+1] := a[i]; p[i+1] := p[i] END;
              INC(max); a[j+1] := k; p[j+1] := pj; up := FALSE;
            ELSE
              (* This node is full, create a new one and load it with N elements
                 of current node *)
              NEW(n); 
              n^.aleft := aright; n^.pleft := pright;
              n^.max := CHR(N); max := CHR(N); 
              (** 10/08/87 : removed n^.p0 := pj; *)
              (* Decide whether to insert k into ptr^ or n2^ *)
              IF (j <= N) THEN
                (* Insert into ptr^ and assign a[N+1] to k *)
                FOR i := N TO j+1 BY -1 DO a[i+1] := a[i]; p[i+1] := p[i]; END;
                a[j+1] := k; p[j+1] := pj; k := a[N+1];
                (** 10/08/87 **)
                n^.p0 := p[N+1];
              ELSE
                (* Insert into n^ and assign a[1] to k *)
                WITH n^ DO
                  (** 10/08/87 **)
                  (* Insert *)
                  DEC(j, N);
                  FOR i := N TO j+1 BY -1 DO a[i+1] := a[i]; p[i+1] := p[i]; END;
                  a[j+1] := k; p[j+1] := pj;
                  k := a[1]; 
                  (* Move all to the left (p0 not occupied yet) *)
                  p0 := p[1];
                  FOR i := 1 TO N DO a[i] := a[i+1]; p[i] := p[i+1] END;
                END; (* WITH *)
              END; (* IF *)
              (* replaces ptr:=npos *)
              SaveNode(DateIndex, n,TSIZE(DateNode),ptr,TRUE);  
              DISPOSE(n);
            END; (* IF *)
            SaveNode(DateIndex, pr,TSIZE(DateNode),ptr1,FALSE);
          END; (* IF *)
          (** 02/07/87 : moved upwards **)
        END; (* IF *)
      END; (* WITH *)
      DISPOSE(pr);
    END; (* IF *)
  END ins;

VAR
  r1,
  oldroot : LONGCARD;
  n       : PtrToDateNode;
  up,leaf : BOOLEAN;
  i,l,m   : INTEGER;
  k       : DateEntry;
BEGIN (* LookupDate *)
  k.key := kw; r1 := RootPos; found := FALSE;
  IF empty[DateIndex] THEN 
    IF new THEN
      NEW(n);
      SaveNode(DateIndex,n,TSIZE(DateNode),r1,TRUE); 
      IF OpResult() THEN
        CreateList(DateIndex,k.first,pos); 
        IF OpResult() THEN
          WITH n^ DO a[1] := k; max := 1C; p0 := zero; p[1] := zero END;
          SaveNode(DateIndex,n,TSIZE(DateNode),r1,FALSE);
        END; (* IF *)
      END; (* IF *)
      DISPOSE(n);
    END; (* IF *)
  ELSE
    up := FALSE; leaf := FALSE; ins(k,r1);
    IF up THEN
      (* Root overflow; relocate old root *)
      NEW(n);
      LoadNode(DateIndex,RootPos,n,TSIZE(DateNode)); 
      IF OpResult() & (n^.max <= CHR(2*N)) THEN
        SaveNode(DateIndex,n,TSIZE(DateNode),oldroot,TRUE);
        IF OpResult() THEN
          (* Create new root *)
          WITH n^ DO a[1] := k; max := 1C; p0 := oldroot; p[1] := r1 END;
          SaveNode(DateIndex,n,TSIZE(DateNode),RootPos,FALSE); 
        END; (* IF *)
      END; (* IF *)
      DISPOSE(n);
    END; (* IF *)
  END; (* IF *)
END LookupDate;



PROCEDURE DeleteKeyword(VAR kw:Keyword; listpos:LONGCARD);
(* Delete entry "pos" from the entire B-Tree *)

  PROCEDURE underflow(p,q:PtrToKeywordNode; i:CARDINAL);
  (* Handles underflow of q with predecessor p. q=p.ptr[i]^. p is never
     saved. *)
  VAR
    r    : PtrToKeywordNode;
    qpos,
    rpos : LONGCARD;
    j    : CARDINAL;
  BEGIN
    IF i>0 THEN
      IF i>1 THEN rpos := p^.p[i-1] ELSE rpos := p^.p0 END;
      NEW(r); LoadNode(KeywordIndex,rpos,r,TSIZE(KeywordNode));
      IF (NOT OpResult()) OR (r^.max > CHR(2*N)) THEN 
        DISPOSE(q); DISPOSE(r); RETURN 
      END; (* IF *)
      IF ORD(r^.max)>N THEN
        (* Rotation r -> p -> q *)
        WITH q^ DO
          FOR j := (N-1) TO 1 BY -1 DO a[j+1] := a[j]; p[j+1] := p[j] END;
          p[1] := p0; p0 := r^.p[ORD(r^.max)];
          max := CHR(N);
        END;
        q^.a[1] := p^.a[i];
        p^.a[i] := r^.a[ORD(r^.max)]; DEC(r^.max);
        SaveNode(KeywordIndex,q,TSIZE(KeywordNode),p^.p[i],FALSE);
        SaveNode(KeywordIndex,r,TSIZE(KeywordNode),rpos,FALSE);
        uf := FALSE;
      ELSE
        (* Merge nodes q and r *)
        qpos := p^.p[i];
        r^.a[N+1] := p^.a[i]; r^.p[N+1] := q^.p0;
        WITH r^ DO
          FOR j := 1 TO N-1 DO 
            a[(N+1)+j] := q^.a[j]; p[(N+1)+j] := q^.p[j];
          END;
          max := CHR(2*N);
        END; (* WITH *)
        WITH p^ DO
          DEC(max);
          FOR j := i TO ORD(max) DO a[j] := a[j+1]; p[j] := p[j+1] END;
          uf := (ORD(max)<N);
        END; (* WITH *)
        DeleteNode(KeywordIndex,qpos);
        SaveNode(KeywordIndex,r,TSIZE(KeywordNode),rpos,FALSE);
      END; (* IF *)
    ELSE (* i=0 *)
      rpos := p^.p[1];
      NEW(r); LoadNode(KeywordIndex,rpos,r,TSIZE(KeywordNode));
      IF (NOT OpResult()) OR (r^.max > CHR(2*N)) THEN
        DISPOSE(q); DISPOSE(r); RETURN
      END; (* IF *)
      IF ORD(r^.max)>N THEN
        (* Rotation q <- p <- r *)
        q^.max := CHR(N);
        q^.a[N] := p^.a[1]; q^.p[N] := r^.p0;
        p^.a[1] := r^.a[1];
        WITH r^ DO
          p0 := p[1]; DEC(max);
          FOR j := 1 TO ORD(max) DO a[j] := a[j+1]; p[j] := p[j+1] END;
        END;
        (** 30/06/87 : P^.P[I] replaced by p^.p0 **)
        SaveNode(KeywordIndex,q,TSIZE(KeywordNode),p^.p0,FALSE);
        SaveNode(KeywordIndex,r,TSIZE(KeywordNode),rpos,FALSE);
        uf := FALSE;
      ELSE
        (* Merge nodes q and r *)
        q^.a[N] := p^.a[1]; q^.p[N] := r^.p0;
        WITH q^ DO
          FOR j := 1 TO N DO a[N+j] := r^.a[j]; p[N+j] := r^.p[j] END;
          max := CHR(2*N);
        END;
        WITH p^ DO
          DEC(max); 
          FOR j := 1 TO ORD(max) DO a[j] := a[j+1]; p[j] := p[j+1] END;
          uf := (ORD(max)<N);
        END;
        DeleteNode(KeywordIndex,rpos); 
        SaveNode(KeywordIndex,q,TSIZE(KeywordNode),p^.p0,FALSE);
      END;
    END; (* IF *)
    DISPOSE(q); DISPOSE(r);
  END underflow;

  PROCEDURE GetRightMost
              ( ppos     : LONGCARD;
                VAR next : PtrToKeywordNode; 
                VAR s    : KeywordEntry );
  (* Gets and deletes rightmost element of tree with root at ppos *)
  VAR
    n,nxt : PtrToKeywordNode;
  BEGIN
    NEW(n); LoadNode(KeywordIndex,ppos,n,TSIZE(KeywordNode));
    IF (NOT OpResult()) OR (n^.max > CHR(2*N)) THEN DISPOSE(n); RETURN END;
    IF equal(n^.p0,zero) THEN
      WITH n^ DO
        s := a[ORD(max)]; DEC(max); uf := (ORD(max)<N);
      END; (* WITH *)
    ELSE
      WITH n^ DO
        GetRightMost(p[ORD(max)],nxt,s);
        IF uf THEN underflow(n,nxt,ORD(max)) END;
      END; (* WITH *)
    END; (* IF *)
    IF uf THEN next := n
          ELSE SaveNode(KeywordIndex,n,TSIZE(KeywordNode),ppos,FALSE); DISPOSE(n)
    END;
  END GetRightMost;
       
  PROCEDURE del(pos:LONGCARD; VAR nmax:CARDINAL; VAR n:PtrToKeywordNode);
  (* Deletes kw from B-Tree with root at pos. Returns no. of elts. in pos.
     If underflow: returns pointer to pos in n *)
  VAR
    nxt   : PtrToKeywordNode;
    q     : LONGCARD;
    i,l,r,k : CARDINAL;
  BEGIN
    IF equal(pos,zero) THEN RETURN (* Nothing found - ignore request *)
    ELSE
      NEW(n); LoadNode(KeywordIndex,pos,n,TSIZE(KeywordNode));
      IF (NOT OpResult()) OR (n^.max > CHR(2*N)) THEN DISPOSE(n); RETURN END;
      WITH n^ DO
        (* Binary search *)
        l := 1; r := ORD(max);
        REPEAT
          k := (l+r) DIV 2; c := Compare(kw,a[k].key);
          IF c<=0 THEN r := k-1 END;
          IF c>=0 THEN l := k+1 END;
        UNTIL l>r;
        IF r=0 THEN q := p0 ELSE q := p[r] END;
        IF l>r+1 THEN
          (* found; try to erase listpos in list *)
          DeleteList(KeywordIndex,a[k].first,listpos,empty); nmax := ORD(max);
          IF NOT empty THEN (** 10/08/87 **) DISPOSE(n); RETURN END;
          IF equal(q,zero) THEN
            (* leaf *)
            DEC(max); DEC(nmax); uf := (nmax<N);
            FOR i := k TO ORD(max) DO a[i] := a[i+1]; p[i] := p[i+1] END;
            IF NOT uf THEN 
              SaveNode(KeywordIndex,n,TSIZE(KeywordNode),pos,FALSE)
            END; (* IF *)
          ELSE
            GetRightMost(q,nxt,s); a[k] := s;
            IF uf THEN
              (* Attention: changes uf! *)
              underflow(n,nxt,r);  (* 29/03/87: r replaces k *)
            END; (* IF *)
            (** 10/08/87 : Moved out of previous block **)
            IF NOT uf THEN 
              SaveNode(KeywordIndex,n,TSIZE(KeywordNode),pos,FALSE)
            END; (* IF *)
            nmax := ORD(max);
          END; (* IF *)
        ELSE
          del(q,nmax,nxt); 
          IF uf THEN 
            underflow(n,nxt,r);
            IF NOT uf THEN 
              SaveNode(KeywordIndex,n,TSIZE(KeywordNode),pos,FALSE)
            END; (* IF *)
          END; (* IF *)
          nmax := ORD(max);
        END; (* IF *)
      END; (* WITH *)
      (** 23/08/87 : "nmax >= N" replaced by "NOT uf" **)
      IF NOT uf THEN DISPOSE(n) END;
    END; (* IF *)
  END del;

VAR
  rt    : KeywordNode;
  next  : LONGCARD;
  nmax  : CARDINAL;
  c     : INTEGER;
  s     : KeywordEntry;
  empty : BOOLEAN;
  uf    : BOOLEAN;
  n     : PtrToKeywordNode;
BEGIN (* DeleteKeyword *)
  (* Check if template *)
  IF (kw[1] = '$') THEN templateEstablished := FALSE END;
  uf := FALSE; del(RootPos,nmax,n);
  IF uf THEN
    IF (nmax=0) THEN
      (* Remove root node *)
      next := n^.p0;
      IF equal(next,zero) THEN
        DeleteNode(KeywordIndex,RootPos); 
      ELSE
        LoadNode(KeywordIndex,next,ADR(rt),TSIZE(KeywordNode)); 
        SaveNode(KeywordIndex,ADR(rt),TSIZE(KeywordNode),RootPos,FALSE);
        DeleteNode(KeywordIndex,next);
      END;
    ELSE
      SaveNode(KeywordIndex,n,TSIZE(KeywordNode),RootPos,FALSE);
    END; (* IF *)
    DISPOSE(n);
  END; (* IF *)
END DeleteKeyword;



PROCEDURE DeleteDate(kw:CARDINAL; listpos:LONGCARD);
(* Delete entry "pos" from the entire B-Tree *)

  PROCEDURE underflow(p,q:PtrToDateNode; i:CARDINAL);
  (* Handles underflow of q with predecessor p. q=p.ptr[i]^. p is never
     saved. *)
  VAR
    r    : PtrToDateNode;
    qpos,
    rpos : LONGCARD;
    j    : CARDINAL;
  BEGIN
    IF i>0 THEN
      IF i>1 THEN rpos := p^.p[i-1] ELSE rpos := p^.p0 END;
      NEW(r); LoadNode(DateIndex,rpos,r,TSIZE(DateNode));
      IF (NOT OpResult()) OR (r^.max > CHR(2*N)) THEN 
        DISPOSE(q); DISPOSE(r); RETURN
      END; (* IF *)
      IF ORD(r^.max)>N THEN
        (* Rotation r -> p -> q *)
        WITH q^ DO
          FOR j := (N-1) TO 1 BY -1 DO a[j+1] := a[j]; p[j+1] := p[j] END;
          p[1] := p0; p0 := r^.p[ORD(r^.max)];
          max := CHR(N);
        END;
        q^.a[1] := p^.a[i];
        p^.a[i] := r^.a[ORD(r^.max)]; DEC(r^.max);
        SaveNode(DateIndex,q,TSIZE(DateNode),p^.p[i],FALSE);
        SaveNode(DateIndex,r,TSIZE(DateNode),rpos,FALSE);
        uf := FALSE;
      ELSE
        (* Merge nodes q and r *)
        qpos := p^.p[i];
        r^.a[N+1] := p^.a[i]; r^.p[N+1] := q^.p0;
        WITH r^ DO
          FOR j := 1 TO N-1 DO 
            a[(N+1)+j] := q^.a[j]; p[(N+1)+j] := q^.p[j];
          END;
          max := CHR(2*N);
        END; (* WITH *)
        WITH p^ DO
          DEC(max);
          FOR j := i TO ORD(max) DO a[j] := a[j+1]; p[j] := p[j+1] END;
          uf := (ORD(max)<N);
        END; (* WITH *)
        DeleteNode(DateIndex,qpos);
        SaveNode(DateIndex,r,TSIZE(DateNode),rpos,FALSE);
      END; (* IF *)
    ELSE (* i=0 *)
      rpos := p^.p[1];
      NEW(r); LoadNode(DateIndex,rpos,r,TSIZE(DateNode));
      IF (NOT OpResult()) OR (r^.max > CHR(2*N)) THEN
        DISPOSE(q); DISPOSE(r); RETURN
      END; (* IF *)
      IF ORD(r^.max)>N THEN
        (* Rotation q <- p <- r *)
        q^.max := CHR(N);
        q^.a[N] := p^.a[1]; q^.p[N] := r^.p0;
        p^.a[1] := r^.a[1];
        WITH r^ DO
          p0 := p[1]; DEC(max);
          FOR j := 1 TO ORD(max) DO a[j] := a[j+1]; p[j] := p[j+1] END;
        END;
        (** 30/06/87 : P[I] replaced by p0 **)
        SaveNode(DateIndex,q,TSIZE(DateNode),p^.p0,FALSE);
        SaveNode(DateIndex,r,TSIZE(DateNode),rpos,FALSE);
        uf := FALSE;
      ELSE
        (* Merge nodes q and r *)
        q^.a[N] := p^.a[1]; q^.p[N] := r^.p0;
        WITH q^ DO
          FOR j := 1 TO N DO a[N+j] := r^.a[j]; p[N+j] := r^.p[j] END;
          max := CHR(2*N);
        END;
        WITH p^ DO
          DEC(max); 
          FOR j := 1 TO ORD(max) DO a[j] := a[j+1]; p[j] := p[j+1] END;
          uf := (ORD(max)<N);
        END;
        DeleteNode(DateIndex,rpos); 
        SaveNode(DateIndex,q,TSIZE(DateNode),p^.p0,FALSE);
      END;
    END; (* IF *)
    DISPOSE(q); DISPOSE(r);
  END underflow;

  PROCEDURE GetRightMost
              ( ppos     : LONGCARD;
                VAR next : PtrToDateNode; 
                VAR s    : DateEntry);
  (* Gets and deletes rightmost element of tree with root at ppos *)
  VAR
    n,nxt : PtrToDateNode;
  BEGIN
    NEW(n); LoadNode(DateIndex,ppos,n,TSIZE(DateNode));
    IF (NOT OpResult()) OR (n^.max > CHR(2*N)) THEN DISPOSE(n); RETURN END;
    IF equal(n^.p0,zero) THEN
      WITH n^ DO
        s := a[ORD(max)]; DEC(max); uf := (ORD(max)<N);
      END; (* WITH *)
    ELSE
      WITH n^ DO
        GetRightMost(p[ORD(max)],nxt,s);
        IF uf THEN underflow(n,nxt,ORD(max)) END;
      END; (* WITH *)
    END; (* IF *)
    IF uf THEN next := n
          ELSE SaveNode(DateIndex,n,TSIZE(DateNode),ppos,FALSE); DISPOSE(n)
    END;
  END GetRightMost;

  PROCEDURE del(pos:LONGCARD; VAR nmax:CARDINAL; VAR n:PtrToDateNode);
  (* Deletes kw from B-Tree with root at pos. Returns no. of elts. in pos.
     If underflow: returns pointer to pos in n *)
  VAR
    nxt   : PtrToDateNode;
    q     : LONGCARD;
    i,l,r,k : CARDINAL;
  BEGIN
    IF equal(pos,zero) THEN RETURN (* Nothing found - ignore request *)
    ELSE
      NEW(n); LoadNode(DateIndex,pos,n,TSIZE(DateNode));
      IF (NOT OpResult()) OR (n^.max > CHR(2*N)) THEN DISPOSE(n); RETURN END;
      WITH n^ DO
        (* Binary search *)
        l := 1; r := ORD(max);
        REPEAT
          k := (l+r) DIV 2;
          IF kw <= a[k].key THEN r := k-1 END;
          IF kw >= a[k].key THEN l := k+1 END;
        UNTIL l>r;
        IF r=0 THEN q := p0 ELSE q := p[r] END;
        IF l>r+1 THEN
          (* found; try to erase listpos in list *)
          DeleteList(DateIndex,a[k].first,listpos,empty); nmax := ORD(max);
          IF NOT empty THEN (** 10/08/87 **) DISPOSE(n); RETURN END;
          IF equal(q,zero) THEN
            (* leaf *)
            DEC(max); DEC(nmax); uf := (nmax<N);
            FOR i := k TO ORD(max) DO a[i] := a[i+1]; p[i] := p[i+1] END;
            IF NOT uf THEN 
              SaveNode(DateIndex,n,TSIZE(DateNode),pos,FALSE) 
            END; (* IF *)
          ELSE
            GetRightMost(q,nxt,s); a[k] := s;
            IF uf THEN
              (* Attention : changes uf! *)
              underflow(n,nxt,r);  (* 29/03/87: r replaces k *)
            END; (* IF *)
            (** 10/08/87 : Moved following IF out of previous block **)
            IF NOT uf THEN
              SaveNode(DateIndex,n,TSIZE(DateNode),pos,FALSE);
            END; (* IF *)
            nmax := ORD(max);
          END; (* IF *)
        ELSE
          del(q,nmax,nxt); 
          IF uf THEN 
            underflow(n,nxt,r);
            IF NOT uf THEN 
              SaveNode(DateIndex,n,TSIZE(DateNode),pos,FALSE) 
            END; (* IF *)
          END; (* IF *)  
          nmax := ORD(max);
        END; (* IF *)
      END; (* WITH *)
      (** 23/08/87 : "nmax >= N" replaced by "NOT uf" **)
      IF NOT uf THEN DISPOSE(n) END;
    END; (* IF *)
  END del;

VAR
  rt    : DateNode;
  next  : LONGCARD;
  nmax  : CARDINAL;
  s     : DateEntry;
  empty : BOOLEAN;
  uf    : BOOLEAN;
  n     : PtrToDateNode;
BEGIN (* DeleteDate *)
  uf := FALSE; del(RootPos,nmax,n);
  IF uf THEN
    IF (nmax=0) THEN
      (* Remove root node *)
      next := n^.p0;
      IF equal(next,zero) THEN
        DeleteNode(DateIndex,RootPos); 
      ELSE
        LoadNode(DateIndex,next,ADR(rt),TSIZE(DateNode)); 
        SaveNode(DateIndex,ADR(rt),TSIZE(DateNode),RootPos,FALSE);
        DeleteNode(DateIndex,next);
      END;
    ELSE
      SaveNode(DateIndex,n,TSIZE(DateNode),RootPos,FALSE);
    END; (* IF *)
    DISPOSE(n);
  END; (* IF *)
END DeleteDate;



PROCEDURE BrowseKeywords(VAR first : Keyword; kwp:kwproc);

  PROCEDURE brw(pos:LONGCARD; VAR res:BOOLEAN);
  VAR
    n   : PtrToKeywordNode;
    i,j : CARDINAL;
  BEGIN
    res := FALSE;
    IF NOT equal(pos,zero) THEN
      NEW(n); LoadNode(KeywordIndex,pos,n,TSIZE(KeywordNode)); 
      IF (NOT OpResult()) OR (n^.max > CHR(2*N)) THEN DISPOSE(n); RETURN END;
      WITH n^ DO
        IF starting THEN
          (* binary search *)
          i := ORD(max);
          WHILE (i # 0) & (Compare(a[i].key, first) >= 0) DO DEC(i) END;
          j := i + 1;
          IF i # 0 THEN brw(p[i], res) ELSE brw(p0, res) END;
        ELSE
          j := 1; brw(p0, res);
        END; (* IF *)
        IF res THEN
          FOR i := j TO ORD(max) DO
            WITH a[i] DO res := kwp(key,first) END;
            IF res THEN brw(p[i],res) END;
            IF NOT res THEN (** 23/08/87 **) DISPOSE(n); RETURN END;
          END; (* FOR *)
        END; (* IF *)
      END; (* WITH *)
      DISPOSE(n); 
    ELSE
      starting := FALSE;
    END; (* IF *)
    res := TRUE;
  END brw;

VAR
  res,
  starting : BOOLEAN;
BEGIN 
  (** 17/06/87 : IF added **)
  IF NOT empty[KeywordIndex] THEN
    starting := TRUE;
    brw(RootPos,res)
  END; (* IF *)
END BrowseKeywords;



PROCEDURE BrowseDates(first : CARDINAL; dp:dateproc);

  PROCEDURE brw(pos:LONGCARD; VAR res:BOOLEAN);
  VAR
    n   : PtrToDateNode;
    i,j : CARDINAL;
  BEGIN
    res := FALSE;
    IF NOT equal(pos,zero) THEN
      NEW(n); LoadNode(DateIndex,pos,n,TSIZE(DateNode)); 
      IF (NOT OpResult()) OR (n^.max > CHR(2*N)) THEN DISPOSE(n); RETURN END;
      WITH n^ DO
        IF starting THEN
          (* binary search *)
          i := ORD(max);
          WHILE (i # 0) & (a[i].key >= first) DO DEC(i) END;
          j := i + 1;
          IF i # 0 THEN brw(p[i], res) ELSE brw(p0, res) END;
        ELSE
          j := 1; brw(p0, res);
        END; (* IF *)
        IF res THEN
          FOR i := j TO ORD(max) DO
            WITH a[i] DO res := dp(key,first) END;
            IF res THEN brw(p[i],res) END;
            IF NOT res THEN (** 23/08/87 **) DISPOSE(n); RETURN END;
          END; (* FOR *)
        END; (* IF *)
      END; (* WITH *)
      DISPOSE(n);
    ELSE
      starting := FALSE;
    END; (* IF *)
    res := TRUE;
  END brw;

VAR
  res,
  starting : BOOLEAN;
BEGIN 
  (** 17/06/87 : IF added **)
  IF NOT empty[DateIndex] THEN
    starting := TRUE;
    brw(RootPos,res)
  END; (* IF *)
END BrowseDates;



PROCEDURE Push;
(* Pushes cur and curpos onto the stack *)
VAR
  p : StackPtr;
BEGIN
  NEW(p); p^.cur := cur; p^.curpos := curpos; p^.next := rootptr; rootptr := p;
END Push;


PROCEDURE Pop(f:IndexType);
(* Pops cur and curpos from the stack, sets root *)
VAR
  p    : StackPtr;
  size : CARDINAL;
BEGIN
  leaf := FALSE;
  p := rootptr; rootptr := rootptr^.next;
  cur := p^.cur; curpos := p^.curpos; DISPOSE(p);
  IF f=KeywordIndex THEN
    size := TSIZE(KeywordNode)
  ELSE
    size := TSIZE(DateNode)
  END; (* IF *)
  (** 20/06/87 : ADR(n) replaced by inde **)
  LoadNode(f,curpos, inde, size);
  IF OpResult() THEN
    root := (rootptr=NIL); cmax := ORD(inde^.nk.max);
    IF cmax > 2*N THEN cmax := 0 END;
  ELSE
    cmax := 0
  END; (* IF *)
END Pop;



PROCEDURE KwLeftDescend;
(* Descends to the lowest left edge of tree at p[cur] *)
VAR
  ptr : LONGCARD;
BEGIN
  (* We are not at a leaf *)
  IF cur=0 THEN ptr := inde^.nk.p0 ELSE ptr := inde^.nk.p[cur] END;
  Push; LoadNode(KeywordIndex, ptr, inde, TSIZE(KeywordNode));
  curpos := ptr; cur := 0;
  WHILE   (NOT equal(inde^.nk.p0,zero)) 
        & OpResult() 
        & (inde^.nk.max <= CHR(2*N))
  DO
    ptr := inde^.nk.p0; Push;
    LoadNode(KeywordIndex, ptr, inde, TSIZE(KeywordNode));
    curpos := ptr;
  END; (* WHILE *)
  IF OpResult() THEN
    leaf := TRUE; root := FALSE; cmax := ORD(inde^.nk.max);
    IF cmax > 2*N THEN cmax := 0 END;
  ELSE
    cmax := 0
  END; (* IF *)
END KwLeftDescend;


PROCEDURE KwRightDescend;
(* Descends to the lowest right edge of tree at p[cur] *)
VAR
  ptr : LONGCARD;
BEGIN
  IF (cur = 0) THEN ptr := inde^.nk.p0 ELSE ptr := inde^.nk.p[cur] END;
  Push; LoadNode(KeywordIndex, ptr,inde,TSIZE(KeywordNode));
  curpos := ptr;
  WHILE  (NOT equal(inde^.nk.p0,zero)) 
       & OpResult()
       & (cur <= 2*N)
  DO
    cur := ORD(inde^.nk.max); ptr := inde^.nk.p[cur]; Push;
    LoadNode(KeywordIndex,ptr,inde, TSIZE(KeywordNode));
    curpos := ptr;
  END; (* WHILE *)
  IF OpResult() THEN
    leaf := TRUE; root := FALSE; cmax := ORD(inde^.nk.max);
    IF cmax > 2*N THEN cmax := 0 END;
    cur := cmax + 1;
  ELSE
    cmax := 0
  END; (* IF *)
END KwRightDescend;


PROCEDURE ResetKeywordIndex;
VAR
  p : StackPtr;
BEGIN
  (* Dispose old stack *)
  IF rootptr#NIL THEN
    WHILE rootptr#NIL DO p := rootptr; rootptr := rootptr^.next; DISPOSE(p) END;
  END;
  (* Load root node *)
  root := TRUE; curpos := RootPos;
  LoadNode(KeywordIndex,RootPos, inde, TSIZE(KeywordNode)); cur := 0;
  IF OpResult() & (inde^.nk.max <= CHR(2*N)) THEN
    leaf := equal(inde^.nk.p0,zero); 
    IF NOT leaf THEN
      KwLeftDescend 
    ELSE
      cmax := ORD(inde^.nk.max)
    END; (* IF *)
    idle := TRUE; movement := dRight;
  ELSE
    cmax := 0
  END; (* IF *)
END ResetKeywordIndex;



PROCEDURE NextKeywordIndex(VAR kw:ARRAY OF CHAR) : BOOLEAN;
VAR
  count : CARDINAL;
BEGIN
  (* Modified recursive descent *)
  IF cmax = 0 THEN Clear(kw); RETURN FALSE;
  ELSIF idle & (movement # dRight) THEN count := 2 ELSE count := 1 END;
  WHILE count # 0 DO
    idle := FALSE; DEC(count);
    LOOP
      IF cur < cmax THEN
        INC(cur); Assign(kw, inde^.nk.a[cur].key);
        IF NOT leaf THEN KwLeftDescend; idle := TRUE END;
        EXIT;
      ELSIF NOT root THEN
        Pop(KeywordIndex);
      ELSE
        (* cur = cmax & root *)
        Clear(kw); IF NOT leaf THEN KwRightDescend; cur := cmax END;
        EXIT;
      END; (* IF *)
    END; (* LOOP *)
  END; (* WHILE *)
  movement := dRight; RETURN FALSE
END NextKeywordIndex;



PROCEDURE PrevKeywordIndex(VAR kw:ARRAY OF CHAR) : BOOLEAN;
VAR
  count : CARDINAL;
BEGIN
  IF cmax = 0 THEN Clear(kw); RETURN FALSE;
  ELSIF idle & (movement # dLeft) THEN count := 2 ELSE count := 1 END;
  WHILE count # 0 DO
    idle := FALSE; DEC(count);
    LOOP
      IF leaf THEN
        IF cur > 1 THEN
          DEC(cur); Assign(kw, inde^.nk.a[cur].key); EXIT;
        ELSIF NOT root THEN 
          Pop(KeywordIndex);
        ELSE
          Clear(kw); EXIT;
        END; (* IF *)
      ELSE (* NOT leaf *)
        IF cur > 0 THEN
          idle := TRUE;
          Assign(kw, inde^.nk.a[cur].key); DEC(cur); KwRightDescend; EXIT;
        ELSIF NOT root THEN
          Pop(KeywordIndex);
        ELSE
          (* root & NOT leaf & cur=0 *)
          Clear(kw); KwLeftDescend; cur := 1; (* no read-again of 1st *)
          EXIT;
        END; (* IF *)
      END; (* IF *)
    END; (* LOOP *)
  END; (* WHILE *)
  movement := dLeft; RETURN FALSE
END PrevKeywordIndex;



PROCEDURE DtLeftDescend;
(* Descends to the lowest left edge of tree at p[cur] *)
VAR
  ptr : LONGCARD;
BEGIN
  (* We are not at a leaf *)
  IF cur=0 THEN ptr := inde^.nd.p0 ELSE ptr := inde^.nd.p[cur] END;
  Push; LoadNode(DateIndex, ptr, inde, TSIZE(DateNode));
  curpos := ptr; cur := 0;
  WHILE  (NOT equal(inde^.nd.p0,zero)) 
       & OpResult() 
       & (inde^.nd.max <= CHR(2*N)) 
  DO
    ptr := inde^.nd.p0; Push;
    LoadNode(DateIndex, ptr, inde, TSIZE(DateNode));
    curpos := ptr;
  END; (* WHILE *)
  IF OpResult() THEN 
    leaf := TRUE; root := FALSE; cmax := ORD(inde^.nd.max);
  ELSE
    cmax := 0
  END; (* IF *)
END DtLeftDescend;


PROCEDURE DtRightDescend;
(* Descends to the lowest right edge of tree at p[cur] *)
VAR
  ptr : LONGCARD;
BEGIN
  IF (cur = 0) THEN ptr := inde^.nd.p0 ELSE ptr := inde^.nd.p[cur] END;
  Push; LoadNode(DateIndex, ptr,inde,TSIZE(DateNode));
  curpos := ptr;
  WHILE (NOT equal(inde^.nd.p0,zero)) & OpResult() & (cur <= 2*N) DO
    cur := ORD(inde^.nd.max); ptr := inde^.nd.p[cur]; Push;
    LoadNode(DateIndex,ptr,inde, TSIZE(DateNode));
    curpos := ptr;
  END; (* WHILE *)
  IF OpResult() & (cur <= 2*N) THEN
    leaf := TRUE; root := FALSE; cmax := ORD(inde^.nd.max);
    cur := cmax + 1;
  ELSE
    cmax := 0
  END; (* IF *)
END DtRightDescend;


PROCEDURE ResetDateIndex;
VAR
  p : StackPtr;
BEGIN
  (* Dispose old stack *)
  IF rootptr#NIL THEN
    WHILE rootptr#NIL DO p := rootptr; rootptr := rootptr^.next; DISPOSE(p) END;
  END;
  (* Load root node *)
  root := TRUE; curpos := RootPos;
  LoadNode(DateIndex,RootPos, inde, TSIZE(DateNode)); cur := 0;
  IF OpResult() & (inde^.nd.max <= CHR(2*N)) THEN
    leaf := equal(inde^.nd.p0,zero); 
    IF NOT leaf THEN
      DtLeftDescend 
    ELSE
      cmax := ORD(inde^.nd.max)
    END; (* IF *)
    idle := TRUE; movement := dRight;
  ELSE
    cmax := 0
  END; (* IF *)
END ResetDateIndex;



PROCEDURE NextDateIndex(VAR kw : CARDINAL);
VAR
  count : CARDINAL;
BEGIN
  (* Modified recursive descent *)
  IF cmax = 0 THEN kw := 0; RETURN
  ELSIF idle & (movement # dRight) THEN count := 2 ELSE count := 1 END;
  WHILE count # 0 DO
    idle := FALSE; DEC(count);
    LOOP
      IF cur < cmax THEN
        INC(cur); kw := inde^.nd.a[cur].key;
        IF NOT leaf THEN DtLeftDescend; idle := TRUE END;
        EXIT;
      ELSIF NOT root THEN
        Pop(DateIndex);
      ELSE
        (* cur = cmax & root *)
        kw := 0; IF NOT leaf THEN DtRightDescend; cur := cmax END;
        EXIT;
      END; (* IF *)
    END; (* LOOP *)
  END; (* WHILE *)
  movement := dRight;
END NextDateIndex;



PROCEDURE PrevDateIndex(VAR kw : CARDINAL);
VAR
  count : CARDINAL;
BEGIN
  IF cmax = 0 THEN kw := 0; RETURN
  ELSIF idle & (movement # dLeft) THEN count := 2 ELSE count := 1 END;
  WHILE count # 0 DO
    idle := FALSE; DEC(count);
    LOOP
      IF leaf THEN
        IF cur > 1 THEN
          DEC(cur); kw := inde^.nd.a[cur].key; EXIT;
        ELSIF NOT root THEN 
          Pop(DateIndex);
        ELSE
          kw := 0; EXIT;
        END; (* IF *)
      ELSE (* NOT leaf *)
        IF cur > 0 THEN
          idle := TRUE;
          kw := inde^.nd.a[cur].key; DEC(cur); DtRightDescend; EXIT;
        ELSIF NOT root THEN
          Pop(DateIndex);
        ELSE
          (* root & NOT leaf & cur=0 *)
          kw := 0; DtLeftDescend; cur := 1; (* no read-again of 1st *)
          EXIT;
        END; (* IF *)
      END; (* IF *)
    END; (* LOOP *)
  END; (* WHILE *)
  movement := dLeft;
END PrevDateIndex;



PROCEDURE ResetTemplateEntered;
BEGIN
  templateEstablished := FALSE;
END ResetTemplateEntered;



PROCEDURE TemplateEntered() : BOOLEAN;
VAR
  d  : BOOLEAN;
  kw : Keyword;
BEGIN
  IF NOT templateEstablished THEN
    ResetKeywordIndex; d := NextTemplate(kw);
    templateInIndex := (kw[0] # 0C) & (kw[1] = '$');
    (** 22/08/87 : ":= TRUE" **)
    templateEstablished := TRUE;
  END; (* IF *)
  RETURN templateInIndex;
END TemplateEntered;



PROCEDURE NextTemplate(VAR kw:ARRAY OF CHAR) : BOOLEAN;
VAR
  x : BOOLEAN;
BEGIN
  x := NextKeywordIndex(kw);
  IF (kw[0]#0C) & (kw[1]#'$') THEN x := PrevKeywordIndex(kw); Clear(kw) END;
  RETURN FALSE;
END NextTemplate;



PROCEDURE PrevTemplate(VAR kw:ARRAY OF CHAR) : BOOLEAN;
BEGIN
  RETURN PrevKeywordIndex(kw);
END PrevTemplate;



PROCEDURE InitLink(f:IndexType; pos:LONGCARD);
BEGIN
  LoadBlock(f, pos, ADR(linkblock), TSIZE(Block)); linkcur := 2C
END InitLink;



PROCEDURE NextLink(f:IndexType; VAR pos:LONGCARD);
BEGIN
  IF linkcur>linkblock.max THEN 
    IF equal(linkblock.next,zero) THEN
      pos := zero; RETURN;
    ELSE
      LoadBlock(f, linkblock.next, ADR(linkblock), TSIZE(Block)); linkcur := 1C;
      IF NOT OpResult() THEN pos := zero; RETURN END;
    END;
  END;
  pos := linkblock.pos[ORD(linkcur)]; INC(linkcur);
  (******)
  (** FIX TO RETRIEVE SCREENS IN BUGGED FILES:
  IF linkblock.next.high > 1 THEN pos := zero END; ***)
  (******) 
END NextLink;


BEGIN
  NEW(inde); rootptr := NIL; templateEstablished := FALSE;
END BTrees.
