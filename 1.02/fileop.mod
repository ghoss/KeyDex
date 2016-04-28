(*****************************************************************************
*
* Name     : FileOp
*
* LastEdit : 28/04/87
* Project  : KeyDex system
* Purpose  : Medium-level file operations on Keydex data files
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
* 21/03/87 : HALT in WriteLine replaced after mysterious halt error due to
*            encountered attribute 20H (??).
*            New definition of "file empty".
* 04/04/87 : Bug in character repetition code of "WriteLine" removed.
* 20/04/87 : Some bugs fixed in "DeleteScreen".
*            "len" = no. of bytes in screen INCLUDING <EOR>.
* 02/05/87 : Added test for record number wraparound.
* 03/05/87 : Record numbers are now 32-bit unsigned.
* 06/06/87 : Adjusted "Open/CreateFile" code and index operations.           
* 16/06/87 : Implemented address arithmetic for "page" accesses.
* 09/07/87 : Bugs fixed in "OpenFile/CreateFile" ('disk error').
* 16/07/87 : Implemented error handling routines and new file system.
* 18/07/87 : Implemented private error handler for scan functions.
*            Implemented buffer code for "LoadScreen".
* 26/07/87 : File scan procedures transferred from "KDFileScan".
* 23/08/87 : Removed ordering code for 'DeleteNode'.
* 27/08/87 : Error fixed in "SaveBlock".
* 02/09/87 : Additions for 'DateCreated'.
* 06/10/87 : Bugs fixed in "Create" and "Load".
* 15/10/87 : Doubled buffer size.
* 05/11/87 : m2c 3.0 : Fixed storage problem.
* 03/12/87 : Removed 'Normalize' where address subtraction occurs.
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

IMPLEMENTATION MODULE FileOp;


FROM SYSTEM IMPORT 
  ADDRESS, ADR, TSIZE, BYTE, WORD;

FROM SystemIO IMPORT
  ScrCh, white, reverse, intensity, underline;

FROM KDStorage IMPORT
  ALLOCATE, DEALLOCATE, Normalize, InverseNormalize;

FROM LongCardinals IMPORT
  LONGCARD, zero, equal, add, inc, dec;

FROM TimeDate IMPORT
  GetTime, Time;

FROM String IMPORT
  AssignStr, Assign;

FROM MsFileSystem IMPORT
  fsSetPos, fsGetPos, fsFile, fsReadBlock, fsWriteBlock, fsReadWord,
  fsWriteWord, fsWriteByte, fsReadByte, fsResult, fsErrorType, fsAccessMode,
  fsShareMode, fsOpen, fsCreate, fsClose, fsLength, fsInstallHandler,
  fsDispatchHandler, fsEnableHandler;

FROM Compress IMPORT
  Pack, Unpack;

FROM KDEdit IMPORT
  page, ClearPages, MinC, MaxC, MinR, MaxR, MaxPage, template;

FROM KDConfig IMPORT
  Keyword, Version;
  
FROM CopyProtect IMPORT
  EvalRelease, ScreenLimit;


CONST
  Deleted   = 128;     (* Flag bits in record header *)
  Template  = 32;

  IDfp    = 6;    (* Position of pointer to first free page in index file *)
  IDbl    = 10;   (* Pointer to first free block *)
  IDroot  = 14;   (* Position of root node *)

  DTrec   = 6;    (* Total amount of records saved *)
  DTstart = 10;   (* First data byte on data file *)

  EOS = 255;     (* End-Of-Screen *)
  BOR = 0;       (* Start-Of-Record *)
  REP = 3;       (* Repeater mark. Format: [REP] [len] [char] *)

  MARKER  = -32767;
  CTRLZ   = 32C;
  BufSize = 2048;   (* Size of scan buffer in bytes *)


TYPE
  filename = ARRAY [0..64] OF CHAR;
  CharPtr = POINTER TO ScrCh;
  BytePtr = POINTER TO BYTE;


VAR
                                       (* .KD1 - The database file *)
  KDFile   : ARRAY FileType OF fsFile; (* .KD2 - The keyword index file *)
                                       (* .KD3 - The date index file *)

  FirstDelNode,    (* Pointers to first deleted index node/block *)
  FirstDelBlock  : ARRAY [KeywordIndex..DateIndex] OF LONGCARD;
  CurrentPos     : LONGCARD;      (* Position of current record *)

  errorHandler : ErrorProc;
  scF          : fsFile;
  enabled      : BOOLEAN;  (* TRUE if critical error handler enabled *)

  currentScanPos,
  lastLoadPos  : LONGCARD;

  cur,
  buffer : BytePtr;        (* Buffer for scan routines *)
  count,
  max    : CARDINAL;
  lastC  : CHAR;
  scDateProc    : dtEnterProc;
  scKwProc      : kwEnterProc;
  scRecProc     : RecProc;
  currPos       : INTEGER;
  currentPage,
  currentLine,
  lastLine,
  pageCount     : CARDINAL;
  repeatCount   : INTEGER;
  currentScreen : LONGCARD;
  attributes,
  oneScreen,
  upperCase     : BOOLEAN;



PROCEDURE CriticalError(err : fsErrorType);
VAR
  r : ErrorType;
BEGIN
  CASE err OF
    etDiskError :
      r := error;
  | etDriveError :
      r := driveerr;
  | etWriteProtect :
      r := writeprotect;
  | etNoAccess :
      r := illegalfile;
  | etDiskChange :
      r := consistencyerr;
  | etEndOfFile :
      RETURN; (* nothing *)
  ELSE
    (* The critical error handler should not react on other errors *)
    ScanEOF := TRUE; RETURN
  END; (* CASE *)
  errorHandler(r);
END CriticalError;


PROCEDURE CriticalHandler(p : ErrorProc);
BEGIN
  errorHandler := p;
END CriticalHandler;


PROCEDURE EnableHandler;
BEGIN
  fsInstallHandler(CriticalError); fsEnableHandler; enabled := TRUE;
END EnableHandler;


PROCEDURE DisableHandler;
BEGIN
  IF enabled THEN fsDispatchHandler; enabled := FALSE; END;
  ScanEOF := TRUE;  (* If error handler dispatches itself *)
END DisableHandler;


PROCEDURE SetToEOF(VAR f:fsFile; VAR pos:LONGCARD);
(* Prepares f for append operations *)
BEGIN
  WITH pos DO fsLength(f,high,low); fsSetPos(f,high,low) END; 
END SetToEOF;


PROCEDURE GetNextBlockPos(f:FileType; VAR p:LONGCARD);
(* Returns end-of-file position of index file in p *)
BEGIN
  IF equal(FirstDelBlock[f], zero) THEN
    WITH p DO fsLength(KDFile[f],high,low) END;
  ELSE
    p := FirstDelBlock[f]
  END;
END GetNextBlockPos;


PROCEDURE OpResult() : BOOLEAN;
BEGIN
  RETURN (fsResult() = etOK) OR (fsResult() = etEndOfFile);
END OpResult;


PROCEDURE SetStatus(VAR Status:ErrorType);
(* Sets status variable according to result of FileSystem status *)
BEGIN
  CASE fsResult() OF
    etOK :
      Status := ok;
  | etNoHandle :
      Status := error;
  | etDiskFull :
      Status := diskfull;
  | etNoFile, etNoAccess :
      Status := nofile;
  ELSE
    Status := error;
  END; (* CASE *)
END SetStatus;



PROCEDURE LoadNode( f   : FileType;
                    pos : LONGCARD; 
                    a   : ADDRESS;
                    s   : CARDINAL );
VAR
  n : CARDINAL; 
BEGIN
  WITH pos DO 
    fsSetPos(KDFile[f],high,low);
  END; (* WITH *)
  fsReadBlock(KDFile[f],a,s,n);
END LoadNode;




PROCEDURE SaveNode( f       : FileType;
                    a       : ADDRESS; 
                    s       : CARDINAL; 
                    VAR pos : LONGCARD;
                    new     : BOOLEAN);
VAR
  mod : BOOLEAN;
  if  : fsFile;
  n   : CARDINAL;
BEGIN
  mod := FALSE; if := KDFile[f];
  IF new THEN
    empty[f] := FALSE;
    WITH FirstDelNode[f] DO
      IF equal(FirstDelNode[f],zero) THEN
        (* Append node to EOF *)
        SetToEOF(if,pos);
      ELSE
        (* Fill gap in index file; get successor of 1st deleted node *)
        pos := FirstDelNode[f];
        WITH pos DO fsSetPos(if,high,low) END;
        (* Successor gets to be the first deleted node *)
        fsReadWord(if,high); fsReadWord(if,low);
        WITH pos DO fsSetPos(if,high,low) END;
        mod := TRUE;
      END; (* IF *)
    END; (* WITH *)
  ELSE
    (* Replace existing node *)
    WITH pos DO fsSetPos(if,high,low) END;
  END;

  fsWriteBlock(if,a,s,n); 

  (* Write position of next deleted node to file *)
  IF mod THEN
    fsSetPos(if,0,IDfp);
    WITH FirstDelNode[f] DO fsWriteWord(if,high); fsWriteWord(if,low) END;
  END;
END SaveNode;



PROCEDURE DeleteNode(f:FileType; pos:LONGCARD);
VAR
  if   : fsFile;
BEGIN
  if := KDFile[f]; empty[f] := equal(pos,RootPos);
  fsSetPos(if,0,IDfp); 
  WITH pos DO fsWriteWord(if,high); fsWriteWord(if,low) END;

  (* Update pointer at "pos" to show to former head of list *)
  WITH pos DO fsSetPos(if,high,low) END;
  WITH FirstDelNode[f] DO fsWriteWord(if,high); fsWriteWord(if,low) END;

  (* Update pointer to head of list *)
  FirstDelNode[f] := pos;
END DeleteNode;



PROCEDURE LoadBlock( f   : FileType;
                     pos : LONGCARD; 
                     a   : ADDRESS; 
                     s   : CARDINAL );
VAR
  n : CARDINAL;
BEGIN
  WITH pos DO 
    fsSetPos(KDFile[f],high,low);
  END;
  fsReadBlock(KDFile[f],a,s,n); 
END LoadBlock;



PROCEDURE SaveBlock( f       : FileType;
                     a       : ADDRESS;
                     s       : CARDINAL; 
                     VAR pos : LONGCARD;
                     new     : BOOLEAN );
VAR
  mod : BOOLEAN;
  if  : fsFile;
  n   : CARDINAL;
BEGIN
  mod := FALSE; if := KDFile[f];
  IF new THEN
    WITH FirstDelBlock[f] DO
      IF equal(FirstDelBlock[f],zero) THEN
        (* Append block to EOF *)
        SetToEOF(if,pos);
      ELSE
        (* Fill gap in index file; get successor of 1st deleted block *)
        pos := FirstDelBlock[f];
        WITH pos DO fsSetPos(if,high,low) END;
        (* Successor gets to be the first deleted block *)
        fsReadWord(if,high); fsReadWord(if,low);
        mod := TRUE;
        WITH pos DO fsSetPos(if,high,low) END;
      END;
    END;
  ELSE
    (* Replace existing block *)
    WITH pos DO fsSetPos(if,high,low) END;
  END;

  fsWriteBlock(if,a,s,n); 
  
  (* Write position of next deleted node to file *)
  IF mod THEN
    fsSetPos(if,0,IDbl);
    (** 27/08/87 **)
    WITH FirstDelBlock[f] DO fsWriteWord(if,high); fsWriteWord(if,low) END;
  END;
END SaveBlock;



PROCEDURE DeleteBlock(f:FileType; pos:LONGCARD);
VAR
  if : fsFile;
BEGIN
  (* Update pointer in index file to show to "pos" *)
  if := KDFile[f];
  fsSetPos(if,0,IDbl); 
  WITH pos DO fsWriteWord(if,high); fsWriteWord(if,low) END;

  (* Update pointer at "pos" to show to former head of list *)
  WITH pos DO fsSetPos(if,high,low) END;
  WITH FirstDelBlock[f] DO fsWriteWord(if,high); fsWriteWord(if,low) END;

  (* Update pointer to head of list *)
  FirstDelBlock[f] := pos;
END DeleteBlock;




PROCEDURE SaveScreen( VAR pos   :LONGCARD; 
                      date      :CARDINAL; 
                      VAR recno :LONGCARD;
                      kwproc    :KeywordProc;
                      dtproc    :DateProc );

  PROCEDURE WritePage;

    PROCEDURE EndOfLine(line:CARDINAL) : INTEGER;
    VAR
      x   : INTEGER;
      adr : CharPtr;
      sc  : ScrCh;
    BEGIN
      x := MaxC; adr := CharPtr(ADR(page[pg]^[line][x]));
      (** 03/12/87 **)
      InverseNormalize(adr);
      WHILE (x>=MinC) DO
        (** 05/11/87 **)
        sc := adr^;
        WITH sc DO
          IF (ch=' ') AND ((at=BYTE(white)) OR (at=BYTE(white+intensity))) THEN
            DEC(x); adr := CharPtr(ADDRESS(adr) - TSIZE(ScrCh));
          ELSE 
            RETURN x
          END; (* IF *)
        END; (* WITH *)
      END; (* WHILE *)
      RETURN x;
    END EndOfLine;


    PROCEDURE WriteLine;
    VAR
      j,j1,count,i,max : INTEGER;
      c,oldc : ScrCh;
      b      : BYTE;
      adr,
      adr1   : CharPtr;
    BEGIN
      WITH oldc DO ch := 0C END;
      adr := CharPtr(ADR(page[pg]^[line,MinC])); Normalize(adr);
      j := MinC; max := EndOfLine(line);
      WHILE (j <= max) AND OpResult() DO
        c := adr^;
        IF (c.ch=oldc.ch) AND (c.at=oldc.at) AND (j<max) THEN
          (* Maybe repeating character sequence *)
          j1 := j; adr1 := adr;
          REPEAT
            INC(j1); 
            adr1 := CharPtr(ADDRESS(adr1) + TSIZE(ScrCh)); c := adr1^;
          UNTIL (j1=max) OR (oldc.ch#c.ch) OR (oldc.at#c.at);
          count := j1-j+1;
          IF count>3 THEN
            fsWriteByte(DataFile,BYTE(REP)); fsWriteByte(DataFile,CHR(count));
            INC(num,2);
          ELSE
            FOR i := 2 TO count DO fsWriteByte(DataFile,oldc.ch) END;
            INC(num,count-1);
          END;
          j := j1; adr := adr1;
        ELSE
          WITH c DO
            IF at<>attr THEN
              CASE ORD(at) OF
                 white : b := BYTE(wh);
                |reverse : b := BYTE(re);
                |underline : b := BYTE(wu);
                |white+intensity : b := BYTE(wi);
                |intensity+underline : b := BYTE(iu);
              ELSE 
                (* unknown attribute, set to white *)
                b := BYTE(wh);
              END;
              fsWriteByte(DataFile,b); attr := at; INC(num);
            END;
            fsWriteByte(DataFile,ch); INC(num);
          END;
          INC(j); adr := CharPtr(ADDRESS(adr) + TSIZE(ScrCh)); oldc := c;
        END;
      END;
      fsWriteByte(DataFile,BYTE(EOL)); INC(num);
    END WriteLine;

  VAR
    first,last,line : CARDINAL;
  BEGIN
    first := MinR; last := MaxR;
    WHILE (first<MaxR) AND (EndOfLine(first)<MinC) DO INC(first) END;
    WHILE (last>MinR) AND (EndOfLine(last)<MinC) DO DEC(last) END;
    fsWriteByte(DataFile,CHR(first)); fsWriteByte(DataFile,CHR(last));
    attr := BYTE(white); INC(num,2);
    FOR line := first TO last DO 
      WriteLine;
      IF NOT OpResult() THEN RETURN END;
    END;
  END WritePage;

VAR
  marker,attr  : BYTE;
  i,pg,num,d,n : CARDINAL;
  crkw,kw : Keyword;
  res     : BOOLEAN;
  temph,templ : CARDINAL;
  DataFile    : fsFile;
BEGIN
  (* Update record number information *)
  DataFile := KDFile[ScreenFile];
  inc(lastrec); recno := lastrec;
  IF EvalRelease() & (recno.low > ScreenLimit) THEN 
    dec(lastrec); RETURN
  END; (* IF *)
  fsSetPos(DataFile,0,DTrec);
  WITH recno DO fsWriteWord(DataFile,high); fsWriteWord(DataFile,low) END;
  IF NOT OpResult() THEN recno := zero; dec(lastrec); RETURN END;

  (* Set position in file to EOF *)
  SetToEOF(DataFile,pos);

  (* Write BOR, marker, date, record number *)
  attr := BYTE(white);
  IF template THEN marker := BYTE(Template) ELSE marker := BYTE(0) END;
  fsWriteByte(DataFile,BOR); fsWriteByte(DataFile,marker); 
  fsWriteWord(DataFile,date); 
  WITH lastrec DO fsWriteWord(DataFile,high); fsWriteWord(DataFile,low) END;
  fsGetPos(DataFile,temph,templ);  (* Get position for length indicator *)
  fsWriteWord(DataFile,0); num := 2;  (* 2 to account for EOS/EOR write *)
  IF NOT OpResult() THEN recno := zero; dec(lastrec); RETURN END;

  (* Write editor pages *)
  FOR pg := 1 TO MaxPage DO 
    WritePage;
    IF NOT OpResult() THEN recno := zero; dec(lastrec); RETURN END;
  END; (* FOR *)
  fsWriteByte(DataFile,BYTE(EOS)); 

  (* Write dates *)
  REPEAT
    dtproc(d); fsWriteWord(DataFile,d); (** 08/06/87 **) INC(num,2);
    IF NOT OpResult() THEN recno := zero; dec(lastrec); RETURN END;
  UNTIL d=0;
  
  (* Write keywords *)
  res := kwproc(kw);
  WHILE ORD(kw[0])>0 DO
    Pack(kw,crkw,i);
    fsWriteBlock(DataFile, ADR(crkw), i+1, n); INC(num,i+1);
    IF NOT OpResult() THEN recno := zero; dec(lastrec); RETURN END;
    res := kwproc(kw);
  END; (* WHILE *)

  fsWriteByte(DataFile,BYTE(EOR)); 
  fsSetPos(DataFile,temph,templ); fsWriteWord(DataFile,num);
  FlushFile(ScreenFile);
  IF NOT OpResult() THEN recno := zero; dec(lastrec); RETURN END;
  CurrentPos := pos; 
END SaveScreen;



PROCEDURE LoadScreen( pos       : LONGCARD; 
                      VAR date  : CARDINAL;
                      VAR recno : LONGCARD;
                      kwproc    : kwEnterProc;
                      dtproc    : dtEnterProc );

  PROCEDURE ReadPage;

    PROCEDURE ReadLine;
    VAR
      oldc : CHAR;
      j   : CARDINAL;
      adr : CharPtr;
    BEGIN
      j := MinC; adr := CharPtr(ADR(page[pg]^[line,MinC])); 
      Normalize(adr);
      REPEAT
        b := curb^; curb := BytePtr(ADDRESS(curb) + 1);
        CASE ORD(b) OF
           32..254 : WITH adr^ DO ch := CHAR(b); at := attr END;
                     INC(j); oldc := CHAR(b);
                     adr := CharPtr(ADDRESS(adr) + TSIZE(ScrCh));
          |wh : attr := BYTE(white);
          |re : attr := BYTE(reverse);
          |wu : attr := BYTE(underline);
          |wi : attr := BYTE(white+intensity);
          |iu : attr := BYTE(underline+intensity);
          |REP: b := curb^; curb := BytePtr(ADDRESS(curb) + 1); DEC(b);
                WHILE ORD(b)>0 DO
                  WITH adr^ DO ch := oldc; at := attr END;
                  INC(j); adr := CharPtr(ADDRESS(adr) + TSIZE(ScrCh)); DEC(b);
                END; (* WHILE *)
        ELSE
          (* ignore unexpected bytes (dirty) *)
        END; (* CASE *)
      UNTIL (ORD(b)=EOL);
    END ReadLine;

  VAR
    first,last,line : CARDINAL;
  BEGIN
    (* Get first and last line of current page *)
    b := curb^; curb := BytePtr(ADDRESS(curb) + 1);
    end := (ORD(b) = EOS) OR (ADDRESS(curb) > ADDRESS(limit));
    IF NOT end THEN
      first := ORD(b);
      b := curb^; curb := BytePtr(ADDRESS(curb) + 1); last := ORD(b);

      (* Read in lines *)
      FOR line := first TO last DO
        ReadLine 
      END; (* IF *)
    END; (* IF *)
  END ReadPage;
  
VAR
  marker,b,attr : BYTE;
  end        : BOOLEAN;
  kw,crkw    : Keyword;
  pg,i,num,d : CARDINAL;
  DataFile   : fsFile;
  curb,buf,
  bufOrig,
  limit      : BytePtr;
  curw       : POINTER TO WORD;
BEGIN
  (* Set position *)
  recno := zero;
  DataFile := KDFile[ScreenFile];
  WITH pos DO fsSetPos(DataFile,high,low) END;

  (* Read BOR, marker, date *)
  fsReadByte(DataFile,marker); 
  IF (marker # BYTE(BOR)) & (fsResult() = etOK) THEN RETURN END;
  fsReadByte(DataFile,marker);
  IF (marker > BYTE(127)) & (fsResult() = etOK) THEN RETURN END;
  fsReadWord(DataFile,date); 
  WITH recno DO fsReadWord(DataFile,high); fsReadWord(DataFile,low) END;

  fsReadWord(DataFile,num);
  ALLOCATE(bufOrig, num); 
  (** 05/11/87 **) buf := bufOrig; Normalize(buf);
  fsReadBlock(DataFile, buf, num, d);
  ClearPages(FALSE);
  limit := BytePtr(ADDRESS(buf) + num - 1);
  IF (num # d) OR (limit^ # BYTE(EOR)) THEN
    DEALLOCATE(bufOrig, num); recno := zero; RETURN;
  END; (* IF *)
  curb := buf;

  template := ODD(ORD(marker) DIV Template);

  (* Read editor pages *)
  end := FALSE; pg := 1; attr := BYTE(white);
  REPEAT ReadPage; INC(pg) UNTIL end;

  (* Read date list *)
  curw := ADDRESS(curb);
  d := CARDINAL(curw^); curb := BytePtr(ADDRESS(curb) + TSIZE(WORD));
  WHILE (d # 0) & (ADDRESS(curb) < ADDRESS(limit)) DO
    dtproc(d); 
    curw := ADDRESS(curb); d := CARDINAL(curw^); 
    curb := BytePtr(ADDRESS(curb) + TSIZE(WORD));
  END; (* WHILE *)

  (* Read keyword list *)
  b := curb^; curb := BytePtr(ADDRESS(curb) + 1);
  WHILE b # BYTE(EOR) DO
    crkw[0] := CHAR(b);
    FOR i := 1 TO (ORD(b) DIV 8) DO
      crkw[i] := CHAR(curb^); curb := BytePtr(ADDRESS(curb) + 1);
    END; (* FOR *)
    Unpack(crkw,kw); kwproc(kw); b := curb^; curb := BytePtr(ADDRESS(curb) + 1);
  END; (* WHILE *)

  DEALLOCATE(bufOrig, num);
  CurrentPos := pos;
END LoadScreen;



PROCEDURE DeleteScreen;
VAR
  b       : BYTE;
  DataFile : fsFile;
BEGIN
  DataFile := KDFile[ScreenFile];
  WITH CurrentPos DO
    fsSetPos(DataFile,high,low); 
    fsReadByte(DataFile,b); b := BYTE(Deleted); fsWriteByte(DataFile,b); 
  END; (* WITH *)
  IF NOT OpResult() THEN RETURN END;
  FlushFile(ScreenFile);
END DeleteScreen;


PROCEDURE ClFile;
VAR
  i : FileType;
BEGIN
  FOR i := ScreenFile TO DateIndex DO 
    fsClose(KDFile[i]);
  END; (* IF *)
END ClFile;


PROCEDURE CreateFile( VAR db,ix : ARRAY OF CHAR; 
                      VAR res   : ErrorType );
VAR
  fn  : filename;
  t   : Time;
  i   : FileType;
  if  : fsFile;
BEGIN
  (** 06/10/87 : 'fileLoaded := FALSE' removed **)
  CloseFile(res);
  (* Check data file *)
  AssignStr(fn,db); fsOpen(fn,smDenyReadWrite, amWrite, if); 
  SetStatus(res); 
  IF res # nofile THEN
    IF res = ok THEN res := fileexists; fsClose(if) END;
    RETURN
  END; (* IF *)
  (* Check keyword index *)
  ix[ORD(ix[0])] := '2';
  AssignStr(fn,ix); fsOpen(fn,smDenyReadWrite, amWrite, if);
  SetStatus(res);
  IF res # nofile THEN
    IF res = ok THEN res := fileexists; fsClose(if) END;
    RETURN
  END; (* IF *)
  (* Check date index *)
  ix[ORD(ix[0])] := '3';
  AssignStr(fn,ix); fsOpen(fn, smDenyReadWrite, amWrite, if);
  SetStatus(res); 
  IF res # nofile THEN
    IF res = ok THEN res := fileexists; fsClose(if) END;
    RETURN
  ELSE
    (* Create all three files *)
    fsCreate(fn); fsOpen(fn, smDenyReadWrite, amWrite, KDFile[DateIndex]);
    SetStatus(res);
    IF res # ok THEN RETURN END;
    ix[ORD(ix[0])] := '2';
    AssignStr(fn,ix); 
    fsCreate(fn); fsOpen(fn, smDenyReadWrite, amWrite, KDFile[KeywordIndex]);
    SetStatus(res);
    IF res # ok THEN RETURN END;
    AssignStr(fn,db);
    fsCreate(fn); fsOpen(fn, smDenyReadWrite, amWrite, KDFile[ScreenFile]);
    SetStatus(res);
    IF res # ok THEN RETURN END;
    (* Write information to data file *)
    GetTime(t);
    if := KDFile[ScreenFile];
    fsSetPos(if, 0, 0);
    fsWriteByte(if,CTRLZ); fsWriteByte(if,Version); 
    dateCreated := t.day;
    fsWriteWord(if,dateCreated); fsWriteWord(if,t.millisec);
    (* Amount of records = long zero *)
    fsWriteWord(if,0); fsWriteWord(if,0);
    SetStatus(res); lastrec := zero;
    IF res=ok THEN
      FOR i := KeywordIndex TO DateIndex DO
        (* Write information to index files *)
        if := KDFile[i];
        fsSetPos(if, 0, 0);
        fsWriteByte(if,CTRLZ); fsWriteByte(if,CHR(Version));
        fsWriteWord(if,t.day); fsWriteWord(if,t.millisec); 
        fsWriteWord(if,0); fsWriteWord(if,0); 
        fsWriteWord(if,0); fsWriteWord(if,0);
        FirstDelNode[i] := zero; FirstDelBlock[i] := zero;
        SetStatus(res);
        IF res # ok THEN ClFile; RETURN END;
      END; (* FOR *)
      Assign(dbname,db); Assign(ixname,ix);
      fileLoaded := TRUE; (** 09/07/87 **)
    ELSE
      ClFile; RETURN
    END; (* IF *)
  END; (* IF *)
  FOR i := ScreenFile TO DateIndex DO FlushFile(i) END;
END CreateFile;



PROCEDURE OpenFile( VAR db,ix : ARRAY OF CHAR;
                    VAR res   : ErrorType);
VAR
  fn    : filename;
  x,time,date,date1,time1 : CARDINAL;
  c     : CHAR;
  i     : FileType;
  if    : fsFile;
BEGIN
  (** 06/10/87 : 'fileLoaded := FALSE' removed **)
  CloseFile(res);
  IF res#ok THEN RETURN END;
  AssignStr(fn,db); 
  fsOpen(fn, smDenyReadWrite, amReadWrite, KDFile[ScreenFile]);
  SetStatus(res);
  IF res # ok THEN RETURN END;
  ix[ORD(ix[0])] := '2';
  AssignStr(fn,ix);
  fsOpen(fn, smDenyReadWrite, amReadWrite, KDFile[KeywordIndex]);
  SetStatus(res);
  IF res#ok THEN RETURN END;
  ix[ORD(ix[0])] := '3';
  AssignStr(fn,ix);
  fsOpen(fn, smDenyReadWrite, amReadWrite, KDFile[DateIndex]);
  SetStatus(res);
  IF res = ok THEN
    (* Read information from data file *)
    if := KDFile[ScreenFile];
    fsSetPos(if, 0, 0);
    fsReadByte(if,c); IF c#CTRLZ THEN res := illegalfile; ClFile; RETURN END;
    fsReadByte(if,c); 
    IF ORD(c)#Version THEN res := illegalversion; ClFile; RETURN END;
    fsReadWord(if,date); fsReadWord(if,time); dateCreated := date;
    WITH lastrec DO fsReadWord(if,high); fsReadWord(if,low) END;
    IF fsResult() = etEndOfFile THEN res := ok ELSE SetStatus(res) END;
    IF res = ok THEN
      (* Read information from index file *)
      FOR i := KeywordIndex TO DateIndex DO
        if := KDFile[i]; 
        fsSetPos(if, 0, 0);
        fsReadByte(if,c); IF c#CTRLZ THEN res := illegalfile; ClFile; RETURN END;
        fsReadByte(if,c); IF ORD(c)#Version THEN res := illegalversion; ClFile; RETURN END;
        fsReadWord(if,date1); fsReadWord(if,time1);
        IF (date#date1) OR (time#time1) THEN res := consistencyerr; ClFile; RETURN END;
        WITH FirstDelNode[i] DO fsReadWord(if,high); fsReadWord(if,low) END;
        WITH FirstDelBlock[i] DO fsReadWord(if,high); fsReadWord(if,low) END;
        (* Test if file is empty *)
        fsReadWord(if,x); 
        empty[i] :=    (fsResult() = etEndOfFile) 
                    OR equal(FirstDelNode[i],RootPos);
        IF fsResult() = etEndOfFile THEN res := ok ELSE SetStatus(res) END;
      END; (* FOR *)
      Assign(dbname,db); Assign(ixname,ix);
      IF res = ok THEN fileLoaded := TRUE END; (** 09/07/87 **)
    END; (* IF *)
  END; (* IF *)
END OpenFile;



PROCEDURE FlushFile(typ:FileType);
VAR
  fn : ARRAY [0..64] OF CHAR;
BEGIN
  fsClose(KDFile[typ]);
  CASE typ OF
    ScreenFile :
      AssignStr(fn,dbname);
  | KeywordIndex :
      ixname[ORD(ixname[0])] := '2'; AssignStr(fn,ixname);
  | DateIndex :
      ixname[ORD(ixname[0])] := '3'; AssignStr(fn,ixname);
  END; (* CASE *)
  fsOpen(fn, smDenyReadWrite, amReadWrite, KDFile[typ]);
END FlushFile;



PROCEDURE CloseFile(VAR res:ErrorType);
VAR
  res1,res2,res3 : ErrorType;
BEGIN
  IF fileLoaded THEN
    fsClose(KDFile[KeywordIndex]); SetStatus(res1);
    fsClose(KDFile[ScreenFile]); SetStatus(res2);
    fsClose(KDFile[DateIndex]); SetStatus(res3);
    IF res1#ok THEN res := res1
    ELSIF res2#ok THEN res := res2
    ELSIF res3#ok THEN res := res3
    ELSE res := ok
    END; (* IF *)
    fileLoaded := FALSE;
  ELSE
    res := ok;
  END;
  empty[KeywordIndex] := TRUE; empty[DateIndex] := TRUE;
END CloseFile;


PROCEDURE ScanReset
            ( attr,
              upCase : BOOLEAN;
              dtproc : dtEnterProc; (* to be called for each date *)
              kwproc : kwEnterProc; (* to be called for each keyword *)
              rcproc : RecProc;     (* called for record date and #no *)
              oneScr : BOOLEAN );   (* TRUE if one screen at a time *)
BEGIN
  scF := KDFile[ScreenFile];
  scDateProc := dtproc; scKwProc := kwproc; scRecProc := rcproc;
  attributes := attr; upperCase := upCase;
  repeatCount := 0; currentScreen := zero;
  oneScreen := oneScr; 
  IF NOT oneScreen THEN 
    WITH currentScanPos DO high := 0; low := DTstart END;
    ScanSetCurrentPos(currentScanPos);
    Synchronize;
  END; (* IF *)
END ScanReset;


PROCEDURE ScanRestorePos;
BEGIN
  WITH currentScanPos DO fsSetPos(scF,high,low) END;
END ScanRestorePos;


PROCEDURE ScanNextByte(VAR b:BYTE);
BEGIN
  LOOP
    INC(count);
    IF count <= max THEN
      b := cur^; cur := BytePtr(ADDRESS(cur) + 1); EXIT;
    ELSIF max = BufSize THEN
      lastLoadPos := currentScanPos;
      fsReadBlock(scF, buffer, BufSize, max);
      count := 0; cur := buffer;
      WITH currentScanPos DO fsGetPos(scF, high, low) END;
    ELSE
      ScanEOF := TRUE; EXIT;
    END; (* IF *)
  END; (* LOOP *)
END ScanNextByte;


PROCEDURE ScanGetCurrentPos(VAR p:LONGCARD);
VAR
  q : LONGCARD;
BEGIN
  WITH q DO high := 0; low := count END;
  add(lastLoadPos, q, p);
END ScanGetCurrentPos;


PROCEDURE ScanSetCurrentPos(p:LONGCARD);
VAR
  b : BYTE;
BEGIN
  dec(p); WITH p DO fsSetPos(scF,high,low) END;
  ScanEOF := FALSE; count := BufSize; max := BufSize;  (* Reset buffer *)
  currentScanPos := p; ScanNextByte(b); 
END ScanSetCurrentPos;


PROCEDURE ScanSetScreenPos(p:LONGCARD);
BEGIN
  ScanSetCurrentPos(p); Synchronize;
END ScanSetScreenPos;


PROCEDURE SkipIndexSection(ret : BOOLEAN);
(* Skips keywords and dates at end of screen *)
VAR      
  b,b1 : BYTE;
  term : BOOLEAN;
  crkw,
  kw   : Keyword;
  i    : CARDINAL;
BEGIN
  (* Read date list *)
  REPEAT 
    ScanNextByte(b); ScanNextByte(b1);
    term := ((b = BYTE(0)) & (b1 = BYTE(0))) OR ScanEOF;
    IF (NOT term) & ret THEN scDateProc(ORD(b)+ORD(b1)*256) END;
  UNTIL term;

  (* Read keyword list *)
  ScanNextByte(b);
  WHILE (b # BYTE(EOR)) & (NOT ScanEOF) DO
    crkw[0] := CHAR(b); 
    FOR i := 1 TO (ORD(b) DIV 8) DO
      ScanNextByte(crkw[i]); 
    END; (* FOR *)
    Unpack(crkw,kw); IF ret THEN scKwProc(kw) END;
    ScanNextByte(b);
  END; (* WHILE *)
END SkipIndexSection;



PROCEDURE Synchronize;
(* Called when next character is a BOR *)

  PROCEDURE Skip(i:CARDINAL);
  BEGIN
    WHILE i>0 DO ScanNextByte(b); DEC(i) END;
  END Skip;

  PROCEDURE ReadWord(VAR w : WORD);
  VAR
    b, b1 : BYTE;
  BEGIN
    ScanNextByte(b); ScanNextByte(b1); w := WORD(ORD(b) + ORD(b1)*256);
  END ReadWord;

VAR
  b     : CHAR;
  len,
  date  : CARDINAL;
  recno : LONGCARD;
BEGIN
  ScanGetCurrentPos(currentScreen);
  ScanNextByte(b);

  IF (NOT ScanEOF) & (ORD(b)#BOR) THEN ScanEOF := TRUE END;

  WHILE (ORD(b)=BOR) & (NOT ScanEOF) DO
    ScanNextByte(b);
    IF ScanEOF THEN RETURN END;
    ReadWord(date); WITH recno DO ReadWord(high); ReadWord(low); END;
    scRecProc(date, recno);
    IF ORD(b)>127 THEN
      (* deleted screen; get its length *)
      ReadWord(len);
      Skip(len); ScanGetCurrentPos(currentScreen); ScanNextByte(b);
      IF ScanEOF THEN RETURN END;
    ELSE
      (* Skip to beginning of character sequence *)
      Skip(2); currentPage := 0; pageCount := 0; 
      LOOP
        ScanNextByte(b);
        IF (ORD(b)=EOS) OR ScanEOF THEN EXIT END;
        currentLine := ORD(b);
        ScanNextByte(b); lastLine := ORD(b);
        INC(currentPage);
        IF (currentLine<=lastLine) THEN
          IF attributes THEN
            (* Return leading blank lines *)
            repeatCount := (currentLine - MinR); lastC := CHR(EOL);
          END; (* IF *)
          EXIT;
        ELSE
          INC(pageCount);
        END; (* IF *)
      END; (* LOOP *)
      IF attributes & (pageCount # 0) THEN
        IF repeatCount = 0 THEN
          repeatCount := MARKER
        ELSE
          repeatCount := - repeatCount
        END; (* IF *)
      END; (* IF *)
      IF ORD(b)=EOS THEN
        SkipIndexSection(FALSE);
        IF oneScreen THEN
          (* Must signal that this screen is empty *)
          ScanEOF := TRUE; RETURN 
        ELSE
          ScanGetCurrentPos(currentScreen);  (** 30/07/87 **)
          ScanNextByte(b);
        END; (* IF *)
      ELSE
        currPos := -1; RETURN;
      END; (* IF *)
    END; (* IF *)
  END; (* WHILE *)
END Synchronize;



PROCEDURE ScanGetCharPos
            ( VAR line : CARDINAL;
              VAR col  : INTEGER );
BEGIN
  line := currentLine; col := currPos;
END ScanGetCharPos;



PROCEDURE ScanGetScreenPos
            ( VAR scr  : LONGCARD;
              VAR page : CARDINAL );
BEGIN
  scr := currentScreen; page := currentPage;
END ScanGetScreenPos;



PROCEDURE ScanNextChar(VAR c:CHAR);
(* Returns the next alphanumeric character in the file. 0C is returned if a
   page or screen break occured. *)
BEGIN
  IF repeatCount=0 THEN
    LOOP
      ScanNextByte(c); IF ScanEOF THEN EXIT END;
      CASE ORD(c) OF
        REP : 
          ScanNextByte(c); 
          IF c > 2C THEN
            repeatCount := ORD(c)-2; c := lastC;
          ELSE
            ScanEOF := TRUE; (* Error halt *)
          END; (* IF *)
          INC(currPos); RETURN;
      | EOL :
          INC(currentLine); 
          IF currentLine>lastLine THEN
            (* A new page begins *)
            ScanNextByte(c); 
            IF ORD(c)#EOS THEN
              LOOP
                currentLine := ORD(c);
                ScanNextByte(c); lastLine := ORD(c);
                INC(currentPage);
                IF currentLine<=lastLine THEN 
                  currPos := -1;
                  (* Return leading blanks of page *)
                  IF attributes THEN
                    c := CHR(EOP); lastC := CHR(EOL);
                    repeatCount := (currentLine - MinR);
                  END; (* IF *)
                  EXIT;
                END; (* IF *)
                ScanNextByte(c);
                IF ORD(c)=EOS THEN
                  SkipIndexSection(TRUE);
                  IF oneScreen THEN
                    ScanEOF := TRUE; (* awful hack! *)
                  ELSE
                    Synchronize;
                  END; (* IF *)
                  c := CHR(EOR); EXIT;
                END; (* IF *)
              END; (* LOOP *)
            ELSE
              (* New screen begins *)
              SkipIndexSection(TRUE);
              IF oneScreen THEN
                ScanEOF := TRUE;  (* awful hack! *)
              ELSE
                Synchronize;
              END; (* IF *)
              c := CHR(EOR);
            END; (* IF *)
          ELSE
            (* still on current page *)
            currPos := -1;
            (** 19/06/87 : sends terminator char on EOL **)
            c := CHR(EOL);
          END;
          IF NOT attributes THEN c := 0C END;
          EXIT;
      | wh..iu : 
          (* ignore attribute toggles *)
          IF attributes THEN EXIT END;
      ELSE
        (* must be a valid text character *)
        INC(currPos); 
        IF upperCase THEN c := CAP(c) END;
        lastC := c; EXIT;
      END; (* CASE *)
    END; (* LOOP *)
  ELSIF repeatCount > 0 THEN
    DEC(repeatCount); c := lastC; INC(currPos);
  ELSE (* repeatCount < 0 *)
    DEC(pageCount); c := CHR(EOP);
    IF pageCount = 0 THEN 
      IF repeatCount # MARKER THEN
        repeatCount := - repeatCount
      ELSE
        repeatCount := 0
      END; (* IF *)
    END; (* IF *)
  END; (* IF *)
END ScanNextChar;



BEGIN (* FileOp *)
  enabled := FALSE; fileLoaded := FALSE;
  WITH RootPos DO high := 0; low := IDroot; END; 
  empty[KeywordIndex] := TRUE; empty[DateIndex] := TRUE;
  ALLOCATE(buffer, BufSize); Normalize(buffer);
END FileOp.
