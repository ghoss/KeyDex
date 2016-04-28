(*****************************************************************************
*
* Name    : FileSystem
*
* Created : 28/06/87
* Project : Modula-2 Library 
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
* 29/07/87 : Fixed bug in "DirQuery".
* 18/08/87 : Implemented "fsGetCommandLine".
* 05/11/87 : m2c 3.0 : Fixed storage problems.
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

IMPLEMENTATION MODULE MsFileSystem;

  FROM SYSTEM IMPORT
    BYTE, WORD, ADDRESS, ADR, DOSCALL, PROCESS, TRANSFER, TSIZE,
    NEWPROCESS, IOTRANSFER;

  FROM RTSMain IMPORT
    ProcessDescriptor;

  FROM Storage IMPORT
    ALLOCATE;

  FROM DOS3 IMPORT
    GetExtendedError, GetProgramSegmentPrefix;


  CONST 
    maxHandler = 16;    (* Max. no. of user defined error handlers *)
    WrkSize1   = 2048;  (* Process workspace sizes *)


  TYPE
    ProcessPtr = POINTER TO ProcessDescriptor;

    fsFile    = WORD;                     (* DOS file handle *)
    callType  = (readProc, writeProc, driveProc, noProc);

    HandlerRec = RECORD
                   proc    : fsErrorHandler;
                   enabled : BOOLEAN;
                 END; (* RECORD *)

    DTA = RECORD
            fill : ARRAY [0..29] OF BYTE;
            str  : ARRAY [0..12] OF CHAR;
          END; (* RECORD *)


  VAR
    error     : CARDINAL;              (* DOS error code as detected *)
    lasterr   : fsErrorType;
    origin    : callType;              (* Where did error come from? *)
    hStack    : ARRAY [0..maxHandler] OF HandlerRec;
    curHdl    : CARDINAL;              (* Points to current error procedure *)
    criterr,
    invalid   : BOOLEAN;               (* TRUE if data read is garbage *)
    curDTA    : POINTER TO DTA;        (* DTA used by 'DirQuery' *)
    WrkSpace1 : ADDRESS;               (* Process stacks *)
    cmdLine   : ARRAY [0..127] OF CHAR;(* DOS Command line *)

    DOSversion : CHAR;             (* Major version number of DOS in use *)

    mainProc,                      (* Process interrupted by critical error *)
    criticalErrorProc : PROCESS;   (* Critical error handler called by DOS *)



  PROCEDURE CriticalError;
  (* This is a critical error interrupt handler called by DOS if a technical
     problem occurs. *)
  CONST
    retry  = 1;
    abort  = 3; (* Undocumented DOS return code!! Use instead of 2. *)
  VAR
    temp       : ProcessPtr;
    t1, t2, t3 : BYTE;
  BEGIN (* CriticalHandler *)
    LOOP
      (* Catch next interrupt $24 *)
      IOTRANSFER(criticalErrorProc, mainProc, 24H);

      (* Gain access to DI through field in process descriptor *)
      temp := ProcessPtr(mainProc);
      criterr := TRUE;

      IF DOSversion >= 3C THEN
        (* We can get an extended error code *)
        GetExtendedError(0, error, t1, t2, t3);
        CASE error OF
          23,25,26,27,29,30 :
            lasterr := etDiskError;
        | 21,31 :
            lasterr := etDriveError;
        | 19 :
            lasterr := etWriteProtect;
        | 32,33 :
            lasterr := etNoAccess;
        | 34 :
            lasterr := etDiskChange;
        ELSE
          lasterr := etInternal; (**) HALT;
        END; (* CASE *)
      ELSE
        error := temp^.topStack^.DI MOD 256;
        CASE error OF
          11,10,8,7,6,4 :
            lasterr := etDiskError;
        | 12,2 :
            lasterr := etDriveError;
        | 0 : 
            lasterr := etWriteProtect;
        ELSE
          lasterr := etInternal; (**) HALT;
        END; (* CASE *)
      END; (* IF *)

      (* Call standard error handler (stack is on our own workspace) *)
      ProcessError; 

      (* Only retry action if user proc installed (otherwise endless loop *)
      invalid := (NOT hStack[curHdl].enabled);
      IF invalid THEN
        temp^.topStack^.AX := abort;
      ELSE
        temp^.topStack^.AX := retry; lasterr := etOK; 
      END; (* IF *)
    END; (* LOOP *)
  END CriticalError;

                            

  PROCEDURE ProcessError;
  (* This procedure checks if an error has occured and calls the error
     handler if necessary. *)
  BEGIN (* ProcessError *)
    IF NOT criterr THEN
      invalid := FALSE;
      CASE origin OF
        noProc :
          (* Standard DOS error *)
          CASE error OF
             0 : lasterr := etOK; RETURN;
          |  2 : lasterr := etNoFile;
          |  3 : lasterr := etNoPath;
          |  4 : lasterr := etNoHandle;
          |  5 : lasterr := etNoAccess;
          | 16 : lasterr := etCurrDir;
          | 17 : lasterr := etDiskChange;
          | 18 : lasterr := etAllFiles;
          |  1,6,11,12,13,15 :
               lasterr := etInternal; (**) HALT;
          END; (* CASE *)
    
      | readProc :
          lasterr := etEndOfFile
   
      | writeProc :
          lasterr := etDiskFull

      | driveProc :
          lasterr := etNoPath;
      END; (* CASE *)
      origin := noProc;
    ELSE
      criterr := FALSE;
    END; (* IF *)
    WITH hStack[curHdl] DO
      (* Call user procedure if enabled *)
      IF enabled THEN proc(lasterr) END;
    END; (* WITH *)
  END ProcessError;


  PROCEDURE fsOpen
              ( fn     : ARRAY OF CHAR;  (* Name of file to be opened *)
                sm     : fsShareMode;    (* Access for other processes *)
                am     : fsAccessMode;   (* Own access intentions *)
                VAR fh : fsFile );       (* Returned file handle *)
  BEGIN (* fsOpen *)
    DOSCALL(3DH, ADR(fn), (ORD(sm)*16+ORD(am)), fh, error);
    ProcessError;
  END fsOpen;


  PROCEDURE fsCreate
              ( fn : ARRAY OF CHAR );    (* Name of file to be created *)
  VAR
    fh : fsFile;
  BEGIN (* fsCreate *)
    DOSCALL(3CH, ADR(fn), 0, fh, error);
    IF error = 0 THEN
      DOSCALL(3EH, fh, error);   (* Close file immediately *)
    END; (* IF *)
    ProcessError;
  END fsCreate;


  PROCEDURE fsClose
              ( fh : fsFile );   (* Handle of file to be closed *)
  BEGIN (* fsClose *)
    DOSCALL(3EH, fh, error);
    ProcessError;
  END fsClose;


  PROCEDURE fsDelete
              ( fn : ARRAY OF CHAR );   (* Name of file to be deleted *)
  BEGIN (* fsDelete *)
    DOSCALL(41H, ADR(fn), error);
    ProcessError;
  END fsDelete;


  PROCEDURE fsRename
              ( old,new : ARRAY OF CHAR );   (* Name of old file and new file *)
  BEGIN (* fsRename *)
    DOSCALL(56H, ADR(old), ADR(new), error);
    ProcessError;
  END fsRename;

  
  PROCEDURE fsFlush
              ( fh : fsFile );    (* Handle of file to be flushed *)
  BEGIN (* fsFlush *)
    HALT;  (* Unimplemented *)
  END fsFlush;


  (*
   *   File Input/Output Operations
   *)


  PROCEDURE fsReadByte
             ( fh    : fsFile;    (* Handle of file to be read *)
               VAR b : BYTE );    (* Byte to be read will be stored here *)
  VAR
    rb : CARDINAL;
  BEGIN (* fsReadByte *)
    DOSCALL(3FH, fh, TSIZE(BYTE), ADR(b), rb, error);
    IF NOT invalid THEN
      IF rb # TSIZE(BYTE) THEN
        origin := readProc;
      END; (* IF *)
      ProcessError;
    ELSE
      invalid := FALSE
    END; (* IF *)
  END fsReadByte;


  PROCEDURE fsReadWord
              ( fh    : fsFile;   (* Handle of file *)
                VAR w : WORD );   (* Word to be read will be stored here *)
  VAR
    rw : CARDINAL;
  BEGIN (* fsReadWord *)
    DOSCALL(3FH, fh, TSIZE(WORD), ADR(w), rw, error);
    IF NOT invalid THEN
      IF rw # TSIZE(WORD) THEN
        origin := readProc;
      END; (* IF *)
      ProcessError;
    ELSE
      invalid := FALSE
    END; (* IF *)
  END fsReadWord;


  PROCEDURE fsWriteByte
              ( fh : fsFile;      (* Handle of file to be written *)
                b  : BYTE );      (* Byte to be written *)
  VAR
    wb : CARDINAL;
  BEGIN (* fsWriteByte *)
    DOSCALL(40H, fh, TSIZE(BYTE), ADR(b), wb, error);
    IF NOT invalid THEN
      IF wb # TSIZE(BYTE) THEN
        origin := writeProc;
      END; (* IF *)
      ProcessError;
    ELSE
      invalid := FALSE
    END; (* IF *)
  END fsWriteByte;


  PROCEDURE fsWriteWord
              ( fh : fsFile;      (* Handle of file *)
                w  : WORD );      (* Word to be written *)
  VAR
    wr : CARDINAL;
  BEGIN (* fsWriteWord *)
    DOSCALL(40H, fh, TSIZE(WORD), ADR(w), wr, error);
    IF NOT invalid THEN
      IF wr # TSIZE(WORD) THEN
        origin := writeProc;
      END; (* IF *)
      ProcessError;
    ELSE
      invalid := FALSE
    END; (* IF *)
  END fsWriteWord;


  PROCEDURE fsReadBlock
              ( fh : fsFile;          (* Handle of file to be read *)
                bf : ADDRESS;         (* Address of destination buffer *)
                rb : CARDINAL;        (* Number of bytes to be read *)
                VAR br : CARDINAL );  (* Bytes actually read *)
  BEGIN (* fsReadBlock *)
    DOSCALL(3FH, fh, rb, bf, br, error);
    IF invalid THEN 
      br := 0; invalid := FALSE;
    ELSIF br # rb THEN
      origin := readProc; ProcessError;
    END; (* IF *)
  END fsReadBlock;


  PROCEDURE fsWriteBlock
              ( fh : fsFile;          (* Handle of file to be written *)
                bf : ADDRESS;         (* Address of source buffer *)
                wb : CARDINAL;        (* Number of bytes to be written *)
                VAR bw : CARDINAL );  (* Bytes actually written *)
  BEGIN (* fsWriteBlock *)
    DOSCALL(40H, fh, wb, bf, bw, error);
    IF invalid THEN 
      bw := 0; invalid := FALSE
    ELSIF bw # wb THEN
      origin := writeProc; ProcessError;
    END; (* IF *)
  END fsWriteBlock;
                               

  PROCEDURE fsSetPos
              ( fh    : fsFile;      (* File to be repositioned *)
                ph,pl : CARDINAL );  (* Low and high 16 bits of position *)
  VAR
    oh,ol : CARDINAL;
  BEGIN (* fsSetPos *)
    DOSCALL(42H, fh, 0, ph, pl, oh, ol, error);
    ProcessError;
  END fsSetPos;


  PROCEDURE fsGetPos
              ( fh : fsFile;              (* File to get position of *)
                VAR ph,pl : CARDINAL );   (* Low and high 16 bits of position *)
  BEGIN (* fsGetPos *)
    DOSCALL(42H, fh, 1, 0, 0, ph, pl, error);
    ProcessError;
  END fsGetPos;
  
  
  PROCEDURE fsLength
              ( fh : fsFile;
                VAR lh, ll : CARDINAL );
  BEGIN (* fsLength *)
    DOSCALL(42H, fh, 2, 0, 0, lh, ll, error);
    ProcessError;
  END fsLength;


  (*
   *   Directory Management Routines
   *)


  PROCEDURE fsMakeDir
              ( dn : ARRAY OF CHAR );   (* Name of directory to create *)
  BEGIN (* fsMakeDir *)
    DOSCALL(39H, ADR(dn), error);
    ProcessError;
  END fsMakeDir;


  PROCEDURE fsRemoveDir
              ( dn : ARRAY OF CHAR );   (* Name of directory to remove *)
  BEGIN (* fsRemoveDir *)
    DOSCALL(3AH, ADR(dn), error);
    ProcessError;
  END fsRemoveDir;


  PROCEDURE fsChangeDir
              ( dn : ARRAY OF CHAR );   (* Name of new working directory *)
  VAR
    cd,d,n : CARDINAL;
  BEGIN (* fsChangeDir *)
    IF dn[1]=':' THEN
      (* Change current drive too *)
      DOSCALL(19H, cd); DOSCALL(0EH, cd, n);
      IF dn[0] < 'A' THEN origin := driveProc
      ELSE
        d := ORD(CAP(dn[0])) - ORD('A');
        IF (d+1) > n THEN
          origin := driveProc;
        ELSE
          DOSCALL(0EH, d, n);
        END; (* IF *)
      END; (* IF *)
    END; (* IF *)
    IF origin = noProc THEN 
      DOSCALL(3BH, ADR(dn), error);
    END; (* IF *)
    ProcessError;
  END fsChangeDir;


  PROCEDURE fsGetCurrentDir
              ( VAR dn : ARRAY OF CHAR );    (* Returns current directory *)
  VAR
    cd : CARDINAL;
  BEGIN (* fsGetCurrentDir *)
    (* Get current drive *)
    DOSCALL(19H, cd);
    dn[0] := CHR(cd + ORD('A')); dn[1] := ':'; dn[2] := '\';
    DOSCALL(47H, 0, ADR(dn[3]), error);
    ProcessError;
  END fsGetCurrentDir;


  PROCEDURE fsDirQuery
              ( fn : ARRAY OF CHAR;    (* File specification to search for *)
                pr : fsQueryProc );    (* Proc to be called for each name *)
  VAR
    res : BOOLEAN;
  BEGIN (* fsDirQuery *)
    (* Reset DTA *)
    DOSCALL(1AH, curDTA);  (** 29/07/87 **)
    (* Find first entry *)
    DOSCALL(4EH, ADR(fn), 0, error);
    ProcessError;  
    IF lasterr = etOK THEN
      LOOP
        res := pr(curDTA^.str);
        IF NOT res THEN lasterr := etAllFiles; EXIT END;
        (* Find next entry *)
        DOSCALL(4FH, error); ProcessError;
        IF lasterr # etOK THEN EXIT END;
      END; (* LOOP *)
    END; (* IF *)
  END fsDirQuery;


(*
 *    Miscallaneous Routines
 *)

  PROCEDURE fsGetCommandLine
              ( VAR s : ARRAY OF CHAR );   (* command line *)
  VAR
    i,max : CARDINAL;
  BEGIN
    max := ORD(cmdLine[0]);
    IF max > HIGH(s) THEN max := HIGH(s) END;
    FOR i := 0 TO max DO
      s[i] := cmdLine[i];
    END; (* FOR *)
  END fsGetCommandLine;


  (*
   *   Error Handling
   *)


  PROCEDURE fsResult() : fsErrorType;
  BEGIN
    RETURN lasterr;
  END fsResult;


  PROCEDURE fsEnableHandler;
              (* Enables the current error handler. If an operation causes an
                 error, the handler is called with the error type. The error
                 handler must then respond with an action code. The
                 default system handler is disabled by default. If it is
                 enabled and an error occurs, the program is terminated. *)
  BEGIN
    hStack[curHdl].enabled := TRUE;
  END fsEnableHandler;


  PROCEDURE fsDisableHandler;
              (* Disables the current error handler. Any error occuring can 
                 only be detected via the 'fsResult' function. *)
  BEGIN
    hStack[curHdl].enabled := FALSE;
  END fsDisableHandler;


  PROCEDURE fsInstallHandler
              ( h : fsErrorHandler );
  BEGIN
    IF curHdl = maxHandler THEN HALT
    ELSE
      INC(curHdl); 
      WITH hStack[curHdl] DO
        enabled := FALSE;
        proc := h;
      END; (* WITH *)
    END; (* IF *)
  END fsInstallHandler;


  PROCEDURE fsDispatchHandler;
  BEGIN
    IF curHdl = 0 THEN HALT
    ELSE
      DEC(curHdl);
    END; (* IF *)
  END fsDispatchHandler;


  PROCEDURE DefaultHandler(et : fsErrorType);
  (* The default error handler. Halts if enabled. *)
  BEGIN
    HALT;
  END DefaultHandler;


TYPE
  CharPtr = POINTER TO CHAR;
VAR
  temp   : CHAR;
  i, len : CARDINAL;
  adr    : ADDRESS;
  adr1   : CharPtr;
BEGIN (* FileSystem *)
  (* Get DOS version number *)
  DOSCALL(30H, DOSversion, temp);
  IF DOSversion < 2C THEN HALT END;

  origin := noProc; lasterr := etOK; criterr := FALSE; invalid := FALSE;

  (* Install default handler *)
  curHdl := 0; 
  WITH hStack[0] DO proc := DefaultHandler; enabled := FALSE; END;

  (* Get command line arguments *)
  IF DOSversion > 2C THEN
    (* Get address of PSP *)
    WITH adr DO
      GetProgramSegmentPrefix(adr.SEGMENT);
      OFFSET := 128;
    END; (* WITH *)
    adr1 := CharPtr(adr); len := ORD(adr1^);
    IF len > 127 THEN
      HALT;  (* We are not prepared for a longer command line *)
    ELSE
      FOR i := 0 TO len DO
        cmdLine[i] := adr1^; adr1 := CharPtr(ADDRESS(adr1) + 1);
      END; (* FOR *)
    END; (* IF *)
  ELSE
    cmdLine[0] := 0C;
  END; (* IF *)

  (* Get current DTA for 'DirQuery' *)
  DOSCALL(2FH, curDTA);

  (* Set up handler processes *)
  ALLOCATE(WrkSpace1, WrkSize1);
  NEWPROCESS(CriticalError, WrkSpace1, WrkSize1, criticalErrorProc);
  TRANSFER(mainProc, criticalErrorProc);
END MsFileSystem.
