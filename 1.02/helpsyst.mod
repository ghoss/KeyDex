(****************************************************************************
*
* Name    : HelpSystem
*
* Created : 23/12/87
* Purpose : Provides the interface to the SoftScreen/Help runtime manager.
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
* 13/04/88 : Declared variable as CARDINAL to avoid range errors.
* 28/04/16 : Code cleanup and release under GPLv3 license
*****************************************************************************)

IMPLEMENTATION MODULE HelpSystem;

FROM SYSTEM IMPORT
  WORD, ADDRESS, CODE, AX, BX, CX, DX, DS, GETREG, SETREG, SWI, ADR;
  
  
TYPE
  Reg80x86 =
    RECORD
      ax, bx, cx, dx, ds : CARDINAL;
    END; (* RECORD *)
    
    
VAR
  installed,              (* TRUE if runtime manager present *)
  error,                  (* TRUE if error occured *)
  enabled    : BOOLEAN;   
  nextScreen : INTEGER;   (* Next help screen to be displayed *)
  helpVector : CARDINAL;  (* Number of runtime interrupt vector *)
  handle     : CARDINAL;  (* Help file handle in use *)
  ErrorProc  : HelpErrorProc;

 
  
PROCEDURE HelpInstalled() : BOOLEAN;
BEGIN
  RETURN enabled;
END HelpInstalled;


PROCEDURE CallRTM(VAR regs : Reg80x86);
  (* Calls the runtime manager with arguments based on the values in
     'r'. Also does error handling. *)
VAR
  ax0, bx0, cx0, dx0, ds0, oldDS : CARDINAL;
  err : HelpErrorType;
BEGIN
  (* Make local copy of parameters *)
  WITH regs DO
    ax0 := ax; bx0 := bx; cx0 := cx; dx0 := dx;
    ds0 := ds;
  END; (* WITH *)
  error := FALSE;
  
  (* Initialize processor registers and do the call *)
  CASE helpVector OF
    60H :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(60H)
  | 61H :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(61H)
  | 62H :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(62H)
  | 63H :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(63H)
  | 64H :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(64H)
  | 65H :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(65H)
  | 66H :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(66H)
  | 67H :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(67H)
  | 68H :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(68H)
  | 69H :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(69H)
  | 6AH :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(6AH)
  | 6BH :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(6BH)
  | 6CH :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(6CH)
  | 6DH :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(6DH)
  | 6EH :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(6EH)
  | 6FH :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(6FH)
  | 70H :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(70H)
  | 71H :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(71H)
  | 72H :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(72H)
  | 73H :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(73H)
  | 74H :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(74H)
  | 75H :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(75H)
  | 76H :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(76H)
  | 77H :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(77H)
  | 78H :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(78H)
  | 79H :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(79H)
  | 7AH :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(7AH)
  | 7BH :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(7BH)
  | 7CH :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(7CH)
  | 7DH :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(7DH)
  | 7EH :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(7EH)
  | 7FH :
      GETREG(DS, oldDS);
      SETREG(AX, ax0); SETREG(BX, bx0); SETREG(CX, cx0); SETREG(DX, dx0);
      SETREG(DS, ds0);
      SWI(7FH)
  END; (* CASE *)
  
  (* Check carry flag to find out if error occured *)
  CODE(
    073H, 005H,        (* JNC @ + 7    *)
    089H, 0C3H,        (* MOV BX, AX   *)
    0B8H, 0FFH, 0FFH   (* MOV AX, (-1) *)
  );
  
  (* Copy processor registers to local storage *)
  GETREG(AX, ax0); GETREG(BX, bx0); GETREG(CX, cx0); GETREG(DX, dx0);
  GETREG(DS, ds0);
  SETREG(DS, oldDS);
  error := (ax0 = 0FFFFH);
  
  (* Set up return values *)
  IF NOT error THEN
    WITH regs DO
      ax := ax0; bx := bx0; cx := cx0; dx := dx0;
      ds := ds0;
    END; (* WITH *)
  ELSE
    CASE INTEGER(bx0) OF
      -2, 3 : 
        err := hetNotInstalled;
    | -1, 0, 1, 6, 7, 8, 9, 10 :
        err := hetInternal;
    | 2, 4, 13 :
        (* Error number 13 is returned for 'no file', don't know why *)
        err := hetNoFile;
    | 5 :
        err := hetDisk;
    ELSE
      err := hetInternal;
    END; (* CASE *)
    ErrorProc(err);
  END; (* IF *) 
END CallRTM;



PROCEDURE OwnError(n : HelpErrorType);
  (* Private error handler, only used during module initialization. *)
BEGIN
  installed := FALSE;
END OwnError;



PROCEDURE InitSystem;
  (* Initializes the help system *)
CONST
  MagicNum = WORD(0FACEH);
TYPE
  IRRange = [60H..7FH];  (* Range in which interrupt vector can be found *)
VAR
  i    : CARDINAL; (** 13/04/88 **)
  p    : ADDRESS;
  regs : Reg80x86;
  ivTable [0:0] : ARRAY [0H..0FFH] OF ADDRESS;  (* Interrupt table *)
BEGIN
  (* First, find the interrupt vector of the runtime manager *)
  i := MIN(IRRange);
  LOOP
    (* Test vector no. i *)
    p := ivTable[i]; INC(p, 2);
    IF (p^ = MagicNum) THEN EXIT END;
    INC(i); IF (i > MAX(IRRange)) THEN EXIT END;
  END; (* LOOP *)
  installed := (i <= MAX(IRRange));
  enabled := FALSE; nextScreen := -1;

  (* Call initialization function 0 *)
  IF installed THEN
    helpVector := i;
    WITH regs DO
      ax := 0; bx := 0;
    END; (* WITH *)
    ErrorProc := OwnError;
    CallRTM(regs);
  END; (* IF *)
END InitSystem;



PROCEDURE OpenHelpFile
            ( name : ARRAY OF CHAR;   (* ASCII file name of help file *)
              proc : HelpErrorProc ); (* Error procedure to be called *)
VAR
  p    : ADDRESS;
  regs : Reg80x86;
BEGIN
  ErrorProc := proc;
  IF installed THEN
    p := ADR(name);
    WITH regs DO
      WITH p DO
        ax := 0100H; bx := 8H;
        ds := SEGMENT; dx := OFFSET;
      END; (* WITH *)
    END; (* WITH *)
    CallRTM(regs);
    handle := regs.ax;
    enabled := NOT error;
  ELSE
    ErrorProc(hetNotInstalled);
  END; (* IF *)
END OpenHelpFile;


     
PROCEDURE CloseHelpFile();
VAR
  regs : Reg80x86;
BEGIN
  IF enabled THEN
    WITH regs DO
      ax := 0200H; bx := handle;
    END; (* WITH *)
    CallRTM(regs);
    enabled := FALSE;
  END; (* IF *)
END CloseHelpFile;


  
PROCEDURE SetHelpContext(n : INTEGER);
BEGIN
  nextScreen := n;
END SetHelpContext;


     
PROCEDURE EnterHelpSystem();
VAR
  regs : Reg80x86;
BEGIN
  IF enabled & (nextScreen # -1) THEN
    WITH regs DO
      ax := 0300H; bx := handle;
      cx := 0H; dx := VAL(CARDINAL, nextScreen);
    END; (* WITH *)
    CallRTM(regs);
  END; (* IF *)
END EnterHelpSystem;



PROCEDURE DisplayHelpPage(n : INTEGER; wait, restore : BOOLEAN);
VAR
  regs : Reg80x86;
BEGIN
  IF enabled THEN
    WITH regs DO
      ax := 0500H; bx := handle; dx := n; cx := 0H;
      IF wait THEN INC(cx, 1) END;
      IF restore THEN INC(cx, 2) END;
    END; (* WITH *)
    CallRTM(regs);
  END; (* IF *)
END DisplayHelpPage;

     
BEGIN (* HelpSystem *)
  InitSystem;
END HelpSystem.