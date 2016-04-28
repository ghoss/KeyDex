(*****************************************************************************
*
* Name     : KDExec
*
* LastEdit : 29/05/87
* Project  : KeyDex system
* Purpose  : Executes DOS programs (shell command).
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
* 03/05/87 : Removed dependency on "KDScreen" and "KDEdit".
* 30/05/87 : Fixed bug in "ProgExec".
* 22/08/87 : Implemented test for extensions.
* 05/11/87 : Moved register variables into local procedure.
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

IMPLEMENTATION MODULE KDExec;


FROM SYSTEM IMPORT 
  ADR, DOSCALL, WORD, GETREG, SETREG, ADDRESS, CODE,
  AX, BX, CX, DX, SI, DI, ES, DS;

FROM SystemIO IMPORT 
  ClearScreen, InstallHandler, UninstallHandler;

FROM String IMPORT
  AssignStr;

FROM Menus IMPORT
  MessageBox;

FROM KDScreen IMPORT
  UpdateTime;


PROCEDURE ProgExec(VAR filename : ARRAY OF CHAR);
(* Executes a DOS file. *)
CONST
  str01 = 'This is not a valid DOS program.';
VAR
  c : CHAR;        (* Would be a string in more sophisticated versions *)
  p : RECORD       (* DOS parameter block *)
        sa : CARDINAL;
        cl : ADDRESS;
        f1h, f1l, f2h, f2l : CARDINAL;
      END;
  progname : ARRAY [0..64] OF CHAR;
  error,
  i       : CARDINAL;
  quit    : BOOLEAN;
  (** 05/11/87 **)
  ax, bx, cx, dx, si, di, es, ds, ss, bp, sp : WORD;
BEGIN
  ClearScreen; UninstallHandler;

  (* Test for valid extension *)

  quit := FALSE; i := ORD(filename[0]);
  IF (i > 4) & (filename[i-3] = '.') THEN
    IF NOT (
         ((filename[i] = 'E') & (filename[i-1] = 'X') & (filename[i-2] = 'E'))
      OR ((filename[i] = 'M') & (filename[i-1] = 'O') & (filename[i-2] = 'C')))
    THEN 
      quit := TRUE;
    END; (* IF *)
  ELSE
    quit := TRUE;
  END; (* IF *)
  IF quit THEN
    MessageBox(str01, TRUE); InstallHandler(UpdateTime); RETURN
  END; (* IF *)

  (* Initialize parameter block *)
  WITH p DO
    sa := 0; cl := ADR(c); f1h := 0; f1l := 0; f2h := 0; f2l := 0;
  END;
  c := 0C;   (* Command line is empty *)
  AssignStr(progname, filename);

  (* Save current program state *)
  GETREG(AX,ax); GETREG(BX,bx); GETREG(CX,cx); GETREG(DX,dx);
  GETREG(ES,es); GETREG(SI,si); GETREG(DS,ds); GETREG(DI,di);
  CODE(8CH,0D0H,89H,0E3H,89H,0E9H);  (* MOV AX,SS / MOV BX,SP / MOV CX,BP *)
  GETREG(AX,ss); GETREG(BX,sp); GETREG(CX,bp);

  DOSCALL(4BH,ADR(progname),ADR(p),0,error);

  (* Restore program state *)
  SETREG(AX,ss); SETREG(BX,sp); SETREG(CX,bp);
  CODE(8EH,0D0H,89H,0DCH,89H,0CDH);  (* MOV SS,AX / MOV SP,BX / MOV BP,CX *)
  SETREG(AX,ax); SETREG(BX,bx); SETREG(CX,cx); SETREG(DX,dx);
  SETREG(ES,es); SETREG(SI,si); SETREG(DS,ds); SETREG(DI,di);

  (* The following code will be executed upon return *)
  error := error MOD 256;
  CASE error OF
     0 : (** 30/05/87 : added **)
   | 5 : MessageBox('Unable to access this file', TRUE);
   | 8 : MessageBox('Insufficient memory for execution', TRUE);
   |11 : MessageBox('Invalid file format', TRUE);
  ELSE
    MessageBox('Unexpected DOS error', TRUE);
  END; (* CASE *)
  InstallHandler(UpdateTime);
END ProgExec;


END KDExec.
