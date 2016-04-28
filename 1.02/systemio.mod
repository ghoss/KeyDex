(*****************************************************************************
*
* Name    : SystemIO
*
* Created : 23/12/86
* Project : KeyDex system.
* Purpose : Basic screen input/output routines. 
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
* 29/05/87 : Removed tests on memory overflow.
* 13/06/87 : Added "ReturnBufferAddress".
* 14/06/87 : Implemented support for CGA and EGA.
*            Rewritten "ScrollArea" and "ClearArea".
* 21/06/87 : Code statements inserted to preserve BP after SWI(10H).
* 29/07/87 : Keyboard routines now call DOS instead of BIOS.
*            Windows Save/Restore routines rewritten.
* 05/11/87 : m2c 3.0 : Fixed storage problems.
* 24/12/87 : Added implementation of 'HelpKey'.
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

IMPLEMENTATION MODULE SystemIO;


FROM SYSTEM IMPORT 
  ADDRESS, ADR, BYTE, TSIZE, SETREG, GETREG, SWI, CODE, AX, BX, CX, DX;

FROM Storage IMPORT
  ALLOCATE, DEALLOCATE;


TYPE
  CharPtr    = POINTER TO ScrCh;
  Descriptor = RECORD
                 hx0,hx1 : ScrX;
                 hy0,hy1 : ScrY;
                 buffer  : CharPtr;  (* dynamic array *)
               END;

  Handle = POINTER TO Descriptor;


VAR
  Scr : 
    POINTER TO ARRAY ScrY,ScrX OF ScrCh; (* Data structure of the screen *)

  VSTATE [0H:449H] : BYTE;      (* Video mode *)
  OFFSET [0H:44EH] : CARDINAL;  (* Offset to screen page *)

  init,
  Installed : BOOLEAN;
  callproc  : PROC;


PROCEDURE ClearScreen();
VAR
  ax : CARDINAL;
  ScrAddress : ADDRESS;
BEGIN
  (*FOR x := 0 TO MaxX DO
    FOR y := 0 TO MaxY DO
      Scr^[y,x] := blank;
    END;
  END;*)

  (* Get current video mode *)
  IF (VSTATE # BYTE(7)) THEN
    (* EGA or CGA *)
    ScrAddress := 0B800H:0;
  ELSE
    (* monochrome video mode *)
    ScrAddress := 0B000H:0;
  END; (* IF *)
  ax := ORD(VSTATE); SETREG(AX,ax); 
  CODE(55H); SWI(10H); CODE(5DH);
  INC(ScrAddress,OFFSET); Scr := ScrAddress; 
END ClearScreen;



PROCEDURE ClearArea(x0,x1:ScrX; y0,y1:ScrY);
BEGIN
  ScrollArea(x0,x1,y0,y1,0);  (* See ROM BIOS manual *)
END ClearArea;



PROCEDURE ScrollArea(x0,x1:ScrX; y0,y1:ScrY; no:INTEGER);
VAR
  ax,bx,cx,dx : CARDINAL;
BEGIN
  ax := ABS(no); IF no >= 0 THEN INC(ax,600H) ELSE INC(ax,700H) END;
  cx := y0*256 + x0; dx := y1*256 + x1;
  bx := white * 256;
  SETREG(AX,ax); SETREG(BX,bx); SETREG(CX,cx); SETREG(DX,dx); 
  CODE(55H); SWI(10H); CODE(5DH);
END ScrollArea;




PROCEDURE SaveArea(x0,x1:ScrX; y0,y1:ScrY; VAR p:Handle; VAR Res:BOOLEAN);
VAR
  size,x,y : CARDINAL;
  q,r : CharPtr;
BEGIN
  size := (x1-x0+1)*(y1-y0+1)*TSIZE(ScrCh);
  NEW(p); 
  WITH p^ DO
    ALLOCATE(buffer, size);
    q := buffer;
    hx0 := x0; hx1 := x1; hy0 := y0; hy1 := y1;
    (* Store the window contents into a linear array *)
    FOR y := y0 TO y1 DO
      r := CharPtr(ADR(Scr^[y,x0]));
      FOR x := x0 TO x1 DO
        q^ := r^; 
        q := CharPtr(ADDRESS(q) + TSIZE(ScrCh)); 
        r := CharPtr(ADDRESS(r) + TSIZE(ScrCh));
      END; (* FOR *)
    END; (* FOR *)
  END; (* WITH *)
  Res := TRUE;
END SaveArea;



PROCEDURE RestoreArea(VAR p:Handle);
VAR
  x,y : CARDINAL;
  q,r : CharPtr;
BEGIN
  WITH p^ DO
    r := buffer;
    FOR y := hy0 TO hy1 DO
      q := CharPtr(ADR(Scr^[y,hx0]));
      FOR x := hx0 TO hx1 DO
        q^ := r^; 
        q := CharPtr(ADDRESS(q) + TSIZE(ScrCh));
        r := CharPtr(ADDRESS(r) + TSIZE(ScrCh));
      END; (* FOR *)
    END; (* FOR *)
    DEALLOCATE(buffer, (hx1-hx0+1)*(hy1-hy0+1)*TSIZE(ScrCh));
  END; (* WITH *)
  DISPOSE(p);
END RestoreArea;



PROCEDURE PutChar(sc:ScrCh; x:ScrX; y:ScrY);
(* Easy work for a monochrome adapter *)
BEGIN
  Scr^[y,x] := sc;
END PutChar;



PROCEDURE GetChar(VAR sc:ScrCh; x:ScrX; y:ScrY);
BEGIN
  sc := Scr^[y,x];
END GetChar;



PROCEDURE ReadChar(VAR C:CHAR; VAR FK:BOOLEAN);
VAR
  A : CARDINAL;
BEGIN
  (* Uses BIOS interrupt $16 *)
  REPEAT UNTIL KeyPressed();

  SETREG(AX,0); SWI(16H); GETREG(AX,A);
  FK := (A MOD 256)=0;
  IF FK THEN 
    C := CHR(A DIV 256)
  ELSE 
    C := CHR(A MOD 256) 
  END; (* IF *)
END ReadChar;



PROCEDURE HelpKey(c : CHAR) : BOOLEAN;
BEGIN
  RETURN (c = CHR(59));   (* Key [F1] *)
END HelpKey;



PROCEDURE KeyPressed():BOOLEAN;
VAR
  A : CARDINAL;
BEGIN
  IF Installed THEN callproc END;

  SETREG(AX,0100H); SWI(16H);
  CODE(9CH,          (* PUSHF        *)
       58H,          (* POP AX       *)
       25H,40H,00H   (* AND AX,0040H *)
      );
  GETREG(AX,A); RETURN (A = 0);
END KeyPressed;



PROCEDURE SetCursor(x:ScrX; y:ScrY);
BEGIN
  (* IF init THEN
    Scr^[oldy,oldx].at := oldat;
  ELSE
    init := TRUE
  END;
  WITH Scr^[y,x] DO
    oldat := at; at := BYTE(reverse+blink); oldx := x; oldy := y;
  END; *)

  SETREG(DX,y*256+x); SETREG(BX,0); SETREG(AX,200H); 
  CODE(55H); SWI(10H); CODE(5DH);
END SetCursor;



PROCEDURE CursorOff();
BEGIN
  (* IF init THEN Scr^[oldy,oldx].at := oldat; init := FALSE END; *)

  SETREG(DX,5050H); SETREG(BX,0); SETREG(AX,200H); 
  CODE(55H); SWI(10H); CODE(5DH);
  SETREG(AX,100H); SETREG(CX,000DH); 
  CODE(55H); SWI(10H); CODE(5DH);
END CursorOff;



PROCEDURE InstallHandler(p:PROC);
BEGIN
  Installed := TRUE; callproc := p;
END InstallHandler;


PROCEDURE UninstallHandler;
BEGIN
  Installed := FALSE;
END UninstallHandler;



PROCEDURE ReturnBufferAddress(VAR a:ADDRESS);
BEGIN
  a := Scr;
END ReturnBufferAddress;



BEGIN (* SystemIO *)
  CursorOff; ClearScreen;
  Installed := FALSE; init := FALSE; blank.ch := ' '; blank.at := BYTE(white);
END SystemIO.
