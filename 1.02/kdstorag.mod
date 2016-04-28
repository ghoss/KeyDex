(*****************************************************************************
*
* Name    : KDStorage
*
* Created : 29/05/87
* Project : KeyDex system.
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
* 06/06/87 : Removed dependency on "Menus".
* 05/11/87 : Added 'Normalize'.
* 03/12/87 : Added 'InverseNormalize'.
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

IMPLEMENTATION MODULE KDStorage;

  FROM SYSTEM IMPORT
    ADDRESS;

  FROM RTSMain IMPORT
    Terminate, Status;

  IMPORT Storage;

  FROM Term IMPORT
    WriteAOC, Write, CursorY;


  PROCEDURE ALLOCATE(VAR p:ADDRESS; s:CARDINAL);
  BEGIN
    IF Storage.Available(s) THEN
      Storage.ALLOCATE(p, s);
    ELSE
      Write(CHR(12)); CursorY := 12; 
      WriteAOC('Memory overflow. Terminating.');
      Terminate(Warning);
    END; (* IF *)
  END ALLOCATE;


  PROCEDURE DEALLOCATE(VAR p:ADDRESS; s:CARDINAL);
  BEGIN
    Storage.DEALLOCATE(p, s);
  END DEALLOCATE;
  
  
  PROCEDURE Normalize(VAR a : ADDRESS);
  BEGIN
    WITH a DO
      INC(SEGMENT, OFFSET DIV 16);
      OFFSET := OFFSET MOD 16;
    END; (* WITH *)
  END Normalize;
  
  
  PROCEDURE InverseNormalize(VAR a : ADDRESS);
  VAR
    max : CARDINAL;
  BEGIN
    WITH a DO
      max := (65535 - OFFSET) DIV 16;
      IF SEGMENT < max THEN max := SEGMENT END;
      INC(OFFSET, max * 16); DEC(SEGMENT, max);
    END; (* WITH *)
  END InverseNormalize;


END KDStorage.
