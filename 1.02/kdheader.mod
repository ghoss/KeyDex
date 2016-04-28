(*****************************************************************************
*
* Name     : KDHeader
*
* LastEdit : 05/04/87
* Project  : KeyDex system
* Purpose  : Displays startup screen with copyright notice.
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
* 20/08/87 : Changed actions to procedure call.
* 11/10/87 : Changes to border, header highlighted
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

IMPLEMENTATION MODULE KDHeader;


FROM SYSTEM IMPORT
  BYTE;
  
FROM SystemIO IMPORT
  white, intensity;
  
FROM Term IMPORT
  Read, WriteAOC, CursorY, CursorX, Font;

FROM CopyProtect IMPORT
  EvalRelease;
  

PROCEDURE TitlePage;
VAR
  c : CHAR;
BEGIN
  CursorY := 6;
  CursorX := 22; WriteAOC('ษอออออออออออออออออออออออออออออออออออป'); INC(CursorY);
  CursorX := 22; WriteAOC('บ          ');
  Font := BYTE(white + intensity);
  WriteAOC('K e y D e x (TM)');
  Font := BYTE(white);
  WriteAOC('         บ'); INC(CursorY);
  CursorX := 22; WriteAOC('วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ'); INC(CursorY);
  CursorX := 22; WriteAOC('บ    Copyright 1987 by Guido Hoss   บ'); INC(CursorY);
  CursorX := 22; WriteAOC('บ        All Rights Reserved.       บ'); INC(CursorY);
  IF EvalRelease() THEN
    CursorX := 22; WriteAOC('บ         ');
    Font := BYTE(white + intensity);
    WriteAOC('Evaluation Release'); 
    Font := BYTE(white); 
    WriteAOC('        บ'); INC(CursorY);
  END; (* IF *)
  CursorX := 22; WriteAOC('บ                                   บ'); INC(CursorY);
  CursorX := 22; WriteAOC('บ         Version : 1.02            บ'); INC(CursorY);
  CursorX := 22; WriteAOC('บ         Date    : 26/03/88        บ'); INC(CursorY);
  CursorX := 22; WriteAOC('ศอออออออออออออออออออออออออออออออออออผ'); INC(CursorY);
  INC(CursorY);
  CursorX := 22; WriteAOC('       Press any key to start..      ');
  Read(c);
END TitlePage;


END KDHeader.
