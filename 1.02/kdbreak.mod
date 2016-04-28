(*****************************************************************************
*
* Name    : KDBreak
*
* Created : 22/08/87
* Project : KeyDex system.
* Purpose : Tests if user interrupts action.
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
* 22/08/87 : The current version polls the keyboard.
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)


IMPLEMENTATION MODULE KDBreak;

FROM SystemIO IMPORT
  KeyPressed, ReadChar;


PROCEDURE UserBreak() : BOOLEAN;
VAR
  c  : CHAR;
  fk : BOOLEAN;
BEGIN
  IF KeyPressed() THEN
    ReadChar(c, fk); RETURN TRUE;
  ELSE
    RETURN FALSE;
  END; (* IF *)
END UserBreak;


END KDBreak.
