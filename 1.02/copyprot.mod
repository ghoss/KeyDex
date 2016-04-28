(*****************************************************************************
*
* Name     : CopyProtect
*
* Created  : 24/10/87
* Project  : KeyDex system
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
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

IMPLEMENTATION MODULE CopyProtect;


CONST
  test    = TRUE;   (* TRUE if evaluation release *)
  
    
PROCEDURE CheckPermission;
(* Hangs system if payment overdue. *)
END CheckPermission;


PROCEDURE EvalRelease() : BOOLEAN;
BEGIN
  RETURN test;
END EvalRelease;


END CopyProtect. 