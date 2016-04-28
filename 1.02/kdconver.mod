(*****************************************************************************
*
* Name     : KDConversions
*
* LastEdit : 05/04/87
* Project  : KeyDex system
* Purpose  : Converts cardinals to strings and vice versa.
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

IMPLEMENTATION MODULE KDConversions;


FROM String IMPORT
  Clear, InsertCh;


CONST 
  NearMaxCard = 65535 DIV 10;


PROCEDURE StrToCard(VAR s:ARRAY OF CHAR; VAR x:CARDINAL; VAR ok:BOOLEAN);
VAR
  c,i : CARDINAL;
BEGIN
  c := 0;
  FOR i := 1 TO ORD(s[0]) DO
    IF (c<NearMaxCard) OR ((c=NearMaxCard) & (s[i]<='5')) THEN
      c := c*10+(ORD(s[i])-ORD('0'));
    ELSE
      ok := FALSE; RETURN (* overflow *)
    END; (* IF *)
  END; (* FOR *)
  x := c; ok := TRUE;
END StrToCard;


PROCEDURE CardToStr(x:CARDINAL; VAR s:ARRAY OF CHAR);
BEGIN
  Clear(s);
  REPEAT
    InsertCh(s,CHR(ORD('0')+(x MOD 10)),1); x := x DIV 10;
  UNTIL (x=0);
END CardToStr;


END KDConversions.
