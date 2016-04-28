(*****************************************************************************
*
* Name     : LongCardinals
*
* LastEdit : 28/04/87
* Project  : General Modula-2 library module.
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
* 02/05/87 : Implemented add and mult.
* 03/06/87 : Implemented inc and dec.
* 05/11/87 : Bug fixed in 'mult'.
* 12/12/87 : Bug fixed in 'add'.
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

IMPLEMENTATION MODULE LongCardinals;


CONST
  MaxCard = 65535;  (* equals 2^16 (word size) - 1 *)


PROCEDURE less(a,b:LONGCARD) : BOOLEAN;
BEGIN
  RETURN (a.high<b.high) OR ((a.high=b.high) AND (a.low<b.low)) 
END less;


PROCEDURE equal(a,b:LONGCARD) : BOOLEAN;
BEGIN
  RETURN (a.high=b.high) AND (a.low=b.low)
END equal;


PROCEDURE multCard(a,b:CARDINAL; VAR result:LONGCARD);
(* This procedure multiplies two 16-bit cardinals *)
VAR
  a1,a2,b1,b2,t : CARDINAL;
  temp : LONGCARD;
BEGIN
  WITH result DO
    a1 := (a DIV 256); a2 := (a MOD 256);
    b1 := (b DIV 256); b2 := (b MOD 256);
    high := a1 * b1; low := a2 * b2;
    t := a1 * b2;
    WITH temp DO high := (t DIV 256); low := (t MOD 256) * 256 END;
    add(temp,result,result);
    t := a2 * b1;
    (** 05/11/87 : "* 256" added **)
    WITH temp DO high := (t DIV 256); low := (t MOD 256) * 256 END;
    add(temp,result,result);
  END; (* WITH *)
END multCard;


PROCEDURE mult(a,b:LONGCARD; VAR result:LONGCARD);
VAR
  temp,temp1 : LONGCARD;
BEGIN
  multCard(a.high, b.low, temp);
  multCard(a.low, b.high, temp1);
  add(temp, temp1, temp);
  WITH temp DO high := low; low := 0 END;
  multCard(a.low, b.low, temp1);
  add(temp, temp1, result);
END mult;



PROCEDURE add(a,b:LONGCARD; VAR result:LONGCARD);
BEGIN
  WITH result DO
    high := a.high + b.high;
    IF b.low <= (MaxCard-a.low) THEN
      (* No overflow *)
      low := a.low + b.low;
    ELSE
      (** 12/12/87 **)
      low := b.low - ((MaxCard - a.low) + 1);;
      INC(high);
    END; (* IF *)
  END; (* WITH *)
END add;


PROCEDURE inc(VAR a:LONGCARD);
BEGIN
  WITH a DO
    IF low < MaxCard THEN
      INC(low)
    ELSE
      INC(high); low := 0;
    END; (* IF *)
  END; (* WITH *)
END inc;


PROCEDURE dec(VAR a:LONGCARD);
BEGIN
  WITH a DO
    IF low > 0 THEN
      DEC(low)
    ELSE
      DEC(high); low := MaxCard;
    END; (* IF *)
  END; (* WITH *)
END dec;


BEGIN
  WITH zero DO high := 0; low := 0 END;
  WITH one DO high := 0; low := 1 END;
END LongCardinals.
