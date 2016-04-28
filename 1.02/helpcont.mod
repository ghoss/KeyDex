(****************************************************************************
*
* Name    : HelpContext
*
* Created : 24/12/87
* Project : KeyDex 1.0x system
* Purpose : Defines help screen numbers.
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
* 28/04/16 : Code cleanup and release under GPLv3 license
*****************************************************************************)

IMPLEMENTATION MODULE HelpContext;


CONST
  sshKD999    =  0;
  sshKD015    =  1;
  sshKD001    =  2;
  sshKD002    =  3;
  sshKD024    =  4;
  sshKD025    =  5;
  sshKD026    =  6;
  sshKD027    =  7;
  sshKD007    =  8;
  sshKD010    =  9;
  sshKD011    = 10;
  sshKD012    = 11;
  sshKD013    = 12;
  sshKD014    = 13;
  sshKD030    = 14;
  sshKD016    = 15;
  sshKD017    = 16;
  sshKD018    = 17;
  sshKD020    = 18;
  sshKD022    = 19;
  sshKD023    = 20;
  sshKD019    = 21;
  sshKD021    = 22;
  sshKD028    = 23;
  sshKD029    = 24;
  sshKD031    = 25;
  sshKD032    = 26;
  sshKD033    = 27;
  sshKD034    = 28;
  sshKD035    = 29;
  sshKD036    = 30;
  sshKD037    = 31;
  sshKD038    = 32;
  sshKD039    = 33;
  sshKD040    = 34;
  sshKD041    = 35;
  sshKD042    = 36;
  sshKD043    = 37;
  sshKD044    = 38;
  sshKD045    = 39;
  sshKD046    = 40;
  sshKD047    = 41;
  sshKD048    = 42;
  sshKD049    = 43;
  sshKD050    = 44;
  sshKD051    = 45;
  sshKD052    = 46;
  sshKD053    = 47;
  sshKD054    = 48;


PROCEDURE ScreenNumber
            ( m : ModuleIndex;
              i : CARDINAL ) : INTEGER;
BEGIN
  CASE m OF
    kdedit :
      CASE i OF
        1 : RETURN sshKD001;
      END; (* CASE *)
  | keydex :
      CASE i OF
        1 : RETURN sshKD007;
      | 2 : RETURN sshKD999;
      | 3 : RETURN sshKD016;
      | 4 : RETURN sshKD015;
      | 5 : RETURN sshKD017;
      | 6 : RETURN -1;
      END; (* CASE *)
  | filestuff :
      CASE i OF
        1 : RETURN sshKD010;
      | 2 : RETURN sshKD011;
      | 3 : RETURN sshKD034;
      | 4 : RETURN sshKD012;
      | 5 : RETURN sshKD013;
      | 6 : RETURN sshKD014;
      | 8 : RETURN sshKD041;
      END; (* CASE *)
  | setstuff :
      CASE i OF
        1, 2 :
            RETURN sshKD024;
      | 3 : RETURN sshKD025;
      | 5 : RETURN sshKD026;
      | 6 : RETURN sshKD044;
      | 7 : RETURN sshKD043;
      | 8 : RETURN sshKD028;
      |10 : RETURN sshKD027;
      |11 : RETURN sshKD041;
      END; (* CASE *)
  | kwstuff :
      CASE i OF
        1 : RETURN -1;
      | 2 : RETURN sshKD035;
      | 3 : RETURN sshKD036;
      | 5 : RETURN sshKD037;
      | 6 : RETURN sshKD039;
      | 7 : RETURN sshKD038;
      | 8 : RETURN sshKD040;
      | 9 : RETURN sshKD042;
      |11 : RETURN sshKD041;
      END; (* CASE *)
  | kdprint :
      CASE i OF
        1 : RETURN sshKD030
      | 2 : RETURN sshKD031;
      | 3 : RETURN sshKD032;
      | 4 : RETURN sshKD033;
      | 6,9 : 
            RETURN sshKD052;
      | 8 : RETURN sshKD041;
      |10 : RETURN sshKD053;
      |11 : RETURN sshKD054;
      END; (* CASE *)
  END; (* CASE *)
END ScreenNumber;

END HelpContext.