(*****************************************************************************
*
* Name    : KDCritErr
*
* Created : 18/07/87
* Project : KeyDex system
* Purpose : Handles critical disk errors.
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
* 27/08/87 : Restored indicator after error.
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)


IMPLEMENTATION MODULE KDCritErr;

FROM SystemIO IMPORT
  InstallHandler, UninstallHandler;

FROM KDScreen IMPORT
  UpdateTime, SetIndicator;

FROM FileOp IMPORT
  CriticalHandler, ErrorType, EnableHandler, DisableHandler;

FROM Menus IMPORT
  DiskErrorBox;

IMPORT
  KDScreen;


VAR
  printer : BOOLEAN;


PROCEDURE CritErr(err : ErrorType);
(* Critical error procedure for disk errors *)
CONST
  prErr = 'Printer not ready.';
VAR
  action : BOOLEAN;
BEGIN
  (* The time update routine must be disabled during a critical error
     because it works with DOS function calls. *)
  UninstallHandler;
  CASE err OF
    driveerr :
      IF NOT printer THEN
        DiskErrorBox('Disk drive not ready.', action); 
      ELSE
        DiskErrorBox(prErr, action);
      END; (* IF *)
  | error :
      DiskErrorBox('Data error while reading disk', action); 
  | writeprotect :
      DiskErrorBox('Disk is write protected.', action); 
  | consistencyerr :
      DiskErrorBox('Wrong disk in drive.', action); 
  | illegalfile :
      DiskErrorBox('Access to file denied by DOS.', action);
  END; (* CASE *)
  InstallHandler(UpdateTime); SetIndicator(KDScreen.wait);
  IF NOT action THEN DisableHandler END;
END CritErr;



PROCEDURE InvokeHandler(pr : BOOLEAN);
BEGIN
  CriticalHandler(CritErr); EnableHandler; printer := pr;
END InvokeHandler;


PROCEDURE DispatchHandler;
(* Activate resp. disable the critical error handler. *)
BEGIN
  DisableHandler; printer := FALSE;
END DispatchHandler;


BEGIN
  printer := FALSE;
END KDCritErr.
