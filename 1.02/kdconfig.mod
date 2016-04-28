(*****************************************************************************
*
* Name     : KDConfig
*
* LastEdit : 05/04/87
* Project  : KeyDex system
* Purpose  : Manages configuration file.
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
* 05/04/87 : Beep mode, frequency and tab stops added to configuration file.
* 28/04/87 : Implemented "StrToDate".
* 03/06/87 : Default pathnames now always terminate with '\'.
* 06/06/87 : Storage optimization.
*            Introduced "DateToStr".
* 13/07/87 : Implemented "FileNameChar".
*            Changed to new file system.
* 30/07/87 : August has 31 days.
* 02/09/87 : Added printer codes to configuration file.
* 26/10/87 : Minor modifications to LoadConfigFile.
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

IMPLEMENTATION MODULE KDConfig;


FROM SYSTEM IMPORT 
  BYTE;

FROM MsFileSystem IMPORT
  fsOpen, fsCreate, fsClose, fsReadByte, fsWriteByte, fsFile, fsDelete, 
  fsWriteWord, fsReadWord, fsResult, fsAccessMode, fsShareMode, fsErrorType,
  fsGetCurrentDir;

FROM Term IMPORT
  WriteString, WriteCard, Write;

FROM String IMPORT
  Clear, AppendCh, AssignAOC;


CONST
  ConfigFile = 'KEYDEX.CFG';

VAR
  cFile   : ARRAY [0..10] OF CHAR;


PROCEDURE DispTime(time:CARDINAL);
BEGIN
  WriteCard(time DIV 60,2,FALSE);
  Write(TimeChar[TimeType]); WriteCard(time MOD 60,2,TRUE);
END DispTime;



PROCEDURE DispDate(day:CARDINAL);
(* Displays date *)
VAR
  s : ARRAY [0..8] OF CHAR;
BEGIN
  DateToStr(day,s); WriteString(s);
END DispDate;



PROCEDURE DateToStr(day:CARDINAL; VAR s:ARRAY OF CHAR);
(* Converts a date in binary format to string format *)
VAR
  d,m,y,x : CARDINAL;
BEGIN
  s[0] := CHR(8);
  s[3] := DateChar[DateType]; s[6] := s[3];
  y := day DIV 512; m := (day DIV 32) MOD 16; d := day MOD 32;
  (* Convert first two digits *)
  CASE DateType OF
    0   : x := m;
  | 1,2 : x := d;
  | 3   : x := y;
  END; (* CASE *)
  s[1] := CHR((x DIV 10) + ORD('0'));
  s[2] := CHR((x MOD 10) + ORD('0'));
  (* Convert middle digits *)
  IF DateType=0 THEN x := d ELSE x := m END;
  s[4] := CHR((x DIV 10) + ORD('0'));
  s[5] := CHR((x MOD 10) + ORD('0'));
  (* Convert last two digits *)
  IF DateType=3 THEN x := d ELSE x := y END;
  s[7] := CHR((x DIV 10) + ORD('0'));
  s[8] := CHR((x MOD 10) + ORD('0'));
END DateToStr;
  


PROCEDURE StrToDate(VAR s:ARRAY OF CHAR; VAR day:CARDINAL);
VAR
  c : CHAR;
  a1,a2,a3,t : INTEGER;
BEGIN
  day := 0; c := DateChar[DateType];
  IF (s[0]=CHR(8)) & (s[3]=c) & (s[6]=c) THEN
    (* Delimiters correct *)
    a1 := (ORD(s[1])-ORD('0'))*10+(ORD(s[2])-ORD('0'));
    a2 := (ORD(s[4])-ORD('0'))*10+(ORD(s[5])-ORD('0'));
    a3 := (ORD(s[7])-ORD('0'))*10+(ORD(s[8])-ORD('0'));
    (** 06/06/87 : "A1 := T" put in cond.stmt. *)
    IF DateType=0 THEN
      t := a2; a2 := a1; a1 := t;
    ELSIF DateType=3 THEN
      t := a3; a3 := a1; a1 := t;
    END;
    (* After this, a1=day, a2=month, a3=year *)
    IF   ((a3>=0) & (a3<=99))
       & ((a2>=1) & (a2<=12))
       & ((a1>=1) & (a1<=INTEGER(ORD(DaysMonth[a2])))) 
    THEN
      day := CARDINAL(a3)*512+CARDINAL(a2)*32+CARDINAL(a1)
    END; (* IF *)
  END; (* IF *)
END StrToDate;



PROCEDURE FileNameChar(c : CHAR) : CHAR;
BEGIN
  c := CAP(c);
  CASE c OF
    '@'..'Z', '0'..':', '!', '#', '-', '.', '\', '^', '`', '_', '~' :
      RETURN c
  ELSE
    RETURN 0C
  END; (* CASE *)
END FileNameChar;


PROCEDURE SaveConfigFile;
VAR 
  b  : BYTE;
  i  : CARDINAL;
  j  : ScrAttribute;
  cf : fsFile;
BEGIN
  fsDelete(cFile); fsCreate(cFile);
  fsOpen(cFile, smDenyReadWrite, amWrite, cf);
  IF fsResult() = etOK THEN
    fsWriteByte(cf, BYTE(26)); fsWriteByte(cf, BYTE(Version));
    fsWriteByte(cf, CHR(DateType)); fsWriteByte(cf, CHR(TimeType));
    fsWriteByte(cf, beepMode); fsWriteWord(cf, beepFrequency); 
    fsWriteByte(cf, CHR(tabInterval));
    FOR i := 0 TO ORD(DataPath[0]) DO fsWriteByte(cf,DataPath[i]) END;
    FOR i := 0 TO ORD(IndexPath[0]) DO fsWriteByte(cf,IndexPath[i]) END;
    FOR i := 0 TO ORD(Program[0]) DO fsWriteByte(cf,Program[i]) END;
    b := VAL(BYTE, pageLength); fsWriteByte(cf, b);
    FOR j := atReverse TO atTitle DO
      WITH printerCode[j] DO
        fsWriteByte(cf, atOn[0]);
        FOR i := 1 TO ORD(atOn[0]) DO fsWriteByte(cf, atOn[i]) END;
        fsWriteByte(cf, atOff[0]);
        FOR i := 1 TO ORD(atOff[0]) DO fsWriteByte(cf, atOff[i]) END;
      END; (* WITH *)
    END; (* FOR *)
    fsClose(cf);
  END; (* IF *)
  IF fsResult() # etOK THEN fsDelete(cFile) END;
END SaveConfigFile;



PROCEDURE LoadConfigFile;
VAR
  b : BYTE;
  i : CARDINAL;
  dir : ARRAY [0..64] OF CHAR;
  ch  : CHAR;
  cf  : fsFile;
  j   : ScrAttribute;
BEGIN
  (* Assign default pathname *)
  fsGetCurrentDir(dir); AssignAOC(DataPath,dir); 
  (** 03/06/87 **)
  IF DataPath[0] # 3C THEN AppendCh(DataPath,'\') END;
  IndexPath := DataPath; Clear(Program);
  TimeType := 0; DateType := 0; beepMode := FALSE; beepFrequency := 100;
  tabInterval := 8; pageLength := 72;
  FOR j := atReverse TO atTitle DO
    WITH printerCode[j] DO
      Clear(atOn); Clear(atOff);
    END; (* WITH *)
  END; (* FOR *)
  (* Load configuration file *)
  fsOpen(cFile, smDenyWrite, amRead, cf);
  IF fsResult() = etOK THEN
    fsReadByte(cf,b); 
    IF b=BYTE(26) THEN
      fsReadByte(cf,b);
      IF b=BYTE(Version) THEN
        fsReadByte(cf,b);
        IF ORD(b) <= 3 THEN 
          DateType := ORD(b); fsReadByte(cf,b);
          IF ORD(b) <= 3 THEN
            TimeType := ORD(b); fsReadByte(cf,b);
            IF ORD(b) <= ORD(TRUE) THEN
              beepMode := BOOLEAN(b); fsReadWord(cf,i);
              IF (i >= 100) & (i <= 4000) THEN
                beepFrequency := i; fsReadByte(cf,b); 
                IF (ORD(b) > 0) & (ORD(b) < 80) THEN
                  tabInterval := ORD(b); fsReadByte(cf, ch);
                  IF ch <= CHR(64) THEN
                    IF ch # 0C THEN DataPath[0] := ch END;
                    FOR i := 1 TO ORD(ch) DO 
                      fsReadByte(cf,DataPath[i])
                    END; (* FOR *)
                    fsReadByte(cf, ch);
                    IF ch <= CHR(64) THEN
                      IF ch # 0C THEN IndexPath[0] := ch END;
                      FOR i := 1 TO ORD(ch) DO 
                        fsReadByte(cf,IndexPath[i])
                      END; (* FOR *)
                      fsReadByte(cf, ch);
                      IF ch <= CHR(64) THEN
                        Program[0] := ch;
                        FOR i := 1 TO ORD(ch) DO 
                          fsReadByte(cf,Program[i])
                        END; (* FOR *)
                        fsReadByte(cf, b); pageLength := ORD(b);
                        FOR j := atReverse TO atTitle DO
                          WITH printerCode[j] DO
                            fsReadByte(cf, atOn[0]);
                            IF ORD(atOn[0]) <= prcLength THEN
                              FOR i := 1 TO ORD(atOn[0]) DO
                                fsReadByte(cf, atOn[i]);
                              END; (* FOR *)
                              fsReadByte(cf, atOff[0]);
                              IF ORD(atOff[0]) <= prcLength THEN
                                FOR i := 1 TO ORD(atOff[0]) DO
                                  fsReadByte(cf, atOff[i]);
                                END; (* FOR *)
                              END; (* IF *)
                            END; (* IF *)
                          END; (* WITH *)
                        END; (* FOR *)
                      END; (* IF *)
                    END; (* IF *)
                  END; (* IF *)
                END; (* IF *)
              END; (* IF *)
            END; (* IF *)
          END; (* IF *)
        END; (* IF *)
      END; (* IF *)
    END; (* IF *)
    fsClose(cf);
  END; (* IF *)
END LoadConfigFile;



BEGIN (* KDConfig *)
  cFile := ConfigFile;
  TimeChar := ':.,h'; DateChar := '//.-'; LoadConfigFile;
  DaysMonth[1]  := CHR(30);
  DaysMonth[2]  := CHR(29);  (* should be improved *)
  DaysMonth[3]  := CHR(31);
  DaysMonth[4]  := CHR(30);
  DaysMonth[5]  := CHR(31);
  DaysMonth[6]  := CHR(30);
  DaysMonth[7]  := CHR(31);
  DaysMonth[8]  := CHR(31);
  DaysMonth[9]  := CHR(30);
  DaysMonth[10] := CHR(31);
  DaysMonth[11] := CHR(30);
  DaysMonth[12] := CHR(31);
END KDConfig.
