(*****************************************************************************
*
* Name    : Sound
*
* Created : 05/06/87
* Project : Modula Library.
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
* 16/06/87 : Hardware determination code rewritten.
* 28/04/16 : Code cleanup and release under GPLv3 license
******************************************************************************)

IMPLEMENTATION MODULE Sound;

FROM SYSTEM IMPORT
  GETREG, SETREG, WORD, BYTE, CODE, AX;

TYPE
  Tone = RECORD
           Pitch    : CARDINAL;    (* in Hz *)
           Duration : CARDINAL;    (* in 100ths of a second *)
         END;

CONST
  PINB        = 0E4H;     
  POUTB       = 0E6H;
  DivisorPort = 66;      (* port for clock divisor    *)
  InitPort    = 67;      (* port for clock init       *)
  SwitchPort  = 97;      (* port to switch on timer   *)
  Initialize  = 0B6H;    (* flags to initialize clock *)
  Clock1      = 59642;   (* part one of clock freq    *)
  Clock2      = 20;      (* part two of clock freq    *)

VAR
  Divisor : CARDINAL;    (* divisor sent to clock     *)
  Divishi : CARDINAL;    (* divisor high byte         *)
  Divislo : CARDINAL;    (* divisor low byte          *)
  Status  : BITSET;      (* sound chip status byte    *)
  Multiplier : CARDINAL;

 

(*-- SoundOn turns the sound ON to the specified frequency (Hz)      *)
PROCEDURE SoundOn(Freq:CARDINAL);
VAR
  I:WORD;
BEGIN
  SETREG(AX, Initialize);
  CODE(POUTB, InitPort);                 (* initialize clock          *)
  Divisor := (Clock1 DIV Freq)*Clock2;   (* calculate clock divisor   *)
  Divishi := Divisor DIV 256;            (* get divisor high byte     *)
  Divislo := Divisor MOD 256;            (* get divisor low byte      *)

  SETREG(AX, Divislo);
  CODE(POUTB, DivisorPort);              (* send low byte to clock    *)
  SETREG(AX, Divishi);
  CODE(POUTB, DivisorPort);              (* ditto for high *)
  CODE(PINB, SwitchPort);
  GETREG(AX,I);
  Status := BITSET(I);                   (* get clock status     *)
  INCL(Status, 0);
  INCL(Status, 1);                       (* turn clock status to ON   *)
  SETREG(AX,Status);
  CODE(POUTB, SwitchPort);               (* send new status to clock  *)
END SoundOn;

 

(*-- SoundOff turns the sound OFF                           *)
PROCEDURE SoundOff();
VAR
  I:WORD;
BEGIN
  CODE(PINB, SwitchPort);
  GETREG(AX,I);
  Status := BITSET(I);                   (* get clock status     *)
  EXCL(Status, 0);
  EXCL(Status, 1);                       (* set clock status to OFF   *)
  SETREG(AX, Status);
  CODE(POUTB, SwitchPort);               (* send new clock status     *)
END SoundOff;

 

(*-- delay is just a wait loop the time, t, is in 100 ths of a sec *)
PROCEDURE delay(t : CARDINAL);
VAR
  i, j, k : CARDINAL;
BEGIN
  FOR i := 1 TO t DO
    FOR j := 1 TO 90 DO
      (* This loop takes about 1/100 second to execute *)
      k := j*3; k := k DIV 5;
    END; (* FOR *)
  END; (* FOR *)
END delay;


(*-- Melody plays a series of sounds, each with a specified duration *)
PROCEDURE Melody(Tune : ARRAY OF Tone);
VAR
  count : CARDINAL;
BEGIN
  FOR count := 0 TO HIGH(Tune) DO
    SoundOn(Tune[count].Pitch);
    delay(Tune[count].Duration*Multiplier);
  END;
  SoundOff();    (* turn the sound off *)
END Melody;



PROCEDURE BeepTone(freq,time : CARDINAL);
BEGIN
  SoundOn(freq); delay(time); SoundOff();
END BeepTone;



(*-- sounds a standard 'Huh?' noise                                  *)
PROCEDURE ErrorNoise();
(*
VAR
  Noise : ARRAY [1..4] OF Tone;
BEGIN
  Noise[1].Pitch := 750;
  Noise[1].Duration := 1;
  Noise[2].Pitch := 1000;
  Noise[2].Duration := 1;
  Noise[3].Pitch := 1500;
  Noise[3].Duration := 2;
  Noise[4].Pitch := 2250;
  Noise[4].Duration := 4;
  Melody(Noise);
*)
END ErrorNoise;

 

(*-- sounds a standard 'LOOK OUT !!' sound                           *)
PROCEDURE UrgentNoise();
VAR
  Noise : ARRAY [1..3] OF Tone;
BEGIN
  Noise[1].Pitch := 450;
  Noise[1].Duration := 3;
  Noise[2].Pitch := 3000;
  Noise[2].Duration := 30;
  Noise[3].Pitch := 4500;
  Noise[3].Duration := 3;
  Melody(Noise);
END UrgentNoise;

 

(*-- sounds a standard 'Ok!' noise                                   *)
PROCEDURE AttentionNoise();
(*
VAR
  Noise : ARRAY [1..5] OF Tone;
BEGIN
  Noise[1].Pitch := 800;
  Noise[1].Duration := 3;
  Noise[2].Pitch := 600;
  Noise[2].Duration := 4;
  Noise[3].Pitch := 1200;
  Noise[3].Duration := 1;
  Noise[4].Pitch := 600;
  Noise[4].Duration := 4;
  Noise[5].Pitch := 1400;
  Noise[5].Duration := 1;
  Melody(Noise);
  Melody(Noise);
*)
END AttentionNoise;

 

(*-- plays standard success Noise                                    *)
PROCEDURE SuccessNoise();
(*
VAR
  Noise : ARRAY [1..2] OF Tone;
BEGIN
  Noise[1].Pitch := 100;
  Noise[1].Duration := 1;
  Noise[2].Pitch := 200;
  Noise[2].Duration := 1;
  Melody(Noise);
*)
END SuccessNoise;

 

(*-- The following code determines whether we are on an IBM PC/AT    *)
(*-- The timing must be adjusted for the AT's faster clock speed     *)
TYPE
  MachineType = (PC, PCAT, NonIBM);

  
PROCEDURE Hardware() : MachineType;
VAR
  HWtype[0F000H:0FFFEH] : BYTE;
BEGIN
  CASE ORD(HWtype) OF
     0FFH, 0FEH, 0FDH :
       RETURN PC;
   | 0FCH :
       RETURN PCAT;
  ELSE
    RETURN NonIBM;
  END; (* CASE *)
END Hardware;

  
BEGIN
  IF Hardware()=PCAT THEN
    Multiplier := 4;
  ELSE
    Multiplier := 1;
  END;
END Sound.
