{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit USystemTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Process,
  UUtilities;

  procedure ListDrives(var out_drives:TStringList);
  procedure ExecuteCommand(commandText:string; var out_results:TStringList);
  function GetTemp:string;

implementation

{$if defined(windows)}
uses Windows;

// https://forum.lazarus.freepascal.org/index.php?topic=594.0
procedure ListDrives(var out_drives:TStringList);
var
  Drive: Char;
  DriveLetter: string;
  OldMode: Word;
  dt:integer;
begin
  out_drives.Clear;
  OldMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    // Search all drive letters
    for Drive := 'A' to 'Z' do
    begin
      DriveLetter := Drive + ':\';

      dt:=GetDriveType(PChar(DriveLetter));
      if (dt=DRIVE_REMOVABLE)
        or (dt=DRIVE_FIXED)
        or (dt=DRIVE_REMOTE)
        or (dt=DRIVE_CDROM)
        or (dt=DRIVE_RAMDISK) then
        out_drives.Add(DriveLetter);
    end;
  except

  end;
  // Restores previous Windows error mode.
  SetErrorMode(OldMode);
end;

{$else}

procedure ListDrives(var out_drives:TStringList);
var tf:TextFile;
    s:string;
begin
  out_drives.Clear;
  ExecuteCommand('ls /',out_drives);
end;

{$endif}

procedure ExecuteCommand(commandText:string; var out_results:TStringList);
const bufSize=2048;
var proc: TProcess;
    bytesRead:integer;
    ms:TStream;
    buf: array[0..bufSize-1] of byte;
    spacePos:integer;
begin
  proc := TProcess.Create(nil);

  {$IFDEF Windows}
  proc.Executable:= 'cmd';
  proc.Parameters.Add('/c');
  proc.Parameters.Add(commandText);
  {$ENDIF Windows}

  {$IFDEF Unix}
  spacePos:=Pos(' ',commandText);
  if spacePos>0 then
  begin
    proc.Executable:= '/bin/sh';
    proc.Parameters.Add('-c');
    proc.Parameters.Add(commandText);
  end
  else
  begin
    proc.Executable := commandText;
  end;
  {$ENDIF Unix}

  proc.Options := proc.Options + [{poWaitOnExit,} poUsePipes, poStderrToOutPut, poNoConsole];
  proc.Execute;

  ms := TMemoryStream.Create;
  repeat
    bytesRead := proc.Output.Read(buf, bufSize);
    ms.Write(buf, bytesRead);
  until BytesRead = 0;  // Stop if no more data is available

  if Assigned(out_results) then
  begin
    ms.Position:=0;
    out_results.LoadFromStream(ms);
  end;

  ms.Free;
  proc.Free;
end;

function GetTemp:string;
var cmdResults:TStringList;
begin
  {$IFDEF Windows}
  result:='N/A';
  {$ENDIF Windows}
  {$IFDEF Unix}
  cmdResults:=TStringList.Create;
  ExecuteCommand('/opt/vc/bin/vcgencmd measure_temp',cmdResults);
  if (cmdResults.Count=1) and (Pos('temp=',cmdResults.Strings[0])=1) then
    result := Copy(cmdResults.Strings[0],6,Length(cmdResults.Strings[0])-6)
  else
    result := 'N/A';
  cmdResults.Free;
  {$ENDIF Unix}
end;

end.

