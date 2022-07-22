{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit UUtilities;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, LCLType, UConstants;

type
  TSwitchToScreenEvent=procedure (dstScreenId:integer; newScreenProcessId:integer=spNone) of object;
  TRepaintRequestEvent=procedure of object;

  TStringsArray=array of string;

  TTrackInfo=record
    path:string;
    title:string;
    expectedLength:integer;
  end;
  TTracksInfoArray=array of TTrackInfo;

  procedure AddString(s:string; inOut_stringsArray:TStringsArray);
  procedure CopyTrackInfo(var src:TTrackInfo; var dst:TTrackInfo);
  function SwapTrackInfo(item1Id,item2Id:integer; var tracksArray:TTracksInfoArray):boolean;
  procedure TerminateApp;

implementation

procedure AddString(s:string; inOut_stringsArray:TStringsArray);
begin
  SetLength(inOut_stringsArray,Length(inOut_stringsArray)+1);
  inOut_stringsArray[High(inOut_stringsArray)]:=s;
end;

procedure CopyTrackInfo(var src:TTrackInfo; var dst:TTrackInfo);
begin
  dst.expectedLength:=src.expectedLength;
  dst.path:=src.path;
  dst.title:=src.title;
end;

function SwapTrackInfo(item1Id,item2Id:integer; var tracksArray:TTracksInfoArray):boolean;
var tempItem:TTrackInfo;
begin
  result:=False;
  if (item1Id<0) or (item2Id<0)
    or (item1Id>=Length(tracksArray)) or (item2Id>=Length(tracksArray)) then exit;
  CopyTrackInfo(tracksArray[item1Id],tempItem);
  CopyTrackInfo(tracksArray[item2Id],tracksArray[item1Id]);
  CopyTrackInfo(tempItem,tracksArray[item2Id]);
  result:=True;
end;

procedure TerminateApp;
begin
  if Application.MessageBox('Do you want to exit?','Exit',MB_ICONQUESTION+MB_YesNo)=idNo then exit;
  Application.Terminate;
end;


end.

