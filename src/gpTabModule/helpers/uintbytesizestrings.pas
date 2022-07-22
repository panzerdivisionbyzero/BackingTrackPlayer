{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit UIntByteSizeStrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TIntSizeString = record
    len:integer;
    text:string;
  end;

  procedure ReadByteSizeString(p:PByte; var out_iss:TIntSizeString); overload;
  procedure ReadIntSizeString(p:PByte; var out_iss:TIntSizeString); overload;
  procedure ReadIntByteSizeString(p:PByte; var out_iss:TIntSizeString); overload;
  function ReadByteSizeString(p:PByte; pBytesRead:PInteger = nil):string; overload;
  function ReadIntSizeString(p:PByte; pBytesRead:PInteger = nil):string; overload;
  function ReadIntByteSizeString(p:PByte; pBytesRead:PInteger = nil):string; overload;

implementation

procedure ReadByteSizeString(p:PByte; var out_iss:TIntSizeString);
begin
  out_iss.len:=0;
  Move(p^,out_iss.len,1);
  SetLength(out_iss.text,out_iss.len);
  if out_iss.len>0 then
    Move(PByte(Integer(p)+1)^,out_iss.text[1],out_iss.len);
end;

procedure ReadIntSizeString(p:PByte; var out_iss:TIntSizeString);
begin
  MOve(p^,out_iss.len,4);
  SetLength(out_iss.text,out_iss.len);
  if out_iss.len>0 then
    Move(PByte(Integer(p)+4)^,out_iss.text[1],out_iss.len);
end;

procedure ReadIntByteSizeString(p:PByte; var out_iss:TIntSizeString);
begin
  ReadByteSizeString(PByte(Integer(p)+4),out_iss);
end;

function ReadByteSizeString(p:PByte; pBytesRead:PInteger = nil):string;
var iss:TIntSizeString;
begin
  ReadByteSizeString(p,iss);
  if pBytesRead<>nil then pBytesRead^:=iss.len+1; // + [len (byte)]
  result:=iss.text;
end;

function ReadIntSizeString(p:PByte; pBytesRead:PInteger = nil):string; overload;
var iss:TIntSizeString;
begin
  ReadIntSizeString(p,iss);
  if pBytesRead<>nil then pBytesRead^:=iss.len+4; // + [len (int)]
  result:=iss.text;
end;

function ReadIntByteSizeString(p:PByte; pBytesRead:PInteger = nil):string; overload;
var iss:TIntSizeString;
begin
  ReadIntByteSizeString(p,iss);
  if pBytesRead<>nil then pBytesRead^:=iss.len+5; // + [len (int)] + [len (byte)]
  result:=iss.text;
end;

end.

