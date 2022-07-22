{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit USimpleMarkersLine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Graphics,
  Dialogs, Forms,
  UConstants,
  UUtilities,
  USimpleComponent,
  USimpleButton,
  USimpleLabel,
  USimpleScrollbar,
  USimpleCheckbox,
  USimpleListbox,
  UPlayer;

type
  TSimpleMarkersLine=class(TSimpleComponent)
    private
      faceColor:integer;
      markersColor:integer;
      loopColor:integer;
      outerLoopColor:integer;

      fBeginMarkerPos:integer;
      fEndMarkerPos:integer;
      fMaxPos:integer;

      procedure RenderImgBuf; override;
    public
      constructor Create;
      destructor Destroy; override;
      procedure ClearMarkers;
      procedure SetMaxPos(newMaxPos:integer);
      procedure SetBeginMarkerPos(pos:integer);
      procedure SetEndMarkerPos(pos:integer);
    published
      property beginMarkerPos:integer read fBeginMarkerPos;
      property endMarkerPos:integer read fEndMarkerPos;
      property maxPos:integer read fMaxPos;
  end;

implementation

// *****************************************************************************
// ********************************************************************** PUBLIC
// *****************************************************************************

constructor TSimpleMarkersLine.Create;
begin
  inherited;

  faceColor:=clBlack;
  markersColor:=clYellow;
  loopColor:=$CCCC00;
  outerLoopColor:=$000088;

  ClearMarkers;
end;

destructor TSimpleMarkersLine.Destroy;
begin
  //

  inherited;
end;

procedure TSimpleMarkersLine.ClearMarkers;
begin
  fBeginMarkerPos:=-1;
  fEndMarkerPos:=-1;
  RenderImgBuf;
end;

procedure TSimpleMarkersLine.SetMaxPos(newMaxPos:integer);
begin
  ClearMarkers;
  fMaxPos:=newMaxPos;
  RenderImgBuf;
end;

procedure TSimpleMarkersLine.SetBeginMarkerPos(pos:integer);
begin
  fBeginMarkerPos:=pos;
  RenderImgBuf;
end;

procedure TSimpleMarkersLine.SetEndMarkerPos(pos:integer);
begin
  fEndMarkerPos:=pos;
  RenderImgBuf;
end;

// *****************************************************************************
// ********************************************************************* PRIVATE
// *****************************************************************************

procedure TSimpleMarkersLine.RenderImgBuf;
var pxb,pxe:integer;
begin
  if (fWidth=0) or (fHeight=0) then exit;

  imgBuf.canvas.Brush.Color:=faceColor;
  imgBuf.canvas.FillRect(0,0,fWidth,fHeight);

  if (fBeginMarkerPos=-1) and (fEndMarkerPos=-1) then exit;

  pxb:=Round(fBeginMarkerPos/fMaxPos*fWidth);
  pxe:=Round(fEndMarkerPos/fMaxPos*fWidth);

  if pxe>pxb then
  begin
    imgBuf.canvas.Brush.Color:=loopColor;
    imgBuf.canvas.FillRect(pxb,0,pxe,fHeight);
  end
  else
  begin
    imgBuf.canvas.Brush.Color:=outerLoopColor;
    imgBuf.canvas.FillRect(0,0,pxe,fHeight);
    imgBuf.canvas.FillRect(pxb,0,fWidth,fHeight);
  end;
  imgBuf.canvas.Brush.Color:=markersColor;
  imgBuf.canvas.FillRect(pxb-2,0,pxb+2,fHeight);
  imgBuf.canvas.FillRect(pxe-2,0,pxe+2,fHeight);
end;

end.

