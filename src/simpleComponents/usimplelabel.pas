{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit USimpleLabel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Graphics, // TBitmap
  Dialogs,
  USimpleComponent;

type
  TTextAlign=(taLeft,taCenter);
  TSimpleLabel=class(TSimpleComponent)
    private
      fCaption:string;
      fTextAlign:TTextAlign;
      fFontColor:integer;
      fTransparent:boolean;
      fTextHeight:integer;

      procedure RenderImgBuf; override;
    public
      constructor Create;
      destructor Destroy; override;
      procedure SetCaption(s:string);
      procedure SetTextAlign(ta:TTextAlign);
      procedure SetTransparency(transparency:boolean);
      function EstimateTextWidth(text:string):integer;
    published
      property caption:string read fCaption;
      property transparent:boolean read fTransparent;
      property textHeight:integer read fTextHeight;
  end;

implementation

// *****************************************************************************
// ********************************************************************** PUBLIC
// *****************************************************************************

constructor TSimpleLabel.Create;
begin
  inherited;

  fTextAlign:=taLeft;
  fFontColor:=$CCCCCC;
  fTransparent:=False;

  imgBuf.Canvas.Brush.Color:=clBlack;
  imgBuf.Canvas.Font.Color:=fFontColor;
  imgBuf.Canvas.Font.Size:=24;
  fTextHeight:=imgBuf.Canvas.TextHeight('Yy');
end;

destructor TSimpleLabel.Destroy;
begin
  //

  inherited;
end;

procedure TSimpleLabel.SetCaption(s:string);
begin
  fCaption:=s;

  RenderImgBuf;
  if fTransparent then
  begin
    imgBuf.Transparent:=False;
    imgBuf.TransparentMode:=tmAuto;
    imgBuf.TransparentColor:=imgBuf.Canvas.Brush.Color;
    imgBuf.Transparent:=fTransparent;
  end;
end;

procedure TSimpleLabel.SetTextAlign(ta:TTextAlign);
begin
  fTextAlign:=ta;
  RenderImgBuf;
end;

procedure TSimpleLabel.SetTransparency(transparency:boolean);
begin
  fTransparent:=transparency;

  imgBuf.TransparentColor:=imgBuf.Canvas.Brush.Color;
  imgBuf.TransparentMode:=tmAuto;
  imgBuf.Transparent:=fTransparent;
  RenderImgBuf;
end;

function TSimpleLabel.EstimateTextWidth(text:string):integer;
begin
  result:=imgBuf.Canvas.TextWidth(text);
end;

// *****************************************************************************
// ********************************************************************* PRIVATE
// *****************************************************************************

procedure TSimpleLabel.RenderImgBuf;
var textW,textL,textT:integer;
begin
  if (not visible) or (fWidth=0) or (fHeight=0) then exit;

  imgBuf.Canvas.Rectangle(1,1,fWidth,fHeight);

  textW:=imgBuf.Canvas.TextWidth(fCaption);
  if textW=0 then exit;
  fTextHeight:=imgBuf.Canvas.TextHeight(fCaption);

  if fTextAlign=taLeft then
    textL:=0
  else
    textL:=Round((fWidth-textW)/2);

  textT:=Round((fHeight-fTextHeight)/2);
  imgBuf.Canvas.TextOut(textL,textT,fCaption);
end;

end.
