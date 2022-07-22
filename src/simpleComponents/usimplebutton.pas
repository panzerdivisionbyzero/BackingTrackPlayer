{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit USimpleButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Graphics, // TBitmap
  Dialogs,
  USimpleComponent,
  USimpleLabel;

const
  labelMargin=2;

type
  TSimpleButton=class(TSimpleComponent)
    private
      fFaceColor:integer;
      fBorderColor:integer;
      fCaptionLabel:TSimpleLabel;

      procedure RenderImgBuf; override;
    public
      constructor Create;
      destructor Destroy; override;
      function GetCaption:string;
      procedure SetCaption(s:string);
      procedure SetSize(w,h:integer);
      procedure SetPos(newLeft,newTop:integer);
  end;

implementation

// *****************************************************************************
// ********************************************************************** PUBLIC
// *****************************************************************************

constructor TSimpleButton.Create;
begin
  inherited;

  fFaceColor:=clBlack;
  fBorderColor:=$CCCCCC;

  imgBuf.Canvas.Brush.Color:=fFaceColor;
  imgBuf.Canvas.Pen.Color:=fBorderColor;
  imgBuf.Canvas.Pen.Width:=2;

  fCaptionLabel:=TSimpleLabel.Create;
  fCaptionLabel.SetCaption('');
  fCaptionLabel.SetSize(fWidth,fHeight);
  fCaptionLabel.SetPos(left,top);
  fCaptionLabel.SetTextAlign(taCenter);
  RegisterComponent(@fCaptionLabel);
end;

destructor TSimpleButton.Destroy;
begin
  //

  inherited;
end;

function TSimpleButton.GetCaption:string;
begin
  result:=fCaptionLabel.caption;
end;

procedure TSimpleButton.SetCaption(s:string);
begin
  fCaptionLabel.SetCaption(s);
  RenderImgBuf;
end;

procedure TSimpleButton.SetSize(w,h:integer);
begin
  inherited;
  fCaptionLabel.SetSize(w-labelMargin*2,h-labelMargin*2);
end;

procedure TSimpleButton.SetPos(newLeft,newTop:integer);
begin
  inherited;
  fCaptionLabel.SetPos(newLeft+labelMargin,newTop+labelMargin);
end;

// *****************************************************************************
// ********************************************************************* PRIVATE
// *****************************************************************************

procedure TSimpleButton.RenderImgBuf;
begin
  if (not visible) or (fWidth=0) or (fHeight=0) then exit;

  imgBuf.Canvas.Rectangle(1,1,fWidth,fHeight);
end;

end.

