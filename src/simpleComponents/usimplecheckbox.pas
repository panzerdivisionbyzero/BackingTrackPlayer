{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit USimpleCheckbox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Graphics, // TBitmap
  Dialogs,
  USimpleComponent,
  USimpleLabel;

const
  labelMargin=8;

type
  TValueChangeEvent = procedure (value:boolean) of object;

  TSimpleCheckbox=class(TSimpleComponent)
    private
      fFaceColor:integer;
      fBorderColor:integer;
      fCaptionLabel:TSimpleLabel;
      fChecked:boolean;

      ValueChangeEvent:TValueChangeEvent;

      procedure RenderImgBuf; override;
    public
      constructor Create;
      destructor Destroy; override;
      procedure SetCaption(s:string);
      procedure SetSize(w,h:integer);
      procedure SetPos(newLeft,newTop:integer);
      procedure CheckBoxClick(sender:TObject; x,y:integer);
      property OnValueChange:TValueChangeEvent read ValueChangeEvent write ValueChangeEvent;
      procedure TriggerValueChangeEvent(value:boolean);
    published
      property checked:boolean read fChecked;
  end;

implementation

// *****************************************************************************
// ********************************************************************** PUBLIC
// *****************************************************************************

constructor TSimpleCheckbox.Create;
begin
  inherited;

  fFaceColor:=clBlack;
  fBorderColor:=$CCCCCC;
  fChecked:=False;

  imgBuf.Canvas.Brush.Color:=fFaceColor;
  imgBuf.Canvas.Pen.Color:=fBorderColor;
  imgBuf.Canvas.Pen.Width:=2;

  fCaptionLabel:=TSimpleLabel.Create;
  fCaptionLabel.SetCaption('');
  fCaptionLabel.SetSize(fWidth,fHeight);
  fCaptionLabel.SetPos(left,top);
  RegisterComponent(@fCaptionLabel);

  self.OnMouseClick:=@CheckBoxClick;
end;

destructor TSimpleCheckbox.Destroy;
begin
  //

  inherited;
end;

procedure TSimpleCheckbox.SetCaption(s:string);
begin
  fCaptionLabel.SetCaption(s);
  RenderImgBuf;
end;

procedure TSimpleCheckbox.SetSize(w,h:integer);
begin
  inherited;
  fCaptionLabel.SetSize(w-labelMargin-h,h);
  fCaptionLabel.SetPos(left+h+labelMargin,top);
end;

procedure TSimpleCheckbox.SetPos(newLeft,newTop:integer);
begin
  inherited;
  fCaptionLabel.SetPos(newLeft+fHeight+labelMargin,newTop);
end;

procedure TSimpleCheckbox.CheckBoxClick(sender:TObject; x,y:integer);
begin
  fChecked:=not fChecked;
  RenderImgBuf;
  TriggerValueChangeEvent(fChecked);
end;

procedure TSimpleCheckbox.TriggerValueChangeEvent(value:boolean);
begin
  if Assigned(OnValueChange) then OnValueChange(value);
end;

// *****************************************************************************
// ********************************************************************* PRIVATE
// *****************************************************************************

procedure TSimpleCheckbox.RenderImgBuf;
begin
  if (not visible) or (fWidth=0) or (fHeight=0) then exit;

  imgBuf.Canvas.Rectangle(1,1,fHeight,fHeight);

  if fChecked then
  begin
    imgBuf.Canvas.MoveTo(Round(fHeight*0.325), Round(fHeight*0.5));
    imgBuf.Canvas.LineTo(Round(fHeight*0.425), Round(fHeight*0.75));
    imgBuf.Canvas.LineTo(Round(fHeight*0.725), Round(fHeight*0.25));
  end;
end;

end.

