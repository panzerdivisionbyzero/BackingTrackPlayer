{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit USimpleScrollbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Graphics, // TBitmap
  Dialogs,
  USimpleComponent,
  USimpleLabel,
  USimpleButton;

const
  labelMargin=0;

type
  TSimpleOrientation=(soHorizontal,soVertical);
  TValueChangeEvent = procedure (value:integer) of object;

  TSimpleScrollbar=class(TSimpleComponent)
    private
      fFaceColor:integer;
      fBorderColor:integer;
      fBarColor:integer;
      fBarLength:integer;
      fOrientation:TSimpleOrientation;
      fMinValue:integer;
      fMaxValue:integer;
      fValue:integer;
      fValueRange:integer;

      fLabel:TSimpleLabel;
      fBtnInc:TSimpleButton;
      fBtnDec:TSimpleButton;

      ValueChangeEvent:TValueChangeEvent;

      procedure RenderImgBuf; override;
      procedure SetValueFromPx(x,y:integer);
      procedure ScrollbarClick(sender:TObject; x,y:integer);
      procedure ScrollbarDragBegin(sender:TObject; x,y:integer);
      procedure ScrollbarDragUpdate(sender:TObject; x,y:integer);
      procedure ScrollbarDragEnd;
      procedure fBtnIncClick(sender:TObject; x,y:integer);
      procedure fBtnDecClick(sender:TObject; x,y:integer);
    public
      constructor Create;
      destructor Destroy; override;
      procedure SetSize(w,h:integer);
      procedure SetPos(newLeft,newTop:integer);
      procedure SetOrientation(newOrientation:TSimpleOrientation);
      procedure SetRange(newMin,newMax:integer);
      procedure SetValue(newValue:integer);
      procedure SetLabelVisibility(labelVisibility:boolean);
      property OnValueChange:TValueChangeEvent read ValueChangeEvent write ValueChangeEvent;
      procedure TriggerValueChangeEvent(value:integer);
    published
      property minValue:integer read fMinValue;
      property maxValue:integer read fMaxValue;
      property value:integer read fValue;
  end;

implementation

// *****************************************************************************
// ********************************************************************** PUBLIC
// *****************************************************************************

constructor TSimpleScrollbar.Create;
begin
  inherited;

  fFaceColor:=clBlack;
  fBorderColor:=$CCCCCC;
  fBarColor:=$000088;

  imgBuf.Canvas.Brush.Color:=fFaceColor;
  imgBuf.Canvas.Pen.Color:=fBorderColor;
  imgBuf.Canvas.Pen.Width:=2;

  self.SetDraggable(True);
  self.OnMouseClick:=@ScrollbarClick;
  self.OnDragBegin:=@ScrollbarDragBegin;
  self.OnDragUpdate:=@ScrollbarDragUpdate;
  self.OnDragEnd:=@ScrollbarDragEnd;

  fBtnInc:=TSimpleButton.Create;
  fBtnInc.OnMouseClick:=@fBtnIncClick;
  fBtnInc.SetDraggable(True);
  RegisterComponent(@fBtnInc);
  fBtnDec:=TSimpleButton.Create;
  fBtnDec.OnMouseClick:=@fBtnDecClick;
  fBtnDec.SetDraggable(True);
  RegisterComponent(@fBtnDec);

  fLabel:=TSimpleLabel.Create;
  fLabel.SetTransparency(True);
  fLabel.SetTextAlign(taCenter);
  RegisterComponent(@fLabel);

  SetRange(0,100);
  SetValue(50);

  SetOrientation(soHorizontal);
end;

destructor TSimpleScrollbar.Destroy;
begin
  //

  inherited;
end;

procedure TSimpleScrollbar.SetSize(w,h:integer);
begin
  inherited;
  fLabel.SetSize(w-labelMargin*2,h-labelMargin*2);
  if fOrientation=soHorizontal then
  begin
    fBtnInc.SetSize(h,h);
    fBtnDec.SetSize(h,h);
    fBarLength:=w-2*h;
  end
  else
  begin
    fBtnInc.SetSize(w,w);
    fBtnDec.SetSize(w,w);
    fBarLength:=h-2*w;
  end;
end;

procedure TSimpleScrollbar.SetPos(newLeft,newTop:integer);
begin
  inherited;
  fLabel.SetPos(newLeft+labelMargin,newTop+labelMargin);
  if fOrientation=soHorizontal then
  begin
    fBtnInc.SetPos(newLeft+fWidth-fHeight,newTop);
    fBtnDec.SetPos(newLeft,newTop);
  end
  else
  begin
    fBtnInc.SetPos(newLeft,newTop);
    fBtnDec.SetPos(newLeft,newTop+fHeight-fWidth);
  end;
end;

procedure TSimpleScrollbar.SetOrientation(newOrientation:TSimpleOrientation);
begin
  fOrientation:=newOrientation;
  if fOrientation=soHorizontal then
  begin
    fBtnInc.SetCaption('>');
    fBtnDec.SetCaption('<');
  end
  else
  begin
    fBtnInc.SetCaption('^');
    fBtnDec.SetCaption('v');
  end;
  SetSize(fWidth,fHeight);
  SetPos(left,top);
end;

procedure TSimpleScrollbar.SetRange(newMin,newMax:integer);
begin
  fMinValue:=newMin;
  fMaxValue:=newMax;
  fValueRange:=fMaxValue-fMinValue;
  SetValue(fValue);
end;

procedure TSimpleScrollbar.SetValue(newValue:integer);
begin
  fValue:=newValue;
  if fValue<fMinValue then fValue:=fMinValue;
  if fValue>fMaxValue then fValue:=fMaxValue;
  fLabel.SetCaption(IntToStr(value));
  RenderImgBuf;
  TriggerValueChangeEvent(fValue);
end;

procedure TSimpleScrollbar.SetLabelVisibility(labelVisibility:boolean);
begin
  fLabel.SetVisibility(labelVisibility);
end;

procedure TSimpleScrollbar.TriggerValueChangeEvent(value:integer);
begin
  if Assigned(OnValueChange) then OnValueChange(value);
end;

// *****************************************************************************
// ********************************************************************* PRIVATE
// *****************************************************************************

procedure TSimpleScrollbar.RenderImgBuf;
begin
  if (not visible) or (fWidth=0) or (fHeight=0) then exit;

  imgBuf.Canvas.Pen.Style:=psClear;
  imgBuf.Canvas.Brush.Color:=fFaceColor;
  imgBuf.Canvas.Rectangle(0,0,fWidth,fHeight);

  imgBuf.Canvas.Brush.Color:=fBarColor;
  if fOrientation=soHorizontal then
  begin
    if fMaxValue-fMinValue<>0 then
      imgBuf.Canvas.Rectangle(fHeight, // height = button width
                              0,
                              fHeight+Round((fValue/fValueRange)*fBarLength),
                              fHeight);
  end
  else
  begin
    if fMaxValue-fMinValue<>0 then
      imgBuf.Canvas.Rectangle(0,
                              fHeight-fWidth-Round((fValue/fValueRange)*fBarLength),
                              fWidth,
                              fHeight-fWidth);
  end;
end;

procedure TSimpleScrollbar.SetValueFromPx(x,y:integer);
begin
  if (fBarLength<=0) then exit;

  if fOrientation=soHorizontal then
  begin
    if (x<fBtnDec.left+fHeight)
      or (x>left+fWidth-fHeight) then exit;
    SetValue(Round(((x-fBtnDec.left-fHeight)/fBarLength)*fValueRange));
  end
  else
  begin
    if (y<fBtnInc.top+fWidth)
      or (y>fBtnDec.top) then exit;
    SetValue(Round(((fBtnDec.top-y)/fBarLength)*fValueRange));
  end;
end;

procedure TSimpleScrollbar.ScrollbarClick(sender:TObject; x,y:integer);
begin
  SetValueFromPx(x,y);
end;

procedure TSimpleScrollbar.ScrollbarDragBegin(sender:TObject; x,y:integer);
begin
  SetValueFromPx(x,y);
end;

procedure TSimpleScrollbar.ScrollbarDragUpdate(sender:TObject; x,y:integer);
begin
  SetValueFromPx(x,y);
end;

procedure TSimpleScrollbar.ScrollbarDragEnd;
begin
  //SetValueFromPx(x,y);
end;

procedure TSimpleScrollbar.fBtnIncClick(sender:TObject; x,y:integer);
begin
  SetValue(fValue+1);
end;

procedure TSimpleScrollbar.fBtnDecClick(sender:TObject; x,y:integer);
begin
  SetValue(fValue-1);
end;

end.

