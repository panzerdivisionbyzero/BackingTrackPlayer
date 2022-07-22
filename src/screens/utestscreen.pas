{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit UTestScreen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Graphics,
  Dialogs, Forms,
  UUtilities,
  UConstants,
  USimpleComponent,
  USimpleButton,
  USimpleLabel,
  USimpleScrollbar,
  USimpleCheckbox,
  USimpleListbox,
  UDirExplorer;

type
  TTestScreen=class(TSimpleComponent)
    private
      faceColor:integer;
      btn1DragBeginPosOffset:TPoint;

      btn0:TSimpleButton;
      btn1:TSimpleButton;
      btn2:TSimpleButton;
      lab0:TSimpleLabel;
      sb0:TSimpleScrollbar;
      sb1:TSimpleScrollbar;
      cb0:TSimpleCheckbox;
      lb0:TSimpleListBox;

      de:TDirExplorer;

      SwitchToScreenEvent:TSwitchToScreenEvent;

      procedure RenderImgBuf; override;
      procedure PrepareComponents;
      procedure Btn0Click(sender:TObject; x,y:integer);
      procedure Btn1Click(sender:TObject; x,y:integer);
      procedure Btn1DragBegin(sender:TObject; x,y:integer);
      procedure Btn1DragUpdate(sender:TObject; x,y:integer);
      procedure Btn1DragEnd;
      procedure Btn2Click(sender:TObject; x,y:integer);
      procedure ListboxItemClick(itemId:integer);
    public
      screenProcessId:integer;
      constructor Create;
      destructor Destroy; override;
      property OnSwitchToScreen:TSwitchToScreenEvent read SwitchToScreenEvent write SwitchToScreenEvent;
      procedure TriggerOnSwitchToScreenEvent(screenId:integer; newScreenProcessId:integer=spNone);
  end;

implementation

// *****************************************************************************
// ********************************************************************** PUBLIC
// *****************************************************************************

constructor TTestScreen.Create;
begin
  inherited;

  faceColor:=clBlack;
  screenProcessId:=-1;

  PrepareComponents;

  de:=TDirExplorer.Create;
  de.AssignListbox(lb0);
  de.GoToDirectory(ExtractFilePath(Application.ExeName));
end;

destructor TTestScreen.Destroy;
begin
  de.Destroy;

  inherited;
end;

procedure TTestScreen.TriggerOnSwitchToScreenEvent(screenId:integer; newScreenProcessId:integer=spNone);
begin
  if Assigned(OnSwitchToScreen) then OnSwitchToScreen(screenId, newScreenProcessId);
end;

// *****************************************************************************
// ********************************************************************* PRIVATE
// *****************************************************************************

procedure TTestScreen.RenderImgBuf;
var textW,textH,textL,textT:integer;
begin
  if (fWidth=0) or (fHeight=0) then exit;

  imgBuf.canvas.Brush.Color:=faceColor;
  imgBuf.canvas.FillRect(0,0,fWidth,fHeight);
end;

procedure TTestScreen.PrepareComponents;
begin
  btn0:=TSimpleButton.Create;
  btn0.SetCaption('asd');
  btn0.SetSize(128,64);
  btn0.SetPos(16,100);
  btn0.OnMouseClick:=@Btn0Click;
  RegisterComponent(@btn0);

  btn1:=TSimpleButton.Create;
  btn1.SetCaption('NEXT');
  btn1.SetSize(64,64);
  btn1.SetPos(150,100);
  btn1.SetDraggable(True);
  btn1.OnMouseClick:=@Btn1Click;
  btn1.OnDragBegin:=@Btn1DragBegin;
  btn1.OnDragUpdate:=@Btn1DragUpdate;
  btn1.OnDragEnd:=@Btn1DragEnd;
  RegisterComponent(@btn1);

  btn2:=TSimpleButton.Create;
  btn2.SetCaption('X');
  btn2.SetSize(64,64);
  btn2.SetPos(944,16);
  btn2.OnMouseClick:=@Btn2Click;
  RegisterComponent(@btn2);

  lab0:=TSimpleLabel.Create;
  lab0.SetTransparency(True);
  lab0.SetCaption('asd');
  lab0.SetSize(20,20);
  lab0.SetPos(92,90);
  RegisterComponent(@lab0);

  sb0:=TSimpleScrollbar.Create;
  sb0.SetSize(400,50);
  sb0.SetPos(16,200);
  RegisterComponent(@sb0);

  sb1:=TSimpleScrollbar.Create;
  sb1.SetSize(600,50);
  sb1.SetPos(16,300);
  RegisterComponent(@sb1);

  cb0:=TSimpleCheckbox.Create;
  cb0.SetSize(400,50);
  cb0.SetPos(16,400);
  cb0.SetCaption('Checkbox');
  RegisterComponent(@cb0);

  lb0:=TSimpleListBox.Create;
  lb0.SetSize(400,260);
  lb0.SetPos(450,16);
  lb0.OnItemClick:=@ListboxItemClick;
  lb0.AddItem('item0');
  lb0.AddItem('item1');
  lb0.AddItem('item2');
  lb0.AddItem('item3');
  lb0.AddItem('item4');
  lb0.AddItem('item5');
  lb0.AddItem('item6');
  RegisterComponent(@lb0);
end;

procedure TTestScreen.Btn0Click(sender:TObject; x,y:integer);
begin
  btn0.SetCaption(
    IntToStr(x-btn0.left-Round(btn0.width/2))+'; '+
    IntToStr(y-btn0.top-Round(btn0.height/2)));
end;

procedure TTestScreen.Btn1Click(sender:TObject; x,y:integer);
begin
  TriggerOnSwitchToScreenEvent(sdDirExplorer);
end;

procedure TTestScreen.Btn1DragBegin(sender:TObject; x,y:integer);
begin
  btn1DragBeginPosOffset.x:=btn1.left-x;
  btn1DragBeginPosOffset.y:=btn1.top-y;
end;

procedure TTestScreen.Btn1DragUpdate(sender:TObject; x,y:integer);
var newLeft,newTop:integer;
begin
  newLeft:=x+btn1DragBeginPosOffset.x;
  newTop:=y+btn1DragBeginPosOffset.y;
  btn1.SetPos(newLeft,newTop);
end;

procedure TTestScreen.Btn1DragEnd;
begin
  //
end;

procedure TTestScreen.Btn2Click(sender:TObject; x,y:integer);
begin
  TerminateApp;
end;

procedure TTestScreen.ListboxItemClick(itemId:integer);
var itemCaption:string;
begin
  if itemId=-1 then exit;
  itemCaption:=lb0.GetItemCaption(itemId);
  if Copy(itemCaption,1,Length(dirPrefix))=dirPrefix then
    de.ChangeDirectory(itemCaption)
  else
    ShowMessage('tojeplik');
end;

end.

