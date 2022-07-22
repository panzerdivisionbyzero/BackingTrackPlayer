{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit UCommandLineScreen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Graphics,
  Dialogs, Forms,
  ExtCtrls,
  UConstants,
  UUtilities,
  USystemTools,
  USimpleComponent,
  USimpleButton,
  USimpleLabel,
  USimpleScrollbar,
  USimpleCheckbox,
  USimpleListbox;

type
  TCommandLineScreen=class(TSimpleComponent)
    private
      faceColor:integer;

      btnExit:TSimpleButton;
      btnType:TSimpleButton;
      labTitle:TSimpleLabel;
      lbResults:TSimpleListbox;

      cmdResults:TStringList;

      SwitchToScreenEvent:TSwitchToScreenEvent;
      RepaintRequestEvent:TRepaintRequestEvent;

      procedure RenderImgBuf; override;
      procedure PrepareComponents;
      procedure RefreshListboxItems;
      procedure BtnExitClick(sender:TObject; x,y:integer);
      procedure BtnTypeClick(sender:TObject; x,y:integer);
    public
      parentScreenId:integer;
      screenProcessId:integer;
      constructor Create;
      destructor Destroy; override;
      procedure ExecuteCommand(commandText:string);
      property OnSwitchToScreen:TSwitchToScreenEvent read SwitchToScreenEvent write SwitchToScreenEvent;
      property OnRepaintRequest:TRepaintRequestEvent read RepaintRequestEvent write RepaintRequestEvent;
      procedure TriggerOnSwitchToScreenEvent(screenId:integer; newScreenProcessId:integer=spNone);
      procedure TriggerOnRepaintRequestEvent;
  end;

implementation

// *****************************************************************************
// ********************************************************************** PUBLIC
// *****************************************************************************

constructor TCommandLineScreen.Create;
begin
  inherited;

  faceColor:=clBlack;
  parentScreenId:=sdDirExplorer;
  screenProcessId:=spNone;
  cmdResults:=TStringList.Create;

  PrepareComponents;
end;

destructor TCommandLineScreen.Destroy;
begin
  cmdResults.Destroy;

  inherited;
end;

procedure TCommandLineScreen.ExecuteCommand(commandText:string);
begin
  cmdResults.Clear;
  USystemTools.ExecuteCommand(commandText,cmdResults);
  cmdResults.Insert(0,'>'+commandText);
  RefreshListboxItems;
end;

procedure TCommandLineScreen.TriggerOnSwitchToScreenEvent(screenId:integer; newScreenProcessId:integer=spNone);
begin
  if Assigned(OnSwitchToScreen) then OnSwitchToScreen(screenId, newScreenProcessId);
end;

procedure TCommandLineScreen.TriggerOnRepaintRequestEvent;
begin
  if Assigned(OnRepaintRequest) then OnRepaintRequest;
end;

// *****************************************************************************
// ********************************************************************* PRIVATE
// *****************************************************************************

procedure TCommandLineScreen.RenderImgBuf;
var labelCenterY,halfTextHeight,cursorLeft:integer;
begin
  if (fWidth=0) or (fHeight=0) then exit;

  imgBuf.canvas.Brush.Color:=faceColor;
  imgBuf.canvas.FillRect(0,0,fWidth,fHeight);
end;

procedure TCommandLineScreen.PrepareComponents;
begin
  btnExit:=TSimpleButton.Create;
  btnExit.SetCaption('X');
  btnExit.SetSize(96,96);
  btnExit.SetPos(1024-96-16,16);
  btnExit.OnMouseClick:=@BtnExitClick;
  RegisterComponent(@btnExit);

  btnType:=TSimpleButton.Create;
  btnType.SetCaption('TYPE');
  btnType.SetSize(96,96);
  btnType.SetPos(1024-96-16,600-96-16);
  btnType.OnMouseClick:=@BtnTypeClick;
  RegisterComponent(@btnType);

  labTitle:=TSimpleLabel.Create;
  labTitle.SetCaption('Command Line');
  labTitle.SetSize(768,96);
  labTitle.SetPos(128,16);
  labTitle.SetTextAlign(taCenter);
  RegisterComponent(@labTitle);

  lbResults:=TSimpleListbox.Create;
  lbResults.SetSize(1024-96-3*16,600-2*16);
  lbResults.SetPos(16,16);
  lbResults.SetItemsHeight(48);
  lbResults.SetBordersVisibility(False);
  RegisterComponent(@lbResults);
end;

procedure TCommandLineScreen.RefreshListboxItems;
var i:integer;
begin
  lbResults.ClearItems;
  for i:=0 to cmdResults.Count-1 do
    lbResults.AddItem(cmdResults.Strings[i]);
  RenderImgBuf;
end;

procedure TCommandLineScreen.BtnExitClick(sender:TObject; x,y:integer);
begin
  if parentScreenId=-1 then TerminateApp
                       else TriggerOnSwitchToScreenEvent(parentScreenId);
end;

procedure TCommandLineScreen.BtnTypeClick(sender:TObject; x,y:integer);
begin
  TriggerOnSwitchToScreenEvent(sdKeyboard,spTypeCommand);
end;

end.

