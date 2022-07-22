{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit UDirExplorerScreen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Graphics,
  Dialogs, Forms, LCLType,
  UConstants,
  UUtilities,
  USimpleComponent,
  USimpleButton,
  USimpleLabel,
  USimpleScrollbar,
  USimpleCheckbox,
  USimpleListbox,
  UDirExplorer,
  USystemTools;

type
  TOpenFileEvent=procedure (filePath:string; screenProcessId:integer=spNone) of object;
  TDirExplorerScreen=class(TSimpleComponent)
    private
      faceColor:integer;

      btnExit:TSimpleButton;
      btnShutdown:TSimpleButton;
      btnCommandLine:TSimpleButton;
      btnCreateSetlist:TSimpleButton;
      labTitle:TsimpleLabel;
      lb0:TSimpleListBox;

      de:TDirExplorer;

      SwitchToScreenEvent:TSwitchToScreenEvent;
      OpenFileEvent:TOpenFileEvent;

      procedure RenderImgBuf; override;
      procedure PrepareComponents;
      procedure BtnExitClick(sender:TObject; x,y:integer);
      procedure BtnShutdownClick(sender:TObject; x,y:integer);
      procedure BtnCommandLineClick(sender:TObject; x,y:integer);
      procedure BtnCreateSetlistClick(sender:TObject; x,y:integer);
      procedure ListboxItemClick(itemId:integer);
      procedure ChangeDirectoryEvent(newDir:string);
      procedure ShutdownRpi;
    public
      previousScreenId:integer;
      screenProcessId:integer;
      constructor Create;
      destructor Destroy; override;
      function GetCurrentDir:string;
      procedure ReDir;
      property OnSwitchToScreen:TSwitchToScreenEvent read SwitchToScreenEvent write SwitchToScreenEvent;
      property OnOpenFile:TOpenFileEvent read OpenFileEvent write OpenFileEvent;
      procedure TriggerOnSwitchToScreenEvent(screenId:integer; newScreenProcessId:integer=spNone);
      procedure TriggerOnOpenFileEvent(filePath:string; newScreenProcessId:integer=spNone);
  end;

implementation

// *****************************************************************************
// ********************************************************************** PUBLIC
// *****************************************************************************

constructor TDirExplorerScreen.Create;
begin
  inherited;

  faceColor:=clBlack;
  previousScreenId:=-1;
  screenProcessId:=spNone;

  PrepareComponents;

  de:=TDirExplorer.Create;
  de.OnChangeDirectory:=@ChangeDirectoryEvent;
  de.AssignListbox(lb0);
  de.GoToDirectory(ExtractFilePath(Application.ExeName));
end;

destructor TDirExplorerScreen.Destroy;
begin
  de.Destroy;

  inherited;
end;

function TDirExplorerScreen.GetCurrentDir:string;
begin
  result:=de.currentDir;
end;

procedure TDirExplorerScreen.ReDir;
begin
  de.GoToDirectory(de.currentDir);
end;

procedure TDirExplorerScreen.TriggerOnSwitchToScreenEvent(screenId:integer; newScreenProcessId:integer=spNone);
begin
  if Assigned(OnSwitchToScreen) then OnSwitchToScreen(screenId,newScreenProcessId);
end;

procedure TDirExplorerScreen.TriggerOnOpenFileEvent(filePath:string; newScreenProcessId:integer=spNone);
begin
  if Assigned(OnOpenFile) then OnOpenFile(filePath,newScreenProcessId);
end;

// *****************************************************************************
// ********************************************************************* PRIVATE
// *****************************************************************************

procedure TDirExplorerScreen.RenderImgBuf;
begin
  if (fWidth=0) or (fHeight=0) then exit;

  imgBuf.canvas.Brush.Color:=faceColor;
  imgBuf.canvas.FillRect(0,0,fWidth,fHeight);
end;

procedure TDirExplorerScreen.PrepareComponents;
begin
  btnExit:=TSimpleButton.Create;
  btnExit.SetCaption('X');
  btnExit.SetSize(96,96);
  btnExit.SetPos(1024-96-16,16);
  btnExit.OnMouseClick:=@BtnExitClick;
  RegisterComponent(@btnExit);

  btnShutdown:=TSimpleButton.Create;
  btnShutdown.SetCaption('SD');
  btnShutdown.SetSize(96,96);
  btnShutdown.SetPos(16,16);
  btnShutdown.OnMouseClick:=@BtnShutdownClick;
  RegisterComponent(@btnShutdown);

  btnCommandLine:=TSimpleButton.Create;
  btnCommandLine.SetCaption('CMD');
  btnCommandLine.SetSize(96,96);
  btnCommandLine.SetPos(16,600-96-16);
  btnCommandLine.OnMouseClick:=@BtnCommandLineClick;
  RegisterComponent(@btnCommandLine);

  btnCreateSetlist:=TSimpleButton.Create;
  btnCreateSetlist.SetCaption('+m3u');
  btnCreateSetlist.Setsize(96,96);
  btnCreateSetlist.SetPos(1024-96-16,600-96-16);
  btnCreateSetlist.OnMouseClick:=@BtnCreateSetlistClick;
  RegisterComponent(@btnCreateSetlist);

  labTitle:=TSimpleLabel.Create;
  labTitle.SetTransparency(False);
  labTitle.SetCaption('');
  labTitle.SetSize(768,96);
  labTitle.SetPos(128,16);
  RegisterComponent(@labTitle);

  lb0:=TSimpleListBox.Create;
  lb0.SetSize(768,448);
  lb0.SetPos(128,128);
  lb0.OnItemClick:=@ListboxItemClick;
  RegisterComponent(@lb0);
end;

procedure TDirExplorerScreen.BtnExitClick(sender:TObject; x,y:integer);
begin
  if previousScreenId=-1 then TerminateApp
                         else TriggerOnSwitchToScreenEvent(previousScreenId);
end;

procedure TDirExplorerScreen.BtnShutdownClick(sender:TObject; x,y:integer);
begin
  ShutdownRpi;
end;

procedure TDirExplorerScreen.BtnCommandLineClick(sender:TObject; x,y:integer);
begin
  TriggerOnSwitchToScreenEvent(sdCommandLine);
end;

procedure TDirExplorerScreen.BtnCreateSetlistClick(sender:TObject; x,y:integer);
begin
  TriggerOnSwitchToScreenEvent(sdKeyboard,spTypeNewSetlistFileName);
end;

procedure TDirExplorerScreen.ListboxItemClick(itemId:integer);
var itemCaption:string;
begin
  if itemId=-1 then exit;
  itemCaption:=lb0.GetItemCaption(itemId);
  if Copy(itemCaption,1,Length(dirPrefix))=dirPrefix then
    de.ChangeDirectory(itemCaption)
  else
    TriggerOnOpenFileEvent(de.currentDir+dirSeparator+itemCaption,screenProcessId);
end;

procedure TDirExplorerScreen.ChangeDirectoryEvent(newDir:string);
begin
  labTitle.SetCaption(newDir);
end;

procedure TDirExplorerScreen.ShutdownRpi;
var sl:TStringList;
begin
  if Application.MessageBox('Execute "sudo shutdown now"?','Shutdown',MB_ICONWARNING+MB_YesNo)=idNo then exit;
  {$if defined(linux)}
  sl:=TStringList.Create;
  ExecuteCommand('sudo shutdown now',sl);
  sl.Free;
  {$else}
  ShowMessage('Cannot shutdown on this OS');
  {$endif}
end;

end.

