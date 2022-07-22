{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit USetlistScreen;

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
  USimpleListbox;

type
  TChooseTrackEvent = procedure(var tracksInfo: TTracksInfoArray;
    currentTrackId: integer) of object;

  TSetlistScreen = class(TSimpleComponent)
  private
    faceColor: integer;

    tracksInfo: TTracksInfoArray;
    fSetlistPath: string;

    btnExit: TSimpleButton;
    btnAddTrack: TSimpleButton;
    btnRemoveTrack: TSimpleButton;
    btnSave: TSimpleButton;
    btnCancel: TSimpleButton;
    btnEditMode: TSimpleButton;
    btnMoveTrackUp: TSimpleButton;
    btnMoveTrackDown: TSimpleButton;
    btnChangeTrackInfo: TSimpleButton;
    labTitle: TsimpleLabel;
    lb0: TSimpleListBox;

    SwitchToScreenEvent: TSwitchToScreenEvent;
    ChooseTrackEvent: TChooseTrackEvent;
    RepaintRequestEvent: TRepaintRequestEvent;

    procedure RenderImgBuf; override;
    procedure PrepareComponents;
    procedure RefreshListBox;
    function ReadSetlistFile(setlistPath: string; tracks: TTracksInfoArray;
      var out_errorMsg: string): boolean;
    function WriteSetlistFile(setlistPath: string; tracks: TTracksInfoArray;
      var out_errorMsg: string): boolean;
    procedure AddTrackInfo(trackPath, trackTitle: string; trackLength: integer);
    procedure RemoveTrackInfo(trackId: integer);
    function AbsolutePath(unknownFormatPath, setlistPath: string): string;
    function LocalPath(baseAbsolutePath, fileAbsolutePath: string): string;
    procedure SetEditMode(newEditMode: boolean);
    procedure BtnExitClick(Sender: TObject; x, y: integer);
    procedure BtnAddTrackClick(Sender: TObject; x, y: integer);
    procedure BtnRemoveTrackClick(Sender: TObject; x, y: integer);
    procedure BtnSaveClick(Sender: TObject; x, y: integer);
    procedure BtnCancelClick(Sender: TObject; x, y: integer);
    procedure BtnEditModeClick(Sender: TObject; x, y: integer);
    procedure BtnMoveTrackUpClick(Sender: TObject; x, y: integer);
    procedure BtnMoveTrackDownClick(Sender: TObject; x, y: integer);
    procedure BtnChangeTrackInfoClick(Sender: TObject; x, y: integer);
    procedure ListboxItemClick(itemId: integer);
  public
    parentScreenId: integer;
    screenProcessId: integer;
    constructor Create;
    destructor Destroy; override;
    function CreateSetlist(setlistPath: string; var out_errorMsg: string): boolean;
    function LoadSetlist(setlistPath: string): boolean;
    function SaveSetlist: boolean;
    function AppendOpenedSetlist(trackPath: string): boolean;
    function GetSelectedTrackTitle: string;
    procedure SetSelectedTrackTitle(newTitle: string);
    procedure SetSelectedTrackLength(newLength:integer);
    function GetSelectedTrackAbsolutePath:string;
    property OnSwitchToScreen: TSwitchToScreenEvent
      read SwitchToScreenEvent write SwitchToScreenEvent;
    property OnChooseTrack: TChooseTrackEvent
      read ChooseTrackEvent write ChooseTrackEvent;
    property OnRepaintRequest: TRepaintRequestEvent
      read RepaintRequestEvent write RepaintRequestEvent;
    procedure TriggerOnSwitchToScreenEvent(screenId: integer;
      newScreenProcessId: integer = spNone);
    procedure TriggerOnChooseTrackEvent(var newTracksInfo: TTracksInfoArray;
      newCurrentTrackId: integer);
    procedure TriggerOnRepaintRequestEvent;
  end;

implementation

// *****************************************************************************
// ********************************************************************** PUBLIC
// *****************************************************************************

constructor TSetlistScreen.Create;
begin
  inherited;

  faceColor := clBlack;
  parentScreenId := sdDirExplorer;
  screenProcessId := spNone;
  fSetlistPath := '';

  PrepareComponents;
end;

destructor TSetlistScreen.Destroy;
begin

  inherited;
end;

function TSetlistScreen.CreateSetlist(setlistPath: string;
  var out_errorMsg: string): boolean;
var
  tf: TextFile;
begin
  Result := False;

  if FileExists(setlistPath) then
  begin
    out_errorMsg := 'File exists';
    exit;
  end;

  try
    AssignFile(tf, setlistPath);
    Rewrite(tf);
    CloseFile(tf);
  except
    on e: Exception do
    begin
      out_errorMsg := e.Message;
      exit;
    end;
  end;

  fSetlistPath:=setlistPath;
  labTitle.SetCaption(ExtractFileName(setlistPath));
  RefreshListBox;

  Result := True;
end;

function TSetlistScreen.LoadSetlist(setlistPath: string): boolean;
var
  errorMsg: string;
begin
  Result := False;
  if not ReadSetlistFile(setlistPath, tracksInfo, errorMsg) then
  begin
    ShowMessage('Error reading setlist file:' + #13 + setlistPath + #13 +
      'Message:' + #13 + errorMsg);
    exit;
  end;

  labTitle.SetCaption(ExtractFileName(setlistPath));
  RefreshListBox;

  Result := True;
end;

function TSetlistScreen.SaveSetlist: boolean;
var
  errorMsg: string;
begin
  Result := False;
  if not WriteSetlistFile(fSetlistPath, tracksInfo, errorMsg) then
  begin
    ShowMessage('Error saving setlist file:' + #13 + fSetlistPath + #13 +
      'Message:' + #13 + errorMsg);
    exit;
  end;

  Result := True;
end;

function TSetlistScreen.AppendOpenedSetlist(trackPath: string): boolean;
begin
  AddTrackInfo(AbsolutePath(trackPath, fSetlistPath),
    ExtractFileName(trackPath), 0);
  RefreshListBox;
end;

function TSetlistScreen.GetSelectedTrackTitle: string;
var itemId: integer;
begin
  itemId := lb0.GetSelectedItemId;
  if itemId = -1 then
    Result := ''
  else
    Result := tracksInfo[itemId].title;
end;

procedure TSetlistScreen.SetSelectedTrackTitle(newTitle: string);
var itemId: integer;
begin
  itemId := lb0.GetSelectedItemId;
  tracksInfo[itemId].title := newTitle;
  RefreshListBox;
  lb0.SetEditMode(True);
  lb0.SelectItem(itemId);
end;

procedure TSetlistScreen.SetSelectedTrackLength(newLength:integer);
var itemId: integer;
begin
  itemId := lb0.GetSelectedItemId;
  tracksInfo[itemId].expectedLength := newLength;
  RefreshListBox;
  lb0.SetEditMode(True);
  lb0.SelectItem(itemId);
end;

function TSetlistScreen.GetSelectedTrackAbsolutePath:string;
var itemId: integer;
begin
  itemId := lb0.GetSelectedItemId;
  result:=tracksInfo[itemId].path;
end;

procedure TSetlistScreen.TriggerOnSwitchToScreenEvent(screenId: integer;
  newScreenProcessId: integer = spNone);
begin
  if Assigned(OnSwitchToScreen) then
    OnSwitchToScreen(screenId, newScreenProcessId);
end;

procedure TSetlistScreen.TriggerOnChooseTrackEvent(var newTracksInfo: TTracksInfoArray;
  newCurrentTrackId: integer);
begin
  if Assigned(OnChooseTrack) then
    OnChooseTrack(newTracksInfo, newCurrentTrackId);
end;

procedure TSetlistScreen.TriggerOnRepaintRequestEvent;
begin
  if Assigned(OnRepaintRequest) then
    OnRepaintRequest;
end;

// *****************************************************************************
// ********************************************************************* PRIVATE
// *****************************************************************************

procedure TSetlistScreen.RenderImgBuf;
begin
  if (fWidth = 0) or (fHeight = 0) then
    exit;

  imgBuf.canvas.Brush.Color := faceColor;
  imgBuf.canvas.FillRect(0, 0, fWidth, fHeight);
end;

procedure TSetlistScreen.PrepareComponents;
begin
  btnExit := TSimpleButton.Create;
  btnExit.SetCaption('X');
  btnExit.SetSize(96, 96);
  btnExit.SetPos(1024 - 96 - 16, 16);
  btnExit.OnMouseClick := @BtnExitClick;
  RegisterComponent(@btnExit);

  btnAddTrack := TSimpleButton.Create;
  btnAddTrack.SetCaption('+T');
  btnAddTrack.SetSize(96, 96);
  btnAddTrack.SetPos(1024 - 96 - 16, 16 + 96 + 16);
  btnAddTrack.OnMouseClick := @BtnAddTrackClick;
  RegisterComponent(@btnAddTrack);

  btnRemoveTrack := TSimpleButton.Create;
  btnRemoveTrack.SetCaption('-T');
  btnRemoveTrack.SetSize(96, 96);
  btnRemoveTrack.SetPos(1024 - 96 - 16, 16 + 2 * (96 + 16));
  btnRemoveTrack.OnMouseClick := @BtnRemoveTrackClick;
  RegisterComponent(@btnRemoveTrack);

  btnSave := TSimpleButton.Create;
  btnSave.SetCaption('SAVE');
  btnSave.SetSize(96, 96);
  btnSave.SetPos(1024 - 96 - 16, 16 + 3 * (96 + 16));
  btnSave.OnMouseClick := @BtnSaveClick;
  RegisterComponent(@btnSave);

  btnCancel := TSimpleButton.Create;
  btnCancel.SetCaption('LOAD');
  btnCancel.SetSize(96, 96);
  btnCancel.SetPos(1024 - 96 - 16, 16 + 4 * (96 + 16));
  btnCancel.OnMouseClick := @BtnCancelClick;
  RegisterComponent(@btnCancel);

  btnEditMode := TSimpleButton.Create;
  btnEditMode.SetCaption('PICK');
  btnEditMode.SetSize(96, 96);
  btnEditMode.SetPos(16, 96 + 16 + 16);
  btnEditMode.OnMouseClick := @BtnEditModeClick;
  RegisterComponent(@btnEditMode);

  btnMoveTrackUp := TSimpleButton.Create;
  btnMoveTrackUp.SetCaption('^');
  btnMoveTrackUp.SetSize(96, 96);
  btnMoveTrackUp.SetPos(16, 2 * (96 + 16) + 16);
  btnMoveTrackUp.OnMouseClick := @BtnMoveTrackUpClick;
  RegisterComponent(@btnMoveTrackUp);

  btnMoveTrackDown := TSimpleButton.Create;
  btnMoveTrackDown.SetCaption('v');
  btnMoveTrackDown.SetSize(96, 96);
  btnMoveTrackDown.SetPos(16, 3 * (96 + 16) + 16);
  btnMoveTrackDown.OnMouseClick := @BtnMoveTrackDownClick;
  RegisterComponent(@btnMoveTrackDown);

  btnChangeTrackInfo := TSimpleButton.Create;
  btnChangeTrackInfo.SetCaption('TITLE');
  btnChangeTrackInfo.SetSize(96, 96);
  btnChangeTrackInfo.SetPos(16, 4 * (96 + 16) + 16);
  btnChangeTrackInfo.OnMouseClick := @BtnChangeTrackInfoClick;
  RegisterComponent(@btnChangeTrackInfo);

  labTitle := TSimpleLabel.Create;
  labTitle.SetTransparency(False);
  labTitle.SetCaption('');
  labTitle.SetSize(768, 96);
  labTitle.SetPos(128, 16);
  labTitle.SetTextAlign(taCenter);
  RegisterComponent(@labTitle);

  lb0 := TSimpleListBox.Create;
  lb0.SetSize(768, 448);
  lb0.SetPos(128, 128);
  lb0.OnItemClick := @ListboxItemClick;
  lb0.OnRepaintRequest := @TriggerOnRepaintRequestEvent;
  RegisterComponent(@lb0);
end;

procedure TSetlistScreen.RefreshListBox;
var i: integer;
    time: string;
begin
  lb0.ClearItems;
  for i := 0 to High(tracksInfo) do
  begin           //Format('%.*d,[length, number])
    time := '(' + Format('%.*d', [2, Trunc(tracksInfo[i].expectedLength / 60)]) + ':'
      + Format('%.*d', [2, tracksInfo[i].expectedLength mod 60]) + ')';
    lb0.AddItem(tracksInfo[i].title + ' ' + time);
  end;
end;

function TSetlistScreen.ReadSetlistFile(setlistPath: string;
  tracks: TTracksInfoArray; var out_errorMsg: string): boolean;
var tf: TextFile;
    s, title: string;
    trackLength, commaPos: integer;
begin
  Result := False;
  SetLength(tracksInfo, 0);
  fSetlistPath := setlistPath;

  try
    AssignFile(tf, setlistPath);
    Reset(tf);
    while not EOF(tf) do
    begin
      ReadLn(tf, s);
      if (s = '') or (s = '#EXTM3U') then
        continue;
      if Pos('#EXTINF', s) = 1 then
      begin
        commaPos := Pos(',', s);
        trackLength := StrToInt(Copy(s, 9, commaPos - 9));
        title := Copy(s, commaPos + 1, Length(s) - commaPos);
        ReadLn(tf, s);
        AddTrackInfo(AbsolutePath(s, setlistPath), title, trackLength);
      end
      else
        AddTrackInfo(AbsolutePath(s, setlistPath),
          ExtractFileName(s), 0);
    end;

    CloseFile(tf);
  except
    on e: Exception do
    begin
      out_errorMsg := e.Message;
      exit;
    end;
  end;

  Result := True;
end;

function TSetlistScreen.WriteSetlistFile(setlistPath: string;
  tracks: TTracksInfoArray; var out_errorMsg: string): boolean;
var i: integer;
    tf: TextFile;
begin
  Result := False;
  try
    AssignFile(tf, setlistPath);
    Rewrite(tf);
    WriteLn(tf, '#EXTM3U');
    for i := 0 to High(tracks) do
    begin
      WriteLn(tf, '#EXTINF:' + IntToStr(tracks[i].expectedLength) + ',' + tracks[i].title);
      WriteLn(tf, LocalPath(ExtractFilePath(setlistPath), tracks[i].path));
    end;
    CloseFile(tf);
  except
    on e: Exception do
    begin
      out_errorMsg := e.Message;
      exit;
    end;
  end;
  Result := True;
end;

procedure TSetlistScreen.AddTrackInfo(trackPath, trackTitle: string; trackLength: integer);
var trackId: integer;
begin
  trackId := Length(tracksInfo);
  SetLength(tracksInfo, trackId + 1);
  tracksInfo[trackId].path := trackPath;
  tracksInfo[trackId].title := trackTitle;
  tracksInfo[trackId].expectedLength := trackLength;
end;

procedure TSetlistScreen.RemoveTrackInfo(trackId: integer);
var i: integer;
begin
  if (trackId < 0) or (trackId >= Length(tracksInfo)) then
    exit;

  for i := trackId to Length(tracksInfo) - 2 do
    CopyTrackInfo(tracksInfo[i + 1], tracksInfo[i]);
  SetLength(tracksInfo, High(tracksInfo));
end;

function TSetlistScreen.AbsolutePath(unknownFormatPath, setlistPath: string): string;
begin
  if FileExists(unknownFormatPath) then
    Result := unknownFormatPath
  else
    Result := ExtractFilePath(setlistPath) + dirSeparator + unknownFormatPath;
end;

function TSetlistScreen.LocalPath(baseAbsolutePath, fileAbsolutePath: string): string;
begin
  if Pos(baseAbsolutePath + dirSeparator, fileAbsolutePath) = 1 then
    Result := Copy(fileAbsolutePath, Length(baseAbsolutePath) + 2, Length(
      fileAbsolutePath) - Length(baseAbsolutePath) - 1)
  else
  if Pos(baseAbsolutePath, fileAbsolutePath) = 1 then
    Result := Copy(fileAbsolutePath, Length(baseAbsolutePath) + 1, Length(
      fileAbsolutePath) - Length(baseAbsolutePath))
  else
    Result := fileAbsolutePath;
end;

procedure TSetlistScreen.SetEditMode(newEditMode: boolean);
begin
  lb0.SetEditMode(newEditMode);
  if lb0.editMode then btnEditMode.SetCaption('*PICK*')
                  else btnEditMode.SetCaption('PICK');
end;

procedure TSetlistScreen.BtnExitClick(Sender: TObject; x, y: integer);
begin
  SetEditMode(False);
  if parentScreenId = -1 then TerminateApp
                         else TriggerOnSwitchToScreenEvent(parentScreenId);
end;

procedure TSetlistScreen.BtnAddTrackClick(Sender: TObject; x, y: integer);
begin
  SetEditMode(False);
  TriggerOnSwitchToScreenEvent(sdDirExplorer, spPickTrack);
end;

procedure TSetlistScreen.BtnRemoveTrackClick(Sender: TObject; x, y: integer);
begin
  if not lb0.editMode then
    exit;
  RemoveTrackInfo(lb0.GetSelectedItemId);
  SetEditMode(False);
  RefreshListBox;
  RenderImgBuf;
end;

procedure TSetlistScreen.BtnSaveClick(Sender: TObject; x, y: integer);
begin
  SetEditMode(False);
  if SaveSetlist then
    ShowMessage('Setlist saved successfully');
end;

procedure TSetlistScreen.BtnCancelClick(Sender: TObject; x, y: integer);
begin
  SetEditMode(False);
  if LoadSetlist(fSetlistPath) then
    ShowMessage('Setlist reloaded successfully');
  RenderImgBuf;
end;

procedure TSetlistScreen.BtnEditModeClick(Sender: TObject; x, y: integer);
begin
  SetEditMode(not lb0.editMode);
end;

procedure TSetlistScreen.BtnMoveTrackUpClick(Sender: TObject; x, y: integer);
var selectedItemId: integer;
begin
  selectedItemId := lb0.GetSelectedItemId;
  if SwapTrackInfo(selectedItemId - 1, selectedItemId, tracksInfo) then
  begin
    RefreshListBox;
    lb0.SetEditMode(True);
    lb0.SelectItem(selectedItemId - 1);
    RenderImgBuf;
  end;
end;

procedure TSetlistScreen.BtnMoveTrackDownClick(Sender: TObject; x, y: integer);
var selectedItemId: integer;
begin
  selectedItemId := lb0.GetSelectedItemId;
  if SwapTrackInfo(selectedItemId, selectedItemId + 1, tracksInfo) then
  begin
    RefreshListBox;
    lb0.SetEditMode(True);
    lb0.SelectItem(selectedItemId + 1);
    RenderImgBuf;
  end;
end;

procedure TSetlistScreen.BtnChangeTrackInfoClick(Sender: TObject; x, y: integer);
begin
  if lb0.GetSelectedItemId = -1 then exit;
  TriggerOnSwitchToScreenEvent(sdKeyboard, spTypeTrackName);
end;

procedure TSetlistScreen.ListboxItemClick(itemId: integer);
begin
  if itemId = -1 then exit;
  TriggerOnChooseTrackEvent(tracksInfo, itemId);
end;

end.
