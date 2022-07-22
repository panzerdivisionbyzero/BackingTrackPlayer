{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit uMainController;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  ExtCtrls,
  Graphics,
  Types,
  Dialogs,
  UConstants,
  UUtilities,
  UInputController,
  USimpleComponent,
  UTestScreen,
  UDirExplorerScreen,
  USetlistScreen,
  UPlayerScreen,
  USettingsScreen,
  UKeyboardScreen,
  UCommandLineScreen;

type
  PCanvas=^TCanvas;
  TMainController=class
    private
      // screens instances:
      screenTest:TTestScreen;
      screenDirExplorer:TDirExplorerScreen;
      screenSetList:TSetlistScreen;
      screenPlayer:TPlayerScreen;
      screenSettings:TSettingsScreen;
      screenCommandLine:TCommandLineScreen;
      screenKeyboard:TKeyboardScreen;

      pScreens:array of PTSimpleComponent; // listed pointers to screens instances (indexes are easier to use in some cases)
      currentScreenId:integer; // pScreens^[] index of active screen

      canvasBmp:TBitmap;

      repaintInProgress:boolean;
      repaintRequestActive:boolean;
      repaintRequestTimer:TTimer;

      RepaintRequestEvent:TRepaintRequestEvent;

      procedure PrepareScreens;
      procedure DragBegin(x,y:integer);
      procedure DragUpdate(x,y:integer);
      procedure DragEnd;
      procedure SingleTap(x,y:integer);
      procedure SwitchToScreen(dstScreenId:integer; screenProcessId:integer=spNone);
      procedure OpenFile(filePath:string; screenProcessId:integer=spNone);
      procedure PlayTracks(var tracksInfo:TTracksInfoArray; currentTrackId:integer);
      procedure SettingsChangeVolEvent(newVolLeft,newVolRight:single);
      procedure SettingsChangeTempoEvent(newTempo:single);
      procedure ReceiveRepaintRequestEvent;
      procedure RepaintRequestTimerTick(Sender:TObject);
    public
      ic:TInputController;
      constructor Create;
      destructor Destroy; override;
      procedure SetScreenSize(new_width,new_height:integer);
      procedure Repaint(pCan:PCanvas);
      property OnRepaintRequest:TRepaintRequestEvent read RepaintRequestEvent write RepaintRequestEvent;
      procedure TriggerOnRepaintRequestEvent;
  end;

implementation

// *****************************************************************************
// ********************************************************************** PUBLIC
// *****************************************************************************

constructor TMainController.Create;
begin
  currentScreenId:=-1;

  ic:=TInputController.Create;
  ic.OnDragBegin:=@DragBegin;
  ic.OnDragUpdate:=@DragUpdate;
  ic.OnDragEnd:=@DragEnd;
  ic.OnSingleTap:=@SingleTap;

  repaintInProgress:=False;
  canvasBmp:=TBitmap.Create;
  canvasBmp.Width:=0;
  canvasBmp.Height:=0;
  canvasBmp.pixelFormat:=pf24bit;

  repaintRequestActive:=False;
  repaintRequestTimer:=TTimer.Create(nil);
  repaintRequestTimer.Interval:=100;
  repaintRequestTimer.OnTimer:=@RepaintRequestTimerTick;
  repaintRequestTimer.Enabled:=True;

  PrepareScreens;

  SwitchToScreen(sdDirExplorer);
end;

destructor TMainController.Destroy;
var i:integer;
begin
  repaintRequestTimer.Enabled:=False;
  FreeAndNil(repaintRequestTimer);
  for i:=0 to High(pScreens) do
    pScreens[i]^.Destroy;

  ic.Destroy;

  inherited;
end;

procedure TMainController.SetScreenSize(new_width,new_height:integer);
var i:integer;
begin
  for i:=0 to High(pScreens) do
    pScreens[i]^.SetSize(new_width,new_height);

  canvasBmp.Width:=new_width;
  canvasBmp.Height:=new_height;
end;

procedure TMainController.Repaint(pCan:PCanvas);
var can:TCanvas;
begin
  if repaintInProgress or (currentScreenId<0) then exit;

  repaintInProgress:=True;

  can:=canvasBmp.Canvas;
  pScreens[currentScreenId]^.Repaint(can);

  pCan^.Draw(0,0,canvasBmp);

  repaintInProgress:=False;
end;

procedure TMainController.TriggerOnRepaintRequestEvent;
begin
  if Assigned(OnRepaintRequest) then OnRepaintRequest;
end;

// *****************************************************************************
// ********************************************************************* PRIVATE
// *****************************************************************************

procedure TMainController.PrepareScreens;
begin
  SetLength(pScreens,7);
  screenTest:=TTestScreen.Create;
  screenTest.SetSize(1024,600);
  screenTest.OnSwitchToScreen:=@SwitchToScreen;
  pScreens[sdTest]:=@screenTest;

  screenDirExplorer:=TDirExplorerScreen.Create;
  screenDirExplorer.SetSize(1024,600);
  screenDirExplorer.OnSwitchToScreen:=@SwitchToScreen;
  screenDirExplorer.OnOpenFile:=@OpenFile;
  pScreens[sdDirExplorer]:=@screenDirExplorer;

  screenSetlist:=TSetListScreen.Create;
  screenSetlist.SetSize(1024,600);
  screenSetlist.OnSwitchToScreen:=@SwitchToScreen;
  screenSetlist.OnChooseTrack:=@PlayTracks;
  screenSetlist.OnRepaintRequest:=@ReceiveRepaintRequestEvent;
  pScreens[sdSetlist]:=@screenSetlist;

  screenPlayer:=TPlayerScreen.Create;
  screenPlayer.SetSize(1024,600);
  screenPlayer.OnSwitchToScreen:=@SwitchToScreen;
  screenPlayer.OnRepaintRequest:=@ReceiveRepaintRequestEvent;
  pScreens[sdPlayer]:=@screenPlayer;

  screenSettings:=TSettingsScreen.Create;
  screenSettings.SetSize(1024,600);
  screenSettings.OnSwitchToScreen:=@SwitchToScreen;
  screenSettings.OnChangeVolume:=@SettingsChangeVolEvent;
  screenSettings.OnChangeTempo:=@SettingsChangeTempoEvent;
  pScreens[sdSettings]:=@screenSettings;

  screenKeyboard:=TKeyboardScreen.Create;
  screenKeyboard.SetSize(1024,600);
  screenKeyboard.OnSwitchToScreen:=@SwitchToScreen;
  screenKeyboard.OnRepaintRequest:=@ReceiveRepaintRequestEvent;
  pScreens[sdKeyboard]:=@screenKeyboard;

  screenCommandLine:=TCommandLineScreen.Create;
  screenCommandLine.SetSize(1024,600);
  screenCommandLine.OnSwitchToScreen:=@SwitchToScreen;
  screenCommandLine.OnRepaintRequest:=@ReceiveRepaintRequestEvent;
  pScreens[sdCommandLine]:=@screenCommandLine;
end;

procedure TMainController.DragBegin(x,y:integer);
begin
  if currentScreenId<0 then exit;
  pScreens[currentScreenId]^.DragBegin(x,y)
end;

procedure TMainController.DragUpdate(x,y:integer);
begin
  if currentScreenId<0 then exit;
  pScreens[currentScreenId]^.DragUpdate(x,y);
end;

procedure TMainController.DragEnd;   
begin
  if currentScreenId<0 then exit;
  pScreens[currentScreenId]^.DragEnd;
end;

procedure TMainController.SingleTap(x,y:integer);
begin
  if currentScreenId<0 then exit;
  pScreens[currentScreenId]^.MouseClick(x,y);
end;

procedure TMainController.SwitchToScreen(dstScreenId:integer; screenProcessId:integer=spNone);
var errorMsg:string;
begin
  if dstScreenId>High(pScreens) then exit;

  if currentScreenId<>sdSettings then // ---------------------------------------
  begin
    case dstScreenId of
      sdDirExplorer: // --------------------------------------------------------
        begin
          screenDirExplorer.screenProcessId:=screenProcessId;
          screenDirExplorer.ReDir;
        end;
      sdSetlist:     // --------------------------------------------------------
        begin
          case screenProcessId of
            spTypeNewSetlistFileName:
              if not screenSetlist.CreateSetlist(
                 screenDirExplorer.GetCurrentDir+dirSeparator+screenKeyboard.GetText, errorMsg) then
              begin
                ShowMessage(errorMsg);
                currentScreenId:=sdDirExplorer;
                exit;
              end;
            spTypeTrackName:
              begin
                screenSetlist.SetSelectedTrackTitle(screenKeyboard.GetText);
                screenSetlist.SetSelectedTrackLength(
                  screenPlayer.CheckOnlyTrackLengthInSeconds(
                    screenSetlist.GetSelectedTrackAbsolutePath));
              end;
          end; // end case screenProcessId of
        end;
      sdPlayer:      // --------------------------------------------------------
        begin
          screenPlayer.parentScreenId:=currentScreenId;
          screenPlayer.screenProcessId:=screenProcessId;
        end;
      sdSettings:    // --------------------------------------------------------
        begin
          screenSettings.parentScreenId:=currentScreenId;
          screenSettings.SetVolume(screenPlayer.GetCurrentVolLeft,
                                   screenPlayer.GetCurrentVolRight);
          screenSettings.SetTempo(screenPlayer.GetCurrentTempo);
          screenSettings.screenProcessId:=screenProcessId;
        end;
      sdCommandLine: // --------------------------------------------------------
        if screenProcessId=spTypeCommand then
          screenCommandLine.ExecuteCommand(screenKeyboard.GetText);
      sdKeyboard:    // --------------------------------------------------------
        begin
          screenKeyboard.parentScreenId:=currentScreenId;
          case currentScreenId of
            sdDirExplorer: begin screenKeyboard.SetText('.m3u');
                                 screenKeyboard.SetCursorPos(0); end;
            sdSetlist    : begin screenKeyboard.SetText(screenSetlist.GetSelectedTrackTitle);
                                 screenKeyboard.SetCursorPos(MaxInt); end;
            sdCommandLine: begin screenKeyboard.SetText('');
                                 screenKeyboard.SetCursorPos(0); end;
          end;
          screenKeyboard.screenProcessId:=screenProcessId;
        end;
    end; // end case dstScreenId of
  end; // ----------------------------------------------------------------------

  if currentScreenId=sdKeyboard then screenKeyboard.Deactivate;
  if dstScreenId    =sdKeyboard then screenKeyboard.Activate;

  currentScreenId:=dstScreenId;
end;

procedure TMainController.OpenFile(filePath:string; screenProcessId:integer=spNone);
var fileExtension:string;
begin
  fileExtension:=ExtractFileExt(filePath);

  if screenProcessId=spNone then
  begin
    case fileExtension of
      '.m3u' : if screenSetlist.LoadSetlist(filePath) then
                 SwitchToScreen(sdSetlist);
      '.mp3' : if screenPlayer.AssignSingleTrack(filePath,ExtractFileName(filePath)) then
                 SwitchToScreen(sdPlayer);
      '.wav' : if screenPlayer.AssignSingleTrack(filePath,ExtractFileName(filePath)) then
                 SwitchToScreen(sdPlayer);
      else
        ShowMessage('Cannot open '+fileExtension+' file');
    end
  end
  else
  if screenProcessId=spPickTrack then
  begin
    if (fileExtension='.mp3') or (fileExtension='.wav') then
    begin
      screenSetlist.AppendOpenedSetlist(filePath);
      SwitchToScreen(sdSetlist);
    end
    else
      ShowMessage('Cannot add '+fileExtension+' file to setlist');
  end;
end;

procedure TMainController.PlayTracks(var tracksInfo:TTracksInfoArray; currentTrackId:integer);
begin
  screenPlayer.AssignTracks(tracksInfo,currentTrackId);
  SwitchToScreen(sdPlayer);
end;

procedure TMainController.SettingsChangeVolEvent(newVolLeft,newVolRight:single);
begin
  screenPlayer.SetVolume(newVolLeft,newVolRight);
end;

procedure TMainController.SettingsChangeTempoEvent(newTempo:single);
begin
  screenPlayer.SetTempo(newTempo);
end;

procedure TMainController.ReceiveRepaintRequestEvent;
begin
  repaintRequestActive:=True;
end;

procedure TMainController.RepaintRequestTimerTick(Sender:TObject);
begin
  if repaintRequestActive then
  begin
    repaintRequestActive:=False;
    TriggerOnRepaintRequestEvent;
  end;
end;

end.

