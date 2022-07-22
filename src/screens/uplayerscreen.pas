{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit UPlayerScreen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls,
  Graphics,
  Dialogs, Forms,
  UConstants,
  UUtilities,
  USystemTools,
  USimpleComponent,
  USimpleButton,
  USimpleLabel,
  USimpleScrollbar,
  USimpleCheckbox,
  USimpleListbox,
  USimpleMarkersLine,
  UPlayer,
  UGPTabModule,
  UGPTabModuleTypes,
  UGPTabTimeUtils,
  UEventSuspender,
  uos_flat;

const
  defVolLeft=0.5;
  defVolRight=0.5;
  defTempo=1;
  defLooped=False;
  loopDelay=0;
  endLoopCut=50;

type
  TPlayerParams=record
    volLeft:single;
    volRight:single;
    tempo:single;
    looped:boolean;
  end;

  TPlayerScreen=class(TSimpleComponent)
    private
      faceColor:integer;
      playing:boolean;

      btnExit:TSimpleButton;
      labTitle:TSimpleLabel;
      labTime:TSimpleLabel;
      labTemp:TSimpleLabel;
      btnPlayPause:TSimpleButton;
      btnNextTrack:TSimpleButton;
      btnPrevTrack:TSimpleButton;
      btnSettings:TSimpleButton;
      btnSetBeginMarker:TSimpleButton;
      btnSetEndMarker:TSimpleButton;
      btnSwitchLoop:TSimpleButton;
      sbTimeTrack:TSimpleScrollbar;
      ml:TSimpleMarkersLine;
      holdScrollbarOnChangeEvent:boolean;

      tabPanel:TSimpleComponent;
      btnNextMeasure:TSimpleButton;
      btnPrevMeasure:TSimpleButton;
      btnNextBeat:TSimpleButton;
      btnPrevBeat:TSimpleButton;
      btnNextTabTrack:TSimpleButton;

      player:TPlayer;
      playerParams:TPlayerParams;
      gpt:TGPTabModule;
      tab:TTabulature;
      tabLoaded:boolean;

      tracksInfo:TTracksInfoArray;
      currentTrackId:integer;
      nextTrackId:integer;

      delayedUnloadTimer:TTimer;
      //holdUnloadTimer:boolean;

      SwitchToScreenEvent:TSwitchToScreenEvent;
      RepaintRequestEvent:TRepaintRequestEvent;

      mousePressed:boolean;
      draggingPB:boolean;
      prevDragPos:TPoint;

      playerChangePosSuspender:TEventSuspender;
      lastSuspendedNewPos:integer;

      tempTimer:TTimer;

      procedure RenderImgBuf; override;
      procedure PrepareComponents;
      function LoadTrack(path,title:string):boolean;
      //procedure UnloadTrack;
      procedure UpdateTimeLabel(newPosMs:integer);
      procedure PlayerChangeTrackLengthEvent(newTrackLengthMs:integer);
      procedure PlayerChangePosSuspenderRequest;
      procedure PlayerChangePosEvent(newPosMs:integer);
      procedure PlayerTrackFinishEvent;
      procedure ScrollbarChangeValueEvent(value:integer);
      procedure BtnExitClick(sender:TObject; x,y:integer);
      procedure BtnPlayPauseClick(sender:TObject; x,y:integer);
      procedure BtnNextClick(sender:TObject; x,y:integer);
      procedure BtnPrevClick(sender:TObject; x,y:integer);
      procedure BtnSetBeginMarkerClick(sender:TObject; x,y:integer);
      procedure BtnSetEndMarkerClick(sender:TObject; x,y:integer);
      procedure BtnSwitchLoopClick(sender:TObject; x,y:integer);
      procedure BtnSettingsClick(sender:TObject; x,y:integer);
      procedure TabPanelMouseClick(sender:TObject; x,y:integer);
      procedure TabPanelDragBegin(sender:TObject; x,y:integer);
      procedure TabPanelDragUpdate(sender:TObject; x,y:integer);
      procedure TabPanelDragEnd;
      procedure BtnNextMeasureClick(sender:TObject; x,y:integer);
      procedure BtnPrevMeasureClick(sender:TObject; x,y:integer);
      procedure BtnNextBeatClick(sender:TObject; x,y:integer);
      procedure BtnPrevBeatClick(sender:TObject; x,y:integer);
      procedure BtnNextTabTrackClick(sender:TObject; x,y:integer);
      procedure DelayedUnloadTimerTick(Sender:TObject);
      procedure TempTimerTick(Sender:TObject);
    public
      parentScreenId:integer;
      screenProcessId:integer;
      constructor Create;
      destructor Destroy; override;
      function AssignSingleTrack(path,title:string):boolean;
      procedure AssignTracks(var newTracksInfo:TTracksInfoArray; newCurrentTrackId:integer);
      function CheckOnlyTrackLengthInSeconds(filePath:string):integer;
      function SwitchToTrack(trackId:integer):boolean;
      function GetCurrentVolLeft:single;
      function GetCurrentVolRight:single;
      function GetCurrentTempo:single;
      procedure SetVolume(newVolLeft,newVolRight:single);
      procedure SetTempo(newTempo:single);
      procedure SetLooped(isLooped:boolean);
      property OnSwitchToScreen:TSwitchToScreenEvent read SwitchToScreenEvent write SwitchToScreenEvent;
      property OnRepaintRequest:TRepaintRequestEvent read RepaintRequestEvent write RepaintRequestEvent;
      procedure TriggerOnSwitchToScreenEvent(screenId:integer; newScreenProcessId:integer=spNone);
      procedure TriggerOnRepaintRequestEvent;
  end;

implementation

// *****************************************************************************
// ********************************************************************** PUBLIC
// *****************************************************************************

constructor TPlayerScreen.Create;
begin
  inherited;

  faceColor:=clBlack;
  parentScreenId:=sdDirExplorer;
  screenProcessId:=spNone;
  playing:=False;
  holdScrollbarOnChangeEvent:=False;

  playerParams.volLeft:=defVolLeft;
  playerParams.volRight:=defVolRight;
  playerParams.tempo:=defTempo;
  playerParams.looped:=defLooped;

  currentTrackId:=-1;
  nextTrackId:=-1;

  playerChangePosSuspender:=TEventSuspender.Create;
  playerChangePosSuspender.OnTimerAction:=@PlayerChangePosSuspenderRequest;
  playerChangePosSuspender.triggerDelay:=200;

  //holdUnloadTimer:=False;
  delayedUnloadTimer:=TTimer.Create(nil);
  delayedUnloadTimer.Interval:=100;
  delayedUnloadTimer.OnTimer:=@DelayedUnloadTimerTick;
  delayedUnloadTimer.Enabled:=False;

  tempTimer:=TTimer.Create(nil);
  tempTimer.Interval:=5000;
  tempTimer.OnTimer:=@TempTimerTick;
  tempTimer.Enabled:=False;

  gpt:=TGPTabModule.Create;
  tabLoaded:=False;

  mousePressed:=False;
  draggingPB:=False;

  PrepareComponents;

  if playerActive then
  begin
    player:=TPlayer.Create;
    player.OnChangeTrackLength:=@PlayerChangeTrackLengthEvent;
    player.OnChangePos:=@PlayerChangePosEvent;
    player.OnTrackFinish:=@PlayerTrackFinishEvent;
  end;

  tempTimer.Enabled:=True;
end;

destructor TPlayerScreen.Destroy;
begin
  FreeAndNil(playerChangePosSuspender);
  FreeAndNil(player);
  FreeAndNil(gpt);

  inherited;
end;

function TPlayerScreen.AssignSingleTrack(path,title:string):boolean;
begin
  ml.ClearMarkers;
  SetLength(tracksInfo,1);
  tracksInfo[0].path:=path;
  tracksInfo[0].title:=title;
  tracksInfo[0].expectedLength:=1;
  result:=SwitchToTrack(0);
end;

procedure TPlayerScreen.AssignTracks(var newTracksInfo:TTracksInfoArray; newCurrentTrackId:integer);
var i:integer;
begin
  ml.ClearMarkers;
  SetLength(tracksInfo,Length(newTracksInfo));
  for i:=0 to High(newTracksInfo) do
  begin
    tracksInfo[i].path:=newTracksInfo[i].path;
    tracksInfo[i].title:=newTracksInfo[i].title;
    tracksInfo[i].expectedLength:=newTracksInfo[i].expectedLength;
  end;
  SwitchToTrack(newCurrentTrackId);
end;

function TPlayerScreen.CheckOnlyTrackLengthInSeconds(filePath:string):integer;
begin
  if AssignSingleTrack(filePath,'') then
    result:=Round(sbTimeTrack.maxValue*0.001)
  else
    result:=0;
end;

function TPlayerScreen.SwitchToTrack(trackId:integer):boolean;
begin
  if currentTrackId<>trackId then ml.ClearMarkers;

  currentTrackId:=trackId;
  btnNextTrack.SetVisibility(currentTrackId<High(tracksInfo));
  btnPrevTrack.SetVisibility(currentTrackId>0);

  result:=LoadTrack(tracksInfo[currentTrackId].path,tracksInfo[currentTrackId].title);

  if not result then
    ShowMessage('Cannot load track:'+#13+tracksInfo[currentTrackId].path);
end;

function TPlayerScreen.GetCurrentVolLeft:single;
begin
  result:=playerParams.volLeft;
end;

function TPlayerScreen.GetCurrentVolRight:single;
begin
  result:=playerParams.volRight;
end;

function TPlayerScreen.GetCurrentTempo:single;
begin
  result:=playerParams.tempo;
end;

procedure TPlayerScreen.SetVolume(newVolLeft,newVolRight:single);
begin
  playerParams.volLeft:=newVolLeft;
  playerParams.volRight:=newVolRight;
  if Assigned(player) then player.SetVolume(newVolLeft,newVolRight);
end;

procedure TPlayerScreen.SetTempo(newTempo:single);
begin
  playerParams.tempo:=newTempo;
  if Assigned(player) then player.SetTempo(newTempo);
end;

procedure TPlayerScreen.SetLooped(isLooped:boolean);
begin
  playerParams.looped:=isLooped;
  if isLooped then btnSwitchLoop.SetCaption('>L<')
              else btnSwitchLoop.SetCaption('>L>');
end;

procedure TPlayerScreen.TriggerOnSwitchToScreenEvent(screenId:integer; newScreenProcessId:integer=spNone);
begin
  if Assigned(OnSwitchToScreen) then OnSwitchToScreen(screenId, newScreenProcessId);
end;

procedure TPlayerScreen.TriggerOnRepaintRequestEvent;
begin
  if Assigned(OnRepaintRequest) then OnRepaintRequest;
end;

// *****************************************************************************
// ********************************************************************* PRIVATE
// *****************************************************************************

procedure TPlayerScreen.RenderImgBuf;
begin
  if (fWidth=0) or (fHeight=0) then exit;

  imgBuf.canvas.Brush.Color:=faceColor;
  imgBuf.canvas.FillRect(0,0,fWidth,fHeight);
end;

procedure TPlayerScreen.PrepareComponents;
begin
  btnExit:=TSimpleButton.Create;
  btnExit.SetCaption('X');
  btnExit.SetSize(96,96);
  btnExit.SetPos(1024-96-16,16);
  btnExit.OnMouseClick:=@BtnExitClick;
  RegisterComponent(@btnExit);

  labTitle:=TSimpleLabel.Create;
  labTitle.SetTextAlign(taCenter);
  labTitle.SetCaption('');
  labTitle.SetSize(768,96);
  labTitle.SetPos(128,16);
  RegisterComponent(@labTitle);

  btnPlayPause:=TSimpleButton.Create;
  btnPlayPause.SetCaption('>');
  btnPlayPause.SetSize(96,96);
  btnPlayPause.SetPos(512-48,600-96-16);
  btnPlayPause.OnMouseClick:=@BtnPlayPauseClick;
  RegisterComponent(@btnPlayPause);

  btnNextTrack:=TSimpleButton.Create;
  btnNextTrack.SetCaption('>|');
  btnNextTrack.SetSize(96,96);
  btnNextTrack.SetPos(1024-96-16,600-96-16);
  btnNextTrack.OnMouseClick:=@BtnNextClick;
  RegisterComponent(@btnNextTrack);

  btnPrevTrack:=TSimpleButton.Create;
  btnPrevTrack.SetCaption('|<');
  btnPrevTrack.SetSize(96,96);
  btnPrevTrack.SetPos(16,600-96-16);
  btnPrevTrack.OnMouseClick:=@BtnPrevClick;
  RegisterComponent(@btnPrevTrack);

  btnSetBeginMarker:=TSimpleButton.Create;
  btnSetBeginMarker.SetCaption('M{');
  btnSetBeginMarker.SetSize(96,96);
  btnSetBeginMarker.SetPos(1024-3*96-3*16,600-96-16);
  btnSetBeginMarker.OnMouseClick:=@BtnSetBeginMarkerClick;
  RegisterComponent(@btnSetBeginMarker);

  btnSetEndMarker:=TSimpleButton.Create;
  btnSetEndMarker.SetCaption('}M');
  btnSetEndMarker.SetSize(96,96);
  btnSetEndMarker.SetPos(1024-2*96-2*16,600-96-16);
  btnSetEndMarker.OnMouseClick:=@BtnSetEndMarkerClick;
  RegisterComponent(@btnSetEndMarker);

  btnSwitchLoop:=TSimpleButton.Create;
  btnSwitchLoop.SetCaption('L');
  btnSwitchLoop.SetSize(96,96);
  btnSwitchLoop.SetPos(1024-4*96-4*16,600-96-16);
  btnSwitchLoop.OnMouseClick:=@BtnSwitchLoopClick;
  RegisterComponent(@btnSwitchLoop);

  btnSettings:=TSimpleButton.Create;
  btnSettings.SetCaption('O');
  btnSettings.SetSize(96,96);
  btnSettings.SetPos(16,16);
  btnSettings.OnMouseClick:=@BtnSettingsClick;
  RegisterComponent(@btnSettings);

  sbTimeTrack:=TSimpleScrollbar.Create;
  sbTimeTrack.SetLabelVisibility(False);
  sbTimeTrack.SetSize(992,96);
  sbTimeTrack.SetPos(16,600-2*96-2*16);
  sbTimeTrack.OnValueChange:=@ScrollbarChangeValueEvent;
  RegisterComponent(@sbTimeTrack);

  labTime:=TSimpleLabel.Create;
  labTime.SetTransparency(True);
  labTime.SetTextAlign(taCenter);
  labTime.SetCaption('');
  labTime.SetSize(992,96);
  labTime.SetPos(16,600-2*96-2*16);
  RegisterComponent(@labTime);

  labTemp:=TSimpleLabel.Create;
  labTemp.SetTransparency(True);
  labTemp.SetTextAlign(taCenter);
  labTemp.SetCaption('');
  labTemp.SetSize(96,96);
  labTemp.SetPos(1024-96-16,600-96-16);
  RegisterComponent(@labTemp);

  ml:=TSimpleMarkersLine.Create;
  ml.SetSize(1024-2*96-2*16,16);
  ml.SetPos(96+16,600-2*96-3*16);
  RegisterComponent(@ml);

  tabPanel:=TSimpleComponent.Create;
  tabPanel.SetSize(912,232);
  tabPanel.SetPos(0,128);
  tabPanel.OnMouseClick:=@TabPanelMouseClick;
  //tabPanel.OnDragBegin:=@TabPanelDragBegin;
  //tabPanel.OnDragUpdate:=@TabPanelDragUpdate;
  //tabPanel.OnDragEnd:=@TabPanelDragEnd;
  RegisterComponent(@tabPanel);

  btnNextMeasure:=TSimpleButton.Create;
  btnNextMeasure.SetCaption('m+');
  btnNextMeasure.SetSize(96,96);
  btnNextMeasure.SetPos(16+96+16,600-96-16);
  btnNextMeasure.OnMouseClick:=@btnNextMeasureClick;
  RegisterComponent(@btnNextMeasure);

  btnPrevMeasure:=TSimpleButton.Create;
  btnPrevMeasure.SetCaption('m-');
  btnPrevMeasure.SetSize(96,96);
  btnPrevMeasure.SetPos(16+2*(96+16),600-96-16);
  btnPrevMeasure.OnMouseClick:=@btnPrevMeasureClick;
  RegisterComponent(@btnPrevMeasure);

  btnNextBeat:=TSimpleButton.Create;
  btnNextBeat.SetCaption('b+');
  btnNextBeat.SetSize(96,64);
  btnNextBeat.SetPos(1024-16-96,128+64+16);
  btnNextBeat.OnMouseClick:=@btnNextBeatClick;
  RegisterComponent(@btnNextBeat);

  btnPrevBeat:=TSimpleButton.Create;
  btnPrevBeat.SetCaption('b-');
  btnPrevBeat.SetSize(96,64);
  btnPrevBeat.SetPos(1024-16-96,128+2*(64+16));
  btnPrevBeat.OnMouseClick:=@btnPrevBeatClick;
  RegisterComponent(@btnPrevBeat);

  btnNextTabTrack:=TSimpleButton.Create;
  btnNextTabTrack.SetCaption('T');
  btnNextTabTrack.SetSize(96,64);
  btnNextTabTrack.SetPos(1024-16-96,128);
  btnNextTabTrack.OnMouseClick:=@btnNextTabTrackClick;
  RegisterComponent(@btnNextTabTrack);
end;

function TPlayerScreen.LoadTrack(path,title:string):boolean;
var tabPath,errorMsg:string;
    i:integer;
begin
  result:=False;

  if not Assigned(player) then exit;

  btnPlayPause.SetCaption('>');
  SetLooped(False);
  playing:=False;

  if not player.LoadTrack(path,
                          playerParams.volLeft,
                          playerParams.volRight,
                          playerParams.tempo) then
  begin
    btnPlayPause.SetVisibility(False);
    labTitle.SetCaption('ERROR loading: '+title);
    labTime.SetCaption('00:00.000');
    exit;
  end;

  btnPlayPause.SetVisibility(True);
  labTitle.SetCaption(title);

  tabPath:=ChangeFileExt(path,'.gp4');
  if FileExists(tabPath) then
  begin
    tabLoaded:=gpt.LoadTab(tabPath,tab,errorMsg);
    if tabLoaded then
    begin
      gpt.display.SetViewWidth(912);
      gpt.display.SetTabPtr(@tab);
      // set the drums track:
      for i:=0 to tab.gpTab.tracksCount-1 do
        if tab.gpTab.tracks[i].drumsTrack then
        begin
          gpt.display.SetTrackId(i);
          break;
        end;
      gpt.display.Draw(tabPanel.GetBitmapCanvas,0,912);
    end
    else
      ShowMessage(errorMsg);
  end;

  result:=True;
end;

procedure TPlayerScreen.UpdateTimeLabel(newPosMs:integer);
var mi,se,ms:word;
begin
  ms:=newPosMs mod 1000;
  se:=Trunc(newPosMs*0.001) mod 60;
  mi:=Trunc(newPosMs/60000);
  labTime.SetCaption(format('%.2d:%.2d.%.3d', [mi, se, ms]));
end;

procedure TPlayerScreen.PlayerChangeTrackLengthEvent(newTrackLengthMs:integer);
begin
  if newTrackLengthMs-1=sbTimeTrack.maxValue then exit;
  holdScrollbarOnChangeEvent:=True;
  sbTimeTrack.SetRange(0,newTrackLengthMs-1);
  ml.SetMaxPos(newTrackLengthMs-1);
  holdScrollbarOnChangeEvent:=False;
end;

procedure TPlayerScreen.PlayerChangePosSuspenderRequest;
begin
  PlayerChangePosEvent(lastSuspendedNewPos);
end;

procedure TPlayerScreen.PlayerChangePosEvent(newPosMs:integer);
var m,b,tb:integer;
begin
  if playerParams.looped
    and (((ml.beginMarkerPos<ml.endMarkerPos) and (newPosMs>ml.endMarkerPos) and (newPosMs>ml.beginMarkerPos))
      or ((ml.beginMarkerPos>ml.endMarkerPos) and (newPosMs>ml.endMarkerPos) and (newPosMs<ml.beginMarkerPos))) then
  begin
    playerChangePosSuspender.CancelSuspendedEvent;
    player.Seek(ml.beginMarkerPos);
    UpdateTimeLabel(ml.beginMarkerPos);
    exit;
  end;

  if not playerChangePosSuspender.EventProcessingInitialization then
  begin
    lastSuspendedNewPos:=newPosMs;
    exit;
  end;

  if newPosMs=sbTimeTrack.value then exit;
  holdScrollbarOnChangeEvent:=True;
  sbTimeTrack.SetValue(newPosMs);
  holdScrollbarOnChangeEvent:=False;
  UpdateTimeLabel(newPosMs);

  if tabLoaded then
  begin
    timeUtils.MillisecondToTabPos(newPosMs,gpt.display.view.trackId,tab.measuresTimeInfo,m,b,tb);
    if (tb<>gpt.display.cursor.trackBeatId) or (m<>gpt.display.cursor.measureId) then
    begin
      gpt.display.GoToMeasureBeat(m,tb);
      gpt.display.Draw(tabPanel.GetBitmapCanvas,0,912);
    end;
  end;

  TriggerOnRepaintRequestEvent;

  playerChangePosSuspender.EventProcessingFinalization;
end;

procedure TPlayerScreen.PlayerTrackFinishEvent;
begin
  if currentTrackId<High(tracksInfo) then nextTrackId:=currentTrackId+1
                                     else nextTrackId:=currentTrackId;
  delayedUnloadTimer.Enabled:=True;
end;

procedure TPlayerScreen.ScrollbarChangeValueEvent(value:integer);
var m,b,tb:integer;
begin
  if Assigned(player) and (not holdScrollbarOnChangeEvent) then
  begin
    playerChangePosSuspender.CancelSuspendedEvent;
    player.Seek(value);
    if tabLoaded then
    begin
      timeUtils.MillisecondToTabPos(value,gpt.display.view.trackId,tab.measuresTimeInfo,m,b,tb);
      if (tb<>gpt.display.cursor.trackBeatId) or (m<>gpt.display.cursor.measureId) then
      begin
        gpt.display.GoToMeasureBeat(m,tb);
        gpt.display.Draw(tabPanel.GetBitmapCanvas,0,912);
      end;
    end;
    UpdateTimeLabel(value);
  end;
end;

procedure TPlayerScreen.BtnExitClick(sender:TObject; x,y:integer);
begin
  if Assigned(player) then player.Stop;
  if parentScreenId=-1 then TerminateApp
                         else TriggerOnSwitchToScreenEvent(parentScreenId);
end;

procedure TPlayerScreen.BtnPlayPauseClick(sender:TObject; x,y:integer);
begin
  if not Assigned(player) then exit;
  playing:=not playing;
  if playing then begin btnPlayPause.SetCaption('||'); player.Play;  end
             else begin btnPlayPause.SetCaption('>');  player.Pause; end;

end;

procedure TPlayerScreen.BtnNextClick(sender:TObject; x,y:integer);
begin
  if not Assigned(player) then exit;
  if currentTrackId<High(tracksInfo) then
  begin
    player.Stop;
    nextTrackId:=currentTrackId+1;
    delayedUnloadTimer.Enabled:=True;
  end;
end;

procedure TPlayerScreen.BtnPrevClick(sender:TObject; x,y:integer);
begin
  if not Assigned(player) then exit;
  if currentTrackId>0 then
  begin
    player.Stop;
    nextTrackId:=currentTrackId-1;
    delayedUnloadTimer.Enabled:=True;
  end;
end;

procedure TPlayerScreen.BtnSetBeginMarkerClick(sender:TObject; x,y:integer);
begin
  if tabLoaded then
  begin
    gpt.display.SetLoopBeginAtCurrentPos;
    gpt.display.Draw(tabPanel.GetBitmapCanvas,0,912);
    ml.SetBeginMarkerPos(Trunc(gpt.display.GetCurrentTrackBeatMS)+1+loopDelay);
  end
  else
    ml.SetBeginMarkerPos(sbTimeTrack.value);
end;

procedure TPlayerScreen.BtnSetEndMarkerClick(sender:TObject; x,y:integer);
begin
  if tabLoaded then
  begin
    gpt.display.SetLoopEndAtCurrentPos;
    gpt.display.Draw(tabPanel.GetBitmapCanvas,0,912);
    ml.SetEndMarkerPos(Trunc(gpt.display.GetCurrentTrackBeatEndMS)+loopDelay-endLoopCut);
  end
  else
    ml.SetEndMarkerPos(sbTimeTrack.value);
end;

procedure TPlayerScreen.BtnSwitchLoopClick(sender:TObject; x,y:integer);
begin
  SetLooped(not playerParams.looped);
end;

procedure TPlayerScreen.BtnSettingsClick(sender:TObject; x,y:integer);
begin
  TriggerOnSwitchToScreenEvent(sdSettings);
end;

procedure TPlayerScreen.TabPanelMouseClick(sender:TObject; x,y:integer);
var m,tb:integer;
begin
  if (not tabLoaded)
    or (not gpt.display.CanvasPxToTabPos(Point(x,y),m,tb)) then exit;
  gpt.display.GoToMeasureBeat(m,tb);
  gpt.display.Draw(tabPanel.GetBitmapCanvas,0,912);
  sbTimeTrack.SetValue(Trunc(gpt.display.cursor.currentMS)+1);
end;

procedure TPlayerScreen.TabPanelDragBegin(sender:TObject; x,y:integer);
begin
  draggingPB:=True;
  prevDragPos.x:=x;
  prevDragPos.y:=y;
end;

procedure TPlayerScreen.TabPanelDragUpdate(sender:TObject; x,y:integer);
begin
  if not tabLoaded then exit;
  gpt.display.SetViewPos(gpt.display.view.left-x+prevDragPos.x);
  gpt.display.Draw(tabPanel.GetBitmapCanvas,0,912);
  prevDragPos.x:=x;
  prevDragPos.y:=y;
end;

procedure TPlayerScreen.TabPanelDragEnd;
begin
  draggingPB:=False;
end;

procedure TPlayerScreen.BtnNextMeasureClick(sender:TObject; x,y:integer);
begin
  if not tabLoaded then exit;
  gpt.display.GoToMeasure(gpt.display.cursor.measureId+1);
  gpt.display.Draw(tabPanel.GetBitmapCanvas,0,912);
  sbTimeTrack.SetValue(Trunc(gpt.display.cursor.currentMS)+1);
end;

procedure TPlayerScreen.BtnPrevMeasureClick(sender:TObject; x,y:integer);
begin
  if not tabLoaded then exit;
  gpt.display.GoToMeasure(gpt.display.cursor.measureId-1);
  gpt.display.Draw(tabPanel.GetBitmapCanvas,0,912);
  sbTimeTrack.SetValue(Trunc(gpt.display.cursor.currentMS)+1);
end;

procedure TPlayerScreen.BtnNextBeatClick(sender:TObject; x,y:integer);
begin
  if not tabLoaded then exit;
  gpt.display.GoToMeasureBeat(gpt.display.cursor.measureId,gpt.display.cursor.trackBeatId+1);
  gpt.display.Draw(tabPanel.GetBitmapCanvas,0,912);
  sbTimeTrack.SetValue(Trunc(gpt.display.cursor.currentMS)+1);
end;

procedure TPlayerScreen.BtnPrevBeatClick(sender:TObject; x,y:integer);
begin
  if not tabLoaded then exit;
  gpt.display.GoToMeasureBeat(gpt.display.cursor.measureId,gpt.display.cursor.trackBeatId-1);
  gpt.display.Draw(tabPanel.GetBitmapCanvas,0,912);
  sbTimeTrack.SetValue(Trunc(gpt.display.cursor.currentMS)+1);
end;

procedure TPlayerScreen.BtnNextTabTrackClick(sender:TObject; x,y:integer);
begin
  if not tabLoaded then exit;
  if gpt.display.view.trackId<tab.gpTab.tracksCount-1 then
    gpt.display.SetTrackId(gpt.display.view.trackId+1)
  else
    gpt.display.SetTrackId(0);
  gpt.display.Draw(tabPanel.GetBitmapCanvas,0,912);
  sbTimeTrack.SetValue(Trunc(gpt.display.cursor.currentMS)+1);
end;

procedure TPlayerScreen.DelayedUnloadTimerTick(Sender:TObject);
begin
  delayedUnloadTimer.Enabled:=False;

  if not Assigned(player) then exit;

  //if holdUnloadTimer then exit;
  //holdUnloadTimer:=True;

  labTitle.SetCaption('<unloading track>');
  TriggerOnRepaintRequestEvent;
  while not player.stopped do sleep(1);

  //if currentTrackId<High(tracksInfo) then currentTrackId:=currentTrackId+1;
  SwitchToTrack(nextTrackId);
end;

procedure TPlayerScreen.TempTimerTick(Sender:TObject);
begin
  if Assigned(labTemp) then labTemp.SetCaption(GetTemp);
  //
end;

end.

