{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit UPlayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Forms, CTypes,
  uos_flat, UConstants, ULibPaths;

type
  TChangeTrackLengthEvent=procedure (newTrackLengthMs:integer) of object;
  TChangePosEvent=procedure (newPosMs:integer) of object;
  TTrackFinishEvent=procedure of object;
  TPlayer=class
    private
      ready:boolean;
      fStopped:boolean;
      fTrackLength:integer;
      PlayerIndex1:integer;
      InputIndex1:integer;
      OutputIndex1:integer;
      PluginIndex2:integer;

      ChangeTrackLengthEvent:TChangeTrackLengthEvent;
      ChangePosEvent:TChangePosEvent;
      TrackFinishEvent:TTrackFinishEvent;

      function SamplesToMilliseconds(samplesPos:integer):integer;
      function MillisecondsToSamples(millisecondsPos:integer):integer;
    public
      //loopProcRunning:boolean;
      //endProcRunning:boolean;

      constructor Create;
      destructor Destroy; override;
      function LoadTrack(filePath:string; initVolumeLeft:double=1; initVolumeRight:double=1; initTempo:double=1):boolean;
      procedure Seek(trackPosMs:integer);
      procedure Play;
      procedure Pause;
      procedure Stop;
      function GetTrackPosMs:integer;
      procedure SetVolume(newVolLeft,newVolRight:single);
      procedure SetTempo(newTempo:single);
      procedure PlayingLoopProc;
      procedure PlayingEndProc;
      property OnChangeTrackLength:TChangeTrackLengthEvent read ChangeTrackLengthEvent write ChangeTrackLengthEvent;
      property OnChangePos:TChangePosEvent read ChangePosEvent write ChangePosEvent;
      property OnTrackFinish:TTrackFinishEvent read TrackFinishEvent write TrackFinishEvent;
      procedure TriggerOnChangeTrackLengthEvent(newTrackLengthMs:integer);
      procedure TriggerOnChangePosEvent(newPosMs:integer);
      procedure TriggerOnTrackFinishEvent;
    published
      property trackLength:integer read fTrackLength;
      property stopped:boolean read fStopped;
  end;

implementation

// *****************************************************************************
// ********************************************************************** PUBLIC
// *****************************************************************************

constructor TPlayer.Create;
var directPA_FileName,directSF_FileName,directMP_FileName,directST_FileName,currentDir:string;
begin
  ready:=False;
  fStopped:=True;
  fTrackLength:=0;
  InputIndex1:=-1;

  //loopProcRunning:=False;
  //endProcRunning:=False;

  {$if defined(windows)}
  directPA_FileName:=PA_FileName;
  directSF_FileName:=SF_FileName;
  directMP_FileName:=MP_FileName;
  directST_FileName:=ST_FileName;
  {$else}
  currentDir:=ExtractFilePath(Application.ExeName);
  directPA_FileName:=currentDir+dirSeparator+PA_FileName;
  directSF_FileName:=currentDir+dirSeparator+SF_FileName;
  directMP_FileName:=currentDir+dirSeparator+MP_FileName;
  directST_FileName:=currentDir+dirSeparator+ST_FileName;
  {$endif}

  ready:=False;
  if (uos_LoadLib(pchar(directPA_FileName),
              pchar(directSF_FileName),
              pchar(directMP_FileName),
              nil,
              nil,
              nil)<>0)
    or (uos_LoadPlugin('soundtouch', Pchar(directST_FileName)) <> 0)  then
  begin
    ShowMessage('Error loading libs');
    exit;
  end;
  ready:=True;
end;

destructor TPlayer.Destroy;
begin
  uos_LoopProcIn(PlayerIndex1, InputIndex1, nil);
  uos_EndProc(PlayerIndex1, nil);

  //while loopProcRunning or endProcRunning do sleep(1);
  //sleep(1000);

  uos_free;

  inherited;
end;

function TPlayer.LoadTrack(filePath:string; initVolumeLeft:double=1; initVolumeRight:double=1; initTempo:double=1):boolean;
var samformat: shortint;
begin
  result:=False;

  if not ready then
  begin
    ShowMessage('Player not initialized');
    exit;
  end;

  if not FileExists(filePath) then
  begin
    ShowMessage('File does not exist:'+#13+filePath);
    exit;
  end;

  if InputIndex1=0 then
  begin
    //while loopProcRunning or endProcRunning do sleep(1);
    uos_LoopProcIn(PlayerIndex1, InputIndex1, nil);
    uos_EndProc(PlayerIndex1, nil);
  end;

  samformat:=0;
  PlayerIndex1 := 0;
  InputIndex1:=-1;
  if uos_CreatePlayer(PlayerIndex1) then
    InputIndex1 := uos_AddFromFile(PlayerIndex1, pchar(filePath), -1, samformat, -1);

  if InputIndex1 = -1 then
  begin
    ShowMessage('Loading file failed');
    exit;
  end;

  {$if defined(cpuarm)} // needs lower latency
  OutputIndex1 := uos_AddIntoDevOut(PlayerIndex1, -1, 0.3, uos_InputGetSampleRate(PlayerIndex1, InputIndex1),
   uos_InputGetChannels(PlayerIndex1, InputIndex1), samformat, -1, -1);
     {$else}
   OutputIndex1 := uos_AddIntoDevOut(PlayerIndex1, -1, -1, uos_InputGetSampleRate(PlayerIndex1, InputIndex1),
   uos_InputGetChannels(PlayerIndex1, InputIndex1), samformat, -1, -1);
    {$endif}

  uos_InputSetLevelEnable(PlayerIndex1, InputIndex1, 2) ;
  uos_InputSetPositionEnable(PlayerIndex1, InputIndex1, 1) ;

  uos_LoopProcIn(PlayerIndex1, InputIndex1, @PlayingLoopProc);

  uos_InputAddDSPVolume(PlayerIndex1, InputIndex1, 1, 1);
  uos_InputSetDSPVolume(PlayerIndex1, InputIndex1, initVolumeLeft, initVolumeRight, True); /// Set volume

  PluginIndex2 := uos_AddPlugin(PlayerIndex1, 'soundtouch', uos_InputGetSampleRate(PlayerIndex1, InputIndex1), -1);

  fTrackLength := uos_InputLength(PlayerIndex1, InputIndex1);
  TriggerOnChangeTrackLengthEvent(SamplesToMilliseconds(fTrackLength));

  uos_EndProc(PlayerIndex1, @PlayingEndProc);
  TriggerOnChangePosEvent(0);

  uos_SetPluginSoundTouch(PlayerIndex1, PluginIndex2, initTempo, 1, initTempo<>1);

  result:=True;
end;

procedure TPlayer.Seek(trackPosMs:integer);    
begin
  if not ready then exit;
  uos_InputSeek(PlayerIndex1, InputIndex1, MillisecondsToSamples(trackPosMs));
end;

procedure TPlayer.Play;
begin
  if not ready then exit;
  if fStopped then
    uos_Play(PlayerIndex1)
  else
    uos_rePlay(PlayerIndex1);

  fStopped:=False;
end;

procedure TPlayer.Pause;
begin
  if not ready then exit;
  uos_Pause(PlayerIndex1);
end;

procedure TPlayer.Stop;
begin
  if not ready then exit;
  uos_Stop(PlayerIndex1);
  uos_LoopProcIn(PlayerIndex1, InputIndex1, nil);
  uos_EndProc(PlayerIndex1, nil);
  fStopped:=True;
end;

function TPlayer.GetTrackPosMs:integer;
begin
  if ready then result:=SamplesToMilliseconds(uos_InputPosition(PlayerIndex1, InputIndex1))
           else result:=0;
end;

procedure TPlayer.SetVolume(newVolLeft,newVolRight:single);
begin
  if not ready then exit;
  uos_InputSetDSPVolume(PlayerIndex1, InputIndex1, newVolLeft, newVolRight, True);
end;

procedure TPlayer.SetTempo(newTempo:single);
begin
  if not ready then exit;
  uos_SetPluginSoundTouch(PlayerIndex1, PluginIndex2, newTempo, 1, newTempo<>1);
end;

procedure TPlayer.PlayingLoopProc;
begin
  //if loopProcRunning then exit;
  //loopProcRunning:=True;
  TriggerOnChangePosEvent(SamplesToMilliseconds(uos_InputPosition(PlayerIndex1, InputIndex1)));
  //loopProcRunning:=False;
end;

procedure TPlayer.PlayingEndProc;
begin
  //if endProcRunning then exit;
  //endProcRunning:=True;

  uos_LoopProcIn(PlayerIndex1, InputIndex1, nil);
  uos_EndProc(PlayerIndex1, nil);

  fStopped:=True;
  TriggerOnTrackFinishEvent;

  //endProcRunning:=False;
end;

procedure TPlayer.TriggerOnChangeTrackLengthEvent(newTrackLengthMs:integer);
begin
  if Assigned(OnChangeTrackLength) then OnChangeTrackLength(newTrackLengthMs);
end;

procedure TPlayer.TriggerOnChangePosEvent(newPosMs:integer);
begin
  if Assigned(OnChangePos) then OnChangePos(newPosMs);
end;

procedure TPlayer.TriggerOnTrackFinishEvent;
begin
  if Assigned(OnTrackFinish) then OnTrackFinish;
end;

// *****************************************************************************
// ********************************************************************* PRIVATE
// *****************************************************************************

function TPlayer.SamplesToMilliseconds(samplesPos:integer):integer;
begin
  result:=Round(samplesPos/uos_InputGetSampleRate(PlayerIndex1,InputIndex1)*1000);
end;

function TPlayer.MillisecondsToSamples(millisecondsPos:integer):integer;
begin
  result:=Round(millisecondsPos/1000*uos_InputGetSampleRate(PlayerIndex1,InputIndex1));
end;


end.

