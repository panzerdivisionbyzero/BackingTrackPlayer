{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit USettingsScreen;

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
  USimpleListbox,
  UPlayer;

type
  TChangeVolumeEvent=procedure (newVolLeft,newVolRight:single) of object;
  TChangeTempoEvent=procedure (newTempo:single) of object;
  TSettingsScreen=class(TSimpleComponent)
    private
      faceColor:integer;
      playing:boolean;

      btnExit:TSimpleButton;
      labTitle:TsimpleLabel;
      labVolume:TSimpleLabel;
      labVolLeft:TSimpleLabel;
      labVolRight:TSimpleLabel;
      labTempo:TSimpleLabel;
      sbVolLeft:TSimpleScrollbar;
      sbVolRight:TSimpleScrollbar;
      sbTempo:TSimpleScrollbar;
      holdScrollbarsOnChangeEvent:boolean;

      SwitchToScreenEvent:TSwitchToScreenEvent;
      ChangeVolumeEvent:TChangeVolumeEvent;
      ChangeTempoEvent:TChangeTempoEvent;

      procedure RenderImgBuf; override;
      procedure PrepareComponents;
      procedure SBVolLeftChangeValueEvent(value:integer);
      procedure SBVolRightChangeValueEvent(value:integer);
      procedure SBTempoChangeValueEvent(value:integer);
      procedure BtnExitClick(sender:TObject; x,y:integer);
    public
      parentScreenId:integer;
      screenProcessId:integer;
      constructor Create;
      destructor Destroy; override;
      procedure SetVolume(newVolLeft,newVolRight:single);
      procedure SetTempo(newTempo:single);
      property OnSwitchToScreen:TSwitchToScreenEvent read SwitchToScreenEvent write SwitchToScreenEvent;
      property OnChangeVolume:TChangeVolumeEvent read ChangeVolumeEvent write ChangeVolumeEvent;
      property OnChangeTempo:TChangeTempoEvent read ChangeTempoEvent write ChangeTempoEvent;
      procedure TriggerOnSwitchToScreenEvent(screenId:integer; newScreenProcessId:integer=spNone);
      procedure TriggerOnChangeVolumeEvent(newVolLeft,newVolRight:single);
      procedure TriggerOnChangeTempoEvent(newTempo:single);

  end;

implementation

// *****************************************************************************
// ********************************************************************** PUBLIC
// *****************************************************************************

constructor TSettingsScreen.Create;
begin
  inherited;

  faceColor:=clBlack;
  parentScreenId:=sdDirExplorer;
  screenProcessId:=spNone;
  holdScrollbarsOnChangeEvent:=False;

  PrepareComponents;
end;

destructor TSettingsScreen.Destroy;
begin
  //

  inherited;
end;

procedure TSettingsScreen.SetVolume(newVolLeft,newVolRight:single);
begin
  holdScrollbarsOnChangeEvent:=True;
  sbVolLeft.SetValue(Round(newVolLeft*100));
  sbVolRight.SetValue(Round(newVolRight*100));
  holdScrollbarsOnChangeEvent:=False;
end;

procedure TSettingsScreen.SetTempo(newTempo:single);
begin
  holdScrollbarsOnChangeEvent:=True;
  sbTempo.SetValue(Round(newTempo*100));
  holdScrollbarsOnChangeEvent:=False;
end;

procedure TSettingsScreen.TriggerOnSwitchToScreenEvent(screenId:integer; newScreenProcessId:integer=spNone);
begin
  if Assigned(OnSwitchToScreen) then OnSwitchToScreen(screenId, newScreenProcessId);
end;

procedure TSettingsScreen.TriggerOnChangeVolumeEvent(newVolLeft,newVolRight:single);
begin
  if Assigned(OnChangeVolume) then OnChangeVolume(newVolLeft,newVolRight);
end;

procedure TSettingsScreen.TriggerOnChangeTempoEvent(newTempo:single);
begin
  if Assigned(OnChangeTempo) then OnChangeTempo(newTempo);
end;

// *****************************************************************************
// ********************************************************************* PRIVATE
// *****************************************************************************

procedure TSettingsScreen.RenderImgBuf;
begin
  if (fWidth=0) or (fHeight=0) then exit;

  imgBuf.canvas.Brush.Color:=faceColor;
  imgBuf.canvas.FillRect(0,0,fWidth,fHeight);
end;

procedure TSettingsScreen.PrepareComponents;
var lineTop,lineBottom,linesDistance,screenWidth,screenHeight:integer;
begin
  screenWidth:=1024;
  screenHeight:=600;
  linesDistance:=Round(screenHeight*0.618);
  lineTop:=Round((screenHeight-linesDistance)/2);
  lineBottom:=lineTop+linesDistance;

  btnExit:=TSimpleButton.Create;
  btnExit.SetCaption('X');
  btnExit.SetSize(96,96);
  btnExit.SetPos(1024-96-16,16);
  btnExit.OnMouseClick:=@BtnExitClick;
  RegisterComponent(@btnExit);

  labTitle:=TSimpleLabel.Create;
  labTitle.SetTextAlign(taCenter);
  labTitle.SetCaption('Settings');
  labTitle.SetSize(768,96);
  labTitle.SetPos(128,16);
  RegisterComponent(@labTitle);

  labVolume:=TSimpleLabel.Create;
  labVolume.SetTextAlign(taCenter);
  labVolume.SetCaption('Volume');
  labVolume.SetSize(128,64);
  labVolume.SetPos(Round(screenWidth*3/10)-64,lineBottom+32);
  RegisterComponent(@labVolume);

  labVolLeft:=TSimpleLabel.Create;
  labVolLeft.SetTextAlign(taCenter);
  labVolLeft.SetCaption('L');
  labVolLeft.SetSize(96,64);
  labVolLeft.SetPos(Round(screenWidth*1/5)-48,lineBottom+32);
  RegisterComponent(@labVolLeft);

  labVolRight:=TSimpleLabel.Create;
  labVolRight.SetTextAlign(taCenter);
  labVolRight.SetCaption('R');
  labVolRight.SetSize(96,64);
  labVolRight.SetPos(Round(screenWidth*2/5)-48,lineBottom+32);
  RegisterComponent(@labVolRight);

  labTempo:=TSimpleLabel.Create;
  labTempo.SetTextAlign(taCenter);
  labTempo.SetCaption('Tempo');
  labTempo.SetSize(128,64);
  labTempo.SetPos(Round(screenWidth*15/20)-64,lineBottom+32);
  RegisterComponent(@labTempo);

  sbVolLeft:=TSimpleScrollbar.Create;
  sbVolLeft.SetSize(96,linesDistance);
  sbVolLeft.SetPos(Round(screenWidth*1/5)-48,lineTop+16);
  sbVolLeft.SetOrientation(soVertical);
  sbVolLeft.SetRange(0,100);
  sbVolLeft.OnValueChange:=@SBVolLeftChangeValueEvent;
  RegisterComponent(@sbVolLeft);

  sbVolRight:=TSimpleScrollbar.Create;
  sbVolRight.SetSize(96,linesDistance);
  sbVolRight.SetPos(Round(screenWidth*2/5)-48,lineTop+16);
  sbVolRight.SetOrientation(soVertical);
  sbVolRight.SetRange(0,100);
  sbVolRight.OnValueChange:=@SBVolRightChangeValueEvent;
  RegisterComponent(@sbVolRight);

  sbTempo:=TSimpleScrollbar.Create;
  sbTempo.SetSize(96,linesDistance);
  sbTempo.SetPos(Round(screenWidth*15/20)-48,lineTop+16);
  sbTempo.SetOrientation(soVertical);
  sbTempo.SetRange(1,200);
  sbTempo.OnValueChange:=@SBTempoChangeValueEvent;
  RegisterComponent(@sbTempo);
end;

procedure TSettingsScreen.SBVolLeftChangeValueEvent(value:integer);
begin
  if holdScrollbarsOnChangeEvent then exit;
  TriggerOnChangeVolumeEvent(value*0.01,sbVolRight.value*0.01);
end;

procedure TSettingsScreen.SBVolRightChangeValueEvent(value:integer);
begin
  if holdScrollbarsOnChangeEvent then exit;
  TriggerOnChangeVolumeEvent(sbVolLeft.value*0.01,value*0.01);
end;

procedure TSettingsScreen.SBTempoChangeValueEvent(value:integer);
begin
  if holdScrollbarsOnChangeEvent then exit;
  TriggerOnChangeTempoEvent(value*0.01);
end;

procedure TSettingsScreen.BtnExitClick(sender:TObject; x,y:integer);
begin
  if parentScreenId=-1 then TerminateApp
                       else TriggerOnSwitchToScreenEvent(parentScreenId);
end;

end.

