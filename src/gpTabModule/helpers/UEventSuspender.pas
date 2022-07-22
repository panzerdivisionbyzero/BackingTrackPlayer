{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit UEventSuspender;

interface

uses
  Types, // TPoint, TRect
  Dialogs,
  ExtCtrls, // TTimer
  SysUtils; // Now()

const
  millisecond = 1/24/3600/1000;

type
  TTimerActionEvent = procedure of object;

  TEventSuspender=class
    private
      processingEventInProgress:boolean; // e.g. renderer is busy
      fLastEventProcessingEndTime:TDateTime;
      fLastEventProcessingStartTime:TDateTime;
      fLastEventProcessingCancelled:boolean;
      fLastEventProcessingMilliseconds:integer;
      timer:TTimer;

      TimerActionEvent:TTimerActionEvent;
      procedure TimerTick(Sender:TObject);
    public
      triggerDelay:integer;
      constructor Create;
      destructor Destroy; override;
      function EventProcessingInitialization:boolean; // "false" means: processing of the "event" is in progress, so the new request cannot be processed;
      procedure EventProcessingFinalization;
      procedure CancelSuspendedEvent;
      property OnTimerAction:TTimerActionEvent read TimerActionEvent write TimerActionEvent;
      procedure TriggerOnTimerActionEvent;
      property lastEventProcessingMilliseconds:integer read fLastEventProcessingMilliseconds;

  end;

implementation

// *****************************************************************************
// ********************************************************************** PUBLIC 
// *****************************************************************************

constructor TEventSuspender.Create;
begin
  processingEventInProgress:=False;
  fLastEventProcessingEndTime:=Now;

  timer:=TTimer.Create(nil);
  timer.OnTimer:=@TimerTick;
  timer.Interval:=10;
  timer.Enabled:=True;

  triggerDelay:=10;
end;

destructor TEventSuspender.Destroy;
begin
  timer.Enabled:=False;
  timer.Destroy;

  inherited;
end;

function TEventSuspender.EventProcessingInitialization:boolean; // "false" means: processing of the "event" is in progress, so the new request cannot be processed;
begin            
  result:=False;

  if processingEventInProgress or (Now < fLastEventProcessingEndTime+millisecond*triggerDelay) then
  begin
    fLastEventProcessingCancelled:=True;
    exit;
  end;

  processingEventInProgress:=True;
  fLastEventProcessingCancelled:=False;
  fLastEventProcessingStartTime:=Now;
  result:=True;
end;

procedure TEventSuspender.EventProcessingFinalization;
begin
  fLastEventProcessingEndTime:=Now;
  fLastEventProcessingMilliseconds:=Trunc((fLastEventProcessingEndTime-fLastEventProcessingStartTime)/millisecond);
  processingEventInProgress:=False;
end;

procedure TEventSuspender.CancelSuspendedEvent;
begin
  fLastEventProcessingCancelled:=False;
  processingEventInProgress:=False;
end;

procedure TEventSuspender.TriggerOnTimerActionEvent;
begin
  if Assigned(OnTimerAction) then OnTimerAction;
end;

// *****************************************************************************
// ********************************************************************* PRIVATE
// *****************************************************************************

procedure TEventSuspender.TimerTick(Sender:TObject);
begin
  if fLastEventProcessingCancelled and not processingEventInProgress then
  begin
    fLastEventProcessingCancelled:=False;
    TriggerOnTimerActionEvent;
  end;
end;

end.
