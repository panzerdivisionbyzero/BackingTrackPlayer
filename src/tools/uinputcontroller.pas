{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit UInputController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Dialogs;

// odroznienie tapa od draga:
// identifying difference between "tap" and "drag":
// 1. each MouseUp following after MouseDown in time < "T", is "tap"
// 2. each MouseUpdate with distance from MouseDown >= "X" pixels, is "drag"

// protection against accidential interruption of dragging:
// if MouseUp is called during dragging, the next MouseDown called in "T" time,
// will be interpreted as DragUpdate, and MouseUp will be ignored;
// if, however, MouseDown will not be called in "T" time, the DragEnd is initiated;

const
  millisecond=1/24/3600/1000;
  tapMaxTime=150; // ms
  tapMaxDistance=50; // px
  dragContinueMaxTime=100; // ms
  dragContinueMaxDistance=50; // px

type
  TDragBeginEvent  = procedure (x,y:integer) of object;
  TDragUpdateEvent = procedure (x,y:integer) of object;
  TDragEndEvent    = procedure of object;
  TSingleTapEvent  = procedure (x,y:integer) of object;

  TInputController=class
    private
      mbPressed:boolean;
      dragging:boolean;
      mousePx:TPoint;
      oldMousePx:TPoint;
      dragBeginPx:TPoint;
      //mouseMoved:boolean;
      lastMouseDownTime:TDateTime;
      lastMouseDownPx:TPoint;
      lastMouseUpPx:TPoint;

      dragEndTimer:TTimer;
      mousePressTimer:TTimer;

      DragBeginEvent:TDragBeginEvent;
      DragUpdateEvent:TDragUpdateEvent;
      DragEndEvent:TDragEndEvent;
      SingleTapEvent:TSingleTapEvent;

      procedure InitDrag;
      procedure DragEndTimerTick(Sender:TObject);
      procedure MousePressTimerTick(Sender:TObject);
      procedure InitDragEndTimer;
      procedure HaltDragEndTimer;
    public
      constructor Create;
      destructor Destroy; override;
      procedure MouseMove(x,y:integer);
      procedure MouseClick(x,y:integer);
      procedure MouseDown(x,y:integer);
      procedure MouseUp(x,y:integer);
      property OnDragBegin:TDragBeginEvent read DragBeginEvent write DragBeginEvent;
      property OnDragUpdate:TDragUpdateEvent read DragUpdateEvent write DragUpdateEvent;
      property OnDragEnd:TDragEndEvent read DragEndEvent write DragEndEvent;
      property OnSingleTap:TSingleTapEvent read SingleTapEvent write SingleTapEvent;
      procedure TriggerOnDragBeginEvent(x,y:integer);
      procedure TriggerOnDragUpdateEvent(x,y:integer);
      procedure TriggerOnDragEndEvent;
      procedure TriggerOnSingleTapEvent(x,y:integer);

  end;

implementation

function Distance(p1,p2:TPoint):double;
begin
  result:=Sqrt(Sqr(p2.x-p1.x)+Sqr(p2.y-p1.y));
end;

// *****************************************************************************
// ********************************************************************** PUBLIC
// *****************************************************************************

constructor TInputController.Create;
begin
  mbPressed:=False;
  dragging:=False;
  mousePx:=Point(0,0);
  oldMousePx:=mousePx;
  dragBeginPx:=mousePx;
  //mouseMoved:=False;
  lastMouseDownTime:=Now;
  lastMouseDownPx:=Point(0,0);
  lastMouseUpPx:=Point(0,0);

  dragEndTimer:=TTimer.Create(nil);
  dragEndTimer.Enabled:=False;
  dragEndTimer.Interval:=dragContinueMaxTime;
  dragEndTimer.OnTimer:=@DragEndTimerTick;

  mousePressTimer:=TTimer.Create(nil);
  mousePressTimer.Interval:=1;
  mousePressTimer.OnTimer:=@MousePressTimerTick;
  mousePressTimer.Enabled:=True;
end;

destructor TInputController.Destroy;
begin
  mousePressTimer.Destroy;
  dragEndTimer.Destroy;

  inherited;
end;

procedure TInputController.MouseMove(x,y:integer);
begin
  oldMousePx:=mousePx;
  mousePx:=Point(x,y);

  if mbPressed and (not dragging)
    and (Distance(mousePx,lastMouseDownPx)>tapMaxDistance) then
  begin
    InitDrag;
  end;
  //
end;

procedure TInputController.MouseClick(x,y:integer);
begin
  mousePx:=Point(x,y);
  oldMousePx:=mousePx;
  TriggerOnSingleTapEvent(x,y);
end;

procedure TInputController.MouseDown(x,y:integer);
begin
  mbPressed:=True;
  mousePx:=Point(x,y);
  if dragging then
  begin
    if (Distance(mousePx,lastMouseUpPx)<=dragContinueMaxDistance) then
    begin
      HaltDragEndTimer;
      TriggerOnDragUpdateEvent(x,y);
      exit;
    end;
    dragging:=False;
    TriggerOnDragEndEvent;
  end;

  lastMouseDownTime:=Now;
  lastMouseDownPx.x:=x;
  lastMouseDownPx.y:=y;


  oldMousePx:=mousePx;
end;

procedure TInputController.MouseUp(x,y:integer);
begin
  mbPressed:=False;
  lastMouseUpPx.x:=x;
  lastMouseUpPx.y:=y;

  if dragging then
  begin
    InitDragEndTimer;
    exit;
  end;

  if ((Now-lastMouseDownTime)/millisecond<tapMaxTime) then
  begin
    TriggerOnSingleTapEvent(x,y);
    exit;
  end;

  mousePx:=Point(x,y);
  //mouseMoved:=False;
end;

procedure TInputController.TriggerOnDragBeginEvent(x,y:integer);
begin
  if Assigned(OnDragBegin) then OnDragBegin(x,y);
end;

procedure TInputController.TriggerOnDragUpdateEvent(x,y:integer);
begin
  if Assigned(OnDragUpdate) then OnDragUpdate(x,y);
end;

procedure TInputController.TriggerOnDragEndEvent;
begin
  if Assigned(OnDragEnd) then OnDragEnd;
end;

procedure TInputController.TriggerOnSingleTapEvent(x,y:integer);
begin
  if Assigned(OnSingleTap) then OnSingleTap(x,y);
end;

// *****************************************************************************
// ********************************************************************* PRIVATE
// *****************************************************************************

procedure TInputController.InitDrag;
begin
  dragging:=True;
  TriggerOnDragBeginEvent(lastMouseDownPx.x,lastMouseDownPx.y);
  TriggerOnDragUpdateEvent(mousePx.x,mousePx.y);
end;

procedure TInputController.DragEndTimerTick(Sender:TObject);
begin
  dragEndTimer.Enabled:=False;
  dragging:=False;
  TriggerOnDragEndEvent;
end;

procedure TInputController.MousePressTimerTick(Sender:TObject);
begin
  if (mbPressed or dragging)
    and ((Now-lastMouseDownTime)/millisecond>tapMaxTime) then
  begin
    if dragging then TriggerOnDragUpdateEvent(mousePx.x,mousePx.y)
                else InitDrag;
  end;
end;

procedure TInputController.InitDragEndTimer;
begin
  dragEndTimer.Enabled:=True;
end;

procedure TInputController.HaltDragEndTimer;
begin
  dragEndTimer.Enabled:=False;
end;


end.

