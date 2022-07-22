{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit USimpleComponent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Dialogs,
  Graphics; // TBitmap

type
  TMouseClickEvent = procedure (sender:TObject; x,y:integer) of object;
  TDragBeginEvent  = procedure (sender:TObject; x,y:integer) of object;
  TDragUpdateEvent = procedure (sender:TObject; x,y:integer) of object;
  TDragEndEvent    = procedure of object;
  TRenderEvent     = procedure of object;

  TDragInfo=record
    draggable:boolean;
    dragging:boolean;
    dragBeginPos:TPoint;
    dragUpdatePos:TPoint;
    prevDragUpdatePos:TPoint;
  end;

  TSimpleComponent=class
    private
      fLeft:integer;
      fTop:integer;
      fVisible:boolean;
      MouseClickEvent:TMouseClickEvent;
      DragBeginEvent:TDragBeginEvent;
      DragUpdateEvent:TDragUpdateEvent;
      DragEndEvent:TDragEndEvent;
    public
      constructor Create;
      destructor Destroy; override;
      procedure SetSize(w,h:integer);
      procedure SetPos(newLeft,newTop:integer);
      procedure SetDraggable(draggable:boolean);
      procedure SetVisibility(newVisibility:boolean);
      procedure Repaint(can:TCanvas);
      function GetBitmapCanvas:TCanvas;
      function GetComponentId(pComponent:Pointer):integer;
      procedure RegisterComponent(pComponent:Pointer);
      procedure UnregisterComponent(pComponent:Pointer);
      procedure MouseClick(x,y:integer);
      function DragBegin(x,y:integer):boolean; // "true" if handled event
      procedure DragUpdate(x,y:integer);
      procedure DragEnd;
      property OnMouseClick:TMouseClickEvent read MouseClickEvent write MouseClickEvent;
      property OnDragBegin:TDragBeginEvent read DragBeginEvent write DragBeginEvent;
      property OnDragUpdate:TDragUpdateEvent read DragUpdateEvent write DragUpdateEvent;
      property OnDragEnd:TDragEndEvent read DragEndEvent write DragEndEvent;
      procedure TriggerMouseClickEvent(sender:TObject; x,y:integer);
      procedure TriggerDragBeginEvent(sender:TObject; x,y:integer);
      procedure TriggerDragUpdateEvent(sender:TObject; x,y:integer);
      procedure TriggerDragEndEvent;
    protected
      dragInfo:TDragInfo;
      fWidth:integer;
      fHeight:integer;
      imgBuf:TBitmap;
      components:array of TSimpleComponent;
      procedure RenderImgBuf; virtual;
    published
      property width:integer read fWidth;
      property height:integer read fHeight;
      property left:integer read fLeft;
      property top:integer read fTop;
      property visible:boolean read fVisible;
  end;
  PTSimpleComponent=^TSimpleComponent;
  TSimpleComponentsArray=array of TSimpleComponent;

implementation

// *****************************************************************************
// ********************************************************************** PUBLIC
// *****************************************************************************

constructor TSimpleComponent.Create;
begin
  fLeft:=0;
  fTop:=0;
  fWidth:=0;
  fHeight:=0;
  fVisible:=True;
  dragInfo.draggable:=False;
  dragInfo.dragging:=False;
  dragInfo.dragBeginPos:=Point(0,0);
  dragInfo.dragUpdatePos:=Point(0,0);
  dragInfo.prevDragUpdatePos:=Point(0,0);

  imgBuf:=TBitmap.Create;
  imgBuf.pixelFormat:=pf24bit;
  imgBuf.Width:=fWidth;
  imgBuf.Height:=fHeight;
end;

destructor TSimpleComponent.Destroy;
var i:integer;
begin
  for i:=0 to High(components) do
    components[i].Destroy;

  imgBuf.Destroy;

  inherited;
end;

procedure TSimpleComponent.SetSize(w,h:integer);
begin
  fWidth:=w;
  fHeight:=h;
  imgBuf.Width:=w;
  imgBuf.Height:=h;

  RenderImgBuf;
end;

procedure TSimpleComponent.SetPos(newLeft,newTop:integer);
begin
  fLeft:=newLeft;
  fTop:=newTop;
  RenderImgBuf;
end;

procedure TSimpleComponent.SetDraggable(draggable:boolean);
begin
  dragInfo.draggable:=draggable;
end;

procedure TSimpleComponent.SetVisibility(newVisibility:boolean);
begin
  fVisible:=newVisibility;
  RenderImgBuf;
end;

procedure TSimpleComponent.Repaint(can:TCanvas);
var i:integer;
begin
  can.Draw(fLeft,fTop,imgBuf);

  for i:=0 to High(components) do
    if components[i].visible then components[i].Repaint(can);
end;

function TSimpleComponent.GetBitmapCanvas:TCanvas;
begin
  result:=imgBuf.Canvas;
end;

function TSimpleComponent.GetComponentId(pComponent:Pointer):integer;
var i:integer;
    typeptr:PTSimpleComponent;
begin
  typeptr:=pComponent;
  for i:=0 to High(components) do
    if components[i]=typeptr^ then
    begin
      result:=i;
      exit;
    end;
  result:=-1;
end;

procedure TSimpleComponent.RegisterComponent(pComponent:Pointer);
var i:integer;
    typeptr:PTSimpleComponent;
begin
  i:=Length(components);
  SetLength(components,i+1);
  typeptr:=pComponent;
  components[i]:=typeptr^;
end;

procedure TSimpleComponent.UnregisterComponent(pComponent:Pointer);
var componentId:integer;
    p1,p2:Pointer;
begin
  componentId:=GetComponentId(pComponent);
  if componentId=-1 then
  begin
    ShowMessage('Component not found');
    exit;
  end;

  if componentId<High(components) then
  begin
    p1:=@components[componentId+1];
    p2:=@components[componentId];
    Move(p1,p2, (High(components)-componentId)*SizeOf(components[componentId]));
  end;

  SetLength(components,Length(components)-1);
end;

procedure TSimpleComponent.MouseClick(x,y:integer);
var i:integer;
begin
  if (x>=fLeft) and (x<=fLeft+fWidth)
    and (y>=fTop) and (y<=fTop+fHeight) then
  begin
    for i:=0 to High(components) do
      components[i].MouseClick(x,y);

    TriggerMouseClickEvent(self,x,y);
  end;
end;

function TSimpleComponent.DragBegin(x,y:integer):boolean;
var i:integer;
begin
  result:=False;
  if (x>=fLeft) and (x<=fLeft+fWidth)
    and (y>=fTop) and (y<=fTop+fHeight) then
  begin
  for i:=0 to High(components) do
    if components[i].DragBegin(x,y) then
    begin
      result:=True;
      exit;
    end;

    if not dragInfo.draggable then exit;
    dragInfo.dragging:=True;
    dragInfo.dragBeginPos:=Point(x,y);
    dragInfo.dragUpdatePos:=dragInfo.dragBeginPos;
    dragInfo.prevDragUpdatePos:=dragInfo.dragBeginPos;
    TriggerDragBeginEvent(self,x,y);
    result:=True;
  end;
end;

procedure TSimpleComponent.DragUpdate(x,y:integer);
var i:integer;
begin
  if //dragInfo.dragging
    {and} (x>=fLeft) and (x<=fLeft+fWidth)
    and (y>=fTop) and (y<=fTop+fHeight) then
  begin
    for i:=0 to High(components) do
      components[i].DragUpdate(x,y);

    //if not dragInfo.draggable then exit;
    if not dragInfo.dragging then exit;
    dragInfo.prevDragUpdatePos:=dragInfo.dragUpdatePos;
    dragInfo.dragUpdatePos:=Point(x,y);
    TriggerDragUpdateEvent(self,x,y);
  end;
end;

procedure TSimpleComponent.DragEnd;
var i:integer;
begin
  //if dragInfo.dragging then
  begin
    for i:=0 to High(components) do
      components[i].DragEnd;

    //if not dragInfo.draggable then exit;
    if not dragInfo.dragging then exit;
    dragInfo.dragging:=False;
    TriggerDragEndEvent;
  end;
end;

procedure TSimpleComponent.TriggerMouseClickEvent(sender:TObject; x,y:integer);
begin
  if Assigned(OnMouseClick) then OnMouseClick(sender,x,y);
end;

procedure TSimpleComponent.TriggerDragBeginEvent(sender:TObject; x,y:integer);
begin
  if Assigned(OnDragBegin) then OnDragBegin(sender,x,y);
end;

procedure TSimpleComponent.TriggerDragUpdateEvent(sender:TObject; x,y:integer);
begin
  if Assigned(OnDragUpdate) then OnDragUpdate(sender,x,y);
end;

procedure TSimpleComponent.TriggerDragEndEvent;
begin
  if Assigned(OnDragEnd) then OnDragEnd;
end;

// *****************************************************************************
// ******************************************************************* PROTECTED
// *****************************************************************************

procedure TSimpleComponent.RenderImgBuf;
begin
  // virtual method overwritten by descendants
end;

end.

