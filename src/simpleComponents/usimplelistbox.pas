{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit USimpleListBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Math, Dialogs,
  UUtilities,
  USimpleComponent,
  USimpleLabel;

const
  itemLeftMargin=8;

type
  TItemClickEvent=procedure (itemId:integer) of object;

  TSimpleListboxItem=record
    simpleLabel:TSimpleLabel;
    itemTop:integer;
    itemBottom:integer;
    selected:boolean;
  end;

  TSimpleListBox=class(TSimpleComponent)
    private
      fFaceColor:integer;
      fBorderColor:integer;
      fBordersVisible:boolean;
      fItemsWidth:integer;
      fItemHeight:integer;
      fScrollPos:integer;
      fScrollMaxPos:integer;
      fEditMode:boolean;

      items:array of TSimpleListboxItem;

      ItemClickEvent:TItemClickEvent;
      RepaintRequestEvent:TRepaintRequestEvent;

      procedure RenderImgBuf; override;
      procedure RefreshMaxScrollPos;
      //procedure RefreshItemsSize;
      procedure RefreshItemsPos;
      procedure RefreshItemPos(itemId:integer);
      function FindItemAtPos(localY:integer):integer;
      procedure SwapItems(item1Id,item2Id:integer);
      procedure ListboxClick(sender:TObject; x,y:integer);
      procedure ListboxDragBegin(sender:TObject; x,y:integer);
      procedure ListboxDragUpdate(sender:TObject; x,y:integer);
      procedure ListboxDragEnd;
    public
      constructor Create;
      destructor Destroy; override;
      procedure SetItemsHeight(newItemsHeight:integer);
      procedure SetBordersVisibility(newBordersVisibility:boolean);
      procedure ClearItems;
      procedure UnselectItems;
      procedure SetEditMode(newEditModeState:boolean);
      procedure SelectItem(itemId:integer);
      function GetSelectedItemId:integer;
      function AddItem(itemCaption:string):integer;
      function GetItemCaption(itemId:integer):string;
      //procedure MoveItemUp(itemId:integer);
      //procedure MoveItemDown(itemId:integer);
      //procedure RemoveItem(itemId:integer);
      procedure SetScrollPos(newScrollPos:integer);
      property OnItemClick:TItemClickEvent read ItemClickEvent write ItemClickEvent;
      property OnRepaintRequest:TRepaintRequestEvent read RepaintRequestEvent write RepaintRequestEvent;
      procedure TriggerOnItemClickEvent(itemId:integer);
      procedure TriggerOnRepaintRequestEvent;
    published
      property editMode:boolean read fEditMode;
  end;

implementation

// *****************************************************************************
// ********************************************************************** PUBLIC
// *****************************************************************************

constructor TSimpleListBox.Create;
begin
  inherited;

  fFaceColor:=$000000;
  fBorderColor:=$CCCCCC;
  fBordersVisible:=True;
  fItemHeight:=96;
  SetDraggable(True);
  fScrollPos:=0;
  fScrollMaxPos:=0;
  fEditMode:=False;

  self.OnMouseClick:=@ListboxClick;
  self.OnDragBegin:=@ListboxDragBegin;
  self.OnDragUpdate:=@ListboxDragUpdate;
  self.OnDragEnd:=@ListboxDragEnd;
end;

destructor TSimpleListBox.Destroy;
begin
  //

  inherited;
end;

procedure TSimpleListBox.SetItemsHeight(newItemsHeight:integer);
begin
  fItemHeight:=newItemsHeight;
  RefreshItemsPos;
  RenderImgBuf;
end;

procedure TSimpleListBox.SetBordersVisibility(newBordersVisibility:boolean);
begin
  fBordersVisible:=newBordersVisibility;
  RenderImgBuf;
end;

procedure TSimpleListBox.ClearItems;
var i:integer;
begin
  fScrollPos:=0;
  fScrollMaxPos:=fHeight;
  for i:=High(items) downto 0 do
  begin
    UnregisterComponent(@items[i].simpleLabel);
    items[i].simpleLabel.Destroy;
  end;
  SetLength(items,0);
  RenderImgBuf;
end;

procedure TSimpleListBox.UnselectItems;
var i:integer;
begin
  for i:=0 to High(items) do
    items[i].selected:=False;
end;

procedure TSimpleListBox.SetEditMode(newEditModeState:boolean);
begin
  fEditMode:=newEditModeState;
  if not fEditMode then
  begin
    UnselectItems;
    RenderImgBuf;
    TriggerOnRepaintRequestEvent;
  end;
end;

procedure TSimpleListBox.SelectItem(itemId:integer);
begin
  if (itemId<0) or (itemId>=Length(items)) then exit;
  items[itemId].selected:=True;
  RenderImgBuf;
  TriggerOnRepaintRequestEvent;
end;

function TSimpleListBox.GetSelectedItemId:integer;
var i:integer;
begin
  for i:=0 to High(items) do
    if items[i].selected then
    begin
      result:=i;
      exit;
    end;
  result:=-1;
end;

function TSimpleListBox.AddItem(itemCaption:string):integer;
begin
  result:=Length(items);
  SetLength(items,result+1);
  items[result].simpleLabel:=TSimpleLabel.Create;
  items[result].simpleLabel.SetCaption(itemCaption);
  items[result].simpleLabel.SetSize(fWidth-itemLeftMargin,fItemHeight);
  items[result].simpleLabel.SetTransparency(True);
  items[result].selected:=False;
  RefreshItemPos(result);
  RegisterComponent(@items[result]);

  RefreshMaxScrollPos;

  RenderImgBuf;
end;

function TSimpleListBox.GetItemCaption(itemId:integer):string;
begin
  if itemId=-1 then exit;
  result:=items[itemId].simpleLabel.caption;
end;

//procedure TSimpleListBox.MoveItemUp(itemId:integer);
//begin
//  SwapItems(itemId-1,itemId);
//  Render;
//  TriggerOnRepaintRequestEvent;
//end;
//
//procedure TSimpleListBox.MoveItemDown(itemId:integer);
//begin
//  SwapItems(itemId,itemId+1);
//  Render;
//  TriggerOnRepaintRequestEvent;
//end;
//
//procedure TSimpleListBox.RemoveItem(itemId:integer);
//var p1,p2:Pointer;
//begin
//  if (itemId<0) or (itemId>=Length(items)) then exit;
//
//  UnregisterComponent(items[itemId].simpleLabel);
//  FreeAndNil(items[itemId].simpleLabel);
//  if itemId<High(items) then
//  begin
//    p1:=@items[itemId+1];
//    p2:=@items[itemId];
//    Move(p1,p2,SizeOf(items[itemId])*(Length(items)-itemId-1));
//  end;
//  SetLength(items,High(items));
//
//  Render;
//  TriggerOnRepaintRequestEvent;
//end;

procedure TSimpleListBox.SetScrollPos(newScrollPos:integer);
begin
  fScrollPos:=newScrollPos;
  if fScrollPos<0 then fScrollPos:=0
  else if fScrollPos>fScrollMaxPos then fScrollPos:=fScrollMaxPos;
  RefreshItemsPos;
  RenderImgBuf;
end;

procedure TSimpleListBox.TriggerOnItemClickEvent(itemId:integer);
begin
  if Assigned(OnItemClick) then OnItemClick(itemId);
end;

procedure TSimpleListBox.TriggerOnRepaintRequestEvent;
begin
  if Assigned(OnRepaintRequest) then OnRepaintRequest;
end;

// *****************************************************************************
// ********************************************************************* PRIVATE
// *****************************************************************************

procedure TSimpleListBox.RenderImgBuf;
var i,itemTop,itemBottom:integer;
begin
  if (not visible) or (fWidth=0) or (fHeight=0) then exit;

  if fBordersVisible then
    imgBuf.Canvas.Pen.Style:=psSolid
  else
    imgBuf.Canvas.Pen.Style:=psClear;

  imgBuf.Canvas.Pen.Width:=2;
  imgBuf.Canvas.Pen.Color:=fBorderColor;

  imgBuf.Canvas.Brush.Color:=fFaceColor;
  imgBuf.Canvas.Rectangle(1,1,fWidth,fHeight);

  imgBuf.Canvas.Pen.Width:=1;
  for i:=0 to High(items) do
  begin
    if (items[i].itemTop>=fHeight)
      or (items[i].itemBottom<=0) then continue;

    if items[i].selected then
    begin
      imgBuf.Canvas.Pen.Color:=clYellow;
      imgBuf.Canvas.Pen.Width:=4;
    end;

    imgBuf.Canvas.Rectangle(1,Max(1,items[i].itemTop),fWidth-1,
                                Min(height-1,items[i].itemBottom));
    if items[i].selected then
    begin
      imgBuf.Canvas.Pen.Color:=fBorderColor;
      imgBuf.Canvas.Pen.Width:=1;
    end
  end;
end;

procedure TSimpleListBox.RefreshMaxScrollPos;
begin
  fScrollMaxPos:=Max(0,Length(items)*fItemHeight-fHeight);
  SetScrollPos(fScrollPos);
end;

//procedure TSimpleListBox.RefreshItemsSize;
//var i:integer;
//begin
//  //for i:=0 to High(items) do
//  //  begin
//  //    items[i].simpleLabel.SetSize();
//  //  end;
//end;

procedure TSimpleListBox.RefreshItemsPos;
var i:integer;
begin
  for i:=0 to High(items) do
    RefreshItemPos(i);
end;

procedure TSimpleListBox.RefreshItemPos(itemId:integer);
var labelVisibility:boolean;
begin
  items[itemId].itemTop:=itemId*fItemHeight-fScrollPos;
  items[itemId].itemBottom:=(itemId+1)*fItemHeight-fScrollPos;
  items[itemId].simpleLabel.SetPos(left+itemLeftMargin,top+items[itemId].itemTop);

  labelVisibility:=(items[itemId].itemBottom>0+(fItemHeight+items[itemId].simpleLabel.textHeight)*0.5)
                   and (items[itemId].itemTop<height-(fItemHeight+items[itemId].simpleLabel.textHeight)*0.5);
  if items[itemId].simpleLabel.visible<>labelVisibility then
    items[itemId].simpleLabel.SetVisibility(labelVisibility);
end;

function TSimpleListBox.FindItemAtPos(localY:integer):integer;
begin
  if localY>=0 then
    result:=Trunc((localY+fScrollPos)/fItemHeight)
  else
    result:=-1;

  if result>High(items) then result:=-1;
end;

procedure TSimpleListBox.SwapItems(item1Id,item2Id:integer);
var tempItem:TSimpleListboxItem;
begin
  if (item1Id<0) or (item2Id<0)
    or (item1Id>=Length(items)) or (item2Id>=Length(items)) then exit;
  ShowMessage(items[item1Id].simpleLabel.caption);
  tempItem:=items[item1Id];
  items[item1Id]:=items[item2Id];
  items[item2Id]:=tempItem;
  ShowMessage(items[item1Id].simpleLabel.caption);
end;

procedure TSimpleListBox.ListboxClick(sender:TObject; x,y:integer);
var itemId:integer;
    itemWasSelected:boolean;
begin
  itemId:=FindItemAtPos(y-top);
  if itemId=-1 then exit;

  if fEditMode then
  begin
    itemWasSelected:=items[itemId].selected;
    UnselectItems;
    items[itemId].selected:=not itemWasSelected;
    RenderImgBuf;
    TriggerOnRepaintRequestEvent;
  end
  else
    TriggerOnItemClickEvent(itemId);
end;

procedure TSimpleListBox.ListboxDragBegin(sender:TObject; x,y:integer);
begin
  //
end;

procedure TSimpleListBox.ListboxDragUpdate(sender:TObject; x,y:integer);
var i:integer;
begin
  SetScrollPos(fScrollPos+dragInfo.prevDragUpdatePos.y-y);
end;

procedure TSimpleListBox.ListboxDragEnd;
begin
  //
end;

end.

