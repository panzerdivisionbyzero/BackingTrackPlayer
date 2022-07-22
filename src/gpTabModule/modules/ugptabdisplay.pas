{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit UGPTabDisplay;

{$mode objfpc}{$H+}

interface

uses
  LCLintf,
  Classes, SysUtils, Types, Dialogs, Math,
  Graphics,
  UGPTabModuleTypes,
  UGPTabFileStructureTypes,
  UGPTabTimeUtils,
  UEventSuspender;

// rules that are worth following:
// - the positions of beats in bar should be local (they do not need to be recalculated when editing the preceding bars)
// - for faster drawing: beat and note positions are final (already scaled)

// TODO:
// - the end of the view range after changing track should reach the end of originally selected beat
// - scrolling view (independently from cursor)
// - local positions of bars content

type
  TCaptionParams=record
    offset:TPoint;
    fontColor:integer;
    fontSize:integer;
    bold:boolean;
  end;

  TMeasureParams=record
    measureTop:integer;
    measureHeight:integer;
    measureInnerMargin:integer;
    borderWidth:integer;
    borderColor:integer;
  end;

  TLineParams=record
    width:integer;
    color:integer;
  end;

  TLoopParams=record
    loopRegionColor:integer;
    cutRegionColor:integer;
  end;

  TBeatParams=record
    defaultWidth:integer;
    footerColor:integer;
    footerPenWidth:integer;
    footerOffsetY:integer;
    footerHeight:integer;
    footerDotR:integer;
  end;

  TDisplayParams=record
    trackCaptionParams:TCaptionParams;
    measureCaptionParams:TCaptionParams;
    noteCaptionParams:TCaptionParams;
    measureParams:TMeasureParams;
    stringParams:TLineParams;
    cursorParams:TLineParams;
    loopParams:TLoopParams;
    beatParams:TBeatParams;
    tempoChangeParams:TCaptionParams;
  end;

const
  defaultParams:TDisplayParams = (
    trackCaptionParams  :(offset:(x: 4;y:  4); fontColor:$00FFFF; fontSize:12; bold:True);
    measureCaptionParams:(offset:(x: 8;y:-20); fontColor:$0000FF; fontSize:12; bold:False);
    noteCaptionParams   :(offset:(x: 0;y:  0); fontColor:$FFFFFF; fontSize:12; bold:True);
    measureParams       :(measureTop:56; measureHeight:144; measureInnerMargin:16; borderWidth:2; borderColor:$808080);
    stringParams        :(width:1; color:$404040);
    cursorParams        :(width:2; color:$0000FF);
    loopParams          :(loopRegionColor:$800000; cutRegionColor:$000040);
    beatParams          :(defaultWidth:256; footerColor:$FFFFFF; footerPenWidth:2; footerOffsetY:-8; footerHeight:32; footerDotR:1);
    tempoChangeParams   :(offset:(x: 0;y:-32); fontColor:$FFFFFF; fontSize:12; bold:True);
  );

type
  TNoteView=record
    left:integer;
    width:integer;
    height:integer;
    stringId:integer;
    text:string;
    tremoloValue:integer; // -1 = brak
  end;
  TNotesViewArray=array of TNoteView;

  TBeatView=record
    posX:integer;
    width:integer;
    notesViews:TNotesViewArray;
    empty:boolean;
    rest:boolean;
    reverseFooter:boolean;
    hasTimeMarkerWithId:integer; // points to index from tab.timeMarkers[] (or -1 if none)
    tupletGroupPos:integer; // for triplets from -1 to 1; default 0;
  end;
  TBeatsViewArray=array of TBeatView;

  TMeasureView=record
    left:integer;
    right:integer;
    beatsViews:TBeatsViewArray;
  end;
  TMeasuresViewArray=array of TMeasureView;

  TTrackView=record
    stringsY:array [0..6] of integer; // from the top
  end;

  TTabCursor=record
    posX:integer;
    measureId:integer;
    trackBeatId:integer;
    currentMS:double;
  end;

const
  smCursorInView=0;
  smMeasureInView=1;
type
  TTabView=record
    left:integer; // offsetX of drawing tabulature content
    width:integer;
    trackId:integer;
    scrollMode:byte;
  end;

  TTabLoop=record
    cursorBegin:TTabCursor;
    cursorEnd:TTabCursor;
  end;

  TDrawRequestParams=record
    can:TCanvas;
    fromX, toX:integer
  end;

  TGPTabDisplay=class
    private
      fpTab:PTTabulature;
      fView:TTabView;
      fCursor:TTabCursor;
      fLoop:TTabLoop;
      fParams:TDisplayParams;
      fMeasuresView:TMeasuresViewArray;
      fTrackView:TTrackView;
      drawSuspender:TEventSuspender;
      cancelledDrawParams:TDrawRequestParams;
      procedure DrawFromSuspendedRequest;
      procedure ResetCursor(var cursor:TTabCursor);
      function BeatDurationToWidthPx(beatDuration:byte; dottedNote, isTuplet:boolean; tupletValue:integer=4):integer;
      procedure PrepareTabView;
      procedure DrawMeasure(var measure:TMeasureView; measureId:integer; can:TCanvas; fromX, toX:integer);
      procedure DrawBeat(var beat:TBeatView; beatId:integer; can:TCanvas; fromX, toX:integer);
      procedure DrawBeatFooter(can:TCanvas; absoluteX:integer; beatHalfWidth, beatDuration:integer; isDottedNote, reverseFooter:boolean);
      procedure DrawNoteTremolo(can:TCanvas; absoluteX:integer; tremoloValue:byte);
      procedure DrawLoop(can:TCanvas; fromX, toX:integer);
      procedure AdjustViewToCursorPos;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Reset;
      procedure ResetLoop;
      procedure SetTabPtr(pTab:PTTabulature);
      procedure SetViewWidth(viewWidth:integer);
      procedure SetViewPos(viewLeft:integer);
      procedure Draw(can:TCanvas; fromX, toX:integer);
      procedure SetTrackId(value:integer);
      procedure GoToMeasure(measureId:integer);
      procedure GoToMeasureBeat(measureId,trackBeatId:integer); overload;
      function GoToMeasureBeat(var inOut_cursor:TTabCursor; measureId,trackBeatId:integer):boolean; overload;
      function GetCurrentTrackBeatMS:double;
      function GetCurrentTrackBeatEndMS:double;
      function CanvasPxToTabPos(px:TPoint; var out_measureId:integer; var out_trackBeatId:integer):boolean;
      procedure SetLoopBeginAtCurrentPos;
      procedure SetLoopEndAtCurrentPos;
      property cursor:TTabCursor read fCursor;
      property view:TTabView read fView;
  end;

implementation

// *****************************************************************************
// ********************************************************************** PUBLIC
// *****************************************************************************

constructor TGPTabDisplay.Create;
begin
  drawSuspender:=TEventSuspender.Create;
  drawSuspender.OnTimerAction:=@DrawFromSuspendedRequest;
  drawSuspender.triggerDelay:=200;
  Reset;
end;

destructor TGPTabDisplay.Destroy;
begin
  drawSuspender.Destroy;

  inherited;
end;

procedure TGPTabDisplay.Reset;
begin
  fpTab:=nil;
  ResetCursor(fCursor);
  ResetLoop;
  fView.trackId:=-1;
  fView.width:=0;
  fView.left:=0;
  fView.scrollMode:=smMeasureInView;
  fParams:=defaultParams;
  SetLength(fMeasuresView,0);
end;

procedure TGPTabDisplay.ResetLoop;
begin
  ResetCursor(fLoop.cursorBegin);
  ResetCursor(fLoop.cursorEnd);
end;

procedure TGPTabDisplay.SetTabPtr(pTab:PTTabulature);
begin
  fpTab:=pTab;
  fView.trackId:=0;
  fView.left:=0;
  PrepareTabView;
  GoToMeasureBeat(0,0);
  ResetLoop;
end;

procedure TGPTabDisplay.SetViewWidth(viewWidth:integer);
begin
  fView.width:=viewWidth;
end;

procedure TGPTabDisplay.SetViewPos(viewLeft:integer);
begin
  fView.left:=viewLeft;
end;

procedure TGPTabDisplay.Draw(can:TCanvas; fromX, toX:integer);
var i:integer;
begin
  if (can=nil) or (not Assigned(can))
    or (fpTab=nil) then exit;

  if not drawSuspender.EventProcessingInitialization then
  begin
    cancelledDrawParams.can:=can;
    cancelledDrawParams.fromX:=fromX;
    cancelledDrawParams.toX:=toX;
    exit;
  end;

  can.Brush.Color:=0;
  can.Brush.Style:=bsSolid;
  can.FillRect(fromX,0,toX,can.Height);

  // loop:
  if ((fLoop.cursorBegin.posX>0) or (fLoop.cursorEnd.posX>0)) then
    DrawLoop(can,fromX,toX);

  // strings:
  can.Pen.Width:=fParams.stringParams.width;
  can.Pen.Color:=fParams.stringParams.color;
  for i:=0 to fpTab^.gpTab.tracks[fView.trackId].numOfStrings-1 do
  begin
    can.MoveTo(fromX,fTrackView.stringsY[i]);
    can.LineTo(toX,fTrackView.stringsY[i]);
  end;

  // cursor:
  can.Pen.Width:=fParams.cursorParams.width;
  can.Pen.Color:=fParams.cursorParams.color;
  can.MoveTo(fCursor.posX-fView.left,fParams.measureParams.measureTop);
  can.LineTo(fCursor.posX-fView.left,fParams.measureParams.measureTop+fParams.measureParams.measureHeight);

  // measures
  for i:=0 to fpTab^.gpTab.measuresCount-1 do
  begin
    if (fMeasuresView[i].right>fView.left+fromX)
      and (fMeasuresView[i].left<=fView.left+toX-1) then
      DrawMeasure(fMeasuresView[i],i,can,fromX,toX);
  end;

  can.Font.Color:=fParams.trackCaptionParams.fontColor;
  can.Font.Size :=fParams.trackCaptionParams.fontSize;
  can.Font.Bold :=fParams.trackCaptionParams.bold;
  can.TextOut(fParams.trackCaptionParams.offset.x,
              fParams.trackCaptionParams.offset.y,
              fpTab^.gpTab.tracks[fView.trackId].name);

  drawSuspender.EventProcessingFinalization;
end;

procedure TGPTabDisplay.SetTrackId(value:integer);
var currentMSbak:double;
    gb:integer;
begin
  if (fpTab=nil) or (value<0)
    or (value>=fpTab^.gpTab.tracksCount) then exit;

  fView.trackId:=value;
  PrepareTabView;
  timeUtils.MillisecondToTabPos(fCursor.currentMS,fView.trackId,fpTab^.measuresTimeInfo,fCursor.measureId,gb,fCursor.trackBeatId);
  currentMSbak:=fCursor.currentMS;
  GoToMeasureBeat(fCursor.measureId,fCursor.trackBeatId);
  fCursor.currentMS:=currentMSbak; // restoring millisecond to avoid decresing currentMS by changing track;
end;

procedure TGPTabDisplay.GoToMeasure(measureId:integer);
begin
  GoToMeasureBeat(measureId,0);
end;

procedure TGPTabDisplay.GoToMeasureBeat(measureId,trackBeatId:integer);
begin
  if not GoToMeasureBeat(fCursor,measureId,trackBeatId) then exit;

  // TODO: what was it doing here?... :)
  //if (fLoop.cursorBegin.posX>0) or (fLoop.cursorEnd.posX>0) then
  //begin
  //  GoToMeasureBeat(fLoop.cursorBegin,measureId,trackBeatId);
  //  GoToMeasureBeat(fLoop.cursorEnd  ,measureId,trackBeatId);
  //end;
  AdjustViewToCursorPos;
end;

function TGPTabDisplay.GoToMeasureBeat(var inOut_cursor:TTabCursor; measureId,trackBeatId:integer):boolean;
begin
  result:=False;
  if (fpTab=nil) or (measureId<0)
    or (measureId>=fpTab^.gpTab.measuresCount) then exit;
  result:=True;

  if (trackBeatId<0) and (measureId>0) then
  begin GoToMeasureBeat(measureId-1,fpTab^.gpTab.measuresTracks[measureId-1,fView.trackId].beatsCount-1); exit; end;
  if (trackBeatId>=fpTab^.gpTab.measuresTracks[measureId,fView.trackId].beatsCount) then
  begin GoToMeasure(measureId+1); exit; end;

  fCursor.posX:=fMeasuresView[measureId].beatsViews[trackBeatId].posX;
  fCursor.measureId:=measureId;
  fCursor.trackBeatId:=trackBeatId;
  fCursor.currentMS:=fpTab^.measuresTimeInfo[fCursor.measureId].tracksBeatsStartMs[fView.trackId,fCursor.trackBeatId];
end;

function TGPTabDisplay.GetCurrentTrackBeatMS:double;
begin
  result:=fCursor.currentMS;
end;

function TGPTabDisplay.GetCurrentTrackBeatEndMS:double;
begin
  // TODO: TEMP: do later better reading beat duration
  if fCursor.trackBeatId<fpTab^.gpTab.measuresTracks[fCursor.measureId,fView.trackId].beatsCount-1 then
  begin
    result:=fpTab^.measuresTimeInfo[fCursor.measureId].tracksBeatsStartMs[fView.trackId,fCursor.trackBeatId+1]-1;
  end
  else
  if fCursor.measureId<fpTab^.gpTab.measuresCount-1 then
    result:=fpTab^.measuresTimeInfo[fCursor.measureId+1].tracksBeatsStartMs[fView.trackId,0]-1
  else
    result:=fCursor.currentMS+fpTab^.measuresTimeInfo[fCursor.measureId].durationMS
      *(1-fpTab^.measuresTimeInfo[fCursor.measureId].tracksBeatsGridRatio[fView.trackId,fCursor.trackBeatId]);
end;

function TGPTabDisplay.CanvasPxToTabPos(px:TPoint; var out_measureId:integer; var out_trackBeatId:integer):boolean;
var i,k,x:integer;
begin
  result:=False;
  if (fpTab=nil) then exit;

  x:=px.x+fView.left;
  for i:=fpTab^.gpTab.measuresCount-1 downto 0 do
  begin
    if x<fMeasuresView[i].left then continue;

    out_measureId:=i;
    result:=True;
    out_trackBeatId:=0;

    // searching the first beat of measure, with lower position;
    // the lower position means that the given position is in previous beat;
    // in case of k=0, theoretically it cannot be lower;
    // the last iteration means that the position is after the last beat
    for k:=0 to fpTab^.gpTab.measuresTracks[i,fView.trackId].beatsCount do
    begin
      if (k=fpTab^.gpTab.measuresTracks[i,fView.trackId].beatsCount)
        or (x<fMeasuresView[i].beatsViews[k].posX) then
      begin
        if k=0 then out_trackBeatId:=0
               else out_trackBeatId:=k-1; // TODO: improve this loop
        exit;
      end;
    end;

    exit;
  end;
end;

procedure TGPTabDisplay.SetLoopBeginAtCurrentPos;
begin
  Move(fCursor,fLoop.cursorBegin,SizeOf(fCursor));
end;

procedure TGPTabDisplay.SetLoopEndAtCurrentPos;
begin
  Move(fCursor,fLoop.cursorEnd,SizeOf(fCursor));
  fLoop.cursorEnd.currentMS:=GetCurrentTrackBeatEndMS;
end;

// *****************************************************************************
// ********************************************************************* PRIVATE
// *****************************************************************************

procedure TGPTabDisplay.DrawFromSuspendedRequest;
begin
  Draw(cancelledDrawParams.can,cancelledDrawParams.fromX,cancelledDrawParams.toX);
end;

procedure TGPTabDisplay.ResetCursor(var cursor:TTabCursor);
begin
  cursor.posX:=0;
  cursor.measureId:=0;
  cursor.trackBeatId:=0;
  cursor.currentMS:=0;
end;

function TGPTabDisplay.BeatDurationToWidthPx(beatDuration:byte; dottedNote, isTuplet:boolean; tupletValue:integer=4):integer;
var d:double;
begin
  result:=byte(beatDuration+2);
  d:=fParams.beatParams.defaultWidth/Power(2,result);
  if dottedNote then d:=d*1.5;
  if isTuplet then d:=d*tupletsRatios[tupletValue];
  result:=Trunc(d);
end;

procedure TGPTabDisplay.PrepareTabView;
var n,nv,b,m,t,x,gridBeat,prevGridBeat:integer;
    measureGridRatio:double;
    can:TCanvas;
begin
  if (fpTab=nil) or (fView.trackId<0)
    or (fView.trackId>=fpTab^.gpTab.tracksCount)then exit;

  can:=TCanvas.Create;
  can.Handle:=GetDC(0) ;
  can.Font.Size:=12;

  for t:=0 to fpTab^.gpTab.tracks[fView.trackId].numOfStrings-1 do
    fTrackView.stringsY[t] := fParams.measureParams.measureTop+fParams.measureParams.measureInnerMargin
      +Round((t/fpTab^.gpTab.tracks[fView.trackId].numOfStrings)*(fParams.measureParams.measureHeight-fParams.measureParams.measureInnerMargin));

  SetLength(fMeasuresView,0);
  SetLength(fMeasuresView,fpTab^.gpTab.measuresCount);
  x:=0;
  for m:=0 to fpTab^.gpTab.measuresCount-1 do
  begin
    fMeasuresView[m].left:=x;
    x:=x+fParams.measureParams.measureInnerMargin;
    measureGridRatio:=0;

    SetLength(fMeasuresView[m].beatsViews,fpTab^.gpTab.measuresTracks[m,fView.trackId].beatsCount);
    for b:=0 to fpTab^.gpTab.measuresTracks[m,fView.trackId].beatsCount-1 do
    begin
      fMeasuresView[m].beatsViews[b].posX:=x;
      fMeasuresView[m].beatsViews[b].width:=BeatDurationToWidthPx(fpTab^.gpTab.measuresTracks[m,fView.trackId].beats[b].duration,
                                                                  fpTab^.gpTab.measuresTracks[m,fView.trackId].beats[b].isDottedNote,
                                                                  fpTab^.gpTab.measuresTracks[m,fView.trackId].beats[b].isTuplet,
                                                                  fpTab^.gpTab.measuresTracks[m,fView.trackId].beats[b].tupletValue);
      // reversing direction if previous beat exists and belongs to the same beat of measure
      gridBeat:=Trunc(fpTab^.measuresTimeInfo[m].numerator / fpTab^.measuresTimeInfo[m].denominator * 4 * measureGridRatio);
      fMeasuresView[m].beatsViews[b].reverseFooter:=(b>0) and (gridBeat=prevGridBeat) and (not fMeasuresView[m].beatsViews[b-1].rest);
      prevGridBeat:=gridBeat;
      measureGridRatio:=measureGridRatio+
        timeUtils.BeatDurationToMeasureQuotient(fpTab^.gpTab.measuresTracks[m,fView.trackId].beats[b].duration,
                                                fpTab^.gpTab.measuresTracks[m,fView.trackId].beats[b].isDottedNote,
                                                fpTab^.gpTab.measuresTracks[m,fView.trackId].beats[b].isTuplet,
                                                fpTab^.gpTab.measuresTracks[m,fView.trackId].beats[b].tupletValue);

      fMeasuresView[m].beatsViews[b].hasTimeMarkerWithId:=fpTab^.measuresTimeInfo[m].tracksBeatsTimeMarkersIds[fView.trackId,b];

      if fpTab^.gpTab.measuresTracks[m,fView.trackId].beats[b].isTuplet then
      begin
        // TODO: calc tupletGroupPos
      end
      else
        fMeasuresView[m].beatsViews[b].tupletGroupPos:=0;

      //if fpTab^.gpTab.measuresTracks[m,fView.trackId].beats[b].hasStatus then
      //begin
      //  fMeasuresView[m].beatsViews[b].empty:=fpTab^.gpTab.measuresTracks[m,fView.trackId].beats[b].status=bsEmpty;
      //  fMeasuresView[m].beatsViews[b].rest:=fpTab^.gpTab.measuresTracks[m,fView.trackId].beats[b].status=bsRest;
      //  if fMeasuresView[m].beatsViews[b].empty or fMeasuresView[m].beatsViews[b].rest then continue;
      //end;
      for n:=0 to fpTab^.gpTab.tracks[fView.trackId].numOfStrings-1 do
      begin
        if ((fpTab^.gpTab.measuresTracks[m,fView.trackId].beats[b].usedStrings shl n) and 64 <> 64) then continue;
        nv:=Length(fMeasuresView[m].beatsViews[b].notesViews);
        SetLength(fMeasuresView[m].beatsViews[b].notesViews,nv+1);
        fMeasuresView[m].beatsViews[b].notesViews[nv].stringId:=n;
        fMeasuresView[m].beatsViews[b].notesViews[nv].text:=IntToStr(fpTab^.gpTab.measuresTracks[m,fView.trackId].beats[b].stringsNotes[n].fretNumber);
        fMeasuresView[m].beatsViews[b].notesViews[nv].width:=can.TextWidth(fMeasuresView[m].beatsViews[b].notesViews[nv].text);
        fMeasuresView[m].beatsViews[b].notesViews[nv].height:=can.TextHeight(fMeasuresView[m].beatsViews[b].notesViews[nv].text);
        fMeasuresView[m].beatsViews[b].notesViews[nv].left:=fMeasuresView[m].beatsViews[b].posX-Round(0.5*fMeasuresView[m].beatsViews[b].notesViews[nv].width);
        if fpTab^.gpTab.measuresTracks[m,fView.trackId].beats[b].stringsNotes[n].hasEffects
          and fpTab^.gpTab.measuresTracks[m,fView.trackId].beats[b].stringsNotes[n].effects.hasTremoloPicking then
          fMeasuresView[m].beatsViews[b].notesViews[nv].tremoloValue:=
            fpTab^.gpTab.measuresTracks[m,fView.trackId].beats[b].stringsNotes[n].effects.tremoloPickingValue
        else
          fMeasuresView[m].beatsViews[b].notesViews[nv].tremoloValue:=-1;
      end;
      x:=x+fMeasuresView[m].beatsViews[b].width;
    end;

    x:=x+fParams.measureParams.measureInnerMargin;
    fMeasuresView[m].right:=x;
    //x:=fMeasuresView[m].right+2;
  end;

  can.Destroy;
end;

procedure TGPTabDisplay.DrawMeasure(var measure:TMeasureView; measureId:integer; can:TCanvas; fromX, toX:integer);
var n,i,stringId:integer;
begin
  can.Pen.Style:=psSolid;
  can.Pen.Width:=fParams.measureParams.borderWidth;
  can.Pen.Color:=fParams.measureParams.borderColor;
  can.Brush.Style:=bsClear;
  can.Font.Color:=fParams.measureCaptionParams.fontColor;
  can.Font.Size:=fParams.measureCaptionParams.fontSize;
  can.Font.Bold:=fParams.measureCaptionParams.bold;
  can.MoveTo(measure.right-fView.left,fParams.measureParams.measureTop);
  can.LineTo(measure.right-fView.left,fParams.measureParams.measureTop+fParams.measureParams.measureHeight);
  can.TextOut(measure.left+fParams.measureCaptionParams.offset.x-fView.left,
              fParams.measureParams.measureTop+fParams.measureCaptionParams.offset.y,IntToStr(measureId+1));

  // tempo changes:
  can.Font.Color:=fParams.tempoChangeParams.fontColor;
  can.Font.Size:=fParams.tempoChangeParams.fontSize;
  can.Font.Bold:=fParams.tempoChangeParams.bold;
  for i:=0 to High(measure.beatsViews) do
  begin
    if (measure.beatsViews[i].posX<fView.left+fromX)
      or (measure.beatsViews[i].posX-fView.left>toX)
      or ((measure.beatsViews[i].hasTimeMarkerWithId=-1)
        and (measureId>0)) then continue;
    if (measure.beatsViews[i].hasTimeMarkerWithId=-1) and (measureId=0) then
      n:=fpTab^.gpTab.tempo
    else
      n:=fpTab^.timeMarkers[measure.beatsViews[i].hasTimeMarkerWithId].tempo;
    can.TextOut(measure.beatsViews[i].posX+fParams.tempoChangeParams.offset.x-fView.left,
      fParams.measureParams.measureTop+fParams.tempoChangeParams.offset.y,'t='+IntToStr(n));
  end;

  // notes:
  can.Font.Color:=fParams.noteCaptionParams.fontColor;
  can.Font.Size:=fParams.noteCaptionParams.fontSize;
  can.Font.Bold:=fParams.noteCaptionParams.bold;
  for i:=0 to High(measure.beatsViews) do
  begin
    if (measure.beatsViews[i].posX<fView.left+fromX)
        or (measure.beatsViews[i].posX-fView.left>toX) then continue;
    //if measure.beatsViews[i].empty then begin can.TextOut(measure.beatsViews[i].posX-fView.left, fTrackView.stringsY[0],'E'); continue; end;
    //if measure.beatsViews[i].rest  then begin can.TextOut(measure.beatsViews[i].posX-fView.left, fTrackView.stringsY[0],'R'); continue; end;
    DrawBeatFooter(can,fMeasuresView[measureId].beatsViews[i].posX-fView.left,
                     Round(fMeasuresView[measureId].beatsViews[i].width*0.5),
                     fpTab^.gpTab.measuresTracks[measureId,fView.trackId].beats[i].duration,
                     fpTab^.gpTab.measuresTracks[measureId,fView.trackId].beats[i].isDottedNote,
                     fMeasuresView[measureId].beatsViews[i].reverseFooter);

    for n:=0 to High(measure.beatsViews[i].notesViews) do
    begin
      stringId:=measure.beatsViews[i].notesViews[n].stringId;
      can.TextOut(measure.beatsViews[i].notesViews[n].left-fView.left,
        fTrackView.stringsY[stringId]-Round(0.5*measure.beatsViews[i].notesViews[n].height),
        measure.beatsViews[i].notesViews[n].text);
      if measure.beatsViews[i].notesViews[n].tremoloValue>-1 then
        DrawNoteTremolo(can,measure.beatsViews[i].posX-fView.left,measure.beatsViews[i].notesViews[n].tremoloValue);
    end;
  end;
end;

procedure TGPTabDisplay.DrawBeat(var beat:TBeatView; beatId:integer; can:TCanvas; fromX, toX:integer);
begin
  //
end;

procedure TGPTabDisplay.DrawBeatFooter(can:TCanvas; absoluteX:integer; beatHalfWidth,
  beatDuration:integer; isDottedNote, reverseFooter:boolean);
var y,i,dotY:integer;
    step:double;
begin
  if beatDuration=nd1 then exit;
  can.Pen.Width:=fParams.beatParams.footerPenWidth;
  can.Pen.Color:=fParams.beatParams.footerColor;
  y:=fParams.measureParams.measureTop+fParams.measureParams.measureHeight+fParams.beatParams.footerOffsetY;
  if beatDuration=nd2 then y:=y+Round(fParams.beatParams.footerHeight*0.618);
  can.MoveTo(absoluteX,y);
  y:=y+fParams.beatParams.footerHeight;
  can.LineTo(absoluteX,y);
  step:=fParams.beatParams.footerHeight*0.618/2; // dividing into intermediate positions: 16, 32
  if (beatDuration<254) and (beatDuration>nd4) then
  begin
    for i:=0 to beatDuration-1 do // from eight note to theoretically max 64, although loop has no limitation; "-1" because the "step" must be multiplied by value decreased by 1;
    begin
      can.MoveTo(absoluteX,y-Round(step*i));
      if reverseFooter then can.LineTo(absoluteX-2*beatHalfWidth,y-Round(step*i))
                       else can.LineTo(absoluteX+  beatHalfWidth,y-Round(step*i));
    end;
    dotY:=Round(step*i);
  end
  else
    dotY:=y;
  if not isDottedNote then exit;
  i:=Round(step);
  can.Ellipse(absoluteX+i-fParams.beatParams.footerDotR,
              dotY-fParams.beatParams.footerDotR,
              absoluteX+i+fParams.beatParams.footerDotR,
              dotY+fParams.beatParams.footerDotR);
end;

procedure TGPTabDisplay.DrawNoteTremolo(can:TCanvas; absoluteX:integer; tremoloValue:byte);
var y,i:integer;
begin
  can.Pen.Width:=2;
  y:=fParams.measureParams.measureTop+fParams.measureParams.measureHeight+
     fParams.beatParams.footerOffsetY+fParams.beatParams.footerHeight-24;
  for i:=1 to tremoloValue do
  begin
    can.MoveTo(absoluteX-8,y+8);
    can.LineTo(absoluteX+8,y);
    y:=y+5;
  end;
end;

procedure TGPTabDisplay.DrawLoop(can:TCanvas; fromX, toX:integer);
var cx1,cx2,color:integer;
begin
  if fLoop.cursorEnd.posX>fLoop.cursorBegin.posX then
  begin
    cx1:=fLoop.cursorBegin.posX-fView.left; if cx1>toX   then exit;
    cx2:=fMeasuresView[fLoop.cursorEnd.measureId].beatsViews[fLoop.cursorEnd.trackBeatId].width+
         fLoop.cursorEnd  .posX-fView.left; if cx2<fromX then exit;
    color:=fParams.loopParams.loopRegionColor;
  end
  else
  begin
    cx1:=fLoop.cursorEnd  .posX-fView.left; if cx1>toX   then exit;
    cx2:=fMeasuresView[fLoop.cursorBegin.measureId].beatsViews[fLoop.cursorBegin.trackBeatId].width+
         fLoop.cursorBegin.posX-fView.left; if cx2<fromX then exit;
    color:=fParams.loopParams.cutRegionColor;
  end;
  // drawing range reduction:
  if cx1<fromX then cx1:=fromX;
  if cx2>toX   then cx2:=toX;
  can.Brush.Color:=color;
  can.Brush.Style:=bsSolid;
  can.FillRect(cx1,fParams.measureParams.measureTop,cx2,fParams.measureParams.measureTop+fParams.measureParams.measureHeight);
end;

procedure TGPTabDisplay.AdjustViewToCursorPos;
begin
  case fView.scrollMode of
    smCursorInView  : begin
                        if (fCursor.posX-fView.left>=fView.width)
                          or (fCursor.posX-fView.left<0) then
                          fView.left:=fCursor.posX;
                      end;
    smMeasureInView : begin
                        if (fMeasuresView[fCursor.measureId].right-fView.left>=fView.width)
                          or (fCursor.posX-fView.left<0) then
                          fView.left:=fCursor.posX;
                      end;
  end;
end;

end.

