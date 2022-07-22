{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit ugptabtimeutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Dialogs,
  uGPTabFileStructureTypes,
  UGPTabModuleTypes;

type
  TGPTabTimeUtils=class
    private
      procedure InsertTimeMarker(measureId,trackId,trackBeatId,tempo:integer; measureTimeRatio:double;
                                 var inOut_timeMarkers:TTimeMarkersArray; var inOut_lTimeMarkers:integer);
      function AddMeasureTimeChunk(var inOut_mti:TMeasureTimeInfo; timeMarkerId, tempo:integer; startGridRatio, endGridRatio:double):integer;
      function CalcTimeChunkDurationMS(startGridRatio,endGridRatio:double; numerator,denominator,tempo:integer):double;
    public
      constructor Create;
      destructor Destroy; override;
      function BeatDurationToMeasureQuotient(beatDuration:byte; dottedNote, isTuplet:boolean; tupletValue:integer=4):double;
      function MillisecondToTabPos(ms:double; trackId:integer; var in_measuresTimeInfo:TMeasuresTimeInfoArray;
                                   var out_measureId:integer; var out_gridBeatId:integer; var out_trackBeatId:integer):boolean;
      procedure PrepareTimeMarkers(var in_tab:TGPTabStructure; var out_timeMarkers:TTimeMarkersArray);
      function TimeMarkersToStr(var in_timeMarkers:TTimeMarkersArray):string;
      procedure PrepareMeasuresTimeInfo(var in_tab:TGPTabStructure; var in_timeMarkers:TTimeMarkersArray; var out_measuresTimeInfo:TMeasuresTimeInfoArray);
      function MeasuresTimeInfoToStr(var in_measuresTimeInfo:TMeasuresTimeInfoArray):string;
  end;

var
  timeUtils:TGPTabTimeUtils;

implementation

// *****************************************************************************
// ********************************************************************** PUBLIC
// *****************************************************************************

constructor TGPTabTimeUtils.Create;
begin
  //
end;

destructor TGPTabTimeUtils.Destroy;
begin
  //
  inherited;
end;

function TGPTabTimeUtils.BeatDurationToMeasureQuotient(beatDuration:byte; dottedNote, isTuplet:boolean; tupletValue:integer=4):double;
begin
  result:=byte(beatDuration+2);
  result:=1/Power(2,result);
  if dottedNote then result:=result*1.5;
  if isTuplet then result:=result*tupletsRatios[tupletValue];
end;

function TGPTabTimeUtils.MillisecondToTabPos(ms:double; trackId:integer; var in_measuresTimeInfo:TMeasuresTimeInfoArray;
                                   var out_measureId:integer; var out_gridBeatId:integer; var out_trackBeatId:integer):boolean;
var i,k:integer;
begin
  result:=False;
  if Length(in_measuresTimeInfo)=0 then exit;
  if trackId>=Length(in_measuresTimeInfo[0].tracksBeatsStartMs) then exit;

  for i:=High(in_measuresTimeInfo) downto 0 do
  begin
    //if i=0 then
    //  ShowMessage('i = '+IntToStr(i)+#13+
    //    IntToStr(ms)+' <? '+FormatFloat('0.####',in_measuresTimeInfo[i].gridBeatsStartMs[0]));
    if ms<in_measuresTimeInfo[i].gridBeatsStartMs[0] then continue;

    out_measureId:=i;

    // searching the first beat of measure, with lower timestamp;
    // the lower timestamp means that the given timestamp is in previous beat;
    // in case of k=0, theoretically it cannot be lower;
    // the last iteration means that the timestamp is after the last beat
    for k:=1 to in_measuresTimeInfo[i].numerator do
      if (k=in_measuresTimeInfo[i].numerator)
        or (ms<in_measuresTimeInfo[i].gridBeatsStartMs[k]) then
      begin
        out_gridBeatId:=k-1;
        result:=True;
        break;
      end;
    if result and (trackId>-1) then
      for k:=0 to Length(in_measuresTimeInfo[i].tracksBeatsStartMs[trackId]) do
      begin
        if (k=Length(in_measuresTimeInfo[i].tracksBeatsStartMs[trackId]))
          or (ms<in_measuresTimeInfo[i].tracksBeatsStartMs[trackId,k]) then
        begin
          out_trackBeatId:=k-1;
          exit;
        end;
      end;

    exit;
  end;
end;

// preparing the structure of timeMarkers[] (tempo values in chronological order)
// - I assumed that the length of each measure is "1"
// - measureDuration = measureLength/tempo
// - for further time changes, I divide the measure into timeChunks[], e.g.:
//   time change markers are at positions in measure: 0.5, 0.75
//   so I divide the measure into: [0..0.5], [0.5..0.75], [0.75..1]
//   therefore measureDuration = the sum of it's chunks duration
procedure TGPTabTimeUtils.PrepareTimeMarkers(var in_tab:TGPTabStructure; var out_timeMarkers:TTimeMarkersArray);
var b,t,m,lTimeMarkers:integer;
    measureGridRatio:double;
begin
  lTimeMarkers:=0;
  for m:=0 to in_tab.measuresCount-1 do
  begin
    for t:=0 to in_tab.tracksCount-1 do
    begin
      measureGridRatio:=0;
      for b:=0 to in_tab.measuresTracks[m,t].beatsCount-1 do
      begin
        if (in_tab.measuresTracks[m,t].beats[b].hasMixTable)
          and (in_tab.measuresTracks[m,t].beats[b].mixTable.tempo>-1) then
        begin
          InsertTimeMarker(m,t,b,in_tab.measuresTracks[m,t].beats[b].mixTable.tempo,measureGridRatio,out_timeMarkers,lTimeMarkers);
        end;
        measureGridRatio:=measureGridRatio+BeatDurationToMeasureQuotient(in_tab.measuresTracks[m,t].beats[b].duration,
                                                                         in_tab.measuresTracks[m,t].beats[b].isDottedNote,
                                                                         in_tab.measuresTracks[m,t].beats[b].isTuplet,
                                                                         in_tab.measuresTracks[m,t].beats[b].tupletValue);
      end; // end for b
    end; // end for t
  end; // end for m
  SetLength(out_timeMarkers,lTimeMarkers);
end;

function TGPTabTimeUtils.TimeMarkersToStr(var in_timeMarkers:TTimeMarkersArray):string;
var i:integer;
begin
  result:='';
  for i:=0 to High(in_timeMarkers) do
  begin
    result:=result+'tm['+IntToStr(i)+']: {measureId:'+IntToStr(in_timeMarkers[i].measureId)+
      '; tempo:'+IntToStr(in_timeMarkers[i].tempo)+
      '; measureGridRatio:'+FormatFloat('0.####',in_timeMarkers[i].measureGridRatio)+'}'+#13+#10;
  end;
end;

// preparing basic information about measuresTimeInfo[]:
// - time signature (e.g. 4/4)
// - timeChunks[]:
//   - each measureTimeInfo gets the first timeChunk, which starts from position 0
//   - each timeMarker occurring in this measure, gets his record,
//     except timeMarkers with measureGridRatio=0, then timeMarker overwrites tempo from measureTimeInfo
// the information about timeChins[].endGridRatio is being added in the second iteration through measuresTimeInfo[],
procedure TGPTabTimeUtils.PrepareMeasuresTimeInfo(var in_tab:TGPTabStructure; var in_timeMarkers:TTimeMarkersArray;
                                                  var out_measuresTimeInfo:TMeasuresTimeInfoArray);
var t,m,b,tr,currentTempo,currentNumerator,currentDenominator,measureFirstTM,measureLastTM,currentMeasureTempoChunk,
    currentGridBeat:integer;
    measureGridRatio,prevMeasureGridRatio,measureStartMs,absoluteTempoChunkEndMs,currentMs:double;
begin
  currentTempo:=in_tab.tempo;
  SetLength(out_measuresTimeInfo,in_tab.measuresCount);
  measureLastTM:=-1;
  // completing information about tempo and time signature of each measure:
  for m:=0 to in_tab.measuresCount-1 do
  begin
    if in_tab.measures[m].hasNumeratorOfKeySignature   then currentNumerator  :=in_tab.measures[m].numeratorValue;
    if in_tab.measures[m].hasDenominatorOfKeySignature then currentDenominator:=in_tab.measures[m].denominatorValue;
    out_measuresTimeInfo[m].numerator:=currentNumerator;
    out_measuresTimeInfo[m].denominator:=currentDenominator;

    measureGridRatio:=0;
    AddMeasureTimeChunk(out_measuresTimeInfo[m],-1,currentTempo,measureGridRatio,-1);

    // iterating throught timeMarkers of this measure:
    measureFirstTM:=measureLastTM+1;
    //measureLastTM:=-1;
    for t:=measureFirstTM to High(in_timeMarkers) do
    begin
      if in_timeMarkers[t].measureId>m then break;
      measureLastTM:=t;
      prevMeasureGridRatio:=measureGridRatio;

      currentTempo:=in_timeMarkers[t].tempo;
      if in_timeMarkers[t].measureGridRatio>0 then
        AddMeasureTimeChunk(out_measuresTimeInfo[m],t,currentTempo,in_timeMarkers[t].measureGridRatio,-1)
      else
      begin
        out_measuresTimeInfo[m].tempoChunks[0].tempo:=currentTempo;
        out_measuresTimeInfo[m].tempoChunks[0].startGridRatio:=in_timeMarkers[t].measureGridRatio;
        out_measuresTimeInfo[m].tempoChunks[0].hasTimeMarkerWithId:=t;
      end;
    end; // end for t

    // finishing timeChunks[]
    out_measuresTimeInfo[m].durationMS:=0;
    for t:=0 to High(out_measuresTimeInfo[m].tempoChunks) do
    begin
      if t<High(out_measuresTimeInfo[m].tempoChunks) then
        out_measuresTimeInfo[m].tempoChunks[t].endGridRatio:=out_measuresTimeInfo[m].tempoChunks[t+1].startGridRatio
      else
        out_measuresTimeInfo[m].tempoChunks[t].endGridRatio:=1;

      // when the timeChunk is finished, I can calculate it's duration:
      out_measuresTimeInfo[m].tempoChunks[t].durationMS:=
        CalcTimeChunkDurationMS(out_measuresTimeInfo[m].tempoChunks[t].startGridRatio,
                                out_measuresTimeInfo[m].tempoChunks[t].endGridRatio,
                                out_measuresTimeInfo[m].numerator,
                                out_measuresTimeInfo[m].denominator,
                                out_measuresTimeInfo[m].tempoChunks[t].tempo);
      out_measuresTimeInfo[m].durationMS:=out_measuresTimeInfo[m].durationMS+out_measuresTimeInfo[m].tempoChunks[t].durationMS;
    end;
  end; // end for m

  // preparing gridBeatsStartMs[] and calculating gridBeatsStartMs[0]:
  currentMs:=0;
  for m:=0 to in_tab.measuresCount-1 do
  begin
    SetLength(out_measuresTimeInfo[m].gridBeatsStartMs,out_measuresTimeInfo[m].numerator);
    //out_measuresTimeInfo[m].gridBeatsStartMs[0]:=currentMs;
    for t:=0 to High(out_measuresTimeInfo[m].tempoChunks) do
    begin
      for b:=0 to out_measuresTimeInfo[m].numerator-1 do
      begin
        prevMeasureGridRatio:=measureGridRatio;
        measureGridRatio:=b/out_measuresTimeInfo[m].denominator;
        if (measureGridRatio<out_measuresTimeInfo[m].tempoChunks[t].startGridRatio) then continue;
        if (measureGridRatio>out_measuresTimeInfo[m].tempoChunks[t].startGridRatio
                            +out_measuresTimeInfo[m].tempoChunks[t].durationMS) then break;
        out_measuresTimeInfo[m].gridBeatsStartMs[b]:=
          currentMs+CalcTimeChunkDurationMS(out_measuresTimeInfo[m].tempoChunks[t].startGridRatio,
                                            measureGridRatio,
                                            out_measuresTimeInfo[m].numerator,
                                            out_measuresTimeInfo[m].denominator,
                                            out_measuresTimeInfo[m].tempoChunks[t].tempo);
      end; // end for b

      // beats of tracks:
      SetLength(out_measuresTimeInfo[m].tracksBeatsStartMs,in_tab.tracksCount);
      SetLength(out_measuresTimeInfo[m].tracksBeatsGridRatio,in_tab.tracksCount);
      SetLength(out_measuresTimeInfo[m].tracksBeatsTimeMarkersIds,in_tab.tracksCount);
      for tr:=0 to in_tab.tracksCount-1 do
      begin
        SetLength(out_measuresTimeInfo[m].tracksBeatsStartMs[tr],in_tab.measuresTracks[m,tr].beatsCount);
        SetLength(out_measuresTimeInfo[m].tracksBeatsGridRatio[tr],in_tab.measuresTracks[m,tr].beatsCount);
        SetLength(out_measuresTimeInfo[m].tracksBeatsTimeMarkersIds[tr],in_tab.measuresTracks[m,tr].beatsCount);
        measureGridRatio:=0;
        for b:=0 to in_tab.measuresTracks[m,tr].beatsCount-1 do
        begin
          if (measureGridRatio<out_measuresTimeInfo[m].tempoChunks[t].startGridRatio) then continue;
          if (measureGridRatio>out_measuresTimeInfo[m].tempoChunks[t].startGridRatio
                              +out_measuresTimeInfo[m].tempoChunks[t].durationMS) then break;

          out_measuresTimeInfo[m].tracksBeatsGridRatio[tr,b]:=measureGridRatio;
          out_measuresTimeInfo[m].tracksBeatsStartMs[tr,b]:=
            currentMs+CalcTimeChunkDurationMS(out_measuresTimeInfo[m].tempoChunks[t].startGridRatio,
                                              measureGridRatio,
                                              out_measuresTimeInfo[m].numerator,
                                              out_measuresTimeInfo[m].denominator,
                                              out_measuresTimeInfo[m].tempoChunks[t].tempo);
          prevMeasureGridRatio:=measureGridRatio;
          measureGridRatio:=measureGridRatio+BeatDurationToMeasureQuotient(in_tab.measuresTracks[m,tr].beats[b].duration,
                                                                           in_tab.measuresTracks[m,tr].beats[b].isDottedNote,
                                                                           in_tab.measuresTracks[m,tr].beats[b].isTuplet,
                                                                           in_tab.measuresTracks[m,tr].beats[b].tupletValue);

          if (out_measuresTimeInfo[m].tempoChunks[t].hasTimeMarkerWithId>-1)
            and (in_timeMarkers[out_measuresTimeInfo[m].tempoChunks[t].hasTimeMarkerWithId].measureGridRatio>=prevMeasureGridRatio)
            and (in_timeMarkers[out_measuresTimeInfo[m].tempoChunks[t].hasTimeMarkerWithId].measureGridRatio<measureGridRatio)then
            out_measuresTimeInfo[m].tracksBeatsTimeMarkersIds[tr,b]:=out_measuresTimeInfo[m].tempoChunks[t].hasTimeMarkerWithId
          else
            out_measuresTimeInfo[m].tracksBeatsTimeMarkersIds[tr,b]:=-1;
        end; // end for b
      end; // end for tr

      currentMs:=currentMs+out_measuresTimeInfo[m].tempoChunks[t].durationMS;
    end; // end for t
  end; // end for m
end;

function TGPTabTimeUtils.MeasuresTimeInfoToStr(var in_measuresTimeInfo:TMeasuresTimeInfoArray):string;
var k,i:integer;
begin
  result:='';
  for i:=0 to High(in_measuresTimeInfo) do
  begin
    result:=result+'mti['+IntToStr(i)+']: {numerator:'+IntToStr(in_measuresTimeInfo[i].numerator)+
      '; denominator:'+IntToStr(in_measuresTimeInfo[i].denominator)+
      '; tempoChunks:';
    for k:=0 to High(in_measuresTimeInfo[i].tempoChunks) do
      result:=result+'tc['+IntToStr(k)+
        ']:{startGridRatio:'+FormatFloat('0.####',in_measuresTimeInfo[i].tempoChunks[k].startGridRatio)+
        '; endGridRatio:'+FormatFloat('0.####',in_measuresTimeInfo[i].tempoChunks[k].endGridRatio)+
        '; tempo:'+IntToStr(in_measuresTimeInfo[i].tempoChunks[k].tempo)+
        '; durationMS:'+FormatFloat('0.####',in_measuresTimeInfo[i].tempoChunks[k].durationMS)+'}';
    result:=result+'; gridBeatsStartMs:';
    for k:=0 to High(in_measuresTimeInfo[i].gridBeatsStartMs) do
      result:=result+'{'+FormatFloat('0.####',in_measuresTimeInfo[i].gridBeatsStartMs[k])+'}';
    result:=result+'}'+#13+#10;
  end;
end;

// *****************************************************************************
// ********************************************************************* PRIVATE
// *****************************************************************************

procedure TGPTabTimeUtils.InsertTimeMarker(measureId,trackId,trackBeatId,tempo:integer; measureTimeRatio:double;
                                           var inOut_timeMarkers:TTimeMarkersArray; var inOut_lTimeMarkers:integer);
var i,targetIndex:integer;
begin
  if inOut_lTimeMarkers>=Length(inOut_timeMarkers) then
    SetLength(inOut_timeMarkers,inOut_lTimeMarkers+100);

  targetIndex:=inOut_lTimeMarkers;
  for i:=inOut_lTimeMarkers-1 downto 0 do
  begin
    if inOut_timeMarkers[i].measureId<measureId then break;
    if inOut_timeMarkers[i].measureGridRatio>measureTimeRatio then
      targetIndex:=i;
  end;
  if targetIndex<inOut_lTimeMarkers then
    Move(inOut_timeMarkers[targetIndex],
         inOut_timeMarkers[targetIndex+1],
         (inOut_lTimeMarkers-targetIndex)*SizeOf(inOut_timeMarkers[targetIndex]));

  inOut_timeMarkers[targetIndex].millisecond:=-1; // can be calculated after all timeMarkers
  inOut_timeMarkers[targetIndex].measureId:=measureId;
  inOut_timeMarkers[targetIndex].measureGridRatio:=measureTimeRatio;
  inOut_timeMarkers[targetIndex].tempo:=tempo;

  inOut_lTimeMarkers:=inOut_lTimeMarkers+1;
end;

function TGPTabTimeUtils.AddMeasureTimeChunk(var inOut_mti:TMeasureTimeInfo; timeMarkerId, tempo:integer; startGridRatio, endGridRatio:double):integer;
begin
  result:=Length(inOut_mti.tempoChunks);
  SetLength(inOut_mti.tempoChunks,result+1);
  inOut_mti.tempoChunks[result].tempo:=tempo;
  inOut_mti.tempoChunks[result].startGridRatio:=startGridRatio;
  inOut_mti.tempoChunks[result].endGridRatio:=endGridRatio;
  inOut_mti.tempoChunks[result].durationMS:=0;
  inOut_mti.tempoChunks[result].hasTimeMarkerWithId:=timeMarkerId;
end;

function TGPTabTimeUtils.CalcTimeChunkDurationMS(startGridRatio,endGridRatio:double; numerator,denominator,tempo:integer):double;
begin
  // factorConvertingToMS * measureChunkLength * timeSgnature / tempo
  result:=60*4*1000*(endGridRatio-startGridRatio)*(numerator/denominator)/tempo;
end;

initialization
  timeUtils:=TGPTabTimeUtils.Create;
finalization
  timeUtils.Free;

end.

