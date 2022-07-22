{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit UGPTabModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  uGPTabFileStructureTypes,
  UGPTabModuleTypes,
  UIntByteSizeStrings,
  ugptabtimeutils,
  UGPTabFileReader,
  UGPTabDisplay;

type
  TGPTabModule=class
    private


    public
      //timeUtils:TGPTabTimeUtils;
      display:TGPTabDisplay;
      constructor Create;
      destructor Destroy; override;
      function LoadTab(filePath:string; var out_tab:TTabulature; var out_errorMsg:string):boolean;
      function TabInfoToStr(var in_tabInfo:TGPTabInfo):string;
      function MeasuresToStr(var in_tab:TGPTabStructure):string;
      function MeasureToStr(measureId:integer; var in_tab:TGPTabStructure):string;
      function TracksToStr(var in_tab:TGPTabStructure):string;
      function TrackToStr(trackId:integer; var in_tab:TGPTabStructure):string;
      function MeasureTrackToStr(measureId,trackId:integer; var in_tab:TGPTabStructure):string;
      function MeasureTrackBeatToStr(var in_beat:TBeat; numOfStrings:integer):string;
      //function NoticeLinesToStr(var in_tab:TGPTabStructure):string;
      //function LyricsToStr(var in_Tab:TGPTabStructure):string;

  end;

implementation

// *****************************************************************************
// ********************************************************************** PUBLIC
// *****************************************************************************

constructor TGPTabModule.Create;
begin
  //timeUtils:=TGPTabTimeUtils.Create;
  display:=TGPTabDisplay.Create;
end;

destructor TGPTabModule.Destroy;
begin
  display.Destroy;
  //timeUtils.Destroy;

  inherited;
end;

function TGPTabModule.LoadTab(filePath:string; var out_tab:TTabulature; var out_errorMsg:string):boolean;
var fileReader:TGPTabFileReader;
begin
  result:=False;
  fileReader:=TGPTabFileReader.Create;
  result:=fileReader.LoadTab(filePath,out_tab,out_errorMsg);
  fileReader.Free;
  if not result then exit;
  timeUtils.PrepareTimeMarkers(out_tab.gpTab,out_tab.timeMarkers);
  timeUtils.PrepareMeasuresTimeInfo(out_tab.gpTab,out_tab.timeMarkers,out_tab.measuresTimeInfo);
  result:=True;
end;

function TGPTabModule.TabInfoToStr(var in_tabInfo:TGPTabInfo):string;
var i:integer;
begin
  result:='title: '       +in_tabInfo.title+#13+#10+
          'subtitle: '    +in_tabInfo.subtitle+#13+#10+
          'artist: '      +in_tabInfo.artist+#13+#10+
          'album: '       +in_tabInfo.album+#13+#10+
          'author: '      +in_tabInfo.author+#13+#10+
          'copyright: '   +in_tabInfo.copyright+#13+#10+
          'tab: '         +in_tabInfo.tab+#13+#10+
          'instructions: '+in_tabInfo.instructions+#13+#10+
          'noticesLinesCount: '+IntToStr(in_tabInfo.noticesLinesCount)+#13+#10;
  for i:=0 to in_tabInfo.noticesLinesCount-1 do
    result:=result+'notice['+IntToStr(i)+']: '+in_tabInfo.noticeLines[i]+#13+#10;
  result:=result+'lyricsTrackNr: '+IntToStr(in_tabInfo.lyricsTrackNr)+#13+#10;
  for i:=0 to 4 do
    if in_tabInfo.lyrics[i].text<>'' then
      result:=result+'lyrics['+IntToStr(i)+']: '+#13+#10+in_tabInfo.lyrics[i].text+#13+#10;
end;

function TGPTabModule.MeasuresToStr(var in_tab:TGPTabStructure):string;
var i:integer;
begin
  for i:=0 to in_tab.measuresCount-1 do
    result:=result+MeasureToStr(i,in_tab)+#13+#10;
end;

function TGPTabModule.MeasureToStr(measureId:integer; var in_tab:TGPTabStructure):string;
begin
  result:='measure['+IntToStr(measureId)+']: {';
  if in_tab.measures[measureId].hasDoubleBar                 then result:=result+'doubleBar; ';
  if in_tab.measures[measureId].hasTonality                  then result:=result+'tonality:'+IntToStr(in_tab.measures[measureId].tonalityValue)+'; ';
  if in_tab.measures[measureId].hasMarker                    then result:=result+'marker:'+in_tab.measures[measureId].markerText+', color:'+IntToStr(in_tab.measures[measureId].markerColor)+'; ';
  if in_tab.measures[measureId].hasNumberOfAlternateEnding   then result:=result+'alternateEndingNum:'+IntToStr(in_tab.measures[measureId].numberOfAlternateEndingValue)+'; ';
  if in_tab.measures[measureId].hasEndOfRepeat               then result:=result+'repeatEnd:'+IntToStr(in_tab.measures[measureId].endOfRepeatValue)+'; ';
  if in_tab.measures[measureId].hasBeginningOfRepeat         then result:=result+'repeatBegin; ';
  if in_tab.measures[measureId].hasDenominatorOfKeySignature then result:=result+'denominator:'+IntToStr(in_tab.measures[measureId].denominatorValue)+'; ';
  if in_tab.measures[measureId].hasNumeratorOfKeySignature   then result:=result+'numerator:'+IntToStr(in_tab.measures[measureId].numeratorValue)+'; ';
  result:=result+' }';
end;

function TGPTabModule.TracksToStr(var in_tab:TGPTabStructure):string;
var i:integer;
begin
  for i:=0 to in_tab.tracksCount-1 do
    result:=result+TrackToStr(i,in_tab)+#13+#10;
end;

function TGPTabModule.TrackToStr(trackId:integer; var in_tab:TGPTabStructure):string;
var i:integer;
begin
  result:='track['+IntToStr(trackId)+
    ']: {banjoTrack:'+IntToStr(Integer(in_tab.tracks[trackId].banjoTrack))+
    '; 12stringsGuitar:'+IntToStr(Integer(in_tab.tracks[trackId].twelveStringGuitar))+
    '; drumsTrack:'+IntToStr(Integer(in_tab.tracks[trackId].drumsTrack))+
    '; name:'+in_tab.tracks[trackId].name+
    '; numOfStrings:'+IntToStr(in_tab.tracks[trackId].numOfStrings)+
    '; stringsTuning:';
  for i:=0 to in_tab.tracks[trackId].numOfStrings-1 do
    result:=result+'['+IntToStr(in_tab.tracks[trackId].stringsTuning[i])+']';
  result:=result+'; port:'+IntToStr(in_tab.tracks[trackId].port)+
    '; channel:'+IntToStr(in_tab.tracks[trackId].channel)+
    '; channelE:'+IntToStr(in_tab.tracks[trackId].channelE)+
    '; numOfFrets:'+IntToStr(in_tab.tracks[trackId].numOfFrets)+
    '; capoHeight:'+IntToStr(in_tab.tracks[trackId].capoHeight)+
    '; color:'+IntToStr(in_tab.tracks[trackId].color)+'};';
end;

function TGPTabModule.MeasureTrackToStr(measureId,trackId:integer; var in_tab:TGPTabStructure):string;
var i:integer;
begin
  result:='beatsCount = '+IntToStr(in_tab.measuresTracks[measureId,trackId].beatsCount);
  for i:=0 to in_tab.measuresTracks[measureId,trackId].beatsCount-1 do
    result:=result+#13+#10+MeasureTrackBeatToStr(in_tab.measuresTracks[measureId,trackId].beats[i],
                                                 in_tab.tracks[trackId].numOfStrings);
end;

function TGPTabModule.MeasureTrackBeatToStr(var in_beat:TBeat; numOfStrings:integer):string;
var i:integer;
begin
  result:='beat={hasStatus:'+IntToStr(Integer(in_beat.hasStatus))+
    '; isTuplet:'+IntToStr(Integer(in_beat.isTuplet))+
    '; hasMixTable:'+IntToStr(Integer(in_beat.hasMixTable))+
    '; hasEffects:'+IntToStr(Integer(in_beat.hasEffects))+
    '; hasText:'+IntToStr(Integer(in_beat.hasText))+
    '; hasChordDiagram:'+IntToStr(Integer(in_beat.hasChordDiagram))+
    '; isDottedNote:'+IntToStr(Integer(in_beat.isDottedNote))+
    '; duration:'+IntToStr(in_beat.duration)+
    '; usedStrings:'+IntToStr(in_beat.usedStrings);
  if in_beat.hasStatus then result:=result+'; status:'+IntToStr(in_beat.status);
  if in_beat.isTuplet then result:=result+'; tupletValue:'+IntToStr(in_beat.tupletValue);
  if in_beat.hasText then result:=result+'; text:'+in_beat.text;
  // TODO: mixTable
  // TODO: effects
  // TODO: chordDiagram

  // notes are listed from the "top" of tabulature, that is from the most significant bit from range [6..0]
  result:=result+#13+#10;
  for i:=0 to numOfStrings-1 do
  begin
    if ((in_beat.usedStrings shl i) and 64 = 64) then
      result:=result+IntToStr(in_beat.stringsNotes[i].fretNumber)
    else
      result:=result+'-';
    result:=result+#13+#10;
  end;

  result:=result+'}';
end;

//function TGPTabModule.NoticeLinesToStr(var in_tab:TGPTabStructure):string;
//var i:integer;
//begin
//  result:='noticeLinesCount = '+IntToStr(in_tab.tabInfo.);
//end;

//function TGPTabModule.LyricsToStr(var in_Tab:TGPTabStructure):string;
//var i:integer;
//begin
//
//end;

// *****************************************************************************
// ********************************************************************* PRIVATE
// *****************************************************************************



end.

