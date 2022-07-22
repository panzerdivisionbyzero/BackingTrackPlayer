{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit UGPTabFileReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uGPTabFileStructureTypes,
  UGPTabModuleTypes,
  UIntByteSizeStrings;

type
  // Guitar Pro 4.06 file format reader class based on description from: http://dguitar.sourceforge.net/GP4format.html
  TGPTabFileReader=class
    private
      function ReadInfo(var fileBuf:TBytesArray; var inOut_c:integer; var out_tab:TGPTabStructure; var out_errorMsg:string):boolean;
      function ReadMidiTable(var fileBuf:TBytesArray; var inOut_c:integer; var out_tab:TGPTabStructure; var out_errorMsg:string):boolean;
      function ReadMeasures(var fileBuf:TBytesArray; var inOut_c:integer; var inOut_tab:TGPTabStructure; var out_errorMsg:string):boolean;
      function ReadTracks(var fileBuf:TBytesArray; var inOut_c:integer; var inOut_tab:TGPTabStructure; var out_errorMsg:string):boolean;
      function ReadBeats(var fileBuf:TBytesArray; var inOut_c:integer; var inOut_tab:TGPTabStructure; var out_errorMsg:string):boolean;
      function ReadBeat(var fileBuf:TBytesArray; var inOut_c:integer; var out_beat:TBeat; var out_errorMsg:string):boolean;
      function ReadChordDiagram(var fileBuf:TBytesArray; var inOut_c:integer; var out_chordDiagram:TChordDiagram; var out_errorMsg:string):boolean;
      function ReadBeatEffects(var fileBuf:TBytesArray; var inOut_c:integer; var out_effects:TBeatEffects; var out_errorMsg:string):boolean;
      function ReadEffectBend(var fileBuf:TBytesArray; var inOut_c:integer; var out_bend:TBend; var out_errorMsg:string):boolean;
      function ReadBeatMixTable(var fileBuf:TBytesArray; var inOut_c:integer; var out_mixTable:TMixTable; var out_errorMsg:string):boolean;
      function ReadBeatNote(var fileBuf:TBytesArray; var inOut_c:integer; var out_note:TNote; var out_errorMsg:string):boolean;
      function ReadBeatNoteEffects(var fileBuf:TBytesArray; var inOut_c:integer; var out_effects:TNoteEffects; var out_errorMsg:string):boolean;
      function ReadGraceNote(var fileBuf:TBytesArray; var inOut_c:integer; var out_graceNote:TGraceNote; var out_errorMsg:string):boolean;
      function ReadChordsDiagrams(var fileBuf:TBytesArray; var inOut_c:integer; var inOut_tab:TGPTabStructure; var out_errorMsg:string):boolean;
    public
      constructor Create;
      destructor Destroy; override;
      function LoadTab(filePath:string; var out_tab:TTabulature; var out_errorMsg:string):boolean;
  end;

implementation

// *****************************************************************************
// ********************************************************************** PUBLIC
// *****************************************************************************

constructor TGPTabFileReader.Create;
begin
  //
end;

destructor TGPTabFileReader.Destroy;
begin
  //

  inherited;
end;

function TGPTabFileReader.LoadTab(filePath:string; var out_tab:TTabulature; var out_errorMsg:string):boolean;
var fs:TFileStream;
    c:integer;
    fileSize:integer;
    fileBuf:TBytesArray;
    bytesRead:integer;
begin
  result:=False;
  try
    fs:=TFileStream.Create(filePath,fmOpenRead);
  except
    out_errorMsg:='Error opening file:'+#13+filePath;
    fs.Free;
    exit;
  end;
  if fs.Size=0 then exit;
  fileSize:=fs.Size;
  SetLength(fileBuf,fileSize);
  fs.Read(fileBuf[0],fileSize);
  fs.Free;

  c:=0;
  out_tab.gpTab.header:=ReadByteSizeString(@fileBuf[c],@bytesRead);
  if out_tab.gpTab.header<>'FICHIER GUITAR PRO v4.06' then begin out_errorMsg:='Unknown header: '+out_tab.gpTab.header; exit; end;

  c:=bytesRead+6; // skipping 6 bytes, I don't know what they mean

  if not ReadInfo(fileBuf,c,out_tab.gpTab,out_errorMsg) then exit;
  Move(fileBuf[c],out_tab.gpTab.tempo ,4); c:=c+4;
  Move(fileBuf[c],out_tab.gpTab.key   ,1); c:=c+1;
  Move(fileBuf[c],out_tab.gpTab.octave,4); c:=c+4;
  if not ReadMidiTable(fileBuf,c,out_tab.gpTab,out_errorMsg) then exit;
  Move(fileBuf[c],out_tab.gpTab.measuresCount,4); c:=c+4;
  Move(fileBuf[c],out_tab.gpTab.tracksCount  ,4); c:=c+4;
  if (not ReadMeasures(fileBuf,c,out_tab.gpTab,out_errorMsg)) then exit;
  if (not ReadTracks(fileBuf,c,out_tab.gpTab,out_errorMsg)) then exit;
  if (not ReadBeats(fileBuf,c,out_tab.gpTab,out_errorMsg)) then exit;
  if (not ReadChordsDiagrams(fileBuf,c,out_tab.gpTab,out_errorMsg))then exit;

  result:=True;
end;

// *****************************************************************************
// ********************************************************************* PRIVATE
// *****************************************************************************

function TGPTabFileReader.ReadInfo(var fileBuf:TBytesArray; var inOut_c:integer; var out_tab:TGPTabStructure; var out_errorMsg:string):boolean;
var bytesRead,i:integer;
begin
  result:=False;
  //ReadIntByteSizeString(@fileBuf[inOut_c],@bytesRead); inOut_c:=inOut_c+bytesRead; // skipping some empty string before title
  out_tab.tabInfo.title        := ReadIntByteSizeString(@fileBuf[inOut_c],@bytesRead); inOut_c:=inOut_c+bytesRead;
  out_tab.tabInfo.subtitle     := ReadIntByteSizeString(@fileBuf[inOut_c],@bytesRead); inOut_c:=inOut_c+bytesRead;
  out_tab.tabInfo.artist       := ReadIntByteSizeString(@fileBuf[inOut_c],@bytesRead); inOut_c:=inOut_c+bytesRead;
  out_tab.tabInfo.album        := ReadIntByteSizeString(@fileBuf[inOut_c],@bytesRead); inOut_c:=inOut_c+bytesRead;
  out_tab.tabInfo.author       := ReadIntByteSizeString(@fileBuf[inOut_c],@bytesRead); inOut_c:=inOut_c+bytesRead;
  out_tab.tabInfo.copyright    := ReadIntByteSizeString(@fileBuf[inOut_c],@bytesRead); inOut_c:=inOut_c+bytesRead;
  out_tab.tabInfo.tab          := ReadIntByteSizeString(@fileBuf[inOut_c],@bytesRead); inOut_c:=inOut_c+bytesRead;
  out_tab.tabInfo.instructions := ReadIntByteSizeString(@fileBuf[inOut_c],@bytesRead); inOut_c:=inOut_c+bytesRead;
  Move(fileBuf[inOut_c],out_tab.tabInfo.noticesLinesCount,4); inOut_c:=inOut_c+4;
  SetLength(out_tab.tabInfo.noticeLines,out_tab.tabInfo.noticesLinesCount);
  for i:=0 to out_tab.tabInfo.noticesLinesCount-1 do
  begin
    out_tab.tabInfo.noticeLines[i]  := ReadIntByteSizeString(@fileBuf[inOut_c],@bytesRead); inOut_c:=inOut_c+bytesRead;
  end;
  inOut_c:=inOut_c+1; // skipping a byte of unknown meaning

  Move(fileBuf[inOut_c],out_tab.tabInfo.lyricsTrackNr,4); inOut_c:=inOut_c+4;
  for i:=0 to 4 do // always 5 fields of lyrics
  begin
    Move(fileBuf[inOut_c],out_tab.tabInfo.lyrics[i].unknownInt,4); inOut_c:=inOut_c+4;
    out_tab.tabInfo.lyrics[i].text:= ReadIntSizeString(@fileBuf[inOut_c],@bytesRead); inOut_c:=inOut_c+bytesRead;
  end;

  result:=True;
end;

function TGPTabFileReader.ReadMidiTable(var fileBuf:TBytesArray; var inOut_c:integer; var out_tab:TGPTabStructure; var out_errorMsg:string):boolean;
var i,n:integer;
begin
  result:=False;
  for n:=0 to midiPortsCount-1 do
    for i:=0 to midiPortChannelsCount-1 do
    begin
      Move(fileBuf[inOut_c],out_tab.midiPorts[n].channels[i],midiChannelSize); inOut_c:=inOut_c+midiChannelSize;
    end;
  result:=True;
end;

function TGPTabFileReader.ReadMeasures(var fileBuf:TBytesArray; var inOut_c:integer; var inOut_tab:TGPTabStructure; var out_errorMsg:string):boolean;
var i,bytesRead:integer;
    b:byte;
begin
  result:=False;
  SetLength(inOut_tab.measures,inOut_tab.measuresCount);
  for i:=0 to inOut_tab.measuresCount-1 do
  begin
    Move(fileBuf[inOut_c],b,1); inOut_c:=inOut_c+1;
    inOut_tab.measures[i].hasDoubleBar                 := b and 128 > 0;
    inOut_tab.measures[i].hasTonality                  := b and  64 > 0;
    inOut_tab.measures[i].hasMarker                    := b and  32 > 0;
    inOut_tab.measures[i].hasNumberOfAlternateEnding   := b and  16 > 0;
    inOut_tab.measures[i].hasEndOfRepeat               := b and   8 > 0;
    inOut_tab.measures[i].hasBeginningOfRepeat         := b and   4 > 0;
    inOut_tab.measures[i].hasDenominatorOfKeySignature := b and   2 > 0;
    inOut_tab.measures[i].hasNumeratorOfKeySignature   := b and   1 > 0;
    if inOut_tab.measures[i].hasNumeratorOfKeySignature then
    begin Move(fileBuf[inOut_c],inOut_tab.measures[i].numeratorValue              ,1); inOut_c:=inOut_c+1; end;
    if inOut_tab.measures[i].hasDenominatorOfKeySignature then
    begin MOve(fileBuf[inOut_c],inOut_tab.measures[i].denominatorValue            ,1); inOut_c:=inOut_c+1; end;
    if inOut_tab.measures[i].hasEndOfRepeat then
    begin Move(fileBuf[inOut_c],inOut_tab.measures[i].endOfRepeatValue            ,1); inOut_c:=inOut_c+1; end;
    if inOut_tab.measures[i].hasNumberOfAlternateEnding then
    begin Move(fileBuf[inOut_c],inOut_tab.measures[i].numberOfAlternateEndingValue,1); inOut_c:=inOut_c+1; end;
    if inOut_tab.measures[i].hasMarker then
    begin
      inOut_tab.measures[i].markerText:=ReadIntByteSizeString(@fileBuf[inOut_c],@bytesRead); inOut_c:=inOut_c+bytesRead;
      Move(fileBuf[inOut_c],inOut_tab.measures[i].markerColor,4);  inOut_c:=inOut_c+4;
    end;
    if inOut_tab.measures[i].hasTonality then
    begin Move(fileBuf[inOut_c],inOut_tab.measures[i].tonalityValue,1); inOut_c:=inOut_c+1; end;
    if i=0 then inOut_c:=inOut_c+1; // skipping byte after the first measure; unknown meaning;
  end;
  result:=True;
end;

function TGPTabFileReader.ReadTracks(var fileBuf:TBytesArray; var inOut_c:integer; var inOut_tab:TGPTabStructure; var out_errorMsg:string):boolean;
var i,bytesRead:integer;
    b:byte;
begin
  result:=False;
  SetLength(inOut_tab.tracks,inOut_tab.tracksCount);
  for i:=0 to inOut_tab.tracksCount-1 do
  begin
    Move(fileBuf[inOut_c],b,1); inOut_c:=inOut_c+1;
    inOut_tab.tracks[i].banjoTrack         := b and   4 > 0;
    inOut_tab.tracks[i].twelveStringGuitar := b and   2 > 0;
    inOut_tab.tracks[i].drumsTrack         := b and   1 > 0;

    inOut_tab.tracks[i].name:=ReadByteSizeString(@fileBuf[inOut_c],@bytesRead); inOut_c:=inOut_c+41;

    Move(fileBuf[inOut_c],inOut_tab.tracks[i].numOfStrings    , 4); inOut_c:=inOut_c+ 4;
    if (inOut_tab.tracks[i].numOfStrings<0) or (inOut_tab.tracks[i].numOfStrings>7) then
    begin out_errorMsg:='trackId = '+IntToStr(i)+'; flags = '+IntToStr(b)+
      '; numOfStrings = '+IntToStr(inOut_tab.tracks[i].numOfStrings)+
      '; name = '+inOut_tab.tracks[i].name; exit; end;
    Move(fileBuf[inOut_c],inOut_tab.tracks[i].stringsTuning[0],28); inOut_c:=inOut_c+28;
    Move(fileBuf[inOut_c],inOut_tab.tracks[i].port            , 4); inOut_c:=inOut_c+ 4;
    Move(fileBuf[inOut_c],inOut_tab.tracks[i].channel         , 4); inOut_c:=inOut_c+ 4;
    Move(fileBuf[inOut_c],inOut_tab.tracks[i].channelE        , 4); inOut_c:=inOut_c+ 4;
    Move(fileBuf[inOut_c],inOut_tab.tracks[i].numOfFrets      , 4); inOut_c:=inOut_c+ 4;
    Move(fileBuf[inOut_c],inOut_tab.tracks[i].capoHeight      , 4); inOut_c:=inOut_c+ 4;
    Move(fileBuf[inOut_c],inOut_tab.tracks[i].color           , 4); inOut_c:=inOut_c+ 4;
  end;
  result:=True;
end;

function TGPTabFileReader.ReadBeats(var fileBuf:TBytesArray; var inOut_c:integer; var inOut_tab:TGPTabStructure; var out_errorMsg:string):boolean;
var b,t,m:integer;
begin
  result:=False;
  SetLength(inOut_tab.measuresTracks,inOut_tab.measuresCount);
  for m:=0 to inOut_tab.measuresCount-1 do
  begin
    SetLength(inOut_tab.measuresTracks[m],inOut_tab.tracksCount);
    for t:=0 to inOut_tab.tracksCount-1 do
    begin
      Move(fileBuf[inOut_c],inOut_tab.measuresTracks[m,t].beatsCount, 4); inOut_c:=inOut_c+4;
      SetLength(inOut_tab.measuresTracks[m,t].beats,inOut_tab.measuresTracks[m,t].beatsCount);
      for b:=0 to inOut_tab.measuresTracks[m,t].beatsCount-1 do
      begin
        if not ReadBeat(fileBuf,inOut_c,inOut_tab.measuresTracks[m,t].beats[b],out_errorMsg) then exit;
      end;
    end; // end for t
  end; // end for m
  result:=True;
end;

function TGPTabFileReader.ReadBeat(var fileBuf:TBytesArray; var inOut_c:integer; var out_beat:TBeat; var out_errorMsg:string):boolean;
var i,bytesRead:integer;
    b:byte;
begin
  result:=False;

  Move(fileBuf[inOut_c],b,1); inOut_c:=inOut_c+1;
  out_beat.hasStatus       := b and  64 > 0;
  out_beat.isTuplet        := b and  32 > 0;
  out_beat.hasMixTable     := b and  16 > 0;
  out_beat.hasEffects      := b and   8 > 0;
  out_beat.hasText         := b and   4 > 0;
  out_beat.hasChordDiagram := b and   2 > 0;
  out_beat.isDottedNote    := b and   1 > 0;

  if out_beat.hasStatus then begin Move(fileBuf[inOut_c],out_beat.status     ,1); inOut_c:=inOut_c+1; end;
  Move(fileBuf[inOut_c],out_beat.duration,1); inOut_c:=inOut_c+1;
  if out_beat.isTuplet  then begin Move(fileBuf[inOut_c],out_beat.tupletValue,4); inOut_c:=inOut_c+4; end;
  if out_beat.hasChordDiagram and not ReadChordDiagram(fileBuf,inOut_c,out_beat.chordDiagram,out_errorMsg) then exit;
  if out_beat.hasText   then
  begin out_beat.text:=ReadIntByteSizeString(@fileBuf[inOut_c],@bytesRead); inOut_c:=inOut_c+bytesRead; end;
  if out_beat.hasEffects      and not ReadBeatEffects(fileBuf,inOut_c,out_beat.effects,out_errorMsg) then exit;
  if out_beat.hasmixTable     and not ReadBeatMixTable(fileBuf,inOut_c,out_beat.mixTable,out_errorMsg) then exit;
  Move(fileBuf[inOut_c],out_beat.usedStrings,1); inOut_c:=inOut_c+1;

  // notes are listed from the "top" of tabulature, that is from the most significant bit from range [6..0]
  for i:=0 to 6 do
    if ((out_beat.usedStrings shl i) and 64 = 64)
      and not ReadBeatNote(fileBuf,inOut_c,out_beat.stringsNotes[i],out_errorMsg) then exit;

  result:=True;
end;

function TGPTabFileReader.ReadChordDiagram(var fileBuf:TBytesArray; var inOut_c:integer; var out_chordDiagram:TChordDiagram; var out_errorMsg:string):boolean;
var bytesRead:integer;
    b:byte;
begin
  result:=False;
  Move(fileBuf[inOut_c],b,1); inOut_c:=inOut_c+1;  // header = 1
  if b<>1 then begin out_errorMsg:='wrong chord header'; exit; end;
  Move(fileBuf[inOut_c],out_chordDiagram.sharp            ,  1); inOut_c:=inOut_c+1+3; // + skipping 3 empty bytes
  Move(fileBuf[inOut_c],out_chordDiagram.root             ,  1); inOut_c:=inOut_c+1;
  Move(fileBuf[inOut_c],out_chordDiagram.chordType        ,  1); inOut_c:=inOut_c+1;
  Move(fileBuf[inOut_c],out_chordDiagram.goesUntil        ,  1); inOut_c:=inOut_c+1;
  Move(fileBuf[inOut_c],out_chordDiagram.lowestNote       ,  4); inOut_c:=inOut_c+4;
  Move(fileBuf[inOut_c],out_chordDiagram.tonality         ,  4); inOut_c:=inOut_c+4;
  Move(fileBuf[inOut_c],out_chordDiagram.hasAddedNote     ,  1); inOut_c:=inOut_c+1;
  out_chordDiagram.name:=ReadByteSizeString(@fileBuf[inOut_c],@bytesRead); inOut_c:=inOut_c+20+2; // constant string length + 2 empty bytes
  Move(fileBuf[inOut_c],out_chordDiagram.fifthTonality    ,  1); inOut_c:=inOut_c+1;
  Move(fileBuf[inOut_c],out_chordDiagram.ninthTonality    ,  1); inOut_c:=inOut_c+1;
  Move(fileBuf[inOut_c],out_chordDiagram.eleventhTonality ,  1); inOut_c:=inOut_c+1;
  Move(fileBuf[inOut_c],out_chordDiagram.baseFret         ,  4); inOut_c:=inOut_c+4;
  Move(fileBuf[inOut_c],out_chordDiagram.frets[0]         ,7*4); inOut_c:=inOut_c+7*4;
  Move(fileBuf[inOut_c],out_chordDiagram.numOfBarres      ,  1); inOut_c:=inOut_c+1;
  Move(fileBuf[inOut_c],out_chordDiagram.barreFrets[0]    ,  5); inOut_c:=inOut_c+5;
  Move(fileBuf[inOut_c],out_chordDiagram.barreStarts[0]   ,  5); inOut_c:=inOut_c+5;
  Move(fileBuf[inOut_c],out_chordDiagram.barreEnds[0]     ,  5); inOut_c:=inOut_c+5;
  Move(fileBuf[inOut_c],out_chordDiagram.omission1        ,  1); inOut_c:=inOut_c+1;
  Move(fileBuf[inOut_c],out_chordDiagram.omission3        ,  1); inOut_c:=inOut_c+1;
  Move(fileBuf[inOut_c],out_chordDiagram.omission5        ,  1); inOut_c:=inOut_c+1;
  Move(fileBuf[inOut_c],out_chordDiagram.omission7        ,  1); inOut_c:=inOut_c+1;
  Move(fileBuf[inOut_c],out_chordDiagram.omission9        ,  1); inOut_c:=inOut_c+1;
  Move(fileBuf[inOut_c],out_chordDiagram.omission11       ,  1); inOut_c:=inOut_c+1;
  Move(fileBuf[inOut_c],out_chordDiagram.omission13       ,  1); inOut_c:=inOut_c+1+1; // + skipping 1 empty byte
  Move(fileBuf[inOut_c],out_chordDiagram.fingering[0]     ,  7); inOut_c:=inOut_c+7;
  Move(fileBuf[inOut_c],out_chordDiagram.showDiagFingering,  1); inOut_c:=inOut_c+1;
  result:=True;
end;

function TGPTabFileReader.ReadBeatEffects(var fileBuf:TBytesArray; var inOut_c:integer; var out_effects:TBeatEffects; var out_errorMsg:string):boolean;
var b:byte;
begin
  result:=False;
  Move(fileBuf[inOut_c],b,1); inOut_c:=inOut_c+1; // header 1
  out_effects.hasStroke                 := b and  64 > 0;
  out_effects.hasTappingPoppingSlapping := b and  32 > 0;
  Move(fileBuf[inOut_c],b,1); inOut_c:=inOut_c+1; // header 2
  out_effects.hasTremoloBar             := b and   4 > 0;
  out_effects.hasPickStroke             := b and   2 > 0;
  out_effects.rasguedo                  := b and   1 > 0;

  if out_effects.hasTappingPoppingSlapping then begin Move(fileBuf[inOut_c],out_effects.tappingPoppingSlappingValue,1); inOut_c:=inOut_c+1; end;
  if out_effects.hasTremoloBar and not ReadEffectBend(fileBuf,inOut_c,out_effects.tremoloBarValue,out_errorMsg) then exit;
  if out_effects.hasStroke     then begin Move(fileBuf[inOut_c],out_effects.upStrokeValue  ,1); inOut_c:=inOut_c+1; end;
  if out_effects.hasStroke     then begin Move(fileBuf[inOut_c],out_effects.downStrokeValue,1); inOut_c:=inOut_c+1; end;
  if out_effects.hasPickStroke then begin Move(fileBuf[inOut_c],out_effects.pickStrokeValue,1); inOut_c:=inOut_c+1; end;

  result:=True;
end;

function TGPTabFileReader.ReadEffectBend(var fileBuf:TBytesArray; var inOut_c:integer; var out_bend:TBend; var out_errorMsg:string):boolean;
var i:integer;
begin
  result:=False;
  Move(fileBuf[inOut_c],out_bend.bendType   ,1); inOut_c:=inOut_c+1;
  Move(fileBuf[inOut_c],out_bend.value      ,4); inOut_c:=inOut_c+4;
  Move(fileBuf[inOut_c],out_bend.pointsCount,4); inOut_c:=inOut_c+4;
  SetLength(out_bend.points,out_bend.pointsCount);
  for i:=0 to out_bend.pointsCount-1 do
  begin
    Move(fileBuf[inOut_c],out_bend.points[i].absoluteTimePos,4); inOut_c:=inOut_c+4;
    Move(fileBuf[inOut_c],out_bend.points[i].verticalPos    ,4); inOut_c:=inOut_c+4;
    Move(fileBuf[inOut_c],out_bend.points[i].vibrato        ,1); inOut_c:=inOut_c+1;
  end;
  result:=True;
end;

function TGPTabFileReader.ReadBeatMixTable(var fileBuf:TBytesArray; var inOut_c:integer; var out_mixTable:TMixTable; var out_errorMsg:string):boolean;
var b:byte;
begin
  result:=False;
  Move(fileBuf[inOut_c],out_mixTable.instrument,1); inOut_c:=inOut_c+1;
  Move(fileBuf[inOut_c],out_mixTable.volume    ,1); inOut_c:=inOut_c+1;
  Move(fileBuf[inOut_c],out_mixTable.pan       ,1); inOut_c:=inOut_c+1;
  Move(fileBuf[inOut_c],out_mixTable.chorus    ,1); inOut_c:=inOut_c+1;
  Move(fileBuf[inOut_c],out_mixTable.reverb    ,1); inOut_c:=inOut_c+1;
  Move(fileBuf[inOut_c],out_mixTable.phaser    ,1); inOut_c:=inOut_c+1;
  Move(fileBuf[inOut_c],out_mixTable.tremolo   ,1); inOut_c:=inOut_c+1;
  Move(fileBuf[inOut_c],out_mixTable.tempo     ,4); inOut_c:=inOut_c+4;
  if out_mixTable.volume  <255 then begin Move(fileBuf[inOut_c],out_mixTable.volumeChangeDuration    ,1); inOut_c:=inOut_c+1; end;
  if out_mixTable.pan     <255 then begin Move(fileBuf[inOut_c],out_mixTable.panChangeDuration       ,1); inOut_c:=inOut_c+1; end;
  if out_mixTable.chorus  <255 then begin Move(fileBuf[inOut_c],out_mixTable.chorusChangeDuration    ,1); inOut_c:=inOut_c+1; end;
  if out_mixTable.reverb  <255 then begin Move(fileBuf[inOut_c],out_mixTable.reverbChangeDuration    ,1); inOut_c:=inOut_c+1; end;
  if out_mixTable.phaser  <255 then begin Move(fileBuf[inOut_c],out_mixTable.phaserChangeDuration    ,1); inOut_c:=inOut_c+1; end;
  if out_mixTable.tremolo <255 then begin Move(fileBuf[inOut_c],out_mixTable.tremoloChangeDuration   ,1); inOut_c:=inOut_c+1; end;
  if out_mixTable.tempo   >-1  then begin Move(fileBuf[inOut_c],out_mixTable.tempoChangeDuration     ,1); inOut_c:=inOut_c+1; end;

  Move(fileBuf[inOut_c],b,1); inOut_c:=inOut_c+1;
  out_mixTable.tremoloToAllTracks := b and  32 > 0;
  out_mixTable.phaserToAllTracks  := b and  16 > 0;
  out_mixTable.reverbToAllTracks  := b and   8 > 0;
  out_mixTable.chorusToAllTracks  := b and   4 > 0;
  out_mixTable.panToAllTracks     := b and   2 > 0;
  out_mixTable.volumeToAllTracks  := b and   1 > 0;

  result:=True;
end;

function TGPTabFileReader.ReadBeatNote(var fileBuf:TBytesArray; var inOut_c:integer; var out_note:TNote; var out_errorMsg:string):boolean;
var b:byte;
begin
  result:=False;
  Move(fileBuf[inOut_c],b,1); inOut_c:=inOut_c+1;
  out_note.hasFingering            := b and 128 > 0;
  out_note.accentuated             := b and  64 > 0;
  out_note.hasType                 := b and  32 > 0;
  out_note.hasDynamic              := b and  16 > 0;
  out_note.hasEffects              := b and   8 > 0;
  out_note.ghostNote               := b and   4 > 0;
  out_note.dotted                  := b and   2 > 0;
  out_note.timeIndependentDuration := b and   1 > 0;

  if out_note.hasType                 then begin Move(fileBuf[inOut_c],out_note.noteType       ,1); inOut_c:=inOut_c+1; end;
  if out_note.timeIndependentDuration then begin
                                           Move(fileBuf[inOut_c],out_note.duration       ,1); inOut_c:=inOut_c+1;
                                           Move(fileBuf[inOut_c],out_note.tupletValue    ,1); inOut_c:=inOut_c+1;
                                           end;
  if out_note.hasDynamic              then begin Move(fileBuf[inOut_c],out_note.dynamicValue   ,1); inOut_c:=inOut_c+1; end;
  if out_note.hasType                 then begin Move(fileBuf[inOut_c],out_note.fretNumber     ,1); inOut_c:=inOut_c+1; end;
  if out_note.hasFingering            then begin
                                           Move(fileBuf[inOut_c],out_note.fingeringLValue,1); inOut_c:=inOut_c+1;
                                           Move(fileBuf[inOut_c],out_note.fingeringRValue,1); inOut_c:=inOut_c+1;
                                           end;
  if out_note.hasEffects and not ReadBeatNoteEffects(fileBuf,inOut_c,out_note.effects,out_errorMsg) then exit;

  result:=True;
end;

function TGPTabFileReader.ReadBeatNoteEffects(var fileBuf:TBytesArray; var inOut_c:integer; var out_effects:TNoteEffects; var out_errorMsg:string):boolean;
var b:byte;
begin
  result:=False;
  Move(fileBuf[inOut_c],b,1); inOut_c:=inOut_c+1; // header 1
  out_effects.hasGraceNote      := b and  16 > 0;
  out_effects.letRing           := b and   8 > 0;
  // skipped GP3 slide
  out_effects.hammerOnOrPullOf  := b and   2 > 0;
  out_effects.hasBend           := b and   1 > 0;

  Move(fileBuf[inOut_c],b,1); inOut_c:=inOut_c+1; // header 2
  out_effects.leftHandVibrato   := b and  64 > 0;
  out_effects.hasTrill          := b and  32 > 0;
  out_effects.hasHarmonic       := b and  16 > 0;
  out_effects.hasSlide          := b and   8 > 0;
  out_effects.hasTremoloPicking := b and   4 > 0;
  out_effects.palmMute          := b and   2 > 0;
  out_effects.staccato          := b and   1 > 0;

  if out_effects.hasBend      and not ReadEffectBend(fileBuf,inOut_c,out_effects.bend     ,out_errorMsg) then exit;
  if out_effects.hasGraceNote and not ReadGraceNote (fileBuf,inOut_c,out_effects.graceNote,out_errorMsg) then exit;
  if out_effects.hasTremoloPicking then begin Move(fileBuf[inOut_c],out_effects.tremoloPickingValue,1); inOut_c:=inOut_c+1; end;
  if out_effects.hasSlide          then begin Move(fileBuf[inOut_c],out_effects.slideValue         ,1); inOut_c:=inOut_c+1; end;
  if out_effects.hasHarmonic       then begin Move(fileBuf[inOut_c],out_effects.harmonicsValue     ,1); inOut_c:=inOut_c+1; end;
  if out_effects.hasTrill          then begin
                                        Move(fileBuf[inOut_c],out_effects.trillFret          ,1); inOut_c:=inOut_c+1;
                                        Move(fileBuf[inOut_c],out_effects.trillPeriod        ,1); inOut_c:=inOut_c+1;
                                        end;
  result:=True;
end;

function TGPTabFileReader.ReadGraceNote(var fileBuf:TBytesArray; var inOut_c:integer; var out_graceNote:TGraceNote; var out_errorMsg:string):boolean;
begin
  result:=False;
  Move(fileBuf[inOut_c],out_graceNote.fret        ,1); inOut_c:=inOut_c+1;
  Move(fileBuf[inOut_c],out_graceNote.dynamicValue,1); inOut_c:=inOut_c+1;
  Move(fileBuf[inOut_c],out_graceNote.transition  ,1); inOut_c:=inOut_c+1;
  Move(fileBuf[inOut_c],out_graceNote.duration    ,1); inOut_c:=inOut_c+1;
  result:=True;
end;

function TGPTabFileReader.ReadChordsDiagrams(var fileBuf:TBytesArray; var inOut_c:integer; var inOut_tab:TGPTabStructure; var out_errorMsg:string):boolean;
var i:integer;
begin
  result:=False;
  Move(fileBuf[inOut_c],inOut_tab.chordsDiagramsCount,4); inOut_c:=inOut_c+4;
  SetLength(inOut_tab.chordsDiagrams,inOut_tab.chordsDiagramsCount);
  for i:=0 to inOut_tab.chordsDiagramsCount-1 do
    if not ReadChordDiagram(fileBuf,inOut_c,inOut_tab.chordsDiagrams[i],out_errorMsg) then exit;

  result:=True;
end;

end.

