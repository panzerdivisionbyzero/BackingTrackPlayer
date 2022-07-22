{
	This unit is part of RPI Backing Track Player.

	Licensed under the terms of the GNU GPL 2.0 license,
	excluding used libraries:
	- United Openlibraries of Sound (uos) licensed under LGPL 2.1;
	and used code snippets marked with link to original source.

	Copyright (c) 2018-2022 by Pawel Witkowski
	pawel.vitek.witkowski@gmail.com
}
unit uGPTabFileStructureTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  midiChannelSize = 12;
  midiPortChannelsCount = 16;
  midiPortsCount = 4;

type
  TBytesArray=array of byte;

  TLyricsPage=record
    unknownInt:integer;
    text:string;
  end;

  TGPTabInfo=record
    title:string;
    subtitle:string;
    artist:string;
    album:string;
    author:string;
    copyright:string;
    tab:string;
    instructions:string;
    noticesLinesCount:integer;
    noticeLines:array of string;
    lyricsTrackNr:integer;
    lyrics:array [0..4] of TLyricsPage;
  end;

  TMidiChannel=record
    instrument:integer;
    volume:byte;
    balance:byte;
    chorus:byte;
    reverb:byte;
    phaser:byte;
    tremolo:byte;
    blank1:byte;
    blank2:byte;
  end;

  TMidiPort=record
    channels:array [0..midiPortChannelsCount-1] of TMidiChannel;
  end;

  TMeasureStructure=record
    hasDoubleBar:boolean;
    hasTonality:boolean;
    hasMarker:boolean;
    hasNumberOfAlternateEnding:boolean;
    hasEndOfRepeat:boolean;
    hasBeginningOfRepeat:boolean;
    hasDenominatorOfKeySignature:boolean;
    hasNumeratorOfKeySignature:boolean;
    numeratorValue:byte;
    denominatorValue:byte;
    endOfRepeatValue:byte;
    numberOfAlternateEndingValue:byte;
    markerText:string;
    markerColor:integer;
    tonalityValue:byte;
  end;

  TTrackStructure=record
    banjoTrack:boolean;
    twelveStringGuitar:boolean;
    drumsTrack:boolean;
    name:string;
    numOfStrings:integer;
    stringsTuning:array[0..6] of integer;
    port:integer;
    channel:integer;
    channelE:integer;
    numOfFrets:integer;
    capoHeight:integer;
    color:integer;
  end;

const
  // beat status:
  bsEmpty = 0;
  bsRest  = 2;
  // note duration:
  nd1 = -2; // whole note
  nd2 = -1; // half note
  nd4 =  0; // quarter note
  nd8 =  1; // eight note
  nd16 = 2; // sixteenth note
  nd32 = 3; // thirty-second note
  nd64 = 4; // sixty-fourth note

type
  TChordDiagram=record
    sharp:boolean;
    root:byte;
    chordType:byte;
    goesUntil:byte;
    lowestNote:integer;
    tonality:integer;
    hasAddedNote:boolean;
    name:string;
    fifthTonality:byte;
    ninthTonality:byte;
    eleventhTonality:byte;
    baseFret:integer;
    frets:array[0..6] of integer;
    numOfBarres:byte;
    barreFrets:array [0..4] of byte;
    barreStarts:array [0..4] of byte;
    barreEnds:array [0..4] of byte;
    omission1:byte;
    omission3:byte;
    omission5:byte;
    omission7:byte;
    omission9:byte;
    omission11:byte;
    omission13:byte;
    fingering:array [0..6] of byte;
    showDiagFingering:boolean;
  end;

  TBendPoint=record
    absoluteTimePos:integer; // for sure "absolute"? or another error in "documentation"?
    verticalPos:integer;
    vibrato:byte;
  end;

  TBend = record
    bendType:byte;
    value:integer;
    pointsCount:integer;
    points:array of TBendPoint;
  end;

  TBeatEffects=record
    hasTappingPoppingSlapping:boolean;
    hasTremoloBar:boolean;
    hasStroke:boolean;
    rasguedo:boolean;
    hasPickStroke:boolean;
    tappingPoppingSlappingValue:byte;
    tremoloBarValue:TBend;
    upStrokeValue:byte;
    downStrokeValue:byte;
    pickStrokeValue:byte;
  end;

  TMixTable=record
    instrument:byte;
    volume:byte;
    pan:byte;
    chorus:byte;
    reverb:byte;
    phaser:byte;
    tremolo:byte;
    tempo:integer;
    volumeChangeDuration:byte;
    panChangeDuration:byte;
    chorusChangeDuration:byte;
    reverbChangeDuration:byte;
    phaserChangeDuration:byte;
    tremoloChangeDuration:byte;
    tempoChangeDuration:byte;
    volumeToAllTracks:boolean;
    panToAllTracks:boolean;
    chorusToAllTracks:boolean;
    reverbToAllTracks:boolean;
    phaserToAllTracks:boolean;
    tremoloToAllTracks:boolean;
  end;

  TGraceNote=record
    fret:byte;
    dynamicValue:byte;
    transition:byte;
    duration:byte;
  end;

  TNoteEffects=record
    hasGraceNote:boolean;
    letRing:boolean;
    hammerOnOrPullOf:boolean;
    hasBend:boolean;
    leftHandVibrato:boolean;
    hasTrill:boolean;
    hasHarmonic:boolean;
    hasSlide:boolean;
    hasTremoloPicking:boolean;
    palmMute:boolean;
    staccato:boolean;
    bend:TBend;
    graceNote:TGraceNote;
    tremoloPickingValue:byte;
    slideValue:byte;
    harmonicsValue:byte;
    trillFret:byte;
    trillPeriod:byte;
  end;

  TNote=record
    hasFingering:boolean;
    accentuated:boolean;
    hasType:boolean;
    hasDynamic:boolean;
    hasEffects:boolean;
    ghostNote:boolean;
    dotted:boolean;
    timeIndependentDuration:boolean;
    noteType:word;
    duration:byte;
    tupletValue:byte;
    dynamicValue:byte;
    fretNumber:byte;
    fingeringLValue:byte;
    fingeringRValue:byte;
    effects:TNoteEffects;
  end;

  TBeat=record
    hasStatus:boolean;
    isTuplet:boolean;
    hasMixTable:boolean;
    hasEffects:boolean;
    hasText:boolean;
    hasChordDiagram:boolean;
    isDottedNote:boolean;
    status:byte;
    duration:byte;
    tupletValue:integer; // 3,5,6,7,9,10,11,12,13
    chordDiagram:TChordDiagram;
    text:string;
    effects:TBeatEffects;
    mixTable:TMixTable;
    usedStrings:byte; // bit by bit, where the least significant bit is the bottom line (the fattest string), assuming the "guitar" has 7 strings
    stringsNotes:array [0..6] of TNote; // notes from top to bottom
  end;

  TMeasureTrackPair=record
    beatsCount:integer;
    beats:array of TBeat;
  end;

  TMeasureTracks=array of TMeasureTrackPair;

  TGPTabStructure=record
    header:string;
    tabInfo:TGPTabInfo;
    tempo:integer;
    key:byte;
    octave:integer;
    midiPorts:array [0..midiPortsCount-1] of TMidiPort;
    measuresCount:integer;
    tracksCount:integer;
    measures:array of TMeasureStructure;
    tracks:array of TTrackStructure;
    measuresTracks:array of TMeasureTracks;
    chordsDiagramsCount:integer;
    chordsDiagrams:array of TChordDiagram;
  end;

implementation

end.

